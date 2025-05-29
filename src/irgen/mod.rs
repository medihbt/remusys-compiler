use std::{cell::Cell, collections::HashMap, hash::Hash, rc::Rc};

use remusys_ir::{
    base::NullableValue,
    ir::{
        ValueSSA,
        block::{BlockData as IRBlockData, BlockRef as IRBlockRef},
        constant::{
            data::ConstData,
            expr::{Array, ConstExprData},
        },
        global::{GlobalData, GlobalDataCommon, GlobalRef, Var, VarInner},
        module::Module as IRModule,
        util::builder::{IRBuilder, IRBuilderFocus},
    },
    typing::{
        context::{PlatformPolicy, TypeContext},
        id::ValTypeID,
        types::FloatTypeKind,
    },
};
use remusys_lang::ast::{
    expr::{literal::Literal, Expr as AstExpr}, stmt::{
        block::Block as AstBlock, decl::{Function as AstFunc, VarDecl}, whilestmt::WhileStmt, Stmt
    }, AstModule
};
use symbol::{SymbolMap, VariableSymbol};
use typing::TypeInfo;

pub mod expr;
pub mod symbol;
pub mod typing;

#[derive(Debug, Clone)]
struct WhileLoopRef(Rc<WhileStmt>);
impl Hash for WhileLoopRef {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        Rc::as_ptr(&self.0).hash(state);
    }
}
impl PartialEq for WhileLoopRef {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}
impl Eq for WhileLoopRef {}
impl Deref for WhileLoopRef {
    type Target = WhileStmt;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

struct WhileLoopInfo {
    entry_block: IRBlockRef,
    body_block:  IRBlockRef,
    exit_block:  IRBlockRef,
}

struct FunctionState {
    ir_func: GlobalRef,
    entry_block: IRBlockRef,
    curr_block:  IRBlockRef,
    while_loop_map: HashMap<WhileLoopRef, WhileLoopInfo>
}

pub struct IRTranslator {
    pub ir_builder: IRBuilder,
    pub type_ctx: Rc<TypeContext>,
    pub symbols: SymbolMap,
    pub strings: HashMap<String, GlobalRef>,
    ast_intrinsic_funcs: AstIntrinsicFuncs,
}

struct AstIntrinsicFuncs {
    start_time: GlobalRef,
    stop_time: GlobalRef,
}

impl Default for AstIntrinsicFuncs {
    fn default() -> Self {
        Self {
            start_time: GlobalRef::new_null(),
            stop_time: GlobalRef::new_null(),
        }
    }
}

impl IRTranslator {
    pub fn new(ast_module: &AstModule, platform_policy: PlatformPolicy) -> Self {
        let type_ctx = TypeContext::new_rc(platform_policy);
        let ir_builder = IRBuilder::new(Rc::new(IRModule::new(
            ast_module.file.clone(),
            type_ctx.clone(),
        )));
        let symbols = SymbolMap::new();

        let mut ret = Self {
            ir_builder,
            type_ctx,
            symbols,
            strings: HashMap::new(),
            ast_intrinsic_funcs: AstIntrinsicFuncs::default(),
        };
        ret.add_missing_timer_functions();
        ret
    }

    /// 选择 ARM64 赛道.
    pub fn new_competition(ast_module: &AstModule) -> Self {
        Self::new(
            ast_module,
            PlatformPolicy {
                ptr_nbits: 64,
                reg_nbits: 64,
            },
        )
    }

    fn add_missing_timer_functions(&mut self) {
        // type: Fn(i32) -> ()
        let timer_func_ty =
            self.type_ctx
                .make_func_type(&[ValTypeID::Int(32)], ValTypeID::Void, false);

        // void _sysy_starttime(int lineno);
        let starttime_func = self
            .ir_builder
            .declare_function("_sysy_starttime", timer_func_ty)
            .unwrap();

        // void _sysy_stoptime(int lineno);
        let stop_time_func = self
            .ir_builder
            .declare_function("_sysy_stoptime", timer_func_ty)
            .unwrap();

        self.ast_intrinsic_funcs.start_time = starttime_func;
        self.ast_intrinsic_funcs.stop_time = stop_time_func;
    }
}

impl IRTranslator {
    pub fn translate(mut self, ast_module: &AstModule) -> Rc<IRModule> {
        for gdef in ast_module.global_defs.iter() {
            self.translate_global_def(gdef);
        }
        self.ir_builder.module
    }

    fn translate_global_def(&mut self, global_decl: &Stmt) {
        match global_decl {
            Stmt::VarDecl(var_decl) => {
                self.translate_global_vardecl(var_decl);
            }
            Stmt::FuncDecl(ast_func) => self.translate_func_decl(ast_func),
            Stmt::UnresolvedVarDecl(_) => panic!(
                "IR generator input should be a normalized SST, but got an unresolved variable declaration"
            ),
            _ => panic!("Statement NOT a global declaration"),
        }
    }
    fn translate_global_vardecl(&mut self, global_vars: &VarDecl) {
        for var in global_vars.defs.iter() {
            let name = var.name.clone();
            let var_typeinfo = TypeInfo::new(&var.var_type, &self.type_ctx);
            let var_content_ty = var_typeinfo
                .get_alloca_data_type()
                .expect("Global variable type must be allocatable");
            let ir_global = {
                let initval = if let AstExpr::None = &var.initval {
                    ValueSSA::ConstData(ConstData::Zero(var_content_ty.clone()))
                } else {
                    // Translate the initializer expression
                    self.translate_expr_force_const(&var.initval, var_content_ty.clone())
                };
                self.ir_builder
                    .define_var(name.as_str(), var.is_const(), var_content_ty, initval)
                    .expect("Failed to define global variable")
            }
            .unwrap();
            // Insert the global variable into the symbol map
            self.symbols
                .insert_variable(var.clone(), ValueSSA::Global(ir_global));
        }
    }
    fn translate_func_decl(&mut self, ast_func: &Rc<AstFunc>) {
        if ast_func.is_intrinsic() {
            // Intrinsic function, no need to define it
            return;
        }
        let name = ast_func.name.as_str();
        let ret_ty = {
            let ret_typeinfo = TypeInfo::new(&ast_func.ret_type, &self.type_ctx);
            match ret_typeinfo {
                TypeInfo::Trivial(val_type_id) => val_type_id,
                _ => panic!(
                    "Global variable type must be a trivial type, but got: {:?}",
                    ret_typeinfo
                ),
            }
        };
        let arg_ty = ast_func
            .resolved_args
            .iter()
            .map(|arg| {
                let arg_ast_ty = &arg.var_type;
                let arg_typeinfo = TypeInfo::new(arg_ast_ty, &self.type_ctx);
                arg_typeinfo.get_value_type_id()
            })
            .collect::<Box<_>>();
        let func_ty = self
            .type_ctx
            .make_func_type(arg_ty.as_ref(), ret_ty, ast_func.is_vararg);

        if ast_func.is_extern() {
            // Extern function, just declare it
            let func_ref = self
                .ir_builder
                .declare_function(name, func_ty)
                .expect("Failed to declare extern function");
            // Insert the function into the symbol map
            self.symbols.insert_function(Rc::clone(ast_func), func_ref);
        } else {
            let func_ref = self
                .ir_builder
                .define_function_with_unreachable(name, func_ty)
                .expect(format!("Failed to define function `{}`", name).as_str());
            // Insert the function into the symbol map
            self.symbols
                .insert_function(Rc::clone(ast_func), func_ref.clone());

            let block_entry = self.ir_builder.get_focus_full().block;
            self.ir_builder
                .set_focus(IRBuilderFocus::Block(block_entry));

            // translate the arguments

            for (i, arg) in ast_func.resolved_args.iter().enumerate() {
                let arg_typeinfo = TypeInfo::new(&arg.var_type, &self.type_ctx);
                let arg_ir_value =
                    ValueSSA::FuncArg(self.ir_builder.get_focus_full().function, i as u32);

                // 值语义的参数类型 (int, float 就这两个) 需要 push 到栈上
                let arg_symbol_value = if arg_typeinfo.is_value_semantic() {
                    let arg_ir_type = arg_typeinfo.get_alloca_data_type().unwrap();
                    let arg_alloca = self.ir_builder.add_alloca_inst(arg_ir_type, 2).unwrap();
                    self.ir_builder
                        .add_store_inst(ValueSSA::Inst(arg_alloca), arg_ir_value, 4)
                        .unwrap();
                    ValueSSA::Inst(arg_alloca)
                } else {
                    // 引用语义的参数类型 (数组, 函数) 直接使用 SSA 值即可
                    arg_ir_value
                };

                // Insert the argument into the symbol map
                self.symbols.insert_variable(arg.clone(), arg_symbol_value);
            }

            // translate the function body
            let body = ast_func.body.borrow();
            let body = body.as_ref().expect("Function body should not be None");
            self.translate_function_body(ast_func, func_ref, body);
        }
    }
    fn translate_function_body(
        &mut self,
        ast_func: &Rc<AstFunc>,
        ir_func: GlobalRef,
        ast_body: &AstBlock,
    ) {
    }

    fn translate_expr_force_const(&mut self, expr: &AstExpr, type_req: ValTypeID) -> ValueSSA {
        match expr {
            AstExpr::None => ValueSSA::ConstData(ConstData::Zero(type_req)),
            AstExpr::Literal(Literal::Int(i)) => {
                ValueSSA::ConstData(ConstData::Int(32, *i as i128))
            }
            AstExpr::String(str) => {
                let str_ref =
                    expr::translate_string_literal(&mut self.ir_builder, &mut self.strings, str);
                ValueSSA::Global(str_ref)
            }
            AstExpr::ArrayInitList(arr) => {
                let arr_expr_ref =
                    expr::translate_array_init_list_const(&self.ir_builder.module, arr);
                ValueSSA::ConstExpr(arr_expr_ref)
            }
            _ => panic!("Expected a constant expression, but got: {:?}", expr),
        }
    }
    fn translate_expr_stmt(&mut self, expr: &AstExpr) -> ValueSSA {
        todo!("Translate expression statement");
    }
}

#[cfg(test)]
mod testing {
    use remusys_ir::ir::util::writer::write_ir_module;

    use super::*;

    #[test]
    fn test_translator() {
        let ast_module = remusys_lang::parser::parse_sysy_file("target/main.sy");
        let ast_module = remusys_lang::normalize::AstNormalizer::new(&ast_module).normalize();
        let ast_module = Rc::new(ast_module);
        let translator = IRTranslator::new_competition(&ast_module);
        let ir_module = translator.translate(&ast_module);
        let out_file = std::fs::File::create("target/main.ir").unwrap();
        let mut out_file = std::io::BufWriter::new(out_file);
        write_ir_module(&ir_module, &mut out_file, false, false, false);
    }
}
