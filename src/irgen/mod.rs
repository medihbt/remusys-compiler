use std::{cell::Cell, collections::HashMap, hash::Hash, ops::Deref, rc::Rc};

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
        opcode::Opcode,
        util::builder::{IRBuilder, IRBuilderFocus},
    },
    typing::{
        context::{PlatformPolicy, TypeContext},
        id::ValTypeID,
        types::FloatTypeKind,
    },
};
use remusys_lang::{
    ast::{
        AstModule,
        expr::{Expr as AstExpr, ident::Ident, literal::Literal, unaryop::ImplicitCast},
        operator::Operator,
        stmt::{
            Stmt,
            block::Block as AstBlock,
            decl::{Function as AstFunc, VarDecl},
            whilestmt::WhileStmt,
        },
    },
    typing::AstType,
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
    body_block: IRBlockRef,
    exit_block: IRBlockRef,
}

struct FunctionState {
    ir_func: GlobalRef,
    entry_block: IRBlockRef,
    curr_block: IRBlockRef,
    while_loop_map: HashMap<WhileLoopRef, WhileLoopInfo>,
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
}

impl IRTranslator {
    fn translate_function_body(
        &mut self,
        ast_func: &Rc<AstFunc>,
        func_ref: GlobalRef,
        body: &AstBlock,
    ) {
        let entry_block = self.ir_builder.get_focus_full().block;
        let entry_next = self
            .ir_builder
            .split_current_block_from_terminator()
            .unwrap();
        let mut func_state = FunctionState {
            ir_func: func_ref,
            entry_block,
            curr_block: entry_next,
            while_loop_map: HashMap::new(),
        };
        self.ir_builder.set_focus(IRBuilderFocus::Block(entry_next));

        // 调整结束语句: 如果函数返回类型是 void 则把末尾的换成 `ret void` 指令
        if let AstType::Void = ast_func.ret_type {
            self.ir_builder.focus_set_return(ValueSSA::None).unwrap();
        }

        // Translate the function body block
        self.translate_block(&mut func_state, body);
    }

    fn translate_block(&mut self, func_stat: &mut FunctionState, ast_block: &AstBlock) {
        for (idx, stmt) in ast_block.stmts.iter().enumerate() {
            let is_block_end = idx == ast_block.stmts.len() - 1;
            self.translate_stmt(func_stat, stmt, is_block_end);
        }
    }

    fn translate_stmt(&mut self, func_stat: &mut FunctionState, stmt: &Stmt, is_block_end: bool) {
        match stmt {
            Stmt::Block(block) => self.translate_block(func_stat, &block),
            Stmt::VarDecl(local) => todo!(),
            Stmt::If(if_stmt) => todo!(),
            Stmt::While(while_stmt) => todo!(),
            Stmt::ExprStmt(expr_stmt) => todo!(),
            Stmt::BreakTo(weak) => todo!(),
            Stmt::ContinueTo(weak) => todo!(),
            Stmt::Return(expr) => {
                let retval = self.translate_alu_expr(func_stat, expr);
                let curr_block = func_stat.curr_block;
                let (retval_block, new_focus) = if is_block_end {
                    // If this is the last statement in the block, we can use the current block
                    // as the return block.
                    (curr_block, curr_block)
                } else {
                    // Otherwise, we need to create a new block for the return statement.
                    let next_block = self
                        .ir_builder
                        .split_current_block_from_terminator()
                        .unwrap();
                    func_stat.curr_block = next_block;
                    (curr_block, next_block)
                };
                self.ir_builder
                    .set_focus(IRBuilderFocus::Block(retval_block));
                // Add the return instruction
                self.ir_builder
                    .focus_set_return(retval)
                    .expect("Failed to set return value");
                // Set the focus to the new block
                self.ir_builder.set_focus(IRBuilderFocus::Block(new_focus));
            }

            Stmt::FuncDecl(_) => {
                panic!(
                    "Function declaration should not be in function body: {:?}",
                    func_stat.ir_func
                );
            }
            Stmt::Break | Stmt::Continue | Stmt::UnresolvedVarDecl(_) => {
                panic!("Unexpected statement in function body: {:?}", stmt);
            }
            Stmt::None => { /* Do nothing for empty statements */ }
        }
    }

    fn translate_alu_expr(&mut self, func_stat: &mut FunctionState, expr: &AstExpr) -> (ValueSSA, TypeInfo) {
        match expr {
            AstExpr::None => (ValueSSA::None, TypeInfo::Trivial(ValTypeID::Void)),
            AstExpr::Literal(Literal::Int(i)) => {
                (
                    ValueSSA::ConstData(ConstData::Int(32, *i as i128)),
                    TypeInfo::Trivial(ValTypeID::Int(32)),
                )
            }
            AstExpr::Literal(Literal::Float(f)) => {
                (
                    ValueSSA::ConstData(ConstData::Float(FloatTypeKind::Ieee32, *f as f32)),
                    TypeInfo::Trivial(ValTypeID::Float(FloatTypeKind::Ieee32)),
                )
            }
            AstExpr::String(s) => {
                let str_ref =
                    expr::translate_string_literal(&mut self.ir_builder, &mut self.strings, s);
                ValueSSA::Global(str_ref)
            }
            AstExpr::Ident(ident) => {
                // 在规范化的 SST 中, 标识符这种左值要参与数学运算的话,
                // 一定要经过一步 `LValueToRValue` 的转换. 因此这里
                // 取到标识符指针的时候就不要再加一个 `load` 指令了.
                self.symbols.get_ident(ident).unwrap_or_else(|| {
                    panic!("Identifier `{}` not found in symbol map", ident.get_name())
                })
            }
            AstExpr::ArrayIndex(idx) => {
                let array = self.translate_alu_expr(func_stat, &idx.array);
                let indices = idx
                    .indices
                    .iter()
                    .map(|i| self.translate_alu_expr(func_stat, i));
                let array_typeinfo = TypeInfo::new(&idx.array.ast_type, &self.type_ctx);
            }
            AstExpr::BinOP(bin_exp) => todo!(),
            AstExpr::CmpOP(cmp_exp) => todo!(),
            AstExpr::UnaryOP(unary) => todo!(),
            AstExpr::ImplicitCast(cast) => self.translate_alu_implicit_cast(func_stat, cast),
            AstExpr::Call(call) => todo!(),
            AstExpr::Assign(assign) => {
                // Translate the left-hand side expression
                let lhs = self.translate_alu_expr(func_stat, &assign.lhs);
                // Translate the right-hand side expression
                let rhs = self.translate_alu_expr(func_stat, &assign.rhs);

                // Perform the assignment operation
                let assign_inst = self.ir_builder.add_store_inst(lhs, rhs, 4).unwrap();
                ValueSSA::Inst(assign_inst)
            }

            AstExpr::IntrinsicTimeStart(lineno) => {
                let lineno_value = ValueSSA::ConstData(ConstData::Int(32, *lineno as i128));
                let call_inst = self
                    .ir_builder
                    .add_call_inst(
                        self.ast_intrinsic_funcs.start_time,
                        [lineno_value].into_iter(),
                    )
                    .unwrap();
                ValueSSA::Inst(call_inst)
            }
            AstExpr::IntrinsicTimeEnd(lineno) => {
                let lineno_value = ValueSSA::ConstData(ConstData::Int(32, *lineno as i128));
                let call_inst = self
                    .ir_builder
                    .add_call_inst(
                        self.ast_intrinsic_funcs.stop_time,
                        [lineno_value].into_iter(),
                    )
                    .unwrap();
                ValueSSA::Inst(call_inst)
            }

            AstExpr::ArrayInitList(_) => {
                panic!("Array initializer should not be in an ALU expression context")
            }
            AstExpr::ShortCircuit(_) => {
                panic!("Short-circuit expression should not be in an ALU expression context")
            }
            AstExpr::RawInitList(_) => {
                panic!("Discovered an AST-only raw initializer expression")
            }
        }
    }

    fn translate_alu_implicit_cast(
        &mut self,
        func_stat: &mut FunctionState,
        cast: &ImplicitCast,
    ) -> ValueSSA {
        let operand = self.translate_alu_expr(func_stat, &cast.expr);
        let target_ast_ty = &cast.target;
        let target_typeinfo = TypeInfo::new(target_ast_ty, &self.type_ctx);
        let inst = match cast.kind {
            Operator::ItoF => self.ir_builder.add_cast_inst(
                Opcode::Sitofp,
                ValTypeID::Float(FloatTypeKind::Ieee32),
                operand,
            ),
            Operator::FtoI => {
                self.ir_builder
                    .add_cast_inst(Opcode::Fptosi, ValTypeID::Int(32), operand)
            }
            Operator::BoolToInt => self.ir_builder.add_select_inst(
                operand,
                ConstData::make_int_valssa(32, 1),
                ConstData::make_int_valssa(32, 0),
            ),
            Operator::BoolToFloat => self.ir_builder.add_select_inst(
                operand,
                ConstData::make_float_valssa(FloatTypeKind::Ieee32, 1.0),
                ConstData::make_float_valssa(FloatTypeKind::Ieee32, 0.0),
            ),
            Operator::LValueToRValue => self.ir_builder.add_load_inst(
                target_typeinfo.get_alloca_data_type().unwrap(),
                4,
                operand,
            ),
            _ => panic!(
                "Unsupported implicit cast operator: {:?} in expression: {:?}",
                cast.kind, cast.expr
            ),
        }
        .unwrap();
        ValueSSA::Inst(inst)
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
