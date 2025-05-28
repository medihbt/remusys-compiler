use std::{cell::Cell, collections::HashMap, rc::Rc};

use remusys_ir::{
    base::NullableValue,
    ir::{
        ValueSSA,
        constant::{
            data::ConstData,
            expr::{Array, ConstExprData},
        },
        global::{GlobalData, GlobalDataCommon, GlobalRef, Var, VarInner},
        module::Module as IRModule,
        util::builder::IRBuilder,
    },
    typing::{
        context::{PlatformPolicy, TypeContext},
        id::ValTypeID,
        types::FloatTypeKind,
    },
};
use remusys_lang::ast::{
    expr::{literal::Literal, Expr}, stmt::{decl::{Function as AstFunc, VarDecl}, Stmt}, AstModule
};
use symbol::SymbolMap;
use typing::TypeInfo;

pub mod symbol;
pub mod typing;

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

    pub fn translate(&mut self, ast_module: &AstModule) -> Rc<IRModule> {
        todo!("Translate the AST module to IR");
    }

    fn translate_global_def(&mut self, global_decl: &Stmt) {
        match global_decl {
            Stmt::VarDecl(var_decl) => {
                self.translate_global_vardecl(var_decl);
            }
            Stmt::FuncDecl(function) => todo!(),
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
        }
    }
    fn translate_func_decl(&mut self, function: &AstFunc) {
        todo!("Translate function declaration: {function:?}");
    }
    fn translate_const_expr(&mut self, expr: &Expr) -> ValueSSA {
        match expr {
            Expr::None => ValueSSA::ConstData(ConstData::Undef(ValTypeID::Void)),
            Expr::Literal(literal) => {
                let const_data = match literal {
                    Literal::Int(i) => ConstData::Int(32, *i as i128),
                    Literal::Float(f) => ConstData::Float(FloatTypeKind::Ieee32, *f as f64),
                };
                ValueSSA::ConstData(const_data)
            }
            Expr::String(s) => {
                let str_global_ref = self.translate_string_literal(s);
                ValueSSA::Global(str_global_ref)
            }
            Expr::ArrayInitList(arr) => todo!(),
            Expr::Ident(ident) => todo!(),
            Expr::ArrayIndex(array_index) => todo!(),
            Expr::BinOP(alu) => todo!(),
            Expr::CmpOP(cmp) => todo!(),
            Expr::ShortCircuit(logic) => todo!(),
            Expr::UnaryOP(unary_exp) => todo!(),
            Expr::Call(call) => todo!(),
            Expr::Assign(assign) => todo!(),
            Expr::ImplicitCast(cast) => todo!(),
            Expr::IntrinsicTimeStart(lineno) => {
                let time_start_inst = self
                    .ir_builder
                    .add_call_inst(
                        self.ast_intrinsic_funcs.start_time,
                        [ValueSSA::ConstData(ConstData::Int(32, *lineno as i128))].into_iter(),
                    )
                    .unwrap();
                ValueSSA::Inst(time_start_inst)
            }
            Expr::IntrinsicTimeEnd(lineno) => {
                let time_end_inst = self
                    .ir_builder
                    .add_call_inst(
                        self.ast_intrinsic_funcs.stop_time,
                        [ValueSSA::ConstData(ConstData::Int(32, *lineno as i128))].into_iter(),
                    )
                    .unwrap();
                ValueSSA::Inst(time_end_inst)
            }
            Expr::RawInitList(_) => todo!(),
        }
    }
    fn translate_string_literal(&mut self, string: &str) -> GlobalRef {
        if let Some(global_ref) = self.strings.get(string) {
            return global_ref.clone();
        }
        // String symbol name: mangle(`Core::Builtin::StringLiteral`).$id
        let str_name = format!("_Z4Core7Builtin14StringLiteral.{}", self.strings.len());
        let str_arrty = self.type_ctx.make_array_type(
            string.len() + 1,  // +1 for null terminator
            ValTypeID::Int(8), // char type
        );
        let str_initval = ConstExprData::Array(Array {
            arrty: str_arrty.clone(),
            elems: string
                .bytes()
                .map(|b| ValueSSA::ConstData(ConstData::Int(8, b as i128)))
                .chain(std::iter::once(ValueSSA::ConstData(ConstData::Int(
                    8, 0, // null terminator
                ))))
                .collect(),
        });
        let str_initval = self.ir_builder.module.insert_expr(str_initval);

        let str_global = Var {
            common: GlobalDataCommon {
                name: str_name.clone(),
                content_ty: ValTypeID::Array(str_arrty.clone()),
                self_ref: Cell::new(GlobalRef::new_null()),
            },
            inner: Cell::new(VarInner {
                readonly: true,
                align_log2: 0,
                init: ValueSSA::ConstExpr(str_initval),
            }),
        };
        let str_global_ref = self
            .ir_builder
            .module
            .insert_global(GlobalData::Var(str_global));
        self.strings
            .insert(string.to_string(), str_global_ref.clone());
        str_global_ref
    }
}
