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
    AstModule,
    expr::{Expr, literal::Literal},
    stmt::{
        Stmt,
        decl::{Function as AstFunc, VarDecl},
    },
};
use symbol::SymbolMap;
use typing::TypeInfo;

pub mod expr;
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
        todo!("Translate constant expression: {expr:?}");
    }
    fn translate_string_literal(&mut self, string: &str) -> GlobalRef {
        expr::translate_string_literal(&mut self.ir_builder, &mut self.strings, string)
    }
}
