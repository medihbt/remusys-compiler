use std::rc::Rc;

use remusys_ir::{
    ir::{module::Module as IRModule, util::builder::IRBuilder},
    typing::
        context::{PlatformPolicy, TypeContext}
    ,
};
use remusys_lang::
    ast::{stmt::{decl::VarDecl, Stmt}, AstModule}
;
use symbol::SymbolMap;

pub mod symbol;
pub mod typing;

pub struct IRTrsnalator {
    pub ir_builder: IRBuilder,
    pub type_ctx: Rc<TypeContext>,
    pub symbols: SymbolMap,
}

impl IRTrsnalator {
    pub fn new(ast_module: &AstModule, platform_policy: PlatformPolicy) -> Self {
        let type_ctx = TypeContext::new_rc(platform_policy);
        let ir_builder = IRBuilder::new(Rc::new(IRModule::new(
            ast_module.file.clone(),
            type_ctx.clone(),
        )));
        let symbols = SymbolMap::new();

        IRTrsnalator {
            ir_builder,
            type_ctx,
            symbols,
        }
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

    pub fn translate(&mut self, ast_module: &AstModule) -> Rc<IRModule> {
        todo!("Translate the AST module to IR");
    }

    fn translate_global_def(&mut self, global_decl: &Stmt) {
        match global_decl {
            Stmt::VarDecl(var_decl) => {
                self.translate_global_vardecl(var_decl);
            },
            Stmt::FuncDecl(function) => todo!(),
            Stmt::UnresolvedVarDecl(_) => panic!(
                "IR generator input should be a normalized SST, but got an unresolved variable declaration"
            ),
            _ => panic!("Statement NOT a global declaration"),
        }
    }
    fn translate_global_vardecl(&mut self, global_vars: &VarDecl) {
        todo!("Translate global variable declaration: {global_vars:?}");
    }
}
