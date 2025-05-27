use std::rc::Rc;

use remusys_ir::{
    ir::{module::Module as IRModule, util::builder::IRBuilder},
    typing::context::{PlatformPolicy, TypeContext},
};
use remusys_lang::ast::AstModule;

/// Placeholder for the actual implementation.
/// This function would contain the logic to convert the AST to IR.
pub fn convert_ast_to_ir(ast: &AstModule) -> IRBuilder {
    let type_ctx = get_target_arch();
    let module = IRModule::new(ast.file.clone(), type_ctx);
    IRBuilder::new(Rc::new(module))
}

/// 我手里刚好有个树莓派, 所以打得是 aarch64 赛道的.
fn get_target_arch() -> Rc<TypeContext> {
    let platform = PlatformPolicy {
        ptr_nbits: 64,
        reg_nbits: 64,
    };
    TypeContext::new_rc(platform)
}
