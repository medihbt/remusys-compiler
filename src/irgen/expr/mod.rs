use std::{
    collections::HashMap,
    ops::{Deref, Range},
};

use remusys_ir::{
    base::APInt, ir::{
        Array as IRArrayExpr, ConstData as IRConstData, ConstExprData as IRExprData,
        ConstExprRef as IRExprRef, GlobalRef as IRGlobalRef, IRBuilder, Module as IRModule,
        ValueSSA as IRValue,
    }, typing::id::ValTypeID as IRTypeID
};
use remusys_lang::ast::expr::{
    Expr as AstExpr, initlist::ArrayInitList, literal::Literal as AstLiteral,
};

use super::TypeInfo;

pub(super) mod array;

pub(super) fn translate_string_literal(
    builder: &mut IRBuilder,
    str_map: &mut HashMap<String, IRGlobalRef>,
    literal: &str,
) -> IRGlobalRef {
    let module = &mut builder.module;
    if let Some(global_ref) = str_map.get(literal) {
        return global_ref.clone();
    }
    // String symbol name: mangle(`Core::Builtin::StringLiteral`).$id
    let str_name = format!("_Z4Core7Builtin14StringLiteral.{}", str_map.len());
    let str_arrty = module.type_ctx.make_array_type(
        literal.len() + 1, // +1 for null terminator
        IRTypeID::Int(8),  // char type
    );
    let str_initval = {
        let mut elems = Vec::with_capacity(literal.len() + 1);
        for b in literal.bytes() {
            elems.push(APInt::from(b).into());
        }
        elems.push(APInt::from(0u8).into()); // null terminator
        let str_initval = IRArrayExpr::from_vec(str_arrty, elems);

        let alloc = &mut module.allocs_mut().exprs;
        IRExprRef::from_alloc(alloc, IRExprData::Array(str_initval))
    };
    let str_var = builder
        .define_var(
            &str_name,
            true,
            IRTypeID::Array(str_arrty),
            IRValue::ConstExpr(str_initval),
        )
        .unwrap();
    str_map.insert(literal.to_string(), str_var);
    str_var
}

pub(super) fn translate_array_init_list_const(module: &IRModule, arr: &ArrayInitList) -> IRExprRef {
    let type_ctx = module.type_ctx.deref();
    let index_stack = array::ArrayInitStateStack::from_array(arr, type_ctx);
    index_stack.build_ir_const(module)
}
