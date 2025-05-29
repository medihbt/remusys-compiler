use std::{
    collections::HashMap,
    ops::{Deref, Range},
};

use remusys_ir::{
    ir::{
        ValueSSA as IRValue,
        constant::{
            data::ConstData as IRConstData,
            expr::{Array as IRArrayExpr, ConstExprData as IRExprData, ConstExprRef as IRExprRef},
        },
        global::GlobalRef as IRGlobalRef,
        module::Module as IRModule,
        util::builder::IRBuilder,
    },
    typing::id::ValTypeID as IRTypeID,
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
    let module = builder.module.deref();
    if let Some(global_ref) = str_map.get(literal) {
        return global_ref.clone();
    }
    // String symbol name: mangle(`Core::Builtin::StringLiteral`).$id
    let str_name = format!("_Z4Core7Builtin14StringLiteral.{}", str_map.len());
    let str_arrty = module.type_ctx.make_array_type(
        literal.len() + 1, // +1 for null terminator
        IRTypeID::Int(8),  // char type
    );
    let str_initval = IRExprData::Array(IRArrayExpr {
        arrty: str_arrty.clone(),
        elems: literal
            .bytes()
            .map(|b| IRValue::ConstData(IRConstData::Int(8, b as i128)))
            .chain(std::iter::once(IRValue::ConstData(IRConstData::Int(
                8, 0, // null terminator
            ))))
            .collect(),
    });
    let str_initval = module.insert_expr(str_initval);
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
