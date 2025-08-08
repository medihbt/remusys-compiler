//! Symbol mapping for the IR generation phase.

use std::{
    collections::HashMap,
    hash::{Hash, Hasher},
    ops::Deref,
    rc::Rc,
};

use remusys_ir::{
    ir::{ValueSSA, GlobalRef},
    typing::types::FuncTypeRef,
};
use remusys_lang::ast::{
    expr::ident::Ident,
    stmt::decl::{Function, Variable},
};

use super::TypeInfo;

pub struct VariableSymbol(Rc<Variable>);
impl Hash for VariableSymbol {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Rc::as_ptr(&self.0).hash(state);
    }
}
impl PartialEq for VariableSymbol {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}
impl Eq for VariableSymbol {}
impl Deref for VariableSymbol {
    type Target = Variable;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl Clone for VariableSymbol {
    fn clone(&self) -> Self {
        VariableSymbol(Rc::clone(&self.0))
    }
}

pub struct FuncSymbol(Rc<Function>);
impl Hash for FuncSymbol {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Rc::as_ptr(&self.0).hash(state);
    }
}
impl PartialEq for FuncSymbol {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}
impl Eq for FuncSymbol {}
impl Deref for FuncSymbol {
    type Target = Function;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl Clone for FuncSymbol {
    fn clone(&self) -> Self {
        FuncSymbol(Rc::clone(&self.0))
    }
}

pub struct SymbolMap {
    pub variables: HashMap<VariableSymbol, (ValueSSA, TypeInfo)>,
    pub functions: HashMap<FuncSymbol, (GlobalRef, FuncTypeRef)>,
}

impl SymbolMap {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
            functions: HashMap::new(),
        }
    }

    pub fn insert_variable(
        &mut self,
        ast_var: Rc<Variable>,
        value_ref: ValueSSA,
        type_info: TypeInfo,
    ) {
        self.variables
            .insert(VariableSymbol(ast_var), (value_ref, type_info));
    }
    pub fn insert_function(
        &mut self,
        ast_func: Rc<Function>,
        global_ref: GlobalRef,
        func_type: FuncTypeRef,
    ) {
        self.functions
            .insert(FuncSymbol(ast_func), (global_ref, func_type));
    }

    pub fn get_variable(&self, var_symbol: &Rc<Variable>) -> Option<(ValueSSA, TypeInfo)> {
        self.variables
            .get(&VariableSymbol(var_symbol.clone()))
            .cloned()
    }
    pub fn get_function(&self, func_symbol: &Rc<Function>) -> Option<(GlobalRef, FuncTypeRef)> {
        self.functions
            .get(&FuncSymbol(func_symbol.clone()))
            .cloned()
    }

    pub fn get_ident(&self, ident: &Ident) -> Option<(ValueSSA, TypeInfo)> {
        match ident {
            Ident::Variable(weak) => self.get_variable(&weak.upgrade().unwrap()),
            Ident::Func(weak) => self
                .get_function(&weak.upgrade().unwrap())
                .map(|(gref, fty)| (ValueSSA::Global(gref), TypeInfo::Func(fty.clone()))),
            Ident::Unresolved(..) => {
                panic!("Unresolved identifiers should not be queried in symbol map")
            }
        }
    }
}
