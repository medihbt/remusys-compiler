//! Symbol mapping for the IR generation phase.

use std::{
    collections::HashMap, hash::{Hash, Hasher}, ops::Deref, rc::Rc
};

use remusys_ir::ir::{global::{func, GlobalRef}, inst::InstRef, ValueSSA};
use remusys_lang::ast::{expr::ident::Ident, stmt::decl::{Function, Variable}};

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
    pub variables: HashMap<VariableSymbol, InstRef>,
    pub functions: HashMap<FuncSymbol,   GlobalRef>,
}

impl SymbolMap {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
            functions: HashMap::new(),
        }
    }

    pub fn insert_variable(&mut self, var_symbol: VariableSymbol, inst_ref: InstRef) {
        self.variables.insert(var_symbol, inst_ref);
    }

    pub fn insert_function(&mut self, func_symbol: FuncSymbol, global_ref: GlobalRef) {
        self.functions.insert(func_symbol, global_ref);
    }

    pub fn get_variable(&self, var_symbol: &VariableSymbol) -> Option<&InstRef> {
        self.variables.get(var_symbol)
    }

    pub fn get_function(&self, func_symbol: &FuncSymbol) -> Option<&GlobalRef> {
        self.functions.get(func_symbol)
    }

    pub fn get_ident(&self, ident: &Ident) -> Option<ValueSSA> {
        match ident {
            Ident::Variable(weak) => {
                let var_symbol = VariableSymbol(weak.upgrade().unwrap());
                self.get_variable(&var_symbol).cloned().map(ValueSSA::Inst)
            },
            Ident::Func(weak) => {
                let func_symbol = FuncSymbol(weak.upgrade().unwrap());
                self.get_function(&func_symbol).cloned().map(ValueSSA::Global)
            },
            Ident::Unresolved(..) => panic!("Unresolved identifiers should not be queried in symbol map"),
        }
    }
}