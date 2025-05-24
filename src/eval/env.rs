use crate::value::{ValueObj, ValueRef};
use std::{
    collections::HashMap,
    rc::{Rc, Weak},
};

// TODO: these Box<str>s may need to be Rc<str>s

pub(crate) struct Frame {
    bindings: HashMap<Box<str>, ValueRef>,
    // TODO: is there ever more than one child?
    child: Option<Box<Frame>>,
    parent: Option<Weak<Frame>>,
    symbols: Weak<SymbolTable>,
}

impl Frame {
    pub(crate) fn root(symbols: Weak<SymbolTable>) -> Self {
        Self {
            bindings: HashMap::new(),
            child: None,
            parent: None,
            symbols,
        }
    }

    pub(crate) fn lookup(&self, name: &str) -> ValueObj {
        self.bindings.get(name).map(Rc::clone)
    }

    pub(crate) fn bind(&mut self, name: impl Into<Box<str>>, val: impl Into<ValueRef>) {
        self.bindings.insert(name.into(), val.into());
    }
}

#[derive(Default)]
pub(crate) struct SymbolTable {
    interned: Vec<Box<str>>,
}
