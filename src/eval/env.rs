use crate::value::Value;
use std::{
    collections::HashMap,
    rc::{Rc, Weak},
};

// TODO: these Box<str>s may need to be Rc<str>s

pub(crate) struct Frame {
    bindings: HashMap<Box<str>, Rc<Value>>,
    // TODO: is there ever more than one child?
    child: Option<Box<Frame>>,
    parent: Option<Weak<Frame>>,
    symbols: Weak<SymbolTable>,
}

impl Frame {
    pub(super) fn root(symbols: Weak<SymbolTable>) -> Self {
        Self {
            bindings: HashMap::new(),
            child: None,
            parent: None,
            symbols,
        }
    }

    pub(crate) fn lookup(&self, name: &str) -> Option<Rc<Value>> {
        self.bindings.get(name).map(Rc::clone)
    }

    pub(super) fn bind(&mut self, name: impl Into<Box<str>>, val: impl Into<Rc<Value>>) {
        self.bindings.insert(name.into(), val.into());
    }
}

#[derive(Default)]
pub(super) struct SymbolTable {
    interned: Vec<Box<str>>,
}
