use crate::value::Value;
use std::{
    collections::HashMap,
    rc::{Rc, Weak},
};

// TODO: these Box<str>s may need to be Rc<str>s

pub(super) struct Frame {
    bindings: HashMap<Box<str>, Rc<Value>>,
    // TODO: is there ever more than one child?
    child: Option<Box<Frame>>,
    parent: Option<Weak<Frame>>,
    symbols: Weak<SymbolTable>,
}

impl Frame {
    pub(super) fn new(symbols: impl Into<Weak<SymbolTable>>) -> Self {
        Self {
            bindings: HashMap::new(),
            child: None,
            parent: None,
            symbols: symbols.into(),
        }
    }
}

#[derive(Default)]
pub(super) struct SymbolTable {
    interned: Vec<Box<str>>,
}
