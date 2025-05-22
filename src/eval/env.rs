use crate::value::Value;
use std::{
    collections::HashMap,
    rc::{Rc, Weak},
};

pub(super) struct Frame {
    bindings: HashMap<String, Rc<Value>>,
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

pub(super) struct SymbolTable;
