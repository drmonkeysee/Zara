use crate::value::ValueRef;
use std::{
    collections::HashMap,
    rc::{Rc, Weak},
    time::Instant,
};

// TODO: these Box<str>s may need to be Rc<str>s

pub(crate) struct Frame {
    bindings: HashMap<Box<str>, ValueRef>,
    symbols: Weak<SymbolTable>,
    sys: Weak<System>,
    // TODO: add parent reference (weak or rc?)
}

impl Frame {
    pub(crate) fn root(symbols: Weak<SymbolTable>, sys: Weak<System>) -> Self {
        Self {
            bindings: HashMap::new(),
            symbols,
            sys,
        }
    }

    pub(crate) fn lookup(&self, name: &str) -> Option<ValueRef> {
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

pub(crate) struct System {
    // TODO: run_args, including zara or program file name
    pub(crate) start_time: Instant,
}

impl System {
    pub(crate) fn new() -> Self {
        Self {
            start_time: Instant::now(),
        }
    }
}
