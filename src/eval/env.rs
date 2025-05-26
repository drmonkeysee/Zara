use crate::value::ValueRef;
use std::{collections::HashMap, rc::Rc, time::Instant};

pub(crate) struct Frame<'a> {
    pub(crate) bnd: &'a mut Binding,
    pub(crate) sym: &'a SymbolTable,
    pub(crate) sys: &'a System,
}

#[derive(Default)]
pub(crate) struct Binding(HashMap<Box<str>, ValueRef>);

impl Binding {
    pub(crate) fn lookup(&self, name: &str) -> Option<ValueRef> {
        self.0.get(name).map(Rc::clone)
    }

    pub(crate) fn bind(&mut self, name: impl Into<Box<str>>, val: impl Into<ValueRef>) {
        self.0.insert(name.into(), val.into());
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
