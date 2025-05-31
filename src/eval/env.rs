use crate::value::Value;
use std::{collections::HashMap, time::Instant};

pub(crate) struct Frame<'a> {
    pub(crate) bnd: &'a mut Binding,
    pub(crate) sym: &'a SymbolTable,
    pub(crate) sys: &'a System,
}

#[derive(Default)]
pub(crate) struct Binding(HashMap<Box<str>, Value>);

impl Binding {
    pub(crate) fn lookup(&self, name: &str) -> Option<Value> {
        self.0.get(name).cloned()
    }

    pub(crate) fn bind(&mut self, name: impl Into<Box<str>>, val: Value) {
        self.0.insert(name.into(), val);
    }
}

pub(crate) struct SymbolTable;

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
