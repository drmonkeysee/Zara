use crate::value::Value;
use std::{collections::HashMap, time::Instant};

pub(crate) struct Frame<'a> {
    pub(crate) scope: &'a mut Binding,
    #[allow(dead_code, reason = "not yet implemented")]
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
    pub(crate) args: Value,
    pub(crate) start_time: Instant,
}

impl System {
    pub(crate) fn new(args: impl IntoIterator<Item = String>) -> Self {
        Self {
            args: Value::list(args.into_iter().map(Value::string).collect::<Vec<_>>()),
            start_time: Instant::now(),
        }
    }
}
