use crate::{syntax::Namespace, value::Value};
use std::{
    collections::{HashMap, HashSet},
    time::Instant,
};

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

pub(super) struct EnvNamespace;

impl Namespace for EnvNamespace {
    fn name_defined(&self, name: &str) -> bool {
        todo!()
    }

    fn get_symbol(&self, symbol: &str) -> Value {
        todo!()
    }

    fn add_name(&mut self, name: &str) {
        todo!()
    }
}

#[derive(Default)]
pub(super) struct SimpleNamespace(HashSet<String>);

impl Namespace for SimpleNamespace {
    fn name_defined(&self, name: &str) -> bool {
        self.0.contains(name)
    }

    fn get_symbol(&self, symbol: &str) -> Value {
        Value::symbol(symbol)
    }

    fn add_name(&mut self, name: &str) {
        self.0.insert(name.to_owned());
    }
}
