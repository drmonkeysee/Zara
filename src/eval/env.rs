use crate::{syntax::Namespace, value::Value};
use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
    time::Instant,
};

pub(crate) struct Frame<'a> {
    pub(crate) scope: &'a mut Binding,
    pub(crate) sym: &'a mut SymbolTable,
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

#[derive(Default)]
pub(crate) struct SymbolTable(HashSet<Rc<str>>);

impl SymbolTable {
    fn get(&mut self, name: &str) -> Rc<str> {
        if let Some(s) = self.0.get(name) {
            Rc::clone(s)
        } else {
            self.0.insert(name.into());
            self.get(name)
        }
    }
}

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

pub(super) struct EnvNamespace<'a>(pub(super) Frame<'a>);

impl Namespace for EnvNamespace<'_> {
    fn name_defined(&self, name: &str) -> bool {
        todo!("env name defined")
    }

    fn get_symbol(&mut self, symbol: &str) -> Rc<str> {
        self.0.sym.get(symbol)
    }
}

pub(super) struct SimpleNamespace;

impl Namespace for SimpleNamespace {
    fn name_defined(&self, _name: &str) -> bool {
        false
    }

    fn get_symbol(&mut self, symbol: &str) -> Rc<str> {
        symbol.into()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn same_symbols() {
        let mut s = SymbolTable::default();

        let a = s.get("foo");
        let b = s.get("foo");

        assert!(Rc::ptr_eq(&a, &b));
    }

    #[test]
    fn different_symbols() {
        let mut s = SymbolTable::default();

        let a = s.get("foo");
        let b = s.get("bar");

        assert!(!Rc::ptr_eq(&a, &b));
    }
}
