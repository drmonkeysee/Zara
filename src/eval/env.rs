use crate::{string::SymbolTable, value::Value};
use std::{cell::RefCell, collections::HashMap, rc::Rc, time::Instant};

pub(crate) struct Frame<'a> {
    pub(crate) scope: Rc<Binding>,
    pub(crate) sym: &'a mut SymbolTable,
    pub(crate) sys: &'a System,
}

#[derive(Default)]
pub(crate) struct Binding(RefCell<HashMap<Rc<str>, Value>>);

impl Binding {
    pub(crate) fn bound(&self, name: &str) -> bool {
        self.0.borrow().contains_key(name)
    }

    pub(crate) fn lookup(&self, name: &str) -> Option<Value> {
        self.0.borrow().get(name).cloned()
    }

    pub(crate) fn sorted_bindings(&self) -> Vec<(Rc<str>, Value)> {
        let mut vec = self
            .0
            .borrow()
            .iter()
            .map(|(k, v)| (Rc::clone(k), v.clone()))
            .collect::<Vec<_>>();
        vec.sort_by(|(a, _), (b, _)| a.cmp(b));
        vec
    }

    pub(crate) fn bind(&self, name: Rc<str>, val: Value) {
        self.0.borrow_mut().insert(name, val);
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

pub(crate) struct Namespace<'a>(pub(crate) Frame<'a>);

impl Namespace<'_> {
    pub(crate) fn name_defined(&self, name: &str) -> bool {
        self.0.scope.bound(name)
    }

    pub(crate) fn get_symbol(&mut self, symbol: &str) -> Rc<str> {
        self.0.sym.get(symbol)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn get_refs_empty() {
        let b = Binding::default();

        let all = b.sorted_bindings();

        assert!(all.is_empty());
    }

    #[test]
    fn get_refs_single() {
        let b = Binding::default();
        b.bind("foo".into(), Value::Unspecified);

        let all = b.sorted_bindings();

        let keys = all.iter().map(|(k, _)| k.as_ref()).collect::<Vec<_>>();
        assert_eq!(keys, ["foo"]);
    }

    #[test]
    fn get_refs_alphabetical() {
        let b = Binding::default();
        b.bind("foo".into(), Value::Unspecified);
        b.bind("bar".into(), Value::Unspecified);
        b.bind("baz".into(), Value::Unspecified);

        let all = b.sorted_bindings();

        let keys = all.iter().map(|(k, _)| k.as_ref()).collect::<Vec<_>>();
        assert_eq!(keys, ["bar", "baz", "foo"]);
    }
}
