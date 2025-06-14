use crate::value::Value;
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

    pub(crate) fn get_refs(&self) -> impl IntoIterator<Item = (&str, &Value)> {
        let mut vec = self
            .0
            .iter()
            .map(|(k, v)| (k.as_ref(), v))
            .collect::<Vec<_>>();
        vec.sort_by_key(|(k, _)| *k);
        vec
    }

    pub(crate) fn bind(&mut self, name: impl Into<Box<str>>, val: Value) {
        self.0.insert(name.into(), val);
    }
}

#[derive(Default)]
pub(crate) struct SymbolTable(HashSet<Rc<str>>);

impl SymbolTable {
    pub(crate) fn get_refs(&self) -> impl IntoIterator<Item = &Rc<str>> {
        let mut vec = self.0.iter().collect::<Vec<_>>();
        vec.sort();
        vec
    }

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

pub(crate) struct Namespace<'a>(pub(crate) Frame<'a>);

impl Namespace<'_> {
    pub(crate) fn name_defined(&self, name: &str) -> bool {
        todo!("env name defined")
    }

    pub(crate) fn get_symbol(&mut self, symbol: &str) -> Rc<str> {
        self.0.sym.get(symbol)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    mod bindings {
        use super::*;

        #[test]
        fn get_refs_empty() {
            let b = Binding::default();

            let all = b.get_refs();

            let vec = all.into_iter().collect::<Vec<_>>();
            assert!(vec.is_empty());
        }

        #[test]
        fn get_refs_single() {
            let mut b = Binding::default();
            b.bind("foo", Value::Unspecified);

            let all = b.get_refs();

            let keys = all.into_iter().map(|(k, _)| k).collect::<Vec<_>>();
            assert_eq!(keys, ["foo"]);
        }

        #[test]
        fn get_refs_alphabetical() {
            let mut b = Binding::default();
            b.bind("foo", Value::Unspecified);
            b.bind("bar", Value::Unspecified);
            b.bind("baz", Value::Unspecified);

            let all = b.get_refs();

            let keys = all.into_iter().map(|(k, _)| k).collect::<Vec<_>>();
            assert_eq!(keys, ["bar", "baz", "foo"]);
        }
    }

    mod symbols {
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

        #[test]
        fn get_refs_empty() {
            let s = SymbolTable::default();

            let all = s.get_refs();

            let vec = all.into_iter().collect::<Vec<_>>();
            assert!(vec.is_empty());
        }

        #[test]
        fn get_refs_single() {
            let mut s = SymbolTable::default();
            s.get("foo");

            let all = s.get_refs();

            let vec = all.into_iter().map(|s| Rc::as_ref(s)).collect::<Vec<_>>();
            assert_eq!(vec, ["foo"]);
        }

        #[test]
        fn get_refs_alphabetical() {
            let mut s = SymbolTable::default();
            s.get("foo");
            s.get("bar");
            s.get("baz");

            let all = s.get_refs();

            let vec = all.into_iter().map(|s| Rc::as_ref(s)).collect::<Vec<_>>();
            assert_eq!(vec, ["bar", "baz", "foo"]);
        }
    }
}
