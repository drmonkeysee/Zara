use crate::value::Value;
use std::{
    cell::RefCell,
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
pub(crate) struct Binding(RefCell<HashMap<Rc<str>, Value>>);

impl Binding {
    pub(crate) fn bound(&self, name: &str) -> bool {
        self.0.borrow().contains_key(name)
    }

    pub(crate) fn lookup(&self, name: &str) -> Option<Value> {
        self.0.borrow().get(name).cloned()
    }

    pub(crate) fn all_bindings(&self) -> impl IntoIterator<Item = (Rc<str>, Value)> {
        // NOTE: this needs to clone because the borrow() Ref guard ends up not
        // living long enough to return a collection of references.
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

#[derive(Default)]
pub(crate) struct SymbolTable(HashSet<Rc<str>>);

impl SymbolTable {
    pub(crate) fn get(&mut self, name: &str) -> Rc<str> {
        if let Some(s) = self.0.get(name) {
            Rc::clone(s)
        } else {
            self.0.insert(name.into());
            self.get(name)
        }
    }

    pub(crate) fn get_refs(&self) -> impl IntoIterator<Item = &Rc<str>> {
        let mut vec = self.0.iter().collect::<Vec<_>>();
        vec.sort();
        vec
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

    mod bindings {
        use super::*;

        #[test]
        fn get_refs_empty() {
            let b = Binding::default();

            let all = b.all_bindings();

            let vec = all.into_iter().collect::<Vec<_>>();
            assert!(vec.is_empty());
        }

        #[test]
        fn get_refs_single() {
            let b = Binding::default();
            b.bind("foo".into(), Value::Unspecified);

            let all = b.all_bindings();

            let keys = all
                .into_iter()
                .map(|(k, _)| k.as_ref().to_owned())
                .collect::<Vec<_>>();
            assert_eq!(keys, ["foo"]);
        }

        #[test]
        fn get_refs_alphabetical() {
            let b = Binding::default();
            b.bind("foo".into(), Value::Unspecified);
            b.bind("bar".into(), Value::Unspecified);
            b.bind("baz".into(), Value::Unspecified);

            let all = b.all_bindings();

            let keys = all
                .into_iter()
                .map(|(k, _)| k.as_ref().to_owned())
                .collect::<Vec<_>>();
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
