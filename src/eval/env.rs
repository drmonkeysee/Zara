use crate::{
    string::{Symbol, SymbolTable},
    value::Value,
};
use std::{cell::RefCell, collections::HashMap, rc::Rc, time::Instant};

pub(crate) struct Frame<'a> {
    pub(crate) scope: Rc<Binding>,
    pub(crate) sym: &'a SymbolTable,
    pub(crate) sys: &'a System,
}

#[derive(Debug, Default)]
pub(crate) struct Binding {
    parent: Option<Rc<Self>>,
    vars: RefCell<HashMap<Symbol, Value>>,
}

impl Binding {
    pub(crate) fn new(parent: impl Into<Rc<Self>>) -> Self {
        Self {
            parent: Some(parent.into()),
            ..Default::default()
        }
    }

    pub(crate) fn bound(&self, name: impl AsRef<str>) -> bool {
        self.local_bound(&name) || self.parent.as_ref().is_some_and(|p| p.bound(name))
    }

    pub(crate) fn binding(&self, name: impl AsRef<str>) -> Option<&Self> {
        self.local_bound(&name)
            .then_some(self)
            .or_else(|| self.parent.as_ref()?.binding(name))
    }

    pub(crate) fn lookup(&self, name: impl AsRef<str>) -> Option<Value> {
        self.vars
            .borrow()
            .get(name.as_ref())
            .cloned()
            .or_else(|| self.parent.as_ref()?.lookup(name))
    }

    pub(crate) fn bind(&self, name: Symbol, val: Value) {
        self.vars.borrow_mut().insert(name, val);
    }

    pub(crate) fn sorted_bindings(&self) -> Vec<(Symbol, Value)> {
        let mut vec = self
            .vars
            .borrow()
            .iter()
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect::<Vec<_>>();
        vec.sort_by(|(a, _), (b, _)| a.cmp(b));
        vec
    }

    fn local_bound(&self, name: impl AsRef<str>) -> bool {
        self.vars.borrow().contains_key(name.as_ref())
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
    pub(crate) fn name_defined(&self, name: impl AsRef<str>) -> bool {
        self.0.scope.bound(name)
    }

    pub(crate) fn get_closure(&self) -> Rc<Binding> {
        Rc::clone(&self.0.scope)
    }

    pub(crate) fn get_symbol(&self, name: impl AsRef<str>) -> Symbol {
        self.0.sym.get(name)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::testutil::some_or_fail;
    use std::ptr;

    #[test]
    fn empty_lookup() {
        let b = Binding::default();

        let v = b.lookup("foo");

        assert!(v.is_none());
        assert!(!b.bound("foo"));
    }

    #[test]
    fn basic_lookup() {
        let b = Binding::default();
        let sym = SymbolTable::default();
        b.bind(sym.get("foo"), Value::Boolean(true));

        let v = b.lookup("foo");

        assert!(matches!(some_or_fail!(v), Value::Boolean(true)));
        assert!(b.bound("foo"));
    }

    #[test]
    fn parent_lookup() {
        let p = Binding::default().into();
        let b = Binding::new(Rc::clone(&p));
        let sym = SymbolTable::default();
        b.bind(sym.get("bar"), Value::string("beef"));
        p.bind(sym.get("foo"), Value::Boolean(true));

        let v = b.lookup("bar");

        assert!(matches!(some_or_fail!(v), Value::String(s) if s.as_ref() == "beef"));
        assert!(b.bound("bar"));

        let v = b.lookup("foo");

        assert!(matches!(some_or_fail!(v), Value::Boolean(true)));
        assert!(b.bound("foo"));

        let v = b.lookup("baz");

        assert!(v.is_none());
        assert!(!b.bound("baz"));
    }

    #[test]
    fn empty_binding() {
        let b = Binding::default();

        let n = b.binding("foo");

        assert!(n.is_none());
    }

    #[test]
    fn basic_binding() {
        let b = Binding::default();
        let sym = SymbolTable::default();
        b.bind(sym.get("foo"), Value::Boolean(true));

        let n = b.binding("foo");

        assert!(ptr::eq(some_or_fail!(n), &b));
    }

    #[test]
    fn parent_binding() {
        let p = Binding::default().into();
        let b = Binding::new(Rc::clone(&p));
        let sym = SymbolTable::default();
        p.bind(sym.get("foo"), Value::Boolean(true));

        let n = b.binding("foo");

        assert!(ptr::eq(some_or_fail!(n), p.as_ref()));
    }

    #[test]
    fn get_refs_empty() {
        let b = Binding::default();

        let all = b.sorted_bindings();

        assert!(all.is_empty());
    }

    #[test]
    fn get_refs_single() {
        let b = Binding::default();
        let sym = SymbolTable::default();
        b.bind(sym.get("foo"), Value::Unspecified);

        let all = b.sorted_bindings();

        let keys = all.iter().map(|(k, _)| k.as_ref()).collect::<Vec<_>>();
        assert_eq!(keys, ["foo"]);
    }

    #[test]
    fn get_refs_alphabetical() {
        let b = Binding::default();
        let sym = SymbolTable::default();
        b.bind(sym.get("foo"), Value::Unspecified);
        b.bind(sym.get("bar"), Value::Unspecified);
        b.bind(sym.get("baz"), Value::Unspecified);

        let all = b.sorted_bindings();

        let keys = all.iter().map(|(k, _)| k.as_ref()).collect::<Vec<_>>();
        assert_eq!(keys, ["bar", "baz", "foo"]);
    }
}
