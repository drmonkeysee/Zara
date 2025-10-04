use super::{ValItem, Value};
use crate::{
    lex::{DisplayTokenLines, TokenLinesMessage},
    string::{CharDatum, StrDatum},
};
use std::{
    collections::{HashMap, HashSet},
    fmt::{self, Display, Formatter, Write},
    ptr,
    slice::Iter,
};

pub(crate) struct Cycle(pub(super) NodeId, pub(super) Value);

impl Cycle {
    pub(crate) fn value(&self) -> &Value {
        &self.1
    }
}

pub(crate) struct Datum<'a>(pub(super) &'a Value);

impl Display for Datum<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.0 {
            Value::Ast(prg) => write!(f, "{{{prg:?}}}"),
            Value::Boolean(b) => write!(f, "#{}", if *b { 't' } else { 'f' }),
            Value::ByteVector(bv) => write_seq("#u8", bv.iter(), f),
            Value::ByteVectorMut(bv) => write_seq("#u8", bv.borrow().iter(), f),
            Value::Character(c) => write!(f, "#\\{}", CharDatum::new(*c)),
            Value::Error(c) => c.fmt(f),
            Value::Intrinsic(p) => p.fmt(f),
            Value::Null => f.write_str("()"),
            Value::Number(n) => n.fmt(f),
            Value::Pair(_) | Value::PairMut(_) => PairDatum(self.0).fmt(f),
            Value::Procedure(p) => p.fmt(f),
            Value::String(s) => StrDatum(s).fmt(f),
            Value::StringMut(s) => StrDatum(&s.borrow()).fmt(f),
            Value::Symbol(s) => s.as_datum().fmt(f),
            Value::TokenList(lines) => DisplayTokenLines(lines).fmt(f),
            Value::Unspecified => f.write_str("#<unspecified>"),
            Value::Vector(v) => write_seq("#", v.iter().map(Value::as_datum), f),
            Value::VectorMut(v) => write_seq("#", v.borrow().iter().map(Value::as_datum), f),
        }
    }
}

pub(crate) struct ValueMessage<'a>(pub(super) &'a Value);

impl Display for ValueMessage<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self.0 {
            Value::Ast(prg) => writeln!(f, "{prg:#?}"),
            Value::TokenList(lines) => TokenLinesMessage(lines).fmt(f),
            _ => Ok(()),
        }
    }
}

pub(crate) struct TypeName<'a>(pub(super) &'a Value);

impl TypeName<'_> {
    pub(crate) const BOOL: &'static str = "boolean";
    pub(crate) const BYTEVECTOR: &'static str = "bytevector";
    pub(crate) const CHAR: &'static str = "character";
    pub(crate) const ERROR: &'static str = "error condition";
    pub(crate) const IMPLIST: &'static str = "improper list";
    pub(crate) const LIST: &'static str = "list";
    pub(crate) const NUMBER: &'static str = "number";
    pub(crate) const PAIR: &'static str = "pair";
    pub(crate) const STRING: &'static str = "string";
    pub(crate) const SYMBOL: &'static str = "symbol";
    pub(crate) const VECTOR: &'static str = "vector";
}

impl Display for TypeName<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.0 {
            Value::Ast(_) => f.write_str("abstract syntax tree"),
            Value::Boolean(_) => f.write_str(Self::BOOL),
            Value::ByteVector(_) | Value::ByteVectorMut(_) => f.write_str(Self::BYTEVECTOR),
            Value::Character(_) => f.write_str(Self::CHAR),
            Value::Error(_) => f.write_str(Self::ERROR),
            Value::Intrinsic(_) => f.write_str("intrinsic"),
            Value::Null => f.write_str("null"),
            Value::Number(_) => f.write_str(Self::NUMBER),
            Value::Pair(p) => f.write_str(p.typename()),
            Value::PairMut(p) => f.write_str(p.borrow().typename()),
            Value::Procedure(_) => f.write_str("procedure"),
            Value::String(_) | Value::StringMut(_) => f.write_str(Self::STRING),
            Value::Symbol(_) => f.write_str(Self::SYMBOL),
            Value::TokenList(_) => f.write_str("token list"),
            Value::Unspecified => f.write_str("unspecified"),
            Value::Vector(_) | Value::VectorMut(_) => f.write_str(Self::VECTOR),
        }
    }
}

// NOTE: pairs and vectors are identified via their untyped pointer address
pub(super) type NodeId = *const ();

struct PairDatum<'a>(&'a Value);

// TODO: the .as_datum() calls need to be dependent on top-level display
impl Display for PairDatum<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let mut it = self.0.iter();
        if let Some(ValItem::Element(v)) = it.next()
            && let Some(p) = v.as_refpair()
        {
            write!(f, "({}", p.as_ref().car.as_datum())?;
        } else {
            unreachable!("expected pair value iterator");
        }
        for item in it {
            match item {
                ValItem::Cycle(_) => {
                    f.write_str(" . dup…")?;
                    break;
                }
                ValItem::Element(v) => {
                    if let Some(p) = v.as_refpair() {
                        write!(f, " {}", p.as_ref().car.as_datum())?;
                    } else if !matches!(v, Value::Null) {
                        write!(f, " . {}", v.as_datum())?;
                    }
                }
            }
        }
        f.write_char(')')
    }
}

struct VecIterator<'a> {
    head: &'a [Value],
    it: Iter<'a, Value>,
}

impl<'a> VecIterator<'a> {
    fn new(head: &'a [Value]) -> Self {
        Self {
            head,
            it: head.iter(),
        }
    }
}

impl Iterator for VecIterator<'_> {
    type Item = ValItem;

    fn next(&mut self) -> Option<Self::Item> {
        let v = self.it.next()?;
        if let Some(vec) = v.as_refvec()
            && ptr::eq(vec.as_ref(), self.head)
        {
            Some(ValItem::Cycle(Cycle(self.head.as_ptr().cast(), v.clone())))
        } else {
            Some(ValItem::Element(v.clone()))
        }
    }
}

#[derive(Default)]
struct Visits(HashMap<NodeId, bool>);

impl Visits {
    fn scan(&mut self, v: &Value) {
        if v.as_refpair().is_some() {
            self.scan_pair(v);
        } else if let Some(vec) = v.as_refvec() {
            self.scan_vec(vec.as_ref());
        }
    }

    fn all_cycles(self) -> HashSet<NodeId> {
        self.0
            .into_iter()
            .filter_map(|(k, v)| v.then_some(k))
            .collect()
    }

    fn scan_pair(&mut self, v: &Value) {
        for item in v.iter() {
            match item {
                ValItem::Cycle(Cycle(id, _)) => {
                    if !self.0.contains_key(&id) {
                        self.0.insert(id, true);
                    }
                    break;
                }
                ValItem::Element(mut v) => {
                    v = if let Some(p) = v.as_refpair() {
                        p.as_ref().car.clone()
                    } else {
                        v
                    };
                    self.scan(&v);
                }
            }
        }
    }

    fn scan_vec(&mut self, v: &[Value]) {
        let vref_id = v.as_ptr().cast();
        match self.0.get_mut(&vref_id) {
            None => {
                // NOTE: keep track of all vector references because a vector cycle
                // may be a nested vector referencing an outer vector and without
                // "remembering" outer vectors we never see the cycle via VecIterator
                // (because the cycle does not exist in any particular vector instance).
                self.0.insert(vref_id, false);
                for item in VecIterator::new(v) {
                    match item {
                        ValItem::Cycle(Cycle(id, _)) => {
                            self.0.insert(id, true);
                            break;
                        }
                        ValItem::Element(v) => self.scan(&v),
                    }
                }
            }
            Some(cyc) => *cyc = true,
        }
    }
}

fn write_seq<T: Display>(
    prefix: &str,
    seq: impl Iterator<Item = T>,
    f: &mut Formatter<'_>,
) -> fmt::Result {
    write!(
        f,
        "{prefix}({})",
        seq.map(|v| v.to_string()).collect::<Vec<_>>().join(" ")
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    mod visits {
        use super::*;
        use crate::value::Pair;
        use std::{cell::RefCell, rc::Rc};

        #[test]
        fn simple_value() {
            // 5
            let v = Value::real(5);
            let mut c = Visits::default();

            c.scan(&v);

            assert!(c.all_cycles().is_empty());
        }

        #[test]
        fn normal_list() {
            // (1 2 3)
            let v = zlist![Value::real(1), Value::real(2), Value::real(3)];
            let mut c = Visits::default();

            c.scan(&v);

            assert!(c.all_cycles().is_empty());
        }

        #[test]
        fn circular_list() {
            // #0=(1 2 3 . #0#)
            let end = RefCell::new(Pair {
                car: Value::real(3),
                cdr: Value::Null,
            })
            .into();
            let p = Pair {
                car: Value::real(1),
                cdr: Value::cons(Value::real(2), Value::PairMut(Rc::clone(&end))),
            }
            .into();
            end.borrow_mut().cdr = Value::Pair(Rc::clone(&p));
            let v = Value::Pair(Rc::clone(&p));
            let mut c = Visits::default();

            c.scan(&v);

            assert_eq!(c.all_cycles().len(), 1);
        }

        #[test]
        fn partly_circular_list() {
            // (1 2 . #0=(3 4 5 . #0#))
            let end = RefCell::new(Pair {
                car: Value::real(5),
                cdr: Value::Null,
            })
            .into();
            let start = Pair {
                car: Value::real(3),
                cdr: Value::cons(Value::real(4), Value::PairMut(Rc::clone(&end))),
            }
            .into();
            end.borrow_mut().cdr = Value::Pair(Rc::clone(&start));
            let p = Pair {
                car: Value::real(1),
                cdr: Value::cons(Value::real(2), Value::Pair(Rc::clone(&start))),
            }
            .into();
            let v = Value::Pair(Rc::clone(&p));
            let mut c = Visits::default();

            c.scan(&v);

            assert_eq!(c.all_cycles().len(), 1);
        }

        #[test]
        fn nested_circular_list() {
            // #0=(#1=(9 8 . #1#) 2 3 . #0#)
            let nested_end = RefCell::new(Pair {
                car: Value::real(8),
                cdr: Value::Null,
            })
            .into();
            let nested_head = Pair {
                car: Value::real(9),
                cdr: Value::PairMut(Rc::clone(&nested_end)),
            }
            .into();
            nested_end.borrow_mut().cdr = Value::Pair(Rc::clone(&nested_head));
            let end = RefCell::new(Pair {
                car: Value::real(3),
                cdr: Value::Null,
            })
            .into();
            let p = Pair {
                car: Value::Pair(Rc::clone(&nested_head)),
                cdr: Value::cons(Value::real(2), Value::PairMut(Rc::clone(&end))),
            }
            .into();
            end.borrow_mut().cdr = Value::Pair(Rc::clone(&p));
            let v = Value::Pair(Rc::clone(&p));
            let mut c = Visits::default();

            c.scan(&v);

            assert_eq!(c.all_cycles().len(), 2);
        }

        #[test]
        fn simple_vector() {
            // #(1 2 3)
            let v = Value::vector([Value::real(1), Value::real(2), Value::real(3)]);
            let mut c = Visits::default();

            c.scan(&v);

            assert!(c.all_cycles().is_empty());
        }

        #[test]
        fn cyclic_vector() {
            // #0=#(1 2 #0#)
            let vec = Rc::new(RefCell::new(vec![Value::real(1), Value::real(2)]));
            let v = Value::VectorMut(Rc::clone(&vec));
            vec.borrow_mut().push(v.clone());
            let mut c = Visits::default();

            c.scan(&v);

            assert_eq!(c.all_cycles().len(), 1);
        }

        #[test]
        fn partly_cyclic_vector() {
            // #(1 2 #0=#(3 4 #0#))
            let vec = Rc::new(RefCell::new(vec![Value::real(3), Value::real(4)]));
            let head = Value::VectorMut(Rc::clone(&vec));
            vec.borrow_mut().push(head.clone());
            let v = Value::vector([Value::real(1), Value::real(2), head.clone()]);
            let mut c = Visits::default();

            c.scan(&v);

            assert_eq!(c.all_cycles().len(), 1);
        }

        #[test]
        fn multiple_cyclic_vector() {
            // #0=#(1 2 #0# 3 #0#)
            let vec = Rc::new(RefCell::new(vec![Value::real(1), Value::real(2)]));
            let v = Value::VectorMut(Rc::clone(&vec));
            vec.borrow_mut().push(v.clone());
            vec.borrow_mut().push(Value::real(3));
            vec.borrow_mut().push(v.clone());

            let mut c = Visits::default();

            c.scan(&v);

            assert_eq!(c.all_cycles().len(), 1);
        }

        #[test]
        fn nested_cyclic_vector() {
            // #0=#(1 2 #1=#(9 8 #1#) 3 #0#)
            let nested_vec = Rc::new(RefCell::new(vec![Value::real(9), Value::real(8)]));
            let nested_v = Value::VectorMut(Rc::clone(&nested_vec));
            nested_vec.borrow_mut().push(nested_v.clone());
            let vec = Rc::new(RefCell::new(vec![
                Value::real(1),
                Value::real(2),
                nested_v.clone(),
                Value::real(3),
            ]));
            let v = Value::VectorMut(Rc::clone(&vec));
            vec.borrow_mut().push(v.clone());
            let mut c = Visits::default();

            c.scan(&v);

            assert_eq!(c.all_cycles().len(), 2);
        }

        #[test]
        fn self_nested_cyclic_vector() {
            // #0=#(1 2 #(3 4 #0#))
            let nested_vec = Rc::new(RefCell::new(vec![Value::real(3), Value::real(4)]));
            let nested_v = Value::VectorMut(Rc::clone(&nested_vec));
            let vec = Rc::new(RefCell::new(vec![
                Value::real(1),
                Value::real(2),
                nested_v.clone(),
            ]));
            let v = Value::VectorMut(Rc::clone(&vec));
            nested_vec.borrow_mut().push(v.clone());
            let mut c = Visits::default();

            c.scan(&v);

            assert_eq!(c.all_cycles().len(), 1);
        }

        #[test]
        fn circular_list_with_cyclic_vector() {
            // #0=(#1=#(9 8 . #1#) 2 3 . #0#)
            let nested_vec = Rc::new(RefCell::new(vec![Value::real(9), Value::real(8)]));
            let nested_v = Value::VectorMut(Rc::clone(&nested_vec));
            nested_vec.borrow_mut().push(nested_v.clone());
            let end = RefCell::new(Pair {
                car: Value::real(3),
                cdr: Value::Null,
            })
            .into();
            let p = Pair {
                car: nested_v.clone(),
                cdr: Value::cons(Value::real(2), Value::PairMut(Rc::clone(&end))),
            }
            .into();
            end.borrow_mut().cdr = Value::Pair(Rc::clone(&p));
            let v = Value::Pair(Rc::clone(&p));
            let mut c = Visits::default();

            c.scan(&v);

            assert_eq!(c.all_cycles().len(), 2);
        }

        #[test]
        fn cyclic_vector_with_circular_list() {
            // #0=#(1 2 #1=(9 8 . #1#) 3 #0#)
            let nested_end = RefCell::new(Pair {
                car: Value::real(8),
                cdr: Value::Null,
            })
            .into();
            let nested_head = Pair {
                car: Value::real(9),
                cdr: Value::PairMut(Rc::clone(&nested_end)),
            }
            .into();
            nested_end.borrow_mut().cdr = Value::Pair(Rc::clone(&nested_head));
            let vec = Rc::new(RefCell::new(vec![
                Value::real(1),
                Value::real(2),
                Value::Pair(Rc::clone(&nested_head)),
                Value::real(3),
            ]));
            let v = Value::VectorMut(Rc::clone(&vec));
            vec.borrow_mut().push(v.clone());
            let mut c = Visits::default();

            c.scan(&v);

            assert_eq!(c.all_cycles().len(), 2);
        }
    }
}
