use super::{Pair, Value};
use crate::{
    lex::{DisplayTokenLines, TokenLinesMessage},
    string::{CharDatum, StrDatum},
};
use std::{
    borrow::Cow,
    cell::Cell,
    collections::{HashMap, HashSet},
    fmt::{self, Display, Formatter, Write},
};

pub(crate) struct SimpleDatum<'a>(pub(super) &'a Value);

impl Display for SimpleDatum<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.0 {
            Value::Ast(prg) => write!(f, "{{{prg:?}}}"),
            Value::Boolean(b) => write!(f, "#{}", if *b { 't' } else { 'f' }),
            Value::ByteVector(bv) => write_seq("#u8", bv.iter(), f),
            Value::ByteVectorMut(bv) => write_seq("#u8", bv.borrow().iter(), f),
            Value::Character(c) => write!(f, "#\\{}", CharDatum::new(*c)),
            Value::Eof => f.write_str("#<eof>"),
            Value::Error(c) => c.fmt(f),
            Value::Intrinsic(p) => p.fmt(f),
            Value::Null => f.write_str("()"),
            Value::Number(n) => n.fmt(f),
            Value::Pair(p) => SimplePairDatum::new(p).fmt(f),
            Value::PairMut(p) => SimplePairDatum::new(&p.borrow()).fmt(f),
            Value::PortInput(p) => write_port(p.borrow(), f),
            Value::PortOutput(p) => write_port(p.borrow(), f),
            Value::Procedure(p) => p.fmt(f),
            Value::String(s) => StrDatum(s).fmt(f),
            Value::StringMut(s) => StrDatum(&s.borrow()).fmt(f),
            Value::Symbol(s) => s.as_datum().fmt(f),
            Value::TokenList(lines) => DisplayTokenLines(lines).fmt(f),
            Value::Unspecified => f.write_str("#<unspecified>"),
            Value::Vector(v) => write_seq("#", v.iter().map(Value::as_simple_datum), f),
            Value::VectorMut(v) => write_seq("#", v.borrow().iter().map(Value::as_simple_datum), f),
        }
    }
}

pub(crate) struct Datum<'a>(pub(super) &'a Value);

impl Display for Datum<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.0 {
            Value::Pair(p) => PairDatum::new(p).fmt(f),
            Value::PairMut(p) => PairDatum::new(&p.borrow()).fmt(f),
            Value::Vector(v) => VecDatum::new(v).fmt(f),
            Value::VectorMut(v) => VecDatum::new(&v.borrow()).fmt(f),
            _ => SimpleDatum(self.0).fmt(f),
        }
    }
}

pub(crate) struct SharedDatum<'a>(pub(super) &'a Value);

impl Display for SharedDatum<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.0 {
            Value::Pair(p) => PairDatum::shared(p).fmt(f),
            Value::PairMut(p) => PairDatum::shared(&p.borrow()).fmt(f),
            Value::Vector(v) => VecDatum::shared(v).fmt(f),
            Value::VectorMut(v) => VecDatum::shared(&v.borrow()).fmt(f),
            _ => SimpleDatum(self.0).fmt(f),
        }
    }
}

pub(crate) struct DisplayDatum<'a>(pub(super) &'a Value);

impl Display for DisplayDatum<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        todo!()
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
    pub(crate) const PORT: &'static str = "port";
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
            Value::Eof => f.write_str("end-of-file"),
            Value::Error(_) => f.write_str(Self::ERROR),
            Value::Intrinsic(_) => f.write_str("intrinsic"),
            Value::Null => f.write_str("null"),
            Value::Number(_) => f.write_str(Self::NUMBER),
            Value::Pair(p) => f.write_str(p.typename()),
            Value::PairMut(p) => f.write_str(p.borrow().typename()),
            Value::PortInput(_) | Value::PortOutput(_) => f.write_str(Self::PORT),
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

struct SimplePairDatum<'a>(&'a Pair, Cell<u32>);

impl<'a> SimplePairDatum<'a> {
    const MAX_PRINT: u32 = 1_000_000;

    fn new(p: &'a Pair) -> Self {
        Self(p, Cell::new(u32::MIN))
    }
}

impl Display for SimplePairDatum<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "({}", self.0.car.as_simple_datum())?;
        for val in self.0.cdr.iter() {
            if let Some(p) = val.as_refpair() {
                write!(f, " {}", p.as_ref().car.as_simple_datum())?;
            } else if !matches!(val, Value::Null) {
                write!(f, " . {}", val.as_simple_datum())?;
            }
            // NOTE: a circular vector rapidly causes a stack overflow but a
            // circular list enters a tight loop that ignores signals and
            // doesn't exhaust resources quickly enough to crash; add a trapdoor
            // to avoid accidentally freezing Zara.
            let c = self.1.get();
            if c > Self::MAX_PRINT {
                f.write_str(" [circular list likely; terminating]…")?;
                break;
            }
            self.1.set(c + 1);
        }
        f.write_char(')')
    }
}

struct NestedDatum<'a> {
    graph: &'a Traverse,
    val: &'a Value,
}

impl Display for NestedDatum<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if let Some(p) = self.val.as_refpair() {
            PairDatum::nested(p.as_ref(), self.graph).fmt(f)
        } else if let Some(vec) = self.val.as_refvec() {
            VecDatum::nested(vec.as_ref(), self.graph).fmt(f)
        } else {
            self.val.as_datum().fmt(f)
        }
    }
}

struct GraphDatum<'a, T: ?Sized> {
    graph: Cow<'a, Traverse>,
    val: &'a T,
}

impl<'a, T: ?Sized> GraphDatum<'a, T> {
    fn nested(val: &'a T, graph: &'a Traverse) -> Self {
        Self {
            graph: Cow::Borrowed(graph),
            val,
        }
    }
}

type PairDatum<'a> = GraphDatum<'a, Pair>;
type VecDatum<'a> = GraphDatum<'a, [Value]>;

impl<'a> PairDatum<'a> {
    fn new(head: &'a Pair) -> Self {
        Self {
            graph: Cow::Owned(Traverse::pair(head)),
            val: head,
        }
    }

    fn shared(head: &'a Pair) -> Self {
        Self {
            graph: Cow::Owned(Traverse::shared_pair(head)),
            val: head,
        }
    }

    fn write_tail(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for item in self.val.cdr.iter() {
            if let Some(p) = item.as_refpair() {
                let pref = p.as_ref();
                if self.graph.contains(pref.node_id()) {
                    write!(f, " . {}", PairDatum::nested(pref, &self.graph))?;
                    break;
                }
                write!(
                    f,
                    " {}",
                    NestedDatum {
                        graph: &self.graph,
                        val: &pref.car
                    }
                )?;
            } else if let Some(v) = item.as_refvec() {
                write!(f, " . {}", VecDatum::nested(v.as_ref(), &self.graph))?;
            } else if !matches!(item, Value::Null) {
                write!(f, " . {}", item.as_datum())?;
            }
        }
        f.write_char(')')
    }
}

impl Display for PairDatum<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if let Some(vs) = self.graph.get(self.val.node_id()) {
            if vs.marked() {
                return write!(f, "#{}#", vs.label);
            }
            write!(f, "#{}=", vs.label)?;
            vs.mark();
        }
        write!(
            f,
            "({}",
            NestedDatum {
                graph: &self.graph,
                val: &self.val.car
            }
        )?;
        self.write_tail(f)
    }
}

impl<'a> VecDatum<'a> {
    fn new(vec: &'a [Value]) -> Self {
        Self {
            graph: Cow::Owned(Traverse::vec(vec)),
            val: vec,
        }
    }

    fn shared(vec: &'a [Value]) -> Self {
        Self {
            graph: Cow::Owned(Traverse::shared_vec(vec)),
            val: vec,
        }
    }

    fn join_items(&self) -> String {
        self.val
            .iter()
            .map(|item| {
                NestedDatum {
                    graph: &self.graph,
                    val: item,
                }
                .to_string()
            })
            .collect::<Vec<_>>()
            .join(" ")
    }
}

impl Display for VecDatum<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if let Some(vs) = self.graph.get(self.val.as_ptr().cast()) {
            if vs.marked() {
                return write!(f, "#{}#", vs.label);
            }
            write!(f, "#{}=", vs.label)?;
            vs.mark();
        }
        write!(f, "#({})", &self.join_items())
    }
}

#[derive(Clone, Debug)]
struct Traverse {
    active: ActiveVisits,
    label: usize,
    nodes: Vec<NodeId>,
    shared: bool,
    visits: HashMap<NodeId, Visit>,
}

impl Traverse {
    fn pair(p: &Pair) -> Self {
        Self::create_pair(p, false)
    }

    fn shared_pair(p: &Pair) -> Self {
        Self::create_pair(p, true)
    }

    fn vec(vec: &[Value]) -> Self {
        Self::create_vec(vec, false)
    }

    fn shared_vec(vec: &[Value]) -> Self {
        Self::create_vec(vec, true)
    }

    fn create_pair(p: &Pair, shared: bool) -> Self {
        let mut me = Self::create(shared);
        me.active.start();
        me.add(p.node_id());
        me.visit(&p.car);
        me.traverse(&p.cdr);
        me.active.end();
        me
    }

    fn create_vec(vec: &[Value], shared: bool) -> Self {
        let mut me = Self::create(shared);
        me.visit_vec(vec);
        me.label_visits();
        me
    }

    fn create(shared: bool) -> Self {
        Self {
            active: ActiveVisits::default(),
            label: usize::MIN,
            nodes: Vec::new(),
            shared,
            visits: HashMap::new(),
        }
    }

    fn contains(&self, id: NodeId) -> bool {
        self.get(id).is_some()
    }

    fn get(&self, id: NodeId) -> Option<&Visit> {
        self.visits.get(&id).filter(|vs| vs.revisited(self.shared))
    }

    fn traverse(&mut self, start: &Value) {
        self.visit(start);
        self.label_visits();
    }

    fn visit(&mut self, v: &Value) {
        if v.is_pair() {
            self.visit_pair(v);
        } else if let Some(vec) = v.as_refvec() {
            self.visit_vec(vec.as_ref());
        }
    }

    fn visit_pair(&mut self, pair_val: &Value) {
        self.active.start();
        for v in pair_val.iter() {
            let nested = if let Some(p) = v.as_refpair() {
                let pref = p.as_ref();
                if !self.add(pref.node_id()) {
                    break;
                }
                pref.car.clone()
            } else {
                v
            };
            self.visit(&nested);
        }
        self.active.end();
    }

    fn visit_vec(&mut self, vec: &[Value]) {
        self.active.start();
        if self.add(vec.as_ptr().cast()) {
            for item in vec {
                self.visit(item);
            }
        }
        self.active.end();
    }

    fn add(&mut self, id: NodeId) -> bool {
        match self.visits.get_mut(&id) {
            None => {
                self.visits.insert(id, Visit::default());
                self.nodes.push(id);
                self.active.add(id);
                true
            }
            Some(vs) => {
                vs.shared = true;
                if !vs.cycle && self.active.contains(id) {
                    vs.cycle = true;
                }
                false
            }
        }
    }

    fn label_visits(&mut self) {
        for n in &self.nodes {
            if let Some(vs) = self.visits.get_mut(n)
                && vs.revisited(self.shared)
            {
                vs.label = self.label;
                self.label += 1;
            }
        }
    }
}

#[derive(Clone, Debug, Default)]
struct Visit {
    cycle: bool,
    flag: Cell<bool>,
    label: usize,
    shared: bool,
}

impl Visit {
    fn marked(&self) -> bool {
        self.flag.get()
    }

    fn mark(&self) {
        self.flag.set(true);
    }

    fn revisited(&self, shared: bool) -> bool {
        (shared && self.shared) || self.cycle
    }
}

#[derive(Clone, Debug, Default)]
struct ActiveVisits {
    scopes: Vec<HashSet<NodeId>>,
}

impl ActiveVisits {
    fn contains(&self, id: NodeId) -> bool {
        self.scopes.iter().any(|s| s.contains(&id))
    }

    fn start(&mut self) {
        self.scopes.push(HashSet::new());
    }

    fn add(&mut self, id: NodeId) {
        self.scopes
            .last_mut()
            .expect("active visit scopes should not be empty")
            .insert(id);
    }

    fn end(&mut self) {
        self.scopes.pop();
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

fn write_port(p: impl Display, f: &mut Formatter<'_>) -> fmt::Result {
    write!(f, "#<port {p}>")
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::testutil::some_or_fail;
    use std::{cell::RefCell, rc::Rc};

    fn cycle_count(graph: &Traverse) -> usize {
        graph.visits.values().fold(usize::MIN, |mut acc, vs| {
            if vs.cycle {
                acc += 1
            };
            acc
        })
    }

    #[test]
    fn normal_list() {
        // (1 2 3)
        let v = zlist![Value::real(1), Value::real(2), Value::real(3)];

        let graph = Traverse::pair(v.as_refpair().unwrap().as_ref());

        assert_eq!(cycle_count(&graph), 0);
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

        let graph = Traverse::pair(&p);

        assert_eq!(cycle_count(&graph), 1);
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

        let graph = Traverse::pair(&p);

        assert_eq!(cycle_count(&graph), 1);
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

        let graph = Traverse::pair(&p);

        assert_eq!(cycle_count(&graph), 2);
        assert_eq!(some_or_fail!(graph.get(p.node_id())).label, 0);
        assert_eq!(some_or_fail!(graph.get(nested_head.node_id())).label, 1);
    }

    #[test]
    fn nested_circular_pair() {
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

        let graph = Traverse::pair(&p);

        assert_eq!(cycle_count(&graph), 2);
        assert_eq!(some_or_fail!(graph.get(p.node_id())).label, 0);
        assert_eq!(some_or_fail!(graph.get(nested_head.node_id())).label, 1);
    }

    #[test]
    fn contained_circular_list() {
        // a ==> #0=(1 2 3 9 #0# 7)
        // b ==> #0=(9 (1 2 3 . #0#) 7)
        let a_end = RefCell::new(Pair {
            car: Value::real(3),
            cdr: Value::Null,
        })
        .into();
        let a = Value::cons(
            Value::real(1),
            Value::cons(Value::real(2), Value::PairMut(Rc::clone(&a_end))),
        );
        let b = zlist![Value::real(9), a.clone(), Value::real(7)];
        a_end.borrow_mut().cdr = b.clone();

        let a_graph = Traverse::pair(a.as_refpair().unwrap().as_ref());
        let b_graph = Traverse::pair(b.as_refpair().unwrap().as_ref());

        assert_eq!(cycle_count(&a_graph), 1);
        assert_eq!(cycle_count(&b_graph), 1);
        assert_eq!(a.as_datum().to_string(), "#0=(1 2 3 9 #0# 7)");
        assert_eq!(a.as_shared_datum().to_string(), a.as_datum().to_string());
        assert_eq!(b.as_datum().to_string(), "#0=(9 (1 2 3 . #0#) 7)");
        assert_eq!(b.as_shared_datum().to_string(), b.as_datum().to_string());
    }

    #[test]
    fn simple_vector() {
        // #(1 2 3)
        let v = Value::vector([Value::real(1), Value::real(2), Value::real(3)]);

        let graph = Traverse::vec(v.as_refvec().unwrap().as_ref());

        assert_eq!(cycle_count(&graph), 0);
    }

    #[test]
    fn cyclic_vector() {
        // #0=#(1 2 #0#)
        let vec = RefCell::new(vec![Value::real(1), Value::real(2)]).into();
        let v = Value::VectorMut(Rc::clone(&vec));
        vec.borrow_mut().push(v.clone());

        let graph = Traverse::vec(&vec.borrow());

        assert_eq!(cycle_count(&graph), 1);
    }

    #[test]
    fn partly_cyclic_vector() {
        // #(1 2 #0=#(3 4 #0#))
        let vec = RefCell::new(vec![Value::real(3), Value::real(4)]).into();
        let head = Value::VectorMut(Rc::clone(&vec));
        vec.borrow_mut().push(head.clone());
        let v = Value::vector([Value::real(1), Value::real(2), head.clone()]);

        let graph = Traverse::vec(v.as_refvec().unwrap().as_ref());

        assert_eq!(cycle_count(&graph), 1);
    }

    #[test]
    fn multiple_cyclic_vector() {
        // #0=#(1 2 #0# 3 #0#)
        let vec = RefCell::new(vec![Value::real(1), Value::real(2)]).into();
        let v = Value::VectorMut(Rc::clone(&vec));
        vec.borrow_mut().push(v.clone());
        vec.borrow_mut().push(Value::real(3));
        vec.borrow_mut().push(v.clone());

        let graph = Traverse::vec(&vec.borrow());

        assert_eq!(cycle_count(&graph), 1);
    }

    #[test]
    fn nested_cyclic_vector() {
        // #0=#(1 2 #1=#(9 8 #1#) 3 #0#)
        let nested_vec = RefCell::new(vec![Value::real(9), Value::real(8)]).into();
        let nested_v = Value::VectorMut(Rc::clone(&nested_vec));
        nested_vec.borrow_mut().push(nested_v.clone());
        let vec = RefCell::new(vec![
            Value::real(1),
            Value::real(2),
            nested_v.clone(),
            Value::real(3),
        ])
        .into();
        let v = Value::VectorMut(Rc::clone(&vec));
        vec.borrow_mut().push(v.clone());

        let graph = Traverse::vec(&vec.borrow());

        assert_eq!(cycle_count(&graph), 2);
        assert_eq!(
            some_or_fail!(graph.get(vec.borrow().as_ptr().cast())).label,
            0
        );
        assert_eq!(
            some_or_fail!(graph.get(nested_vec.borrow().as_ptr().cast())).label,
            1
        );
    }

    #[test]
    fn self_nested_cyclic_vector() {
        // #0=#(1 2 #(3 4 #0#))
        let nested_vec = RefCell::new(vec![Value::real(3), Value::real(4)]).into();
        let nested_v = Value::VectorMut(Rc::clone(&nested_vec));
        let vec = RefCell::new(vec![Value::real(1), Value::real(2), nested_v.clone()]).into();
        let v = Value::VectorMut(Rc::clone(&vec));
        nested_vec.borrow_mut().push(v.clone());

        let graph = Traverse::vec(&vec.borrow());

        assert_eq!(cycle_count(&graph), 1);
    }

    #[test]
    fn mutually_nested_vectors() {
        // #0=#(1 #(9 #0# 7) 3)
        // #0=#(9 #(1 #0# 3) 7)
        let a_vec = RefCell::new(vec![Value::real(1), Value::Unspecified, Value::real(3)]).into();
        let b = Value::vector_mut(vec![
            Value::real(9),
            Value::VectorMut(Rc::clone(&a_vec)),
            Value::real(7),
        ]);
        a_vec.borrow_mut()[1] = b.clone();
        let a = Value::VectorMut(a_vec);

        let a_graph = Traverse::vec(a.as_refvec().unwrap().as_ref());
        let b_graph = Traverse::vec(b.as_refvec().unwrap().as_ref());

        assert_eq!(cycle_count(&a_graph), 1);
        assert_eq!(cycle_count(&b_graph), 1);
        assert_eq!(a.as_datum().to_string(), "#0=#(1 #(9 #0# 7) 3)");
        assert_eq!(a.as_shared_datum().to_string(), a.as_datum().to_string());
        assert_eq!(b.as_datum().to_string(), "#0=#(9 #(1 #0# 3) 7)");
        assert_eq!(b.as_shared_datum().to_string(), b.as_datum().to_string());
    }

    #[test]
    fn circular_list_with_cyclic_vector() {
        // #0=(#1=#(9 8 #1#) 2 3 . #0#)
        let nested_vec = RefCell::new(vec![Value::real(9), Value::real(8)]).into();
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

        let graph = Traverse::pair(&p);

        assert_eq!(cycle_count(&graph), 2);
        assert_eq!(some_or_fail!(graph.get(p.node_id())).label, 0);
        assert_eq!(
            some_or_fail!(graph.get(nested_vec.borrow().as_ptr().cast())).label,
            1
        );
        assert_eq!(v.as_datum().to_string(), "#0=(#1=#(9 8 #1#) 2 3 . #0#)");
        assert_eq!(v.as_shared_datum().to_string(), v.as_datum().to_string());
    }

    #[test]
    fn list_ending_with_cyclic_vector() {
        // (#0=#(9 8 #1#) 2 3 . #0#)
        let nested_vec = RefCell::new(vec![Value::real(9), Value::real(8)]).into();
        let nested_v = Value::VectorMut(Rc::clone(&nested_vec));
        nested_vec.borrow_mut().push(nested_v.clone());
        let p = Pair {
            car: nested_v.clone(),
            cdr: Value::cons(
                Value::real(2),
                Value::cons(Value::real(3), nested_v.clone()),
            ),
        }
        .into();
        let v = Value::Pair(Rc::clone(&p));

        let graph = Traverse::pair(&p);

        assert_eq!(cycle_count(&graph), 1);
        assert_eq!(
            some_or_fail!(graph.get(nested_vec.borrow().as_ptr().cast())).label,
            0
        );
        assert_eq!(v.as_datum().to_string(), "(#0=#(9 8 #0#) 2 3 . #0#)");
        assert_eq!(v.as_shared_datum().to_string(), v.as_datum().to_string());
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
        let vec = RefCell::new(vec![
            Value::real(1),
            Value::real(2),
            Value::Pair(Rc::clone(&nested_head)),
            Value::real(3),
        ])
        .into();
        let v = Value::VectorMut(Rc::clone(&vec));
        vec.borrow_mut().push(v.clone());

        let graph = Traverse::vec(&vec.borrow());

        assert_eq!(cycle_count(&graph), 2);
        assert_eq!(
            some_or_fail!(graph.get(vec.borrow().as_ptr().cast())).label,
            0
        );
        assert_eq!(some_or_fail!(graph.get(nested_head.node_id())).label, 1);
        assert_eq!(v.as_datum().to_string(), "#0=#(1 2 #1=(9 8 . #1#) 3 #0#)");
        assert_eq!(v.as_shared_datum().to_string(), v.as_datum().to_string());
    }

    #[test]
    fn get_does_not_return_non_cycles() {
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

        let graph = Traverse::pair(&p);

        assert_eq!(cycle_count(&graph), 1);
        assert!(graph.contains(start.node_id()));
        assert!(!graph.contains(p.node_id()));
    }

    #[test]
    fn list_contains_duplicate_list() {
        // (1 2 (9 8) 3 (9 8) 4)
        let inner = zlist![Value::real(9), Value::real(8)];
        let val = zlist![
            Value::real(1),
            Value::real(2),
            inner.clone(),
            Value::real(3),
            inner,
            Value::real(4),
        ];

        let graph = Traverse::pair(val.as_refpair().unwrap().as_ref());

        assert_eq!(cycle_count(&graph), 0);
        assert_eq!(val.as_datum().to_string(), "(1 2 (9 8) 3 (9 8) 4)");
        assert_eq!(
            val.as_simple_datum().to_string(),
            val.as_datum().to_string()
        );
        assert_eq!(val.as_shared_datum().to_string(), "(1 2 #0=(9 8) 3 #0# 4)");
    }

    #[test]
    fn vector_contains_duplicate_vector() {
        // #(1 2 #(9 8) 3 #(9 8) 4)
        let inner = Value::vector([Value::real(9), Value::real(8)]);
        let val = Value::vector([
            Value::real(1),
            Value::real(2),
            inner.clone(),
            Value::real(3),
            inner,
            Value::real(4),
        ]);

        let graph = Traverse::vec(val.as_refvec().unwrap().as_ref());

        assert_eq!(cycle_count(&graph), 0);
        assert_eq!(val.as_datum().to_string(), "#(1 2 #(9 8) 3 #(9 8) 4)");
        assert_eq!(
            val.as_simple_datum().to_string(),
            val.as_datum().to_string()
        );
        assert_eq!(
            val.as_shared_datum().to_string(),
            "#(1 2 #0=#(9 8) 3 #0# 4)"
        );
    }

    #[test]
    fn list_contains_duplicate_vector() {
        // (1 2 #(9 8) 3 #(9 8) 4)
        let inner = Value::vector([Value::real(9), Value::real(8)]);
        let val = zlist![
            Value::real(1),
            Value::real(2),
            inner.clone(),
            Value::real(3),
            inner,
            Value::real(4),
        ];

        let graph = Traverse::pair(val.as_refpair().unwrap().as_ref());

        assert_eq!(cycle_count(&graph), 0);
        assert_eq!(val.as_datum().to_string(), "(1 2 #(9 8) 3 #(9 8) 4)");
        assert_eq!(
            val.as_simple_datum().to_string(),
            val.as_datum().to_string()
        );
        assert_eq!(val.as_shared_datum().to_string(), "(1 2 #0=#(9 8) 3 #0# 4)");
    }

    #[test]
    fn vector_contains_duplicate_list() {
        // #(1 2 (9 8) 3 (9 8) 4)
        let inner = zlist![Value::real(9), Value::real(8)];
        let val = Value::vector([
            Value::real(1),
            Value::real(2),
            inner.clone(),
            Value::real(3),
            inner,
            Value::real(4),
        ]);

        let graph = Traverse::vec(val.as_refvec().unwrap().as_ref());

        assert_eq!(cycle_count(&graph), 0);
        assert_eq!(val.as_datum().to_string(), "#(1 2 (9 8) 3 (9 8) 4)");
        assert_eq!(
            val.as_simple_datum().to_string(),
            val.as_datum().to_string()
        );
        assert_eq!(val.as_shared_datum().to_string(), "#(1 2 #0=(9 8) 3 #0# 4)");
    }
}
