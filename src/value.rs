macro_rules! zlist {
    () => {
        Value::Null
    };
    ($exp:expr $(, $exps:expr)* $(,)?) => {
        Value::cons($exp, zlist![$($exps),*])
    };
}

mod condition;
mod display;
#[cfg(test)]
mod tests;

pub(crate) use self::{
    condition::Condition,
    display::{Datum, TypeName, ValueMessage},
};
use crate::{
    eval::{Intrinsic, Procedure},
    lex::TokenLine,
    number::{Number, Real},
    string::Symbol,
    syntax::Sequence,
};
use std::{
    cell::{Cell, Ref, RefCell, RefMut},
    collections::HashMap,
    ptr,
    rc::Rc,
};
pub(crate) use zlist;

#[derive(Clone, Debug)]
pub(crate) enum Value {
    Ast(Rc<Sequence>),
    Boolean(bool),
    ByteVector(Rc<[u8]>),
    ByteVectorMut(Rc<RefCell<Vec<u8>>>),
    Character(char),
    Error(Rc<Condition>),
    Intrinsic(Rc<Intrinsic>),
    Null,
    Number(Number),
    Pair(Rc<Pair>),
    PairMut(Rc<RefCell<Pair>>),
    Procedure(Rc<Procedure>),
    String(Rc<str>),
    StringMut(Rc<RefCell<String>>),
    Symbol(Symbol),
    TokenList(Rc<[TokenLine]>),
    Unspecified,
    Vector(Rc<[Self]>),
    VectorMut(Rc<RefCell<Vec<Self>>>),
}

impl Value {
    #[allow(clippy::similar_names, reason = "lisp terms-of-art")]
    pub(crate) fn cons(car: Self, cdr: Self) -> Self {
        Self::Pair(Pair { car, cdr }.into())
    }

    #[allow(clippy::similar_names, reason = "lisp terms-of-art")]
    pub(crate) fn cons_mut(car: Self, cdr: Self) -> Self {
        Self::PairMut(RefCell::new(Pair { car, cdr }).into())
    }

    pub(crate) fn list<I>(items: I) -> Self
    where
        I: IntoIterator<Item = Self>,
        <I as IntoIterator>::IntoIter: DoubleEndedIterator,
    {
        Self::make_list(items, Self::cons)
    }

    pub(crate) fn list_mut<I>(items: I) -> Self
    where
        I: IntoIterator<Item = Self>,
        <I as IntoIterator>::IntoIter: DoubleEndedIterator,
    {
        Self::make_list(items, Self::cons_mut)
    }

    pub(crate) fn list_cons<I>(items: I) -> Self
    where
        I: IntoIterator<Item = Self>,
        <I as IntoIterator>::IntoIter: DoubleEndedIterator,
    {
        Self::make_improper_list(items, Self::cons)
    }

    pub(crate) fn list_cons_mut<I>(items: I) -> Self
    where
        I: IntoIterator<Item = Self>,
        <I as IntoIterator>::IntoIter: DoubleEndedIterator,
    {
        Self::make_improper_list(items, Self::cons_mut)
    }

    pub(crate) fn procedure(p: impl Into<Rc<Procedure>>) -> Self {
        Self::Procedure(p.into())
    }

    pub(crate) fn real(r: impl Into<Real>) -> Self {
        Self::Number(Number::real(r))
    }

    pub(crate) fn string(s: impl Into<Rc<str>>) -> Self {
        Self::String(s.into())
    }

    pub(crate) fn string_mut(s: impl Into<String>) -> Self {
        Self::StringMut(RefCell::new(s.into()).into())
    }

    pub(crate) fn strmut_from_chars(c: impl IntoIterator<Item = char>) -> Self {
        Self::StringMut(RefCell::new(c.into_iter().collect()).into())
    }

    pub(crate) fn vector(items: impl IntoIterator<Item = Self>) -> Self {
        Self::Vector(items.into_iter().collect())
    }

    pub(crate) fn vector_mut(items: impl IntoIterator<Item = Self>) -> Self {
        Self::VectorMut(RefCell::new(items.into_iter().collect()).into())
    }

    pub(crate) fn bytevector_mut(bytes: impl IntoIterator<Item = u8>) -> Self {
        Self::ByteVectorMut(RefCell::new(bytes.into_iter().collect()).into())
    }

    fn make_list<I>(items: I, ctor: impl Fn(Self, Self) -> Self) -> Self
    where
        I: IntoIterator<Item = Self>,
        <I as IntoIterator>::IntoIter: DoubleEndedIterator,
    {
        items
            .into_iter()
            .rev()
            .fold(Self::Null, |head, item| ctor(item, head))
    }

    fn make_improper_list<I>(items: I, ctor: impl Fn(Self, Self) -> Self) -> Self
    where
        I: IntoIterator<Item = Self>,
        <I as IntoIterator>::IntoIter: DoubleEndedIterator,
    {
        items
            .into_iter()
            .rev()
            .reduce(|head, item| ctor(item, head))
            .unwrap_or(Self::Null)
    }

    // NOTE: procedure eq? -> is same object
    pub(crate) fn is(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Ast(a), Self::Ast(b)) => Rc::ptr_eq(a, b),
            (Self::Boolean(a), Self::Boolean(b)) => a == b,
            (Self::ByteVector(a), Self::ByteVector(b)) => Rc::ptr_eq(a, b),
            (Self::ByteVectorMut(a), Self::ByteVectorMut(b)) => Rc::ptr_eq(a, b),
            (Self::Error(a), Self::Error(b)) => Rc::ptr_eq(a, b),
            (Self::Intrinsic(a), Self::Intrinsic(b)) => Rc::ptr_eq(a, b),
            (Self::Null, Self::Null) | (Self::Unspecified, Self::Unspecified) => true,
            (Self::Pair(a), Self::Pair(b)) => Rc::ptr_eq(a, b),
            (Self::PairMut(a), Self::PairMut(b)) => Rc::ptr_eq(a, b),
            (Self::Procedure(a), Self::Procedure(b)) => Rc::ptr_eq(a, b),
            (Self::String(a), Self::String(b)) => Rc::ptr_eq(a, b),
            (Self::StringMut(a), Self::StringMut(b)) => Rc::ptr_eq(a, b),
            (Self::Symbol(a), Self::Symbol(b)) => a.is(b),
            (Self::TokenList(a), Self::TokenList(b)) => Rc::ptr_eq(a, b),
            (Self::Vector(a), Self::Vector(b)) => Rc::ptr_eq(a, b),
            (Self::VectorMut(a), Self::VectorMut(b)) => Rc::ptr_eq(a, b),
            _ => false,
        }
    }

    // NOTE: procedure eqv? -> is equivalent object
    pub(crate) fn is_eqv(&self, other: &Self) -> bool {
        self.is(other)
            || match (self, other) {
                (Self::Character(a), Self::Character(b)) => a == b,
                (Self::Number(a), Self::Number(b)) => a.is_eqv(b),
                _ => false,
            }
    }

    pub(crate) fn is_pair(&self) -> bool {
        matches!(self, Value::Pair(_) | Value::PairMut(_))
    }

    pub(crate) fn is_list_element(&self) -> bool {
        self.is_pair() || matches!(self, Value::Null)
    }

    pub(crate) fn display_message(&self) -> ValueMessage<'_> {
        ValueMessage(self)
    }

    pub(crate) fn as_datum(&self) -> Datum<'_> {
        Datum(self)
    }

    pub(crate) fn as_typename(&self) -> TypeName<'_> {
        TypeName(self)
    }

    pub(crate) fn as_refpair(&self) -> Option<PairRef<'_>> {
        match self {
            Self::Pair(s) => Some(PairRef::Imm(s)),
            Self::PairMut(s) => Some(PairRef::Mut(s.borrow())),
            _ => None,
        }
    }

    pub(crate) fn as_refstr(&self) -> Option<StrRef<'_>> {
        match self {
            Self::String(s) => Some(StrRef::Imm(s)),
            Self::StringMut(s) => Some(StrRef::Mut(s.borrow())),
            _ => None,
        }
    }

    pub(crate) fn as_mutrefstr(&self) -> Option<RefMut<'_, String>> {
        if let Self::StringMut(s) = self {
            Some(s.borrow_mut())
        } else {
            None
        }
    }

    pub(crate) fn as_refvec(&self) -> Option<VecRef<'_>> {
        match self {
            Self::Vector(v) => Some(VecRef::Imm(v)),
            Self::VectorMut(v) => Some(VecRef::Mut(v.borrow())),
            _ => None,
        }
    }

    pub(crate) fn as_mutrefvec(&self) -> Option<RefMut<'_, Vec<Self>>> {
        if let Self::VectorMut(v) = self {
            Some(v.borrow_mut())
        } else {
            None
        }
    }

    pub(crate) fn as_refbv(&self) -> Option<BvRef<'_>> {
        match self {
            Self::ByteVector(bv) => Some(BvRef::Imm(bv)),
            Self::ByteVectorMut(bv) => Some(BvRef::Mut(bv.borrow())),
            _ => None,
        }
    }

    pub(crate) fn as_mutrefbv(&self) -> Option<RefMut<'_, Vec<u8>>> {
        if let Self::ByteVectorMut(bv) = self {
            Some(bv.borrow_mut())
        } else {
            None
        }
    }

    pub(crate) fn iter(&self) -> ValueIterator {
        ValueIterator::new(self)
    }
}

// NOTE: procedure equal? -> value equality
impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        self.is_eqv(other)
            || match (self, other) {
                (Self::ByteVector(a), Self::ByteVector(b)) => a == b,
                (Self::ByteVectorMut(a), Self::ByteVectorMut(b)) => a == b,
                (Self::ByteVector(a), Self::ByteVectorMut(b))
                | (Self::ByteVectorMut(b), Self::ByteVector(a)) => a.as_ref() == *b.borrow(),
                (Self::Pair(a), Self::Pair(b)) => a == b,
                (Self::PairMut(a), Self::PairMut(b)) => a == b,
                (Self::Pair(a), Self::PairMut(b)) | (Self::PairMut(b), Self::Pair(a)) => {
                    *a.as_ref() == *b.borrow()
                }
                (Self::String(a), Self::String(b)) => a == b,
                (Self::StringMut(a), Self::StringMut(b)) => a == b,
                (Self::String(a), Self::StringMut(b)) | (Self::StringMut(b), Self::String(a)) => {
                    a.as_ref() == *b.borrow()
                }
                (Self::Vector(a), Self::Vector(b)) => a == b,
                (Self::VectorMut(a), Self::VectorMut(b)) => a == b,
                (Self::Vector(a), Self::VectorMut(b)) | (Self::VectorMut(b), Self::Vector(a)) => {
                    a.as_ref() == *b.borrow()
                }
                _ => false,
            }
    }
}

impl Eq for Value {}

pub(crate) struct ValueIterator(Option<Value>);

impl ValueIterator {
    fn new(v: &Value) -> Self {
        Self(Some(v.clone()))
    }
}

impl Iterator for ValueIterator {
    type Item = Value;

    fn next(&mut self) -> Option<Self::Item> {
        let curr = self.0.take()?;
        if let Some(p) = curr.as_refpair() {
            let _ = self.0.insert(p.as_ref().cdr.clone());
        }
        Some(curr)
    }
}

pub(crate) type BvRef<'a> = CollRef<'a, [u8], Vec<u8>>;
pub(crate) type PairRef<'a> = CollRef<'a, Pair, Pair>;
pub(crate) type StrRef<'a> = CollRef<'a, str, String>;
pub(crate) type VecRef<'a> = CollRef<'a, [Value], Vec<Value>>;
pub(crate) type PairLenResult = Result<usize, InvalidList>;

pub(crate) trait CollSized {
    fn len(&self) -> usize;
}

pub(crate) trait PairSized {
    fn len(&self) -> PairLenResult;
}

pub(crate) enum CollRef<'a, T: ?Sized, M> {
    Imm(&'a T),
    Mut(Ref<'a, M>),
}

impl<T: ?Sized, M: AsRef<T>> AsRef<T> for CollRef<'_, T, M> {
    fn as_ref(&self) -> &T {
        match self {
            Self::Imm(t) => t,
            Self::Mut(r) => r.as_ref(),
        }
    }
}

impl<T, M: AsRef<[T]>> CollSized for CollRef<'_, [T], M> {
    fn len(&self) -> usize {
        match self {
            Self::Imm(c) => *c,
            Self::Mut(c) => c.as_ref(),
        }
        .len()
    }
}

impl PairSized for PairRef<'_> {
    fn len(&self) -> PairLenResult {
        match self {
            Self::Imm(p) => p,
            Self::Mut(p) => p.as_ref(),
        }
        .len()
    }
}

impl Default for StrRef<'_> {
    fn default() -> Self {
        Self::Imm("")
    }
}

impl CollSized for StrRef<'_> {
    fn len(&self) -> usize {
        match self {
            Self::Imm(s) => *s,
            Self::Mut(s) => s.as_ref(),
        }
        .chars()
        .count()
    }
}

#[derive(Debug)]
pub(crate) enum InvalidList {
    Cycle,
    Improper,
}

#[derive(Debug, Eq, PartialEq)]
pub(crate) struct Pair {
    pub(crate) car: Value,
    pub(crate) cdr: Value,
}

impl Pair {
    pub(crate) fn is_list(&self) -> bool {
        let graph = Traverse::pair(self);
        !graph.has_cycles() && self.cdr.iter().all(|item| item.is_list_element())
    }

    pub(crate) fn len(&self) -> PairLenResult {
        let graph = Traverse::pair(self);
        if graph.has_cycles() {
            Err(InvalidList::Cycle)
        } else {
            self.cdr.iter().try_fold(1usize, |acc, item| match item {
                Value::Null => Ok(acc),
                Value::Pair(_) | Value::PairMut(_) => Ok(acc + 1),
                _ => Err(InvalidList::Improper),
            })
        }
    }

    fn node_id(&self) -> NodeId {
        ptr::from_ref(self).cast()
    }

    fn typename(&self) -> &str {
        if self.is_list() {
            TypeName::LIST
        } else {
            TypeName::PAIR
        }
    }
}

impl AsRef<Self> for Pair {
    fn as_ref(&self) -> &Self {
        self
    }
}

#[derive(Clone, Debug)]
pub(crate) struct Traverse {
    cycles_only: bool,
    label: usize,
    nodes: Vec<NodeId>,
    visits: HashMap<NodeId, Visit>,
}

impl Traverse {
    pub(crate) fn value(v: &Value) -> Self {
        let mut me = Self::create(true);
        me.traverse(v);
        me
    }

    fn pair(p: &Pair) -> Self {
        let mut me = Self::create(true);
        me.add(p.node_id());
        me.visit(&p.car);
        me.traverse(&p.cdr);
        me
    }

    fn create(cycles_only: bool) -> Self {
        Self {
            cycles_only,
            label: usize::MIN,
            nodes: Vec::new(),
            visits: HashMap::new(),
        }
    }

    pub(crate) fn has_cycles(&self) -> bool {
        self.visits.values().any(|vs| vs.cycle)
    }

    fn get(&self, id: NodeId) -> Option<&Visit> {
        self.visits
            .get(&id)
            .filter(|vs| !self.cycles_only || vs.cycle)
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

    fn visit_pair(&mut self, v: &Value) {
        for v in ValueIterator(Some(v.clone())) {
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
    }

    fn visit_vec(&mut self, vec: &[Value]) {
        if self.add(vec.as_ptr().cast()) {
            for item in vec {
                self.visit(item);
            }
        }
    }

    fn add(&mut self, id: NodeId) -> bool {
        match self.visits.get_mut(&id) {
            None => {
                self.visits.insert(id, Visit::default());
                self.nodes.push(id);
                true
            }
            Some(vs) => {
                vs.cycle = true;
                false
            }
        }
    }

    fn label_visits(&mut self) {
        for n in &self.nodes {
            if let Some(vs) = self.visits.get_mut(n)
                && (!self.cycles_only || vs.cycle)
            {
                vs.label = self.label;
                self.label += 1;
            }
        }
    }
}

// NOTE: pairs and vectors are identified via their untyped pointer address
type NodeId = *const ();

#[derive(Clone, Debug, Default)]
struct Visit {
    cycle: bool,
    flag: Cell<bool>,
    label: usize,
}

impl Visit {
    fn marked(&self) -> bool {
        self.flag.get()
    }

    fn mark(&self) {
        self.flag.set(true);
    }
}
