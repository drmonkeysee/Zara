macro_rules! zlist {
    () => {
        Value::Null
    };
    ($exp:expr $(, $exps:expr)* $(,)?) => {
        Value::cons($exp, zlist![$($exps),*])
    };
}

mod condition;
#[cfg(test)]
mod tests;

pub(crate) use self::condition::Condition;
use crate::{
    eval::{Intrinsic, Procedure},
    lex::{DisplayTokenLines, TokenLine, TokenLinesMessage},
    number::{Number, Real},
    string::{CharDatum, StrDatum, Symbol},
    syntax::Sequence,
};
use std::{
    cell::{Ref, RefCell, RefMut},
    collections::HashSet,
    fmt::{self, Display, Formatter},
    ops::ControlFlow,
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

    pub(crate) fn display_message(&self) -> ValueMessage<'_> {
        ValueMessage(self)
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

impl Display for Value {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::Ast(prg) => write!(f, "{{{prg:?}}}"),
            Self::Boolean(b) => write!(f, "#{}", if *b { 't' } else { 'f' }),
            Self::ByteVector(bv) => write_seq("#u8", bv, f),
            Self::ByteVectorMut(bv) => write_seq("#u8", &bv.borrow(), f),
            Self::Character(c) => write!(f, "#\\{}", CharDatum::new(*c)),
            Self::Error(c) => c.fmt(f),
            Self::Intrinsic(p) => p.fmt(f),
            Self::Null => f.write_str("()"),
            Self::Number(n) => n.fmt(f),
            Self::Pair(p) => write!(f, "({p})"),
            Self::PairMut(p) => write!(f, "({})", p.borrow()),
            Self::Procedure(p) => p.fmt(f),
            Self::String(s) => StrDatum(s).fmt(f),
            Self::StringMut(s) => StrDatum(&s.borrow()).fmt(f),
            Self::Symbol(s) => s.as_datum().fmt(f),
            Self::TokenList(lines) => DisplayTokenLines(lines).fmt(f),
            Self::Unspecified => f.write_str("#<unspecified>"),
            Self::Vector(v) => write_seq("#", v, f),
            Self::VectorMut(v) => write_seq("#", &v.borrow(), f),
        }
    }
}

pub(crate) type BvRef<'a> = CollRef<'a, [u8], Vec<u8>>;
pub(crate) type PairRef<'a> = CollRef<'a, Pair, Pair>;
pub(crate) type StrRef<'a> = CollRef<'a, str, String>;
pub(crate) type VecRef<'a> = CollRef<'a, [Value], Vec<Value>>;

pub(crate) trait CollSized {
    fn len(&self) -> usize;
}

pub(crate) trait PairSized {
    fn len(&self) -> Option<usize>;
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
    fn len(&self) -> Option<usize> {
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

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct Pair {
    pub(crate) car: Value,
    pub(crate) cdr: Value,
}

impl Pair {
    pub(crate) fn is_list(&self) -> bool {
        self.iter().all(|v| match v {
            PairFlow::Continue(_) | PairFlow::Break(PairStop::End(Value::Null)) => true,
            PairFlow::Break(_) => false,
        })
    }

    pub(crate) fn len(&self) -> Option<usize> {
        let len = self.iter().fold(0, |acc, v| match v {
            PairFlow::Continue(_) => acc + 1,
            PairFlow::Break(PairStop::End(Value::Null)) => acc,
            PairFlow::Break(_) => 0,
        });
        if len > 0 { Some(len) } else { None }
    }

    fn iter(&self) -> PairIterator {
        PairIterator::new(self)
    }

    fn typename(&self) -> &str {
        if self.is_list() {
            TypeName::LIST
        } else {
            TypeName::PAIR
        }
    }
}

impl Display for Pair {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        // TODO: can i get rid of this first step
        let mut it = self.iter();
        if let Some(PairFlow::Continue(car)) = it.next() {
            car.fmt(f)?;
        }
        for v in it {
            match v {
                PairFlow::Continue(v) => write!(f, " {v}")?,
                PairFlow::Break(PairStop::Cycle(_)) => f.write_str(" . dup…")?,
                PairFlow::Break(PairStop::End(Value::Null)) => (),
                PairFlow::Break(PairStop::End(v)) => write!(f, " . {v}")?,
            }
        }
        Ok(())
    }
}

impl AsRef<Self> for Pair {
    fn as_ref(&self) -> &Self {
        self
    }
}

pub(crate) struct ValueMessage<'a>(&'a Value);

impl Display for ValueMessage<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self.0 {
            Value::Ast(prg) => writeln!(f, "{prg:#?}"),
            Value::TokenList(lines) => TokenLinesMessage(lines).fmt(f),
            _ => Ok(()),
        }
    }
}

pub(crate) struct TypeName<'a>(&'a Value);

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

type PairFlow = ControlFlow<PairStop, Value>;

enum PairStop {
    Cycle(Pair),
    End(Value),
}

struct PairIterator {
    head: Option<Pair>,
    tail: Option<PairStop>,
    visited: HashSet<*const Pair>,
}

impl PairIterator {
    fn new(head: &Pair) -> Self {
        let mut visited = HashSet::new();
        visited.insert(ptr::from_ref(head));
        Self {
            head: Some(head.clone()),
            tail: None,
            visited,
        }
    }
}

impl Iterator for PairIterator {
    type Item = PairFlow;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(curr) = self.head.take() {
            if let Some(p) = curr.cdr.as_refpair() {
                let pref = p.as_ref();
                let pp = ptr::from_ref(pref);
                if self.visited.contains(&pp) {
                    self.tail = Some(PairStop::Cycle(pref.clone()));
                } else {
                    self.visited.insert(pp);
                    let _ = self.head.insert(pref.clone());
                }
            } else {
                self.tail = Some(PairStop::End(curr.cdr));
            }
            Some(Self::Item::Continue(curr.car))
        } else if let Some(end) = self.tail.take() {
            Some(Self::Item::Break(end))
        } else {
            None
        }
    }
}

fn write_seq<'a, T: 'a + Display>(
    prefix: &str,
    seq: &'a [T],
    f: &mut Formatter<'_>,
) -> fmt::Result {
    write!(
        f,
        "{prefix}({})",
        seq.iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>()
            .join(" ")
    )
}
