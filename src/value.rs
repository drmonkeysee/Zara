macro_rules! zlist {
    () => {
        Value::null()
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
    eval::Procedure,
    lex::{DisplayTokenLines, TokenLine, TokenLinesMessage},
    number::{Number, Real},
    string::{CharDatum, StrDatum, SymbolDatum},
    syntax::Program,
};
use std::{
    fmt::{self, Display, Formatter},
    rc::Rc,
};
pub(crate) use zlist;

#[derive(Clone, Debug)]
pub(crate) enum Value {
    Ast(Rc<Program>),
    Boolean(bool),
    ByteVector(Rc<[u8]>),
    Character(char),
    Number(Number),
    Pair(Option<Rc<Pair>>),
    Procedure(Rc<Procedure>),
    String(Rc<str>),
    Symbol(Rc<str>),
    TokenList(Rc<[TokenLine]>),
    Unspecified,
    Vector(Rc<[Value]>),
    /*
     * TODO: Mutable Values
     * StringMut(Rc<RefCell<str>>)
     * PairMut(Option<Rc<RefCell<Pair>>>) ?? is refcell needed here? or is mut just swapping out the pair value
     * VectorMut(Rc<RefCell<[Value]>>)
     * ByteVectorMut(Rc<RefCell<[u8]>>)
     */
}

impl Value {
    pub(crate) fn null() -> Self {
        Self::Pair(None)
    }

    #[allow(clippy::similar_names, reason = "lisp terms-of-art")]
    pub(crate) fn cons(car: Value, cdr: Value) -> Self {
        Self::Pair(Some(Pair { car, cdr }.into()))
    }

    pub(crate) fn list<I>(items: I) -> Self
    where
        I: IntoIterator<Item = Self>,
        <I as IntoIterator>::IntoIter: DoubleEndedIterator,
    {
        items
            .into_iter()
            .rev()
            .fold(zlist![], |head, item| Self::cons(item, head))
    }

    pub(crate) fn improper_list<I>(items: I) -> Self
    where
        I: IntoIterator<Item = Self>,
        <I as IntoIterator>::IntoIter: DoubleEndedIterator,
    {
        items
            .into_iter()
            .rev()
            .reduce(|head, item| Self::cons(item, head))
            .unwrap_or_else(Self::null)
    }

    pub(crate) fn real(r: impl Into<Real>) -> Self {
        Self::Number(Number::real(r))
    }

    pub(crate) fn string(s: impl Into<Rc<str>>) -> Self {
        Self::String(s.into())
    }

    pub(crate) fn symbol(s: impl Into<Rc<str>>) -> Self {
        Self::Symbol(s.into())
    }

    pub(crate) fn vector(items: impl IntoIterator<Item = Self>) -> Self {
        Self::Vector(items.into_iter().collect::<Rc<[_]>>())
    }

    // NOTE: procedure eq? -> is same object
    pub(crate) fn is(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Ast(a), Self::Ast(b)) => Rc::ptr_eq(a, b),
            (Self::Boolean(a), Self::Boolean(b)) => a == b,
            (Self::ByteVector(a), Self::ByteVector(b)) => Rc::ptr_eq(a, b),
            (Self::Pair(None), Self::Pair(None)) | (Self::Unspecified, Self::Unspecified) => true,
            (Self::Pair(Some(a)), Self::Pair(Some(b))) => Rc::ptr_eq(a, b),
            (Self::Procedure(a), Self::Procedure(b)) => Rc::ptr_eq(a, b),
            (Self::String(a), Self::String(b)) | (Self::Symbol(a), Self::Symbol(b)) => {
                Rc::ptr_eq(a, b)
            }
            (Self::TokenList(a), Self::TokenList(b)) => Rc::ptr_eq(a, b),
            (Self::Vector(a), Self::Vector(b)) => Rc::ptr_eq(a, b),
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

    pub(crate) fn display_message(&self) -> ValueMessage {
        ValueMessage(self)
    }

    pub(crate) fn as_typename(&self) -> TypeName {
        TypeName(self)
    }
}

// NOTE: procedure equal? -> value equality
impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        self.is_eqv(other)
            || match (self, other) {
                (Self::ByteVector(a), Self::ByteVector(b)) => a == b,
                (Self::Pair(Some(a)), Self::Pair(Some(b))) => a == b,
                (Self::String(a), Self::String(b)) => a == b,
                (Self::Vector(a), Self::Vector(b)) => a == b,
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
            Self::Character(c) => write!(f, "#\\{}", CharDatum::new(*c)),
            Self::Number(n) => n.fmt(f),
            Self::Pair(None) => f.write_str("()"),
            Self::Pair(Some(p)) => write!(f, "({p})"),
            Self::Procedure(p) => p.fmt(f),
            Self::String(s) => StrDatum(s).fmt(f),
            Self::Symbol(s) => SymbolDatum(s).fmt(f),
            Self::TokenList(lines) => DisplayTokenLines(lines).fmt(f),
            Self::Unspecified => f.write_str("#<unspecified>"),
            Self::Vector(v) => write_seq("#", v, f),
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub(crate) struct Pair {
    car: Value,
    cdr: Value,
}

impl Pair {
    pub(crate) fn is_list(&self) -> bool {
        if let Value::Pair(p) = &self.cdr {
            p.as_ref().is_none_or(|r| r.is_list())
        } else {
            false
        }
    }
}

impl Display for Pair {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.car.fmt(f)?;
        if let Value::Pair(p) = &self.cdr {
            if let Some(p) = p {
                write!(f, " {p}")?;
            }
            Ok(())
        } else {
            write!(f, " . {}", self.cdr)
        }
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
    pub(crate) const NUMBER: &'static str = "number";
    pub(crate) const STRING: &'static str = "string";
    pub(crate) const SYMBOL: &'static str = "symbol";
}

impl Display for TypeName<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.0 {
            Value::Ast(_) => f.write_str("abstract syntax tree"),
            Value::Boolean(_) => f.write_str(Self::BOOL),
            Value::ByteVector(_) => f.write_str("bytevector"),
            Value::Character(_) => f.write_str("character"),
            Value::Number(_) => f.write_str(Self::NUMBER),
            Value::Pair(None) => f.write_str("list"),
            Value::Pair(Some(p)) => f.write_str(if p.is_list() { "list" } else { "pair" }),
            Value::Procedure(_) => f.write_str("procedure"),
            Value::String(_) => f.write_str(Self::STRING),
            Value::Symbol(_) => f.write_str(Self::SYMBOL),
            Value::TokenList(_) => f.write_str("token list"),
            Value::Unspecified => f.write_str("unspecified"),
            Value::Vector(_) => f.write_str("vector"),
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
