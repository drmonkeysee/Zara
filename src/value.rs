macro_rules! zlist {
    () => {
        Value::null()
    };
    ($exp:expr $(, $exps:expr)* $(,)?) => {
        Value::cons($exp, zlist![$($exps),*])
    };
}

#[cfg(test)]
mod tests;

use crate::{
    eval::{Arity, MAX_ARITY, Procedure},
    lex::{DisplayTokenLines, TokenLine, TokenLinesMessage},
    number::{Number, Real},
    string::{CharDatum, StrDatum, SymbolDatum},
    syntax::Program,
};
use std::{
    fmt::{self, Display, Formatter, Write},
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
    // TODO: figure out symbol table
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
        Self::Pair(Some(Pair::cons(car, cdr).into()))
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

    pub(crate) fn display_message(&self) -> ValueMessage {
        ValueMessage(self)
    }

    pub(crate) fn as_typename(&self) -> TypeName {
        TypeName(self)
    }
}

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

#[derive(Debug)]
pub(crate) struct Pair {
    car: Value,
    cdr: Value,
}

impl Pair {
    #[allow(clippy::similar_names, reason = "lisp terms-of-art")]
    pub(crate) fn cons(car: Value, cdr: Value) -> Self {
        Self { car, cdr }
    }

    fn is_list(&self) -> bool {
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

#[derive(Debug)]
pub(crate) struct Condition {
    kind: ConditionKind,
    irritants: Value,
    msg: Box<str>,
}

impl Condition {
    pub(crate) fn bind_error(name: &str) -> Self {
        Self {
            kind: ConditionKind::Env,
            irritants: Value::Unspecified,
            msg: format!("unbound variable: {name}").into(),
        }
    }

    pub(crate) fn proc_error(typename: &str) -> Self {
        Self {
            kind: ConditionKind::Env,
            irritants: Value::Unspecified,
            msg: format!("expected procedure, got: {typename}").into(),
        }
    }

    pub(crate) fn arg_error(name: &str, expected_type: &str, arg: &Value) -> Self {
        Self {
            kind: ConditionKind::Env,
            irritants: zlist![arg.clone()],
            msg: format!(
                "invalid type for arg `{name}` - expected: {expected_type}, got: {}",
                arg.as_typename()
            )
            .into(),
        }
    }

    pub(crate) fn arity_error(name: &str, expected: &Arity, actual: usize) -> Self {
        let expected = if actual > MAX_ARITY as usize {
            format!("args exceed max arity: {MAX_ARITY}")
        } else {
            format!(
                "expected{}: {}",
                if expected.is_empty() { "" } else { " at least" },
                expected.start
            )
        };
        Self {
            kind: ConditionKind::Env,
            irritants: Value::Unspecified,
            msg: format!("{name} arity mismatch - {expected}, got: {actual}",).into(),
        }
    }

    pub(crate) fn system_error(msg: impl Into<Box<str>>) -> Self {
        Self {
            kind: ConditionKind::System,
            irritants: Value::Unspecified,
            msg: msg.into(),
        }
    }
}

impl Display for Condition {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "#<{} \"{}\"", self.kind, self.msg)?;
        match &self.irritants {
            Value::Unspecified => (),
            v => write!(f, " {v}")?,
        }
        f.write_char('>')
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
    pub(crate) const STRING: &'static str = "string";
}

impl Display for TypeName<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.0 {
            Value::Ast(_) => f.write_str("abstract syntax tree"),
            Value::Boolean(_) => f.write_str("boolean"),
            Value::ByteVector(_) => f.write_str("bytevector"),
            Value::Character(_) => f.write_str("character"),
            Value::Number(_) => f.write_str("number"),
            Value::Pair(None) => f.write_str("list"),
            Value::Pair(Some(p)) => f.write_str(if p.is_list() { "list" } else { "pair" }),
            Value::Procedure(_) => f.write_str("procedure"),
            Value::String(_) => f.write_str(Self::STRING),
            Value::Symbol(_) => f.write_str("symbol"),
            Value::TokenList(_) => f.write_str("token list"),
            Value::Unspecified => f.write_str("unspecified"),
            Value::Vector(_) => f.write_str("vector"),
        }
    }
}

#[derive(Debug)]
enum ConditionKind {
    Env,
    #[allow(dead_code, reason = "not yet implemented")]
    File,
    #[allow(dead_code, reason = "not yet implemented")]
    General,
    #[allow(dead_code, reason = "not yet implemented")]
    Read,
    System,
}

impl Display for ConditionKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Env => f.write_str("env-error"),
            Self::File => f.write_str("file-error"),
            Self::General => f.write_str("exception"),
            Self::Read => f.write_str("read-error"),
            Self::System => f.write_str("sys-error"),
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
