use super::{Pair, Traverse, Value};
use crate::{
    lex::{DisplayTokenLines, TokenLinesMessage},
    string::{CharDatum, StrDatum},
};
use std::fmt::{self, Display, Formatter, Write};

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
            Value::Pair(p) => PairDatum(p).fmt(f),
            Value::PairMut(p) => PairDatum(&p.borrow()).fmt(f),
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

struct PairDatum<'a>(&'a Pair);

// TODO: the .as_datum() calls need to be dependent on top-level display
impl Display for PairDatum<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let graph = Traverse::pair(self.0);
        let head_id = self.0.node_id();

        if let Some(vs) = graph.get(head_id)
            && vs.cycle
        {
            write!(f, "#{}=", vs.label)?;
        }
        write!(f, "({}", self.0.car.as_datum())?;

        for item in self.0.cdr.iter() {
            if let Some(p) = item.as_refpair() {
                let pref = p.as_ref();
                let id = pref.node_id();
                if let Some(vs) = graph.get(id)
                    && vs.cycle
                {
                    if id == head_id {
                        write!(f, " . #{}#", vs.label)?;
                    } else {
                        write!(f, " . {}", item.as_datum())?;
                    }
                    break;
                } else {
                    write!(f, " {}", pref.car.as_datum())?;
                }
            } else if !matches!(item, Value::Null) {
                write!(f, " . {}", item.as_datum())?;
            }
            /*
            match item {
                ValItem::Cycle(Cycle(id, _)) => {
                    if let Some(vs) = graph.get(id)
                        && vs.cycle
                    {
                        write!(f, " . #{}#", vs.label)?;
                        break;
                    } else {
                        unreachable!("unvisited cycle reached");
                    }
                }
                ValItem::Element(v) => {
                    if let Some(p) = v.as_refpair() {
                        let pref = p.as_ref();
                        if let Some(vs) = graph.get(ptr::from_ref(pref).cast())
                            && vs.cycle
                        {
                            write!(f, " . {}", v.as_datum())?;
                            break;
                        } else {
                            write!(f, " {}", pref.car.as_datum())?;
                        }
                    } else if !matches!(v, Value::Null) {
                        write!(f, " . {}", v.as_datum())?;
                    }
                }
            }
            */
        }
        f.write_char(')')
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
