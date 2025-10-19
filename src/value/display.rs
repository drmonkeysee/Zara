use super::{Pair, Traverse, Value};
use crate::{
    lex::{DisplayTokenLines, TokenLinesMessage},
    string::{CharDatum, StrDatum},
};
use std::{
    borrow::Cow,
    fmt::{self, Display, Formatter, Write},
};

pub(crate) struct Datum<'a>(pub(super) &'a Value);

impl Display for Datum<'_> {
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
            Value::Pair(p) => PairDatum::new(p).fmt(f),
            Value::PairMut(p) => PairDatum::new(&p.borrow()).fmt(f),
            Value::Port(p) => write!(f, "#<port {p}>"),
            Value::Procedure(p) => p.fmt(f),
            Value::String(s) => StrDatum(s).fmt(f),
            Value::StringMut(s) => StrDatum(&s.borrow()).fmt(f),
            Value::Symbol(s) => s.as_datum().fmt(f),
            Value::TokenList(lines) => DisplayTokenLines(lines).fmt(f),
            Value::Unspecified => f.write_str("#<unspecified>"),
            Value::Vector(v) => VecDatum::new(v).fmt(f),
            Value::VectorMut(v) => VecDatum::new(&v.borrow()).fmt(f),
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
            Value::Eof => f.write_str("end-of-file"),
            Value::Error(_) => f.write_str(Self::ERROR),
            Value::Intrinsic(_) => f.write_str("intrinsic"),
            Value::Null => f.write_str("null"),
            Value::Number(_) => f.write_str(Self::NUMBER),
            Value::Pair(p) => f.write_str(p.typename()),
            Value::PairMut(p) => f.write_str(p.borrow().typename()),
            Value::Port(_) => f.write_str("port"),
            Value::Procedure(_) => f.write_str("procedure"),
            Value::String(_) | Value::StringMut(_) => f.write_str(Self::STRING),
            Value::Symbol(_) => f.write_str(Self::SYMBOL),
            Value::TokenList(_) => f.write_str("token list"),
            Value::Unspecified => f.write_str("unspecified"),
            Value::Vector(_) | Value::VectorMut(_) => f.write_str(Self::VECTOR),
        }
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
