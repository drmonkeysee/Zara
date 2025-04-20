use crate::{
    constant::Constant,
    lex::{DisplayTokenLines, TokenLine, TokenLinesMessage},
    syntax::Program,
};
use std::fmt::{self, Display, Formatter};

#[derive(Debug)]
pub(crate) enum Value {
    Ast(Program),
    ByteVector(Box<[u8]>),
    Constant(Constant),
    TokenList(Box<[TokenLine]>),
}

impl Value {
    pub(crate) fn as_datum(&self) -> Datum {
        Datum(self)
    }

    pub(crate) fn display_message(&self) -> ValueMessage {
        ValueMessage(self)
    }

    pub(crate) fn as_typename(&self) -> TypeName {
        TypeName(self)
    }
}

pub(crate) struct Datum<'a>(&'a Value);

impl Display for Datum<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self.0 {
            Value::Ast(prg) => write!(f, "{{{prg:?}}}"),
            Value::ByteVector(bv) => write!(
                f,
                "#u8({})",
                bv.into_iter()
                    .map(|b| b.to_string())
                    .collect::<Vec<_>>()
                    .join(" ")
            ),
            Value::Constant(con) => con.as_datum().fmt(f),
            Value::TokenList(lines) => DisplayTokenLines(lines).fmt(f),
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

impl Display for TypeName<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.0 {
            Value::Ast(_) => "abstract syntax tree".fmt(f),
            Value::ByteVector(_) => "bytevector".fmt(f),
            Value::Constant(c) => c.as_typename().fmt(f),
            Value::TokenList(_) => "token list".fmt(f),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    mod display {
        use super::*;

        #[test]
        fn bytevector_datum() {
            let v = Value::ByteVector([1, 2, 3].into());

            assert_eq!(v.as_datum().to_string(), "#u8(1 2 3)");
        }

        #[test]
        fn empty_bytevector_datum() {
            let v = Value::ByteVector([].into());

            assert_eq!(v.as_datum().to_string(), "#u8()");
        }

        #[test]
        fn bytevector_typename() {
            let v = Value::ByteVector([].into());

            assert_eq!(v.as_typename().to_string(), "bytevector");
        }

        #[test]
        fn constant_datum() {
            let v = Value::Constant(Constant::Boolean(true));

            assert_eq!(v.as_datum().to_string(), "#t");
        }

        #[test]
        fn constant_typename() {
            let v = Value::Constant(Constant::Boolean(true));

            assert_eq!(v.as_typename().to_string(), "boolean");
        }
    }
}
