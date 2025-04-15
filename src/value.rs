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
}

pub(crate) struct Datum<'a>(&'a Value);

impl Display for Datum<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self.0 {
            Value::Ast(prg) => format!("{{{prg:?}}}").fmt(f),
            Value::ByteVector(_) => todo!("print as #u8(...)"),
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::Expression;

    #[test]
    fn display_ast() {
        let val = Value::Ast(
            Program::new([
                Expression::constant(Constant::Character('a')),
                Expression::constant(Constant::Character('b')),
                Expression::constant(Constant::Character('c')),
            ])
            .into(),
        );

        assert_eq!(
            val.as_datum().to_string(),
            "{Program([Literal(Constant(Character('a'))), Literal(Constant(Character('b'))), Literal(Constant(Character('c')))])}"
        );
    }
}
