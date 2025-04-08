use crate::{
    lex::{DisplayTokenLines, TokenLine, TokenLinesMessage},
    literal::Literal,
    syntax::Expression,
};
use std::fmt::{self, Display, Formatter};

#[derive(Debug)]
pub(crate) enum Value {
    Ast(Box<Expression>),
    Literal(Literal),
    TokenList(Vec<TokenLine>),
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
            Value::Ast(expr) => format!("{{{expr:?}}}").fmt(f),
            Value::Literal(lit) => lit.as_datum().fmt(f),
            Value::TokenList(lines) => DisplayTokenLines(lines).fmt(f),
        }
    }
}

pub(crate) struct ValueMessage<'a>(&'a Value);

impl Display for ValueMessage<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self.0 {
            Value::Ast(expr) => writeln!(f, "{expr:#?}"),
            Value::TokenList(lines) => TokenLinesMessage(lines).fmt(f),
            _ => Ok(()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn display_ast() {
        let val = Value::Ast(
            Expression::Begin(vec![
                Expression::literal(Literal::Character('a')),
                Expression::literal(Literal::Character('b')),
                Expression::literal(Literal::Character('c')),
            ])
            .into(),
        );

        assert_eq!(
            val.as_datum().to_string(),
            "{Begin([Constant(Literal(Character('a'))), Constant(Literal(Character('b'))), Constant(Literal(Character('c')))])}"
        );
    }
}
