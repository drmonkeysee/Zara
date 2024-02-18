use crate::{
    lex::{DisplayTokenLines, Token, TokenLine, TokenLinesMessage},
    literal::Literal,
};
use std::{
    error::Error,
    fmt,
    fmt::{Display, Formatter},
};

#[derive(Debug)]
pub(crate) enum Expression {
    Ast(Box<Expression>),
    Begin(Vec<Expression>),
    Empty,
    Literal(Literal),
    TokenList(Vec<TokenLine>),
}

impl Expression {
    pub(crate) fn has_value(&self) -> bool {
        !matches!(self, Self::Empty)
    }

    pub(crate) fn as_datum(&self) -> Datum {
        Datum(self)
    }

    pub(crate) fn display_message(&self) -> ExpressionMessage {
        ExpressionMessage(self)
    }
}

pub(crate) struct Datum<'a>(&'a Expression);

impl Display for Datum<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self.0 {
            Expression::Ast(expr) => format!("{{{expr:?}}}").fmt(f),
            Expression::Begin(_) => format!("#<expr-datum-undef({:?})>", self.0).fmt(f),
            Expression::Empty => Ok(()),
            Expression::Literal(lit) => lit.as_datum().fmt(f),
            Expression::TokenList(lines) => DisplayTokenLines(lines).fmt(f),
        }
    }
}

pub(crate) struct ExpressionMessage<'a>(&'a Expression);

impl Display for ExpressionMessage<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self.0 {
            Expression::Ast(expr) => writeln!(f, "{expr:#?}"),
            Expression::TokenList(lines) => TokenLinesMessage(lines).fmt(f),
            _ => writeln!(f, "#<expr-extended-undef({:?})>", self.0),
        }
    }
}

#[derive(Debug)]
pub(super) enum ExpressionError {
    Unimplemented(Token),
}

impl Display for ExpressionError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        format!("#<expr-error-display-undef({self:?})>").fmt(f)
    }
}

impl Error for ExpressionError {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn display_ast() {
        let expr = Expression::Ast(
            Expression::Begin(vec![
                Expression::Literal(Literal::Character('a')),
                Expression::Literal(Literal::Character('b')),
                Expression::Literal(Literal::Character('c')),
            ])
            .into(),
        );

        assert_eq!(
            expr.as_datum().to_string(),
            "{Begin([Literal(Character('a')), Literal(Character('b')), Literal(Character('c'))])}"
        );
    }

    #[test]
    fn display_empty() {
        let expr = Expression::Empty;

        assert_eq!(expr.as_datum().to_string(), "");
    }

    #[test]
    fn display_invalid_expr() {
        let expr = Expression::Begin(vec![
            Expression::Literal(Literal::Character('a')),
            Expression::Literal(Literal::Character('b')),
            Expression::Literal(Literal::Character('c')),
        ]);

        assert_eq!(
            expr.as_datum().to_string(),
            format!("#<expr-datum-undef({expr:?})>")
        );
    }
}
