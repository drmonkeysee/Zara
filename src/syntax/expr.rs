use crate::{
    lex::{DisplayLexLines, ExtendedDisplayLexLines, LexLine, Token},
    literal::Literal,
};
use std::{
    error::Error,
    fmt,
    fmt::{Display, Formatter},
};

#[derive(Debug)]
pub enum Expression {
    Ast(Box<Expression>),
    Begin(Vec<Expression>),
    Empty,
    Literal(Literal),
    TokenList(Vec<LexLine>),
}

impl Expression {
    pub fn has_repr(&self) -> bool {
        !matches!(self, Self::Empty)
    }

    pub fn as_datum(&self) -> Datum {
        Datum(self)
    }

    pub(crate) fn extended_display(&self) -> ExtendedExpression {
        ExtendedExpression(self)
    }
}

pub struct Datum<'a>(&'a Expression);

impl Display for Datum<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.0 {
            Expression::Ast(expr) => write!(f, "{{{expr:?}}}"),
            Expression::Empty => Ok(()),
            Expression::Literal(lit) => lit.as_datum().fmt(f),
            Expression::TokenList(lines) => DisplayLexLines(lines).fmt(f),
            _ => write!(f, "#<expr-datum-undef({:?})>", self.0),
        }
    }
}

pub(crate) struct ExtendedExpression<'a>(&'a Expression);

impl Display for ExtendedExpression<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.0 {
            Expression::Ast(expr) => writeln!(f, "{expr:#?}"),
            Expression::TokenList(lines) => ExtendedDisplayLexLines(lines).fmt(f),
            _ => writeln!(f, "#<expr-extended-undef({:?})>", self.0),
        }
    }
}

#[derive(Debug)]
pub(super) enum ExpressionError {
    Unimplemented(Token),
}

impl Display for ExpressionError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "#<expr-error-display-undef({self:?})>")
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
