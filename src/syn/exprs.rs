use crate::{
    lex,
    lex::{LexLine, Token},
    literal::Literal,
};
use std::{
    error::Error,
    fmt,
    fmt::{Display, Formatter},
};

pub(super) type ExpressionResult = Result<Expression, ExpressionError>;

#[derive(Debug)]
pub enum Expression {
    Ast(Box<Expression>),
    Begin(Vec<Expression>),
    Empty,
    Literal(Literal),
    TokenStream(Vec<LexLine>),
}

impl Expression {
    pub fn has_repr(&self) -> bool {
        !matches!(self, Self::Empty)
    }

    pub(crate) fn extended_display(&self) -> ExtendedExpression {
        ExtendedExpression(self)
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Ast(expr) => write!(f, "{{{expr:?}}}"),
            Self::Empty => Ok(()),
            Self::Literal(lit) => lit.fmt(f),
            Self::TokenStream(lines) => lex::display_token_stream(lines, f),
            _ => write!(f, "#<expr-display-undef({self:?})>"),
        }
    }
}

pub(crate) struct ExtendedExpression<'a>(&'a Expression);

impl Display for ExtendedExpression<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.0 {
            Expression::Ast(expr) => writeln!(f, "{{{expr:#?}}}"),
            Expression::TokenStream(lines) => lex::extended_display_token_stream(lines, f),
            _ => writeln!(f, "#<expr-extdisplay-undef({:?})>", self.0),
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
            expr.to_string(),
            "{Begin([Literal(Character('a')), Literal(Character('b')), Literal(Character('c'))])}"
        );
    }

    #[test]
    fn display_empty() {
        let expr = Expression::Empty;

        assert_eq!(expr.to_string(), "");
    }

    #[test]
    fn display_literal() {
        let expr = Expression::Literal(Literal::Boolean(true));

        assert_eq!(expr.to_string(), "#t");
    }

    #[test]
    fn display_invalid_expr() {
        let expr = Expression::Begin(vec![
            Expression::Literal(Literal::Character('a')),
            Expression::Literal(Literal::Character('b')),
            Expression::Literal(Literal::Character('c')),
        ]);

        assert_eq!(expr.to_string(), format!("#<expr-display-undef({expr:?})>"));
    }
}
