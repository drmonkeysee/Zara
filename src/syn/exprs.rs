use crate::{lex::Token, literal::Literal};
use std::{
    fmt::{Display, Error, Formatter},
    write,
};

pub(super) type ExpressionResult = Result<Expression, ExpressionError>;

#[derive(Debug)]
pub enum Expression {
    Ast(Box<Expression>),
    Begin(Vec<Expression>),
    Empty,
    Literal(Literal),
    TokenStream(Vec<Token>),
}

#[derive(Debug)]
pub(super) enum ExpressionError {
    Unimplemented(Token),
}

impl Expression {
    pub fn has_repr(&self) -> bool {
        if let Self::Empty = self {
            false
        } else {
            true
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match self {
            Self::Ast(expr) => write!(f, "{{{:?}}}", expr),
            Self::Empty => Ok(()),
            Self::Literal(lit) => f.write_str(&lit.to_string()),
            _ => write!(f, "#undef({:?})", self),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn display_ast() {
        let expr = Expression::Ast(Box::new(Expression::Begin(vec![
            Expression::Literal(Literal::Character('a')),
            Expression::Literal(Literal::Character('b')),
            Expression::Literal(Literal::Character('c')),
        ])));

        assert_eq!(
            expr.to_string(),
            format!(
                "{{{:?}}}",
                Expression::Begin(vec![
                    Expression::Literal(Literal::Character('a')),
                    Expression::Literal(Literal::Character('b')),
                    Expression::Literal(Literal::Character('c')),
                ])
            )
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

        assert_eq!(expr.to_string(), format!("#undef({:?})", expr));
    }
}
