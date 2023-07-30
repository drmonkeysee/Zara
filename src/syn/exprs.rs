use crate::{lex::Token, literal::Literal};
use std::fmt::{Display, Formatter};

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
        !matches!(self, Self::Empty)
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Ast(expr) => write!(f, "{{{:?}}}", expr),
            Self::Empty => Ok(()),
            Self::Literal(lit) => f.write_str(&lit.to_string()),
            Self::TokenStream(tokens) => write!(f, "{:?}", tokens.as_slice()),
            _ => write!(f, "#<expr_undef({:?})>", self),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lex::TokenKind;

    #[test]
    fn display_ast() {
        let expr = Expression::Ast(Box::new(Expression::Begin(vec![
            Expression::Literal(Literal::Character('a')),
            Expression::Literal(Literal::Character('b')),
            Expression::Literal(Literal::Character('c')),
        ])));

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
    fn display_token_stream() {
        let expr = Expression::TokenStream(vec![
            Token {
                kind: TokenKind::ParenLeft,
                span: 0..1,
            },
            Token {
                kind: TokenKind::Literal(Literal::Boolean(false)),
                span: 1..3,
            },
            Token {
                kind: TokenKind::ParenRight,
                span: 3..4,
            },
        ]);

        // TODO: TokenStream probably needs to be more than a vec since
        // it will need the src text as well; that will make this display
        // easier to get right.
        assert_eq!(
            expr.to_string(),
            "[LPAREN[0..1](\"(\"), LITERAL<Boolean(false)>[1..3](\"#f\"), RPAREN[3..4](\")\")]"
        );
    }

    #[test]
    fn display_invalid_expr() {
        let expr = Expression::Begin(vec![
            Expression::Literal(Literal::Character('a')),
            Expression::Literal(Literal::Character('b')),
            Expression::Literal(Literal::Character('c')),
        ]);

        assert_eq!(expr.to_string(), format!("#<expr_undef({:?})>", expr));
    }
}
