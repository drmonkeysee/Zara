use crate::{
    lex::{LexLine, Token},
    literal::Literal,
};
use std::{
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
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Ast(expr) => write!(f, "{{{:?}}}", expr),
            Self::Empty => Ok(()),
            Self::Literal(lit) => f.write_str(&lit.to_string()),
            Self::TokenStream(tokens) => write!(f, "{:?}", tokens.as_slice()),
            _ => write!(f, "#<expression-display-undefined({:?})>", self),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        lex::TokenKind,
        txt::{TextContext, TextLine},
    };

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
        let expr = Expression::TokenStream(vec![LexLine(
            vec![
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
            ],
            TextLine {
                ctx: TextContext {
                    name: "mylib".to_owned(),
                    path: None,
                }
                .into(),
                line: "(#f)".to_owned(),
                lineno: 1,
            },
        )]);

        assert_eq!(
            expr.to_string(),
            "[1: LPAREN[0..1](\"(\"), LITERAL<Boolean(false)>[1..3](\"#f\"), RPAREN[3..4](\")\")]"
        );
    }

    #[test]
    fn display_invalid_expr() {
        let expr = Expression::Begin(vec![
            Expression::Literal(Literal::Character('a')),
            Expression::Literal(Literal::Character('b')),
            Expression::Literal(Literal::Character('c')),
        ]);

        assert_eq!(
            expr.to_string(),
            format!("#<expression-display-undefined({:?})>", expr)
        );
    }
}
