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
            Self::Ast(expr) => write!(f, "{{{expr:?}}}"),
            Self::Empty => Ok(()),
            Self::Literal(lit) => lit.fmt(f),
            Self::TokenStream(lexlines) => f.write_str(&format_token_stream(lexlines)),
            _ => write!(f, "#<expression-display-undefined({self:?})>"),
        }
    }
}

fn format_token_stream(lexlines: &[LexLine]) -> String {
    if lexlines.len() < 2 {
        format!(
            "[{}]",
            lexlines
                .iter()
                .map(|line| line.to_string())
                .collect::<String>()
        )
    } else {
        format!(
            "[\n{}]",
            lexlines
                .iter()
                .map(|line| format!("\t{line},\n"))
                .collect::<String>()
        )
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
    fn display_empty_token_stream() {
        let expr = Expression::TokenStream(Vec::new());

        assert_eq!(expr.to_string(), "[]");
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
            "[1:LPAREN[0..1](\"(\"), LITERAL<Boolean(false)>[1..3](\"#f\"), RPAREN[3..4](\")\")]"
        );
    }

    #[test]
    fn display_multiline_token_stream() {
        let expr = Expression::TokenStream(vec![
            LexLine(
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
            ),
            LexLine(
                vec![
                    Token {
                        kind: TokenKind::ParenLeft,
                        span: 0..1,
                    },
                    Token {
                        kind: TokenKind::Literal(Literal::Boolean(true)),
                        span: 2..4,
                    },
                    Token {
                        kind: TokenKind::ParenRight,
                        span: 5..6,
                    },
                ],
                TextLine {
                    ctx: TextContext {
                        name: "mylib".to_owned(),
                        path: None,
                    }
                    .into(),
                    line: "( #t )".to_owned(),
                    lineno: 2,
                },
            ),
            LexLine(
                vec![
                    Token {
                        kind: TokenKind::ParenLeft,
                        span: 0..1,
                    },
                    Token {
                        kind: TokenKind::Literal(Literal::Character('a')),
                        span: 1..4,
                    },
                    Token {
                        kind: TokenKind::ParenRight,
                        span: 4..5,
                    },
                ],
                TextLine {
                    ctx: TextContext {
                        name: "mylib".to_owned(),
                        path: None,
                    }
                    .into(),
                    line: "(#\\a)".to_owned(),
                    lineno: 3,
                },
            ),
        ]);

        assert_eq!(
            expr.to_string(),
            "[\n\
            \t1:LPAREN[0..1](\"(\"), LITERAL<Boolean(false)>[1..3](\"#f\"), RPAREN[3..4](\")\"),\n\
            \t2:LPAREN[0..1](\"(\"), LITERAL<Boolean(true)>[2..4](\"#t\"), RPAREN[5..6](\")\"),\n\
            \t3:LPAREN[0..1](\"(\"), LITERAL<Character('a')>[1..4](\"#\\a\"), RPAREN[4..5](\")\"),\n\
            ]"
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
            format!("#<expression-display-undefined({expr:?})>")
        );
    }
}
