use crate::{
    lex::{DisplayTokenLines, TokenKind, TokenLine, TokenLinesMessage},
    literal::Literal,
};
use std::{
    error::Error,
    fmt::{self, Display, Formatter},
    ops::Range,
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
pub(super) struct ExpressionError {
    pub(super) kind: ExpressionErrorKind,
    pub(super) span: Range<usize>,
}

impl Display for ExpressionError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        self.kind.fmt(f)
    }
}

impl Error for ExpressionError {}

#[derive(Debug)]
pub(super) enum ExpressionErrorKind {
    SeqInvalid(TokenKind),
    StrInvalid(TokenKind),
    StrUnterminated,
    Unimplemented(TokenKind),
}

impl Display for ExpressionErrorKind {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::SeqInvalid(t) => format_unexpected_error("sequence", t, f),
            Self::StrInvalid(t) => format_unexpected_error("string", t, f),
            Self::StrUnterminated => "unterminated string-literal".fmt(f),
            Self::Unimplemented(t) => format!("{t} parsing not yet implemented").fmt(f),
        }
    }
}

fn format_unexpected_error(kind: &str, token: &TokenKind, f: &mut Formatter) -> fmt::Result {
    format!("unexpected token in {kind}: {token}").fmt(f)
}

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

    mod error {
        use super::*;

        #[test]
        fn display_invalid_seq() {
            let err = ExpressionError {
                kind: ExpressionErrorKind::SeqInvalid(TokenKind::Comment),
                span: 0..5,
            };

            assert_eq!(
                err.to_string(),
                format!("unexpected token in sequence: {}", TokenKind::Comment)
            );
        }

        #[test]
        fn display_invalid_str() {
            let err = ExpressionError {
                kind: ExpressionErrorKind::StrInvalid(TokenKind::Comment),
                span: 0..5,
            };

            assert_eq!(
                err.to_string(),
                format!("unexpected token in string: {}", TokenKind::Comment)
            );
        }

        #[test]
        fn display_unterminated_str() {
            let err = ExpressionError {
                kind: ExpressionErrorKind::StrUnterminated,
                span: 0..5,
            };

            assert_eq!(err.to_string(), "unterminated string-literal");
        }

        #[test]
        fn display_unimplemented() {
            let err = ExpressionError {
                kind: ExpressionErrorKind::Unimplemented(TokenKind::Comment),
                span: 0..5,
            };

            assert_eq!(
                err.to_string(),
                format!("{} parsing not yet implemented", TokenKind::Comment)
            );
        }
    }
}
