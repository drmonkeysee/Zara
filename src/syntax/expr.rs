use crate::{lex::TokenKind, value::Value};
use std::{
    error::Error,
    fmt::{self, Display, Formatter},
    ops::Range,
};

#[derive(Debug)]
pub(crate) enum Expression {
    Begin(Vec<Expression>),
    Constant(Value),
}

impl Expression {
    pub(crate) fn eval(self) -> Value {
        match self {
            Self::Begin(seq) => seq.into_iter().last().map_or(Value::Empty, Self::eval),
            Self::Constant(val) => val,
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
