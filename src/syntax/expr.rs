use crate::{constant::Constant, lex::TokenKind, value::Value};
use std::{
    error::Error,
    fmt::{self, Display, Formatter},
    ops::Range,
};

#[derive(Debug)]
pub(crate) enum Expression {
    Call {
        args: Box<[Expression]>,
        proc: Box<Expression>,
    },
    // TODO: dump Empty in favor of Option<..>?
    Empty,
    Identifier(Box<str>),
    Literal(Value),
    Seq(Box<[Expression]>),
}

impl Expression {
    pub(crate) fn constant(con: Constant) -> Self {
        Self::Literal(Value::Constant(con))
    }

    pub(crate) fn eval(self) -> Option<Value> {
        match self {
            Self::Call { .. } => todo!("no idea what to do here"),
            Self::Empty => None,
            Self::Identifier(_) => todo!("this is dependent on current environment frame"),
            Self::Literal(v) => Some(v),
            Self::Seq(seq) => seq.into_iter().map(Self::eval).last()?,
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
    // TODO: what do these two need to store?
    ByteVectorOutOfRange,
    ByteVectorInvalidItem,
    CommentBlockInvalid(TokenKind),
    CommentBlockUnterminated,
    IdentifierInvalid(TokenKind),
    IdentifierUnterminated,
    ListUnterminated,
    SeqInvalid(TokenKind),
    StrInvalid(TokenKind),
    StrUnterminated,
    Unimplemented(TokenKind),
}

impl Display for ExpressionErrorKind {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::ByteVectorOutOfRange => todo!(),
            Self::ByteVectorInvalidItem => todo!(),
            Self::CommentBlockInvalid(t) => format_unexpected_error("comment block", t, f),
            // TODO: can i share tokenerrorkind display here
            Self::CommentBlockUnterminated => "unterminated block comment".fmt(f),
            Self::IdentifierInvalid(t) => format_unexpected_error("verbatim identifier", t, f),
            Self::IdentifierUnterminated => "unterminated verbatim identifier".fmt(f),
            Self::ListUnterminated => "unterminated list expression".fmt(f),
            Self::SeqInvalid(t) => format_unexpected_error("sequence", t, f),
            Self::StrInvalid(t) => format_unexpected_error("string", t, f),
            Self::StrUnterminated => "unterminated string constant".fmt(f),
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

    mod eval {
        use super::*;
        use crate::testutil::some_or_fail;

        #[test]
        fn empty() {
            let expr = Expression::Empty;

            let o = expr.eval();

            assert!(o.is_none());
        }

        #[test]
        fn constant() {
            let expr = Expression::Literal(Value::Constant(Constant::Boolean(true)));

            let o = expr.eval();

            let v = some_or_fail!(o);
            assert!(matches!(v, Value::Constant(Constant::Boolean(true))));
        }

        #[test]
        fn empty_sequence() {
            let expr = Expression::Seq([].into());

            let o = expr.eval();

            assert!(o.is_none());
        }

        #[test]
        fn sequence() {
            let expr = Expression::Seq(
                [
                    Expression::Literal(Value::Constant(Constant::Boolean(true))),
                    Expression::Literal(Value::Constant(Constant::Character('a'))),
                    Expression::Literal(Value::Constant(Constant::Character('b'))),
                ]
                .into(),
            );

            let o = expr.eval();

            let v = some_or_fail!(o);
            assert!(matches!(v, Value::Constant(Constant::Character('b'))));
        }
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
        fn display_invalid_identifier() {
            let err = ExpressionError {
                kind: ExpressionErrorKind::IdentifierInvalid(TokenKind::Comment),
                span: 0..5,
            };

            assert_eq!(
                err.to_string(),
                format!(
                    "unexpected token in verbatim identifier: {}",
                    TokenKind::Comment
                )
            );
        }

        #[test]
        fn display_unterminated_identifier() {
            let err = ExpressionError {
                kind: ExpressionErrorKind::IdentifierUnterminated,
                span: 0..5,
            };

            assert_eq!(err.to_string(), "unterminated verbatim identifier");
        }

        #[test]
        fn display_unterminated_list() {
            let err = ExpressionError {
                kind: ExpressionErrorKind::ListUnterminated,
                span: 0..5,
            };

            assert_eq!(err.to_string(), "unterminated list expression");
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

            assert_eq!(err.to_string(), "unterminated string constant");
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
