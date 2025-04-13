use crate::{constant::Constant, lex::TokenKind, txt::TextLine, value::Value};
use std::{
    error::Error,
    fmt::{self, Display, Formatter},
    iter::Peekable,
    ops::Range,
    rc::Rc,
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
pub(super) struct ExprCtx {
    pub(super) span: Range<usize>,
    pub(super) txt: Rc<TextLine>,
}

#[derive(Debug)]
pub(super) struct ExpressionError {
    pub(super) ctx: ExprCtx,
    pub(super) kind: ExpressionErrorKind,
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

struct GroupBy<I: Iterator> {
    peek: Peekable<I>,
}

impl<I: Iterator> GroupBy<I> {
    fn new(peek: Peekable<I>) -> Self {
        Self { peek }
    }
}

impl<'a, I: Iterator<Item = &'a ExpressionError>> Iterator for GroupBy<I> {
    type Item = (&'a TextLine, Vec<&'a ExpressionError>);

    // NOTE: this assumes grouped items are contiguous in the original sequence
    fn next(&mut self) -> Option<Self::Item> {
        let start = self.peek.next()?;
        let key = &start.ctx.txt;
        let mut group = vec![start];
        group.extend(self.peek.next_if(|err| Rc::ptr_eq(key, &err.ctx.txt)));
        Some((Rc::as_ref(key), group))
    }
}

pub(super) trait PeekableExt<I: Iterator> {
    fn groupby_txt(self) -> GroupBy<I>;
}

impl<'a, I: Iterator<Item = &'a ExpressionError>> PeekableExt<I> for Peekable<I> {
    fn groupby_txt(self) -> GroupBy<I> {
        GroupBy { peek: self }
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
        use crate::testutil::make_textline;

        #[test]
        fn display_invalid_seq() {
            let err = ExpressionError {
                ctx: ExprCtx {
                    txt: make_textline().into(),
                    span: 0..5,
                },
                kind: ExpressionErrorKind::SeqInvalid(TokenKind::Comment),
            };

            assert_eq!(
                err.to_string(),
                format!("unexpected token in sequence: {}", TokenKind::Comment)
            );
        }

        #[test]
        fn display_invalid_identifier() {
            let err = ExpressionError {
                ctx: ExprCtx {
                    txt: make_textline().into(),
                    span: 0..5,
                },
                kind: ExpressionErrorKind::IdentifierInvalid(TokenKind::Comment),
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
                ctx: ExprCtx {
                    txt: make_textline().into(),
                    span: 0..5,
                },
                kind: ExpressionErrorKind::IdentifierUnterminated,
            };

            assert_eq!(err.to_string(), "unterminated verbatim identifier");
        }

        #[test]
        fn display_unterminated_list() {
            let err = ExpressionError {
                ctx: ExprCtx {
                    txt: make_textline().into(),
                    span: 0..5,
                },
                kind: ExpressionErrorKind::ListUnterminated,
            };

            assert_eq!(err.to_string(), "unterminated list expression");
        }

        #[test]
        fn display_invalid_str() {
            let err = ExpressionError {
                ctx: ExprCtx {
                    txt: make_textline().into(),
                    span: 0..5,
                },
                kind: ExpressionErrorKind::StrInvalid(TokenKind::Comment),
            };

            assert_eq!(
                err.to_string(),
                format!("unexpected token in string: {}", TokenKind::Comment)
            );
        }

        #[test]
        fn display_unterminated_str() {
            let err = ExpressionError {
                ctx: ExprCtx {
                    txt: make_textline().into(),
                    span: 0..5,
                },
                kind: ExpressionErrorKind::StrUnterminated,
            };

            assert_eq!(err.to_string(), "unterminated string constant");
        }

        #[test]
        fn display_unimplemented() {
            let err = ExpressionError {
                ctx: ExprCtx {
                    txt: make_textline().into(),
                    span: 0..5,
                },
                kind: ExpressionErrorKind::Unimplemented(TokenKind::Comment),
            };

            assert_eq!(
                err.to_string(),
                format!("{} parsing not yet implemented", TokenKind::Comment)
            );
        }
    }

    mod groupby {
        use super::*;

        #[test]
        fn empty() {
            let errs = Vec::new();

            let groups: Vec<_> = errs.into_iter().peekable().groupby_txt().collect();

            assert!(groups.is_empty());
        }
    }
}
