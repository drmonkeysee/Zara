use crate::{constant::Constant, lex::TokenKind, txt::TextLine, value::Value};
use std::{
    error::Error,
    fmt::{self, Display, Formatter},
    iter::Peekable,
    ops::Range,
    rc::Rc,
};

#[derive(Debug)]
pub(crate) struct Program(pub(super) Box<[Expression]>);

impl Program {
    pub(super) fn new(seq: impl Into<Box<[Expression]>>) -> Self {
        Self(seq.into())
    }

    pub(crate) fn eval(self) -> Option<Value> {
        self.0.into_iter().map(Expression::eval).last()?
    }
}

#[derive(Debug)]
pub(super) struct ExpressionType<T> {
    pub(super) ctx: ExprCtx,
    pub(super) kind: T,
}

#[derive(Debug)]
pub(super) struct ExprCtx {
    pub(super) span: Range<usize>,
    pub(super) txt: Rc<TextLine>,
}

impl ExprCtx {
    pub(super) fn into_error(self, kind: ExpressionErrorKind) -> ExpressionError {
        ExpressionError { kind, ctx: self }
    }
}

pub(super) type Expression = ExpressionType<ExpressionKind>;

impl Expression {
    pub(super) fn constant(con: Constant, ctx: ExprCtx) -> Self {
        Self {
            ctx,
            kind: ExpressionKind::Literal(Value::Constant(con)),
        }
    }

    fn eval(self) -> Option<Value> {
        match self.kind {
            ExpressionKind::Call { .. } => todo!("no idea what to do here"),
            ExpressionKind::Empty => None,
            ExpressionKind::Identifier(_) => {
                todo!("this is dependent on current environment frame")
            }
            ExpressionKind::Literal(v) => Some(v),
        }
    }
}

#[derive(Debug)]
pub(super) enum ExpressionKind {
    Call {
        args: Box<[Expression]>,
        proc: Box<Expression>,
    },
    // TODO: dump Empty in favor of Option<..>?
    Empty,
    Identifier(Box<str>),
    Literal(Value),
}

pub(super) struct ProgramError;

pub(super) type ExpressionError = ExpressionType<ExpressionErrorKind>;

impl Display for ExpressionError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        self.kind.fmt(f)
    }
}

impl Error for ExpressionError {}

#[derive(Debug)]
pub(super) enum ExpressionErrorKind {
    // TODO: what do these two need to store?
    ByteVectorInvalidItem,
    ByteVectorOutOfRange,
    ByteVectorUnterminated,
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
            Self::ByteVectorInvalidItem => todo!(),
            Self::ByteVectorOutOfRange => todo!(),
            Self::ByteVectorUnterminated => "unterminated bytevector".fmt(f),
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

pub(super) struct GroupBy<I> {
    peek: I,
}

impl<'a, I: Iterator<Item = &'a ExpressionError>> Iterator for GroupBy<Peekable<I>> {
    type Item = (&'a TextLine, Vec<&'a ExpressionError>);

    // NOTE: this assumes grouped items are contiguous in the original sequence
    fn next(&mut self) -> Option<Self::Item> {
        let start = self.peek.next()?;
        let key = &start.ctx.txt;
        let mut group = vec![start];
        while let Some(err) = self.peek.next_if(|err| Rc::ptr_eq(key, &err.ctx.txt)) {
            group.push(err);
        }
        Some((Rc::as_ref(key), group))
    }
}

pub(super) trait PeekableExt<I: Iterator> {
    fn groupby_txt(self) -> GroupBy<Self>
    where
        Self: Sized;
}

impl<'a, I: Iterator<Item = &'a ExpressionError>> PeekableExt<I> for Peekable<I> {
    fn groupby_txt(self) -> GroupBy<Self> {
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
        use crate::testutil::{make_textline, some_or_fail};

        #[test]
        fn empty() {
            let expr = Expression {
                ctx: ExprCtx {
                    span: 0..0,
                    txt: make_textline().into(),
                },
                kind: ExpressionKind::Empty,
            };

            let o = expr.eval();

            assert!(o.is_none());
        }

        #[test]
        fn constant() {
            let expr = Expression::constant(
                Constant::Boolean(true),
                ExprCtx {
                    span: 0..6,
                    txt: make_textline().into(),
                },
            );

            let o = expr.eval();

            let v = some_or_fail!(o);
            assert!(matches!(v, Value::Constant(Constant::Boolean(true))));
        }

        #[test]
        fn empty_program() {
            let prg = Program::new([]);

            let o = prg.eval();

            assert!(o.is_none());
        }

        #[test]
        fn program() {
            let txt = make_textline().into();
            let prg = Program::new([
                Expression::constant(
                    Constant::Boolean(true),
                    ExprCtx {
                        span: 0..6,
                        txt: Rc::clone(&txt),
                    },
                ),
                Expression::constant(
                    Constant::Character('a'),
                    ExprCtx {
                        span: 6..8,
                        txt: Rc::clone(&txt),
                    },
                ),
                Expression::constant(
                    Constant::Character('b'),
                    ExprCtx {
                        span: 8..10,
                        txt: Rc::clone(&txt),
                    },
                ),
            ]);

            let o = prg.eval();

            let v = some_or_fail!(o);
            assert!(matches!(v, Value::Constant(Constant::Character('b'))));
        }
    }

    mod error {
        use super::*;
        use crate::testutil::make_textline;

        #[test]
        fn display_unterminated_bytevector() {
            let err = ExpressionError {
                ctx: ExprCtx {
                    span: 0..5,
                    txt: make_textline().into(),
                },
                kind: ExpressionErrorKind::ByteVectorUnterminated,
            };

            assert_eq!(err.to_string(), "unterminated bytevector");
        }

        #[test]
        fn display_invalid_seq() {
            let err = ExpressionError {
                ctx: ExprCtx {
                    span: 0..5,
                    txt: make_textline().into(),
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
                    span: 0..5,
                    txt: make_textline().into(),
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
                    span: 0..5,
                    txt: make_textline().into(),
                },
                kind: ExpressionErrorKind::IdentifierUnterminated,
            };

            assert_eq!(err.to_string(), "unterminated verbatim identifier");
        }

        #[test]
        fn display_unterminated_list() {
            let err = ExpressionError {
                ctx: ExprCtx {
                    span: 0..5,
                    txt: make_textline().into(),
                },
                kind: ExpressionErrorKind::ListUnterminated,
            };

            assert_eq!(err.to_string(), "unterminated list expression");
        }

        #[test]
        fn display_invalid_str() {
            let err = ExpressionError {
                ctx: ExprCtx {
                    span: 0..5,
                    txt: make_textline().into(),
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
                    span: 0..5,
                    txt: make_textline().into(),
                },
                kind: ExpressionErrorKind::StrUnterminated,
            };

            assert_eq!(err.to_string(), "unterminated string constant");
        }

        #[test]
        fn display_unimplemented() {
            let err = ExpressionError {
                ctx: ExprCtx {
                    span: 0..5,
                    txt: make_textline().into(),
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
        use crate::testutil::{make_textline, make_textline_no};
        use std::ptr;

        #[test]
        fn empty() {
            let errs = Vec::new();

            let groups: Vec<_> = errs.into_iter().peekable().groupby_txt().collect();

            assert!(groups.is_empty());
        }

        #[test]
        fn single() {
            let txt = Rc::new(make_textline());
            let errs = [ExpressionError {
                ctx: ExprCtx {
                    span: 0..5,
                    txt: Rc::clone(&txt),
                },
                kind: ExpressionErrorKind::StrUnterminated,
            }];

            let groups: Vec<_> = errs.iter().peekable().groupby_txt().collect();

            assert_eq!(groups.len(), 1);
            let (key, group) = &groups[0];
            assert!(ptr::eq(*key, Rc::as_ptr(&txt)));
            assert_eq!(group.len(), 1);
            assert!(ptr::eq(group[0], &errs[0]));
        }

        #[test]
        fn one_group() {
            let txt = Rc::new(make_textline());
            let errs = [
                ExpressionError {
                    ctx: ExprCtx {
                        span: 0..5,
                        txt: Rc::clone(&txt),
                    },
                    kind: ExpressionErrorKind::StrUnterminated,
                },
                ExpressionError {
                    ctx: ExprCtx {
                        span: 5..7,
                        txt: Rc::clone(&txt),
                    },
                    kind: ExpressionErrorKind::ListUnterminated,
                },
                ExpressionError {
                    ctx: ExprCtx {
                        span: 7..10,
                        txt: Rc::clone(&txt),
                    },
                    kind: ExpressionErrorKind::CommentBlockUnterminated,
                },
            ];

            let groups: Vec<_> = errs.iter().peekable().groupby_txt().collect();

            assert_eq!(groups.len(), 1);
            let (key, group) = &groups[0];
            assert!(ptr::eq(*key, Rc::as_ptr(&txt)));
            assert_eq!(group.len(), 3);
            assert!(ptr::eq(group[0], &errs[0]));
            assert!(ptr::eq(group[1], &errs[1]));
            assert!(ptr::eq(group[2], &errs[2]));
        }

        #[test]
        fn two_groups() {
            let txt1 = Rc::new(make_textline_no(1));
            let txt2 = Rc::new(make_textline_no(2));
            let errs = [
                ExpressionError {
                    ctx: ExprCtx {
                        span: 0..5,
                        txt: Rc::clone(&txt1),
                    },
                    kind: ExpressionErrorKind::StrUnterminated,
                },
                ExpressionError {
                    ctx: ExprCtx {
                        span: 5..7,
                        txt: Rc::clone(&txt1),
                    },
                    kind: ExpressionErrorKind::ListUnterminated,
                },
                ExpressionError {
                    ctx: ExprCtx {
                        span: 0..3,
                        txt: Rc::clone(&txt2),
                    },
                    kind: ExpressionErrorKind::CommentBlockUnterminated,
                },
            ];

            let groups: Vec<_> = errs.iter().peekable().groupby_txt().collect();

            assert_eq!(groups.len(), 2);

            let (key, group) = &groups[0];
            assert!(ptr::eq(*key, Rc::as_ptr(&txt1)));
            assert_eq!(group.len(), 2);
            assert!(ptr::eq(group[0], &errs[0]));
            assert!(ptr::eq(group[1], &errs[1]));

            let (key, group) = &groups[1];
            assert!(ptr::eq(*key, Rc::as_ptr(&txt2)));
            assert_eq!(group.len(), 1);
            assert!(ptr::eq(group[0], &errs[2]));
        }

        #[test]
        fn non_contiguous() {
            let txt1 = Rc::new(make_textline_no(1));
            let txt2 = Rc::new(make_textline_no(2));
            let errs = [
                ExpressionError {
                    ctx: ExprCtx {
                        span: 0..5,
                        txt: Rc::clone(&txt1),
                    },
                    kind: ExpressionErrorKind::StrUnterminated,
                },
                ExpressionError {
                    ctx: ExprCtx {
                        span: 0..3,
                        txt: Rc::clone(&txt2),
                    },
                    kind: ExpressionErrorKind::CommentBlockUnterminated,
                },
                ExpressionError {
                    ctx: ExprCtx {
                        span: 5..7,
                        txt: Rc::clone(&txt1),
                    },
                    kind: ExpressionErrorKind::ListUnterminated,
                },
            ];

            let groups: Vec<_> = errs.iter().peekable().groupby_txt().collect();

            assert_eq!(groups.len(), 3);

            let (key, group) = &groups[0];
            assert!(ptr::eq(*key, Rc::as_ptr(&txt1)));
            assert_eq!(group.len(), 1);
            assert!(ptr::eq(group[0], &errs[0]));

            let (key, group) = &groups[1];
            assert!(ptr::eq(*key, Rc::as_ptr(&txt2)));
            assert_eq!(group.len(), 1);
            assert!(ptr::eq(group[0], &errs[1]));

            let (key, group) = &groups[2];
            assert!(ptr::eq(*key, Rc::as_ptr(&txt1)));
            assert_eq!(group.len(), 1);
            assert!(ptr::eq(group[0], &errs[2]));
        }
    }
}
