use crate::{constant::Constant, lex::TokenKind, txt::TextLine, value::Value};
use std::{
    error::Error,
    fmt::{self, Display, Formatter},
    iter::Peekable,
    ops::Range,
    rc::Rc,
};

#[derive(Debug)]
pub(super) struct ExpressionType<T> {
    pub(super) kind: T,
    pub(super) span: Range<usize>,
    pub(super) txt: Rc<TextLine>,
}

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
        let key = &start.txt;
        let mut group = vec![start];
        while let Some(err) = self.peek.next_if(|err| Rc::ptr_eq(key, &err.txt)) {
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
        fn display_unterminated_bytevector() {
            let err = ExpressionError {
                kind: ExpressionErrorKind::ByteVectorUnterminated,
                span: 0..5,
                txt: make_textline().into(),
            };

            assert_eq!(err.to_string(), "unterminated bytevector");
        }

        #[test]
        fn display_invalid_seq() {
            let err = ExpressionError {
                kind: ExpressionErrorKind::SeqInvalid(TokenKind::Comment),
                span: 0..5,
                txt: make_textline().into(),
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
                txt: make_textline().into(),
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
                txt: make_textline().into(),
            };

            assert_eq!(err.to_string(), "unterminated verbatim identifier");
        }

        #[test]
        fn display_unterminated_list() {
            let err = ExpressionError {
                kind: ExpressionErrorKind::ListUnterminated,
                span: 0..5,
                txt: make_textline().into(),
            };

            assert_eq!(err.to_string(), "unterminated list expression");
        }

        #[test]
        fn display_invalid_str() {
            let err = ExpressionError {
                kind: ExpressionErrorKind::StrInvalid(TokenKind::Comment),
                span: 0..5,
                txt: make_textline().into(),
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
                txt: make_textline().into(),
            };

            assert_eq!(err.to_string(), "unterminated string constant");
        }

        #[test]
        fn display_unimplemented() {
            let err = ExpressionError {
                kind: ExpressionErrorKind::Unimplemented(TokenKind::Comment),
                span: 0..5,
                txt: make_textline().into(),
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
                kind: ExpressionErrorKind::StrUnterminated,
                txt: Rc::clone(&txt),
                span: 0..5,
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
                    kind: ExpressionErrorKind::StrUnterminated,
                    span: 0..5,
                    txt: Rc::clone(&txt),
                },
                ExpressionError {
                    kind: ExpressionErrorKind::ListUnterminated,
                    span: 5..7,
                    txt: Rc::clone(&txt),
                },
                ExpressionError {
                    kind: ExpressionErrorKind::CommentBlockUnterminated,
                    span: 7..10,
                    txt: Rc::clone(&txt),
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
                    kind: ExpressionErrorKind::StrUnterminated,
                    span: 0..5,
                    txt: Rc::clone(&txt1),
                },
                ExpressionError {
                    kind: ExpressionErrorKind::ListUnterminated,
                    span: 5..7,
                    txt: Rc::clone(&txt1),
                },
                ExpressionError {
                    kind: ExpressionErrorKind::CommentBlockUnterminated,
                    span: 0..3,
                    txt: Rc::clone(&txt2),
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
                    kind: ExpressionErrorKind::StrUnterminated,
                    span: 0..5,
                    txt: Rc::clone(&txt1),
                },
                ExpressionError {
                    kind: ExpressionErrorKind::CommentBlockUnterminated,
                    span: 0..3,
                    txt: Rc::clone(&txt2),
                },
                ExpressionError {
                    kind: ExpressionErrorKind::ListUnterminated,
                    span: 5..7,
                    txt: Rc::clone(&txt1),
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
