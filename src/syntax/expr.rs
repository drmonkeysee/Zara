use crate::{
    Exception,
    eval::{EvalResult, Frame},
    lex::TokenKind,
    number::NumericError,
    txt::{LineNumber, TextLine, TxtSpan},
    value::{Condition, Value, ValueRef},
};
use std::{
    error::Error,
    fmt::{self, Display, Formatter},
    iter::Peekable,
    rc::Rc,
};

#[derive(Debug)]
pub(crate) struct Program(Box<[Expression]>);

impl Program {
    pub(super) fn new(seq: impl Into<Box<[Expression]>>) -> Self {
        Self(seq.into())
    }

    pub(crate) fn eval(self, env: &Frame) -> EvalResult {
        #[allow(
            clippy::double_ended_iterator_last,
            reason = "iterator consumed intentionally"
        )]
        self.0
            .into_iter()
            .map(|expr| expr.eval(env))
            .last()
            .unwrap_or(Ok(Value::Unspecified.into()))
    }

    #[cfg(test)]
    pub(super) fn unwrap(self) -> Box<[Expression]> {
        self.0
    }
}

#[derive(Debug)]
pub(super) struct ExpressionType<T> {
    pub(super) ctx: ExprCtx,
    pub(super) kind: T,
}

#[derive(Clone, Debug)]
pub(super) struct ExprCtx {
    pub(super) span: TxtSpan,
    pub(super) txt: Rc<TextLine>,
}

impl ExprCtx {
    pub(super) fn into_expr(self, kind: ExpressionKind) -> Expression {
        Expression { kind, ctx: self }
    }

    pub(super) fn into_error(self, kind: ExpressionErrorKind) -> ExpressionError {
        ExpressionError { kind, ctx: self }
    }
}

#[derive(Clone, Copy, Debug, Default)]
pub(super) struct ExprEnd {
    pub(super) lineno: LineNumber,
    pub(super) pos: usize,
}

pub(super) type Expression = ExpressionType<ExpressionKind>;

impl Expression {
    pub(super) fn string(value: impl Into<Rc<str>>, ctx: ExprCtx) -> Self {
        Self {
            ctx,
            kind: ExpressionKind::Literal(Value::string(value)),
        }
    }

    pub(super) fn symbol(name: impl Into<Rc<str>>, ctx: ExprCtx) -> Self {
        Self {
            ctx,
            kind: ExpressionKind::Literal(Value::symbol(name)),
        }
    }

    pub(super) fn variable(name: impl Into<Box<str>>, ctx: ExprCtx) -> Self {
        Self {
            ctx,
            kind: ExpressionKind::Variable(name.into()),
        }
    }

    fn eval(self, env: &Frame) -> EvalResult {
        match self.kind {
            ExpressionKind::Call { args, proc } => eval_call(*proc, args, env),
            ExpressionKind::Literal(v) => Ok(v.into()),
            ExpressionKind::Variable(n) => env
                .bnd
                .lookup(&n)
                .ok_or_else(|| Exception::new(Condition::bind_error(&n))),
        }
    }
}

#[derive(Debug)]
pub(super) enum ExpressionKind {
    #[allow(dead_code, reason = "not yet implemented")]
    Call {
        args: Box<[Expression]>,
        proc: Box<Expression>,
    },
    Literal(Value),
    Variable(Box<str>),
}

impl ExpressionKind {
    fn as_typename(&self) -> TypeName {
        TypeName(self)
    }
}

pub(super) type ExpressionError = ExpressionType<ExpressionErrorKind>;

impl Display for ExpressionError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        self.kind.fmt(f)
    }
}

impl Error for ExpressionError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        if let ExpressionErrorKind::ByteVectorInvalidNumber(inner) = &self.kind {
            Some(inner)
        } else {
            None
        }
    }
}

#[derive(Debug)]
pub(super) enum ExpressionErrorKind {
    ByteVectorInvalidItem(ExpressionKind),
    ByteVectorInvalidNumber(NumericError),
    ByteVectorUnterminated,
    CommentBlockInvalid(TokenKind),
    CommentBlockUnterminated,
    DatumExpected,
    DatumInvalid(ExpressionKind),
    IdentifierInvalid(TokenKind),
    IdentifierUnterminated,
    ListUnterminated,
    PairIncomplete,
    PairUnexpected,
    PairUnterminated,
    ProcedureEmpty,
    SeqInvalid(TokenKind),
    StrInvalid(TokenKind),
    StrUnterminated,
    Unimplemented(TokenKind),
    VectorInvalidItem(ExpressionKind),
    VectorUnterminated,
}

impl Display for ExpressionErrorKind {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::ByteVectorInvalidItem(k) => {
                write!(f, "expected byte literal, got: {}", k.as_typename())
            }
            Self::ByteVectorInvalidNumber(err) => err.fmt(f),
            Self::ByteVectorUnterminated => f.write_str("unterminated bytevector"),
            Self::CommentBlockInvalid(t) => format_unexpected_token("comment block", t, f),
            // TODO: can i share tokenerrorkind display here
            Self::CommentBlockUnterminated => f.write_str("unterminated block comment"),
            Self::DatumExpected => f.write_str("expected datum"),
            Self::DatumInvalid(k) => write!(f, "unexpected datum type: {}", k.as_typename()),
            Self::IdentifierInvalid(t) => format_unexpected_token("verbatim identifier", t, f),
            Self::IdentifierUnterminated => f.write_str("unterminated verbatim identifier"),
            Self::ListUnterminated => f.write_str("unterminated list expression"),
            Self::PairIncomplete => f.write_str("missing first pair expression"),
            Self::PairUnexpected => f.write_str("unexpected pair syntax"),
            Self::PairUnterminated => f.write_str("unterminated pair expression"),
            Self::ProcedureEmpty => f.write_str("empty procedure call"),
            Self::SeqInvalid(t) => format_unexpected_token("sequence", t, f),
            Self::StrInvalid(t) => format_unexpected_token("string", t, f),
            Self::StrUnterminated => f.write_str("unterminated string constant"),
            Self::Unimplemented(t) => write!(f, "{t} parsing not yet implemented"),
            Self::VectorInvalidItem(k) => {
                write!(f, "unexpected vector item type: {}", k.as_typename())
            }
            Self::VectorUnterminated => f.write_str("unterminated vector"),
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

pub(super) trait PeekableExt {
    fn groupby_txt(self) -> GroupBy<Self>
    where
        Self: Sized;
}

impl<'a, I: Iterator<Item = &'a ExpressionError>> PeekableExt for Peekable<I> {
    fn groupby_txt(self) -> GroupBy<Self> {
        GroupBy { peek: self }
    }
}

struct TypeName<'a>(&'a ExpressionKind);

impl Display for TypeName<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.0 {
            ExpressionKind::Call { .. } => f.write_str("procedure call"),
            ExpressionKind::Variable(_) => f.write_str("variable"),
            ExpressionKind::Literal(val) => val.as_typename().fmt(f),
        }
    }
}

fn eval_call(proc: Expression, args: Box<[Expression]>, env: &Frame) -> EvalResult {
    let proc = proc.eval(env)?;
    let Value::Procedure(p) = &*proc else {
        return Err(Exception::new(Condition::proc_error(
            &proc.as_typename().to_string(),
        )));
    };
    if !p.matches_arity(args.len()) {
        return Err(Exception::new(Condition::arity_error(
            p.name(),
            p.arity(),
            args.len(),
        )));
    }
    let args = args
        .into_iter()
        .map(|expr| expr.eval(env))
        .collect::<Result<Vec<ValueRef>, Exception>>()?;
    // TODO: do intrinsic calls need their own call frame?
    p.apply(&args, env)
}

fn format_unexpected_token(kind: &str, token: &TokenKind, f: &mut Formatter) -> fmt::Result {
    write!(f, "unexpected token in {kind}: {token}")
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::testutil::{TestEnv, make_textline, some_or_fail};

    mod display {
        use super::*;

        #[test]
        fn call_typename() {
            let proc = Expression::variable(
                "foo",
                ExprCtx {
                    span: 0..5,
                    txt: make_textline().into(),
                },
            );
            let expr = ExpressionKind::Call {
                args: [].into(),
                proc: proc.into(),
            };

            assert_eq!(expr.as_typename().to_string(), "procedure call");
        }

        #[test]
        fn variable_typename() {
            let expr = ExpressionKind::Variable("foo".into());

            assert_eq!(expr.as_typename().to_string(), "variable");
        }

        #[test]
        fn literal_typename() {
            let expr = ExpressionKind::Literal(Value::ByteVector([].into()));

            assert_eq!(expr.as_typename().to_string(), "bytevector");
        }
    }

    mod eval {
        use super::*;
        use crate::testutil::{err_or_fail, ok_or_fail};

        #[test]
        fn constant() {
            let expr = ExprCtx {
                span: 0..6,
                txt: make_textline().into(),
            }
            .into_expr(ExpressionKind::Literal(Value::Boolean(true)));
            let mut env = TestEnv::default();
            let f = env.new_frame();

            let r = expr.eval(&f);

            let v = ok_or_fail!(r);
            assert!(matches!(*v, Value::Boolean(true)));
        }

        #[test]
        fn variable() {
            let expr = Expression::variable(
                "x",
                ExprCtx {
                    span: 0..6,
                    txt: make_textline().into(),
                },
            );
            let mut env = TestEnv::default();
            env.binding.bind("x", Value::string("foo"));
            let f = env.new_frame();

            let r = expr.eval(&f);

            let v = ok_or_fail!(r);
            assert!(matches!(&*v, Value::String(s) if &**s == "foo"));
        }

        #[test]
        fn missing_variable() {
            let expr = Expression::variable(
                "x",
                ExprCtx {
                    span: 0..6,
                    txt: make_textline().into(),
                },
            );
            let mut env = TestEnv::default();
            let f = env.new_frame();

            let r = expr.eval(&f);

            let err = err_or_fail!(r);
            assert_eq!(err.to_string(), "#<env-error \"unbound variable: x\">");
        }

        #[test]
        fn empty_program() {
            let prg = Program::new([]);
            let mut env = TestEnv::default();
            let f = env.new_frame();

            let r = prg.eval(&f);

            let v = ok_or_fail!(r);
            assert!(matches!(*v, Value::Unspecified));
        }

        #[test]
        fn program() {
            let txt = make_textline().into();
            let prg = Program::new([
                ExprCtx {
                    span: 0..6,
                    txt: Rc::clone(&txt),
                }
                .into_expr(ExpressionKind::Literal(Value::Boolean(true))),
                ExprCtx {
                    span: 6..8,
                    txt: Rc::clone(&txt),
                }
                .into_expr(ExpressionKind::Literal(Value::Character('a'))),
                ExprCtx {
                    span: 8..10,
                    txt: Rc::clone(&txt),
                }
                .into_expr(ExpressionKind::Literal(Value::Character('b'))),
            ]);
            let mut env = TestEnv::default();
            let f = env.new_frame();

            let r = prg.eval(&f);

            let v = ok_or_fail!(r);
            assert!(matches!(*v, Value::Character('b')));
        }

        mod forms {
            use super::*;
            use crate::eval::Procedure;

            #[test]
            fn call_no_args() {
                let txt = make_textline().into();
                let expr = ExprCtx {
                    span: 0..5,
                    txt: Rc::clone(&txt),
                }
                .into_expr(ExpressionKind::Call {
                    proc: Expression::variable(
                        "foo",
                        ExprCtx {
                            span: 1..4,
                            txt: Rc::clone(&txt),
                        },
                    )
                    .into(),
                    args: [].into(),
                });
                let mut env = TestEnv::default();
                env.binding.bind(
                    "foo",
                    Value::Procedure(
                        Procedure::intrinsic("foo", 0..0, |_, _| Ok(Value::symbol("bar").into()))
                            .into(),
                    ),
                );
                let f = env.new_frame();

                let r = expr.eval(&f);

                let v = ok_or_fail!(r);
                assert!(matches!(&*v, Value::Symbol(s) if &**s == "bar"));
            }

            #[test]
            fn call_with_args() {
                let txt = make_textline().into();
                let expr = ExprCtx {
                    span: 0..18,
                    txt: Rc::clone(&txt),
                }
                .into_expr(ExpressionKind::Call {
                    proc: Expression::variable(
                        "foo",
                        ExprCtx {
                            span: 1..4,
                            txt: Rc::clone(&txt),
                        },
                    )
                    .into(),
                    args: [
                        Expression::string(
                            "one",
                            ExprCtx {
                                span: 5..8,
                                txt: Rc::clone(&txt),
                            },
                        ),
                        Expression::string(
                            "two",
                            ExprCtx {
                                span: 9..12,
                                txt: Rc::clone(&txt),
                            },
                        ),
                        Expression::string(
                            "three",
                            ExprCtx {
                                span: 13..16,
                                txt: Rc::clone(&txt),
                            },
                        ),
                    ]
                    .into(),
                });
                let mut env = TestEnv::default();
                env.binding.bind(
                    "foo",
                    Value::Procedure(
                        Procedure::intrinsic("foo", 3..3, |args, _| {
                            let s = args
                                .iter()
                                .map(|v| {
                                    let Value::String(s) = &**v else {
                                        unreachable!()
                                    };
                                    s.clone()
                                })
                                .collect::<Vec<_>>()
                                .join(", ");
                            Ok(Value::string(s).into())
                        })
                        .into(),
                    ),
                );
                let f = env.new_frame();

                let r = expr.eval(&f);

                let v = ok_or_fail!(r);
                assert!(matches!(&*v, Value::String(s) if &**s == "one, two, three"));
            }

            #[test]
            fn call_unbound_procedure() {
                let txt = make_textline().into();
                let expr = ExprCtx {
                    span: 0..5,
                    txt: Rc::clone(&txt),
                }
                .into_expr(ExpressionKind::Call {
                    proc: Expression::variable(
                        "foo",
                        ExprCtx {
                            span: 1..4,
                            txt: Rc::clone(&txt),
                        },
                    )
                    .into(),
                    args: [].into(),
                });
                let mut env = TestEnv::default();
                let f = env.new_frame();

                let r = expr.eval(&f);

                let err = err_or_fail!(r);
                assert_eq!(err.to_string(), "#<env-error \"unbound variable: foo\">");
            }

            #[test]
            fn call_not_a_procedure() {
                let txt = make_textline().into();
                let expr = ExprCtx {
                    span: 0..5,
                    txt: Rc::clone(&txt),
                }
                .into_expr(ExpressionKind::Call {
                    proc: Expression::variable(
                        "foo",
                        ExprCtx {
                            span: 1..4,
                            txt: Rc::clone(&txt),
                        },
                    )
                    .into(),
                    args: [].into(),
                });
                let mut env = TestEnv::default();
                env.binding.bind("foo", Value::string("foo"));
                let f = env.new_frame();

                let r = expr.eval(&f);

                let err = err_or_fail!(r);
                assert_eq!(
                    err.to_string(),
                    "#<env-error \"expected procedure, found: string\">"
                );
            }

            #[test]
            fn call_too_many_args() {
                let txt = make_textline().into();
                let expr = ExprCtx {
                    span: 0..7,
                    txt: Rc::clone(&txt),
                }
                .into_expr(ExpressionKind::Call {
                    proc: Expression::variable(
                        "foo",
                        ExprCtx {
                            span: 1..4,
                            txt: Rc::clone(&txt),
                        },
                    )
                    .into(),
                    args: [ExprCtx {
                        span: 5..6,
                        txt: Rc::clone(&txt),
                    }
                    .into_expr(ExpressionKind::Literal(Value::number(5)))]
                    .into(),
                });
                let mut env = TestEnv::default();
                env.binding.bind(
                    "foo",
                    Value::Procedure(
                        Procedure::intrinsic("foo", 0..0, |_, _| Ok(Value::symbol("bar").into()))
                            .into(),
                    ),
                );
                let f = env.new_frame();

                let r = expr.eval(&f);

                let err = err_or_fail!(r);
                assert_eq!(
                    err.to_string(),
                    "#<env-error \"foo arity mismatch - expected: 0, found: 1\">"
                );
            }

            #[test]
            fn call_too_few_args() {
                let txt = make_textline().into();
                let expr = ExprCtx {
                    span: 0..7,
                    txt: Rc::clone(&txt),
                }
                .into_expr(ExpressionKind::Call {
                    proc: Expression::variable(
                        "foo",
                        ExprCtx {
                            span: 1..4,
                            txt: Rc::clone(&txt),
                        },
                    )
                    .into(),
                    args: [].into(),
                });
                let mut env = TestEnv::default();
                env.binding.bind(
                    "foo",
                    Value::Procedure(
                        Procedure::intrinsic("foo", 1..1, |_, _| Ok(Value::symbol("bar").into()))
                            .into(),
                    ),
                );
                let f = env.new_frame();

                let r = expr.eval(&f);

                let err = err_or_fail!(r);
                assert_eq!(
                    err.to_string(),
                    "#<env-error \"foo arity mismatch - expected: 1, found: 0\">"
                );
            }

            #[test]
            fn call_too_few_args_variable_arity() {
                let txt = make_textline().into();
                let expr = ExprCtx {
                    span: 0..7,
                    txt: Rc::clone(&txt),
                }
                .into_expr(ExpressionKind::Call {
                    proc: Expression::variable(
                        "foo",
                        ExprCtx {
                            span: 1..4,
                            txt: Rc::clone(&txt),
                        },
                    )
                    .into(),
                    args: [].into(),
                });
                let mut env = TestEnv::default();
                env.binding.bind(
                    "foo",
                    Value::Procedure(
                        Procedure::intrinsic("foo", 1..2, |_, _| Ok(Value::symbol("bar").into()))
                            .into(),
                    ),
                );
                let f = env.new_frame();

                let r = expr.eval(&f);

                let err = err_or_fail!(r);
                assert_eq!(
                    err.to_string(),
                    "#<env-error \"foo arity mismatch - expected at least: 1, found: 0\">"
                );
            }

            #[test]
            fn call_args_eval_failure() {
                let txt = make_textline().into();
                let expr = ExprCtx {
                    span: 0..13,
                    txt: Rc::clone(&txt),
                }
                .into_expr(ExpressionKind::Call {
                    proc: Expression::variable(
                        "foo",
                        ExprCtx {
                            span: 1..4,
                            txt: Rc::clone(&txt),
                        },
                    )
                    .into(),
                    args: [
                        ExprCtx {
                            span: 5..6,
                            txt: Rc::clone(&txt),
                        }
                        .into_expr(ExpressionKind::Literal(Value::number(5))),
                        Expression::variable(
                            "x",
                            ExprCtx {
                                span: 7..8,
                                txt: Rc::clone(&txt),
                            },
                        )
                        .into(),
                        Expression::variable(
                            "y",
                            ExprCtx {
                                span: 9..10,
                                txt: Rc::clone(&txt),
                            },
                        )
                        .into(),
                        Expression::variable(
                            "z",
                            ExprCtx {
                                span: 11..12,
                                txt: Rc::clone(&txt),
                            },
                        )
                        .into(),
                    ]
                    .into(),
                });
                let mut env = TestEnv::default();
                env.binding.bind(
                    "foo",
                    Value::Procedure(
                        Procedure::intrinsic("foo", 4..4, |_, _| Ok(Value::symbol("bar").into()))
                            .into(),
                    ),
                );
                env.binding.bind("y", Value::string("beef"));
                let f = env.new_frame();

                let r = expr.eval(&f);

                // NOTE: missing variable "z" is not hit
                let err = err_or_fail!(r);
                assert_eq!(err.to_string(), "#<env-error \"unbound variable: x\">");
            }
        }
    }

    mod error {
        use super::*;

        #[test]
        fn display_invalid_bytevector_item() {
            let err = ExprCtx {
                span: 0..5,
                txt: make_textline().into(),
            }
            .into_error(ExpressionErrorKind::ByteVectorInvalidItem(
                ExpressionKind::Variable("foobar".into()),
            ));

            assert_eq!(err.to_string(), "expected byte literal, got: variable");
        }

        #[test]
        fn display_invalid_bytevector_number() {
            let err = ExprCtx {
                span: 0..5,
                txt: make_textline().into(),
            }
            .into_error(ExpressionErrorKind::ByteVectorInvalidNumber(
                NumericError::ByteConversionInvalidRange,
            ));

            assert_eq!(err.to_string(), "integer literal out of range: [0, 255]");
        }

        #[test]
        fn display_unterminated_bytevector() {
            let err = ExprCtx {
                span: 0..5,
                txt: make_textline().into(),
            }
            .into_error(ExpressionErrorKind::ByteVectorUnterminated);

            assert_eq!(err.to_string(), "unterminated bytevector");
        }

        #[test]
        fn display_invalid_block_comment() {
            let err = ExprCtx {
                span: 0..5,
                txt: make_textline().into(),
            }
            .into_error(ExpressionErrorKind::CommentBlockInvalid(TokenKind::Comment));

            assert_eq!(
                err.to_string(),
                format!("unexpected token in comment block: {}", TokenKind::Comment)
            );
        }

        #[test]
        fn display_unterminated_block_comment() {
            let err = ExprCtx {
                span: 0..5,
                txt: make_textline().into(),
            }
            .into_error(ExpressionErrorKind::CommentBlockUnterminated);

            assert_eq!(err.to_string(), "unterminated block comment");
        }

        #[test]
        fn display_invalid_vector_item() {
            let err = ExprCtx {
                span: 0..5,
                txt: make_textline().into(),
            }
            .into_error(ExpressionErrorKind::VectorInvalidItem(
                ExpressionKind::Variable("foobar".into()),
            ));

            assert_eq!(err.to_string(), "unexpected vector item type: variable");
        }

        #[test]
        fn display_unterminated_vector() {
            let err = ExprCtx {
                span: 0..5,
                txt: make_textline().into(),
            }
            .into_error(ExpressionErrorKind::VectorUnterminated);

            assert_eq!(err.to_string(), "unterminated vector");
        }

        #[test]
        fn display_expected_datum() {
            let err = ExprCtx {
                span: 0..5,
                txt: make_textline().into(),
            }
            .into_error(ExpressionErrorKind::DatumExpected);

            assert_eq!(err.to_string(), "expected datum");
        }

        #[test]
        fn display_invalid_datum() {
            let txt = make_textline().into();
            let err = ExprCtx {
                span: 0..5,
                txt: Rc::clone(&txt),
            }
            .into_error(ExpressionErrorKind::DatumInvalid(ExpressionKind::Call {
                proc: Expression::variable(
                    "foo",
                    ExprCtx {
                        span: 0..1,
                        txt: Rc::clone(&txt),
                    },
                )
                .into(),
                args: [].into(),
            }));

            assert_eq!(err.to_string(), "unexpected datum type: procedure call");
        }

        #[test]
        fn display_invalid_identifier() {
            let err = ExprCtx {
                span: 0..5,
                txt: make_textline().into(),
            }
            .into_error(ExpressionErrorKind::IdentifierInvalid(TokenKind::Comment));

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
            let err = ExprCtx {
                span: 0..5,
                txt: make_textline().into(),
            }
            .into_error(ExpressionErrorKind::IdentifierUnterminated);

            assert_eq!(err.to_string(), "unterminated verbatim identifier");
        }

        #[test]
        fn display_unterminated_list() {
            let err = ExprCtx {
                span: 0..5,
                txt: make_textline().into(),
            }
            .into_error(ExpressionErrorKind::ListUnterminated);

            assert_eq!(err.to_string(), "unterminated list expression");
        }

        #[test]
        fn display_incomplete_pair() {
            let err = ExprCtx {
                span: 0..5,
                txt: make_textline().into(),
            }
            .into_error(ExpressionErrorKind::PairIncomplete);

            assert_eq!(err.to_string(), "missing first pair expression");
        }

        #[test]
        fn display_unexpected_pair() {
            let err = ExprCtx {
                span: 0..5,
                txt: make_textline().into(),
            }
            .into_error(ExpressionErrorKind::PairUnexpected);

            assert_eq!(err.to_string(), "unexpected pair syntax");
        }

        #[test]
        fn display_unterminated_pair() {
            let err = ExprCtx {
                span: 0..5,
                txt: make_textline().into(),
            }
            .into_error(ExpressionErrorKind::PairUnterminated);

            assert_eq!(err.to_string(), "unterminated pair expression");
        }

        #[test]
        fn display_empty_procedure_call() {
            let err = ExprCtx {
                span: 0..5,
                txt: make_textline().into(),
            }
            .into_error(ExpressionErrorKind::ProcedureEmpty);

            assert_eq!(err.to_string(), "empty procedure call");
        }

        #[test]
        fn display_invalid_seq() {
            let err = ExprCtx {
                span: 0..5,
                txt: make_textline().into(),
            }
            .into_error(ExpressionErrorKind::SeqInvalid(TokenKind::Comment));

            assert_eq!(
                err.to_string(),
                format!("unexpected token in sequence: {}", TokenKind::Comment)
            );
        }

        #[test]
        fn display_invalid_str() {
            let err = ExprCtx {
                span: 0..5,
                txt: make_textline().into(),
            }
            .into_error(ExpressionErrorKind::StrInvalid(TokenKind::Comment));

            assert_eq!(
                err.to_string(),
                format!("unexpected token in string: {}", TokenKind::Comment)
            );
        }

        #[test]
        fn display_unterminated_str() {
            let err = ExprCtx {
                span: 0..5,
                txt: make_textline().into(),
            }
            .into_error(ExpressionErrorKind::StrUnterminated);

            assert_eq!(err.to_string(), "unterminated string constant");
        }

        #[test]
        fn display_unimplemented() {
            let err = ExprCtx {
                span: 0..5,
                txt: make_textline().into(),
            }
            .into_error(ExpressionErrorKind::Unimplemented(TokenKind::Comment));

            assert_eq!(
                err.to_string(),
                format!("{} parsing not yet implemented", TokenKind::Comment)
            );
        }

        #[test]
        fn byte_invalid_source() {
            let err = ExprCtx {
                span: 0..5,
                txt: make_textline().into(),
            }
            .into_error(ExpressionErrorKind::ByteVectorInvalidNumber(
                NumericError::ByteConversionInvalidRange,
            ));

            let inner = some_or_fail!(err.source());

            assert!(matches!(
                inner.downcast_ref::<NumericError>().unwrap(),
                NumericError::ByteConversionInvalidRange
            ));
        }

        #[test]
        fn other_source() {
            let err = ExprCtx {
                span: 0..5,
                txt: make_textline().into(),
            }
            .into_error(ExpressionErrorKind::StrUnterminated);

            let inner = err.source();

            assert!(inner.is_none());
        }
    }

    mod groupby {
        use super::*;
        use crate::testutil::make_textline_no;
        use std::ptr;

        #[test]
        fn empty() {
            let errs = Vec::new();

            let groups = errs
                .into_iter()
                .peekable()
                .groupby_txt()
                .collect::<Vec<_>>();

            assert!(groups.is_empty());
        }

        #[test]
        fn single() {
            let txt = make_textline().into();
            let errs = [ExprCtx {
                span: 0..5,
                txt: Rc::clone(&txt),
            }
            .into_error(ExpressionErrorKind::StrUnterminated)];

            let groups = errs.iter().peekable().groupby_txt().collect::<Vec<_>>();

            assert_eq!(groups.len(), 1);
            let (key, group) = &groups[0];
            assert!(ptr::eq(*key, Rc::as_ptr(&txt)));
            assert_eq!(group.len(), 1);
            assert!(ptr::eq(group[0], &errs[0]));
        }

        #[test]
        fn one_group() {
            let txt = make_textline().into();
            let errs = [
                ExprCtx {
                    span: 0..5,
                    txt: Rc::clone(&txt),
                }
                .into_error(ExpressionErrorKind::StrUnterminated),
                ExprCtx {
                    span: 5..7,
                    txt: Rc::clone(&txt),
                }
                .into_error(ExpressionErrorKind::ListUnterminated),
                ExprCtx {
                    span: 7..10,
                    txt: Rc::clone(&txt),
                }
                .into_error(ExpressionErrorKind::CommentBlockUnterminated),
            ];

            let groups = errs.iter().peekable().groupby_txt().collect::<Vec<_>>();

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
            let txt1 = make_textline_no(1).into();
            let txt2 = make_textline_no(2).into();
            let errs = [
                ExprCtx {
                    span: 0..5,
                    txt: Rc::clone(&txt1),
                }
                .into_error(ExpressionErrorKind::StrUnterminated),
                ExprCtx {
                    span: 5..7,
                    txt: Rc::clone(&txt1),
                }
                .into_error(ExpressionErrorKind::ListUnterminated),
                ExprCtx {
                    span: 0..3,
                    txt: Rc::clone(&txt2),
                }
                .into_error(ExpressionErrorKind::CommentBlockUnterminated),
            ];

            let groups = errs.iter().peekable().groupby_txt().collect::<Vec<_>>();

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
            let txt1 = make_textline_no(1).into();
            let txt2 = make_textline_no(2).into();
            let errs = [
                ExprCtx {
                    span: 0..5,
                    txt: Rc::clone(&txt1),
                }
                .into_error(ExpressionErrorKind::StrUnterminated),
                ExprCtx {
                    span: 0..3,
                    txt: Rc::clone(&txt2),
                }
                .into_error(ExpressionErrorKind::CommentBlockUnterminated),
                ExprCtx {
                    span: 5..7,
                    txt: Rc::clone(&txt1),
                }
                .into_error(ExpressionErrorKind::ListUnterminated),
            ];

            let groups = errs.iter().peekable().groupby_txt().collect::<Vec<_>>();

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
