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

        assert_eq!(expr.as_typename().to_string(), "call");
    }

    #[test]
    fn define_typename() {
        let expr = ExpressionKind::Define {
            name: "foo".into(),
            expr: Some(
                Expression::string(
                    "bar",
                    ExprCtx {
                        span: 0..5,
                        txt: make_textline().into(),
                    },
                )
                .into(),
            ),
        };

        assert_eq!(expr.as_typename().to_string(), "definition");
    }

    #[test]
    fn if_typename() {
        let txt = make_textline().into();
        let expr = ExpressionKind::If {
            test: ExprCtx {
                span: 0..4,
                txt: Rc::clone(&txt),
            }
            .into_expr(ExpressionKind::Literal(Value::Boolean(true)))
            .into(),
            con: Expression::string(
                "bar",
                ExprCtx {
                    span: 5..8,
                    txt: Rc::clone(&txt),
                },
            )
            .into(),
            alt: None,
        };

        assert_eq!(expr.as_typename().to_string(), "conditional");
    }

    #[test]
    fn set_typename() {
        let expr = ExpressionKind::Set {
            var: "foo".into(),
            expr: Expression::string(
                "bar",
                ExprCtx {
                    span: 0..5,
                    txt: make_textline().into(),
                },
            )
            .into(),
        };

        assert_eq!(expr.as_typename().to_string(), "assignment");
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
    use crate::{
        eval::Procedure,
        testutil::{err_or_fail, extract_or_fail, ok_or_fail},
    };

    #[test]
    fn literal() {
        let expr = ExprCtx {
            span: 0..6,
            txt: make_textline().into(),
        }
        .into_expr(ExpressionKind::Literal(Value::Boolean(true)));
        let mut env = TestEnv::default();
        let mut f = env.new_frame();

        let r = expr.eval(&mut f);

        let v = ok_or_fail!(r);
        assert!(matches!(v, Value::Boolean(true)));
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
        let mut f = env.new_frame();

        let r = expr.eval(&mut f);

        let v = ok_or_fail!(r);
        assert!(matches!(v, Value::String(s) if s.as_ref() == "foo"));
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
        let mut f = env.new_frame();

        let r = expr.eval(&mut f);

        let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
        assert_eq!(err.to_string(), "#<env-error \"unbound variable: x\">");
    }

    mod program {
        use super::*;

        #[test]
        fn empty() {
            let prg = Program::new([]);
            let mut env = TestEnv::default();
            let mut f = env.new_frame();

            let r = prg.eval(&mut f);

            let v = ok_or_fail!(r);
            assert!(matches!(v, Value::Unspecified));
        }

        #[test]
        fn simple() {
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
            let mut f = env.new_frame();

            let r = prg.eval(&mut f);

            let v = ok_or_fail!(r);
            assert!(matches!(v, Value::Character('b')));
        }

        #[test]
        fn multiple_procedures() {
            let txt = make_textline().into();
            let prg = Program::new([
                ExprCtx {
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
                }),
                ExprCtx {
                    span: 6..11,
                    txt: Rc::clone(&txt),
                }
                .into_expr(ExpressionKind::Call {
                    proc: Expression::variable(
                        "baz",
                        ExprCtx {
                            span: 7..10,
                            txt: Rc::clone(&txt),
                        },
                    )
                    .into(),
                    args: [].into(),
                }),
            ]);
            let mut env = TestEnv::default();
            env.binding.bind(
                "foo",
                Value::Procedure(
                    Procedure::intrinsic("foo", 0..0, |_, f| {
                        f.scope.bind("foo_called", Value::Boolean(true));
                        Ok(Value::symbol("bar"))
                    })
                    .into(),
                ),
            );
            env.binding.bind(
                "baz",
                Value::Procedure(
                    Procedure::intrinsic("baz", 0..0, |_, _| Ok(Value::Character('a'))).into(),
                ),
            );
            let mut f = env.new_frame();

            let r = prg.eval(&mut f);

            let v = ok_or_fail!(r);
            assert!(matches!(v, Value::Character('a')));
            assert!(f.scope.lookup("foo_called").is_some());
        }

        #[test]
        fn exception_interrupts_execution() {
            let txt = make_textline().into();
            let prg = Program::new([
                ExprCtx {
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
                    args: [].into(),
                }),
                ExprCtx {
                    span: 6..12,
                    txt: Rc::clone(&txt),
                }
                .into_expr(ExpressionKind::Call {
                    proc: Expression::variable(
                        "fail",
                        ExprCtx {
                            span: 7..11,
                            txt: Rc::clone(&txt),
                        },
                    )
                    .into(),
                    args: [].into(),
                }),
                ExprCtx {
                    span: 13..18,
                    txt: Rc::clone(&txt),
                }
                .into_expr(ExpressionKind::Call {
                    proc: Expression::variable(
                        "baz",
                        ExprCtx {
                            span: 14..17,
                            txt: Rc::clone(&txt),
                        },
                    )
                    .into(),
                    args: [].into(),
                }),
            ]);
            let mut env = TestEnv::default();
            env.binding.bind(
                "foo",
                Value::Procedure(
                    Procedure::intrinsic("foo", 0..0, |_, f| {
                        f.scope.bind("foo_called", Value::Boolean(true));
                        Ok(Value::symbol("bar"))
                    })
                    .into(),
                ),
            );
            env.binding.bind(
                "fail",
                Value::Procedure(
                    Procedure::intrinsic("baz", 0..0, |_, _| {
                        Err(Condition::system_error("oh no").into())
                    })
                    .into(),
                ),
            );
            env.binding.bind(
                "baz",
                Value::Procedure(
                    Procedure::intrinsic("baz", 0..0, |_, f| {
                        f.scope.bind("baz_called", Value::Boolean(true));
                        Ok(Value::Character('a'))
                    })
                    .into(),
                ),
            );
            let mut f = env.new_frame();

            let r = prg.eval(&mut f);

            let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
            assert_eq!(err.to_string(), "#<sys-error \"oh no\">");
            assert!(f.scope.lookup("foo_called").is_some());
            assert!(f.scope.lookup("baz_called").is_none());
        }
    }

    mod forms {
        use super::*;

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
                    Procedure::intrinsic("foo", 0..0, |_, _| Ok(Value::symbol("bar"))).into(),
                ),
            );
            let mut f = env.new_frame();

            let r = expr.eval(&mut f);

            let v = ok_or_fail!(r);
            assert!(matches!(v, Value::Symbol(s) if s.as_ref() == "bar"));
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
                                let Value::String(s) = v else { unreachable!() };
                                s.clone()
                            })
                            .collect::<Vec<_>>()
                            .join(", ");
                        Ok(Value::string(s))
                    })
                    .into(),
                ),
            );
            let mut f = env.new_frame();

            let r = expr.eval(&mut f);

            let v = ok_or_fail!(r);
            assert!(matches!(v, Value::String(s) if s.as_ref() == "one, two, three"));
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
            let mut f = env.new_frame();

            let r = expr.eval(&mut f);

            let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
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
            let mut f = env.new_frame();

            let r = expr.eval(&mut f);

            let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
            assert_eq!(
                err.to_string(),
                "#<env-error \"expected procedure, got: string\">"
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
                .into_expr(ExpressionKind::Literal(Value::real(5)))]
                .into(),
            });
            let mut env = TestEnv::default();
            env.binding.bind(
                "foo",
                Value::Procedure(
                    Procedure::intrinsic("foo", 0..0, |_, _| Ok(Value::symbol("bar"))).into(),
                ),
            );
            let mut f = env.new_frame();

            let r = expr.eval(&mut f);

            let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
            assert_eq!(
                err.to_string(),
                "#<env-error \"foo arity mismatch - expected: 0, got: 1\">"
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
                    Procedure::intrinsic("foo", 1..1, |_, _| Ok(Value::symbol("bar"))).into(),
                ),
            );
            let mut f = env.new_frame();

            let r = expr.eval(&mut f);

            let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
            assert_eq!(
                err.to_string(),
                "#<env-error \"foo arity mismatch - expected: 1, got: 0\">"
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
                    Procedure::intrinsic("foo", 1..2, |_, _| Ok(Value::symbol("bar"))).into(),
                ),
            );
            let mut f = env.new_frame();

            let r = expr.eval(&mut f);

            let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
            assert_eq!(
                err.to_string(),
                "#<env-error \"foo arity mismatch - expected at least: 1, got: 0\">"
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
                    .into_expr(ExpressionKind::Literal(Value::real(5))),
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
                    Procedure::intrinsic("foo", 4..4, |_, f| {
                        f.scope.bind("foo_called", Value::Boolean(true));
                        Ok(Value::symbol("bar"))
                    })
                    .into(),
                ),
            );
            env.binding.bind("y", Value::string("beef"));
            let mut f = env.new_frame();

            let r = expr.eval(&mut f);

            // NOTE: missing variable "z" is not hit
            let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
            assert_eq!(err.to_string(), "#<env-error \"unbound variable: x\">");
            assert!(f.scope.lookup("foo_called").is_none());
        }

        #[test]
        fn define_with_expr() {
            let txt = make_textline().into();
            let expr = ExprCtx {
                span: 0..10,
                txt: Rc::clone(&txt),
            }
            .into_expr(ExpressionKind::Define {
                name: "foo".into(),
                expr: Some(
                    Expression::string(
                        "one",
                        ExprCtx {
                            span: 5..8,
                            txt: Rc::clone(&txt),
                        },
                    )
                    .into(),
                ),
            });
            let mut env = TestEnv::default();
            let mut f = env.new_frame();

            let r = expr.eval(&mut f);

            let v = ok_or_fail!(r);
            assert!(matches!(v, Value::Unspecified));
            assert!(matches!(f.scope.lookup("foo"), Some(Value::String(s)) if s.as_ref() == "one"))
        }

        #[test]
        fn define_no_expr() {
            let txt = make_textline().into();
            let expr = ExprCtx {
                span: 0..10,
                txt: Rc::clone(&txt),
            }
            .into_expr(ExpressionKind::Define {
                name: "foo".into(),
                expr: None,
            });
            let mut env = TestEnv::default();
            let mut f = env.new_frame();

            let r = expr.eval(&mut f);

            let v = ok_or_fail!(r);
            assert!(matches!(v, Value::Unspecified));
            assert!(matches!(f.scope.lookup("foo"), Some(Value::Unspecified)))
        }

        #[test]
        fn define_eval_failure() {
            let txt = make_textline().into();
            let expr = ExprCtx {
                span: 0..7,
                txt: Rc::clone(&txt),
            }
            .into_expr(ExpressionKind::Define {
                name: "foo".into(),
                expr: Some(
                    Expression::variable(
                        "x",
                        ExprCtx {
                            span: 5..6,
                            txt: Rc::clone(&txt),
                        },
                    )
                    .into(),
                ),
            });
            let mut env = TestEnv::default();
            let mut f = env.new_frame();

            let r = expr.eval(&mut f);

            let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
            assert_eq!(err.to_string(), "#<env-error \"unbound variable: x\">");
            assert!(f.scope.lookup("foo").is_none());
        }

        #[test]
        fn set_with_expr() {
            let txt = make_textline().into();
            let expr = ExprCtx {
                span: 0..10,
                txt: Rc::clone(&txt),
            }
            .into_expr(ExpressionKind::Set {
                var: "foo".into(),
                expr: Expression::string(
                    "one",
                    ExprCtx {
                        span: 5..8,
                        txt: Rc::clone(&txt),
                    },
                )
                .into(),
            });
            let mut env = TestEnv::default();
            env.binding.bind("foo", Value::Unspecified);
            let mut f = env.new_frame();

            let r = expr.eval(&mut f);

            let v = ok_or_fail!(r);
            assert!(matches!(v, Value::Unspecified));
            assert!(matches!(f.scope.lookup("foo"), Some(Value::String(s)) if s.as_ref() == "one"))
        }

        #[test]
        fn set_eval_failure() {
            let txt = make_textline().into();
            let expr = ExprCtx {
                span: 0..7,
                txt: Rc::clone(&txt),
            }
            .into_expr(ExpressionKind::Set {
                var: "foo".into(),
                expr: Expression::variable(
                    "x",
                    ExprCtx {
                        span: 5..6,
                        txt: Rc::clone(&txt),
                    },
                )
                .into(),
            });
            let mut env = TestEnv::default();
            env.binding.bind("foo", Value::Unspecified);
            let mut f = env.new_frame();

            let r = expr.eval(&mut f);

            let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
            assert_eq!(err.to_string(), "#<env-error \"unbound variable: x\">");
            assert!(matches!(f.scope.lookup("foo"), Some(Value::Unspecified)));
        }

        #[test]
        fn set_eval_no_binding() {
            let txt = make_textline().into();
            let expr = ExprCtx {
                span: 0..7,
                txt: Rc::clone(&txt),
            }
            .into_expr(ExpressionKind::Set {
                var: "foo".into(),
                expr: Expression::variable(
                    "x",
                    ExprCtx {
                        span: 5..6,
                        txt: Rc::clone(&txt),
                    },
                )
                .into(),
            });
            let mut env = TestEnv::default();
            let mut f = env.new_frame();

            let r = expr.eval(&mut f);

            let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
            assert_eq!(err.to_string(), "#<env-error \"unbound variable: foo\">");
            assert!(f.scope.lookup("foo").is_none());
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
    fn display_invalid_define() {
        let txt = make_textline().into();
        let err = ExprCtx {
            span: 0..5,
            txt: Rc::clone(&txt),
        }
        .into_error(ExpressionErrorKind::DefineInvalid);

        assert_eq!(
            err.to_string(),
            "invalid form, expected: (define <identifier> [expression])"
        );
    }

    #[test]
    fn display_disallowed_define() {
        let txt = make_textline().into();
        let err = ExprCtx {
            span: 0..5,
            txt: Rc::clone(&txt),
        }
        .into_error(ExpressionErrorKind::DefineNotAllowed);

        assert_eq!(err.to_string(), "define not allowed in this context");
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
    fn display_quote_invalid() {
        let err = ExprCtx {
            span: 0..5,
            txt: make_textline().into(),
        }
        .into_error(ExpressionErrorKind::QuoteInvalid);

        assert_eq!(err.to_string(), "invalid form, expected: (quote <datum>)");
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
    fn display_set_invalid() {
        let err = ExprCtx {
            span: 0..5,
            txt: make_textline().into(),
        }
        .into_error(ExpressionErrorKind::SetInvalid);

        assert_eq!(
            err.to_string(),
            "invalid form, expected: (set! <variable> <expression>)"
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

        assert_eq!(err.to_string(), "unterminated string literal");
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
