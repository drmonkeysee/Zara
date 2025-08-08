use super::*;
use crate::{
    string::SymbolTable,
    testutil::{TestEnv, empty_procedure_body, make_textline, ok_or_fail},
};

mod display {
    use super::*;

    #[test]
    fn call_typename() {
        let sym = SymbolTable::default();
        let proc = Expression::variable(
            sym.get("foo"),
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
    fn define_typename() {
        let sym = SymbolTable::default();
        let expr = ExpressionKind::Define {
            name: sym.get("foo"),
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
    fn lambda_typename() {
        let expr = ExpressionKind::Lambda(
            ok_or_fail!(Lambda::new([], None, empty_procedure_body())).into(),
        );

        assert_eq!(expr.as_typename().to_string(), "lambda");
    }

    #[test]
    fn set_typename() {
        let sym = SymbolTable::default();
        let expr = ExpressionKind::Set {
            var: sym.get("foo"),
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
        let sym = SymbolTable::default();
        let expr = ExpressionKind::Variable(sym.get("foo"));

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
        eval::Intrinsic,
        testutil::{err_or_fail, extract_or_fail},
    };

    #[test]
    fn literal() {
        let expr = ExprCtx {
            span: 0..6,
            txt: make_textline().into(),
        }
        .into_expr(ExpressionKind::Literal(Value::Boolean(true)));
        let env = TestEnv::default();
        let f = env.new_frame();

        let r = expr.eval(&f);

        let v = ok_or_fail!(r);
        assert!(matches!(v, Value::Boolean(true)));
    }

    #[test]
    fn variable() {
        let env = TestEnv::default();
        let expr = Expression::variable(
            env.symbols.get("x"),
            ExprCtx {
                span: 0..6,
                txt: make_textline().into(),
            },
        );
        env.binding.bind(env.symbols.get("x"), Value::string("foo"));
        let f = env.new_frame();

        let r = expr.eval(&f);

        let v = ok_or_fail!(r);
        assert!(matches!(v, Value::String(s) if s.as_ref() == "foo"));
    }

    #[test]
    fn missing_variable() {
        let env = TestEnv::default();
        let expr = Expression::variable(
            env.symbols.get("x"),
            ExprCtx {
                span: 0..6,
                txt: make_textline().into(),
            },
        );
        let f = env.new_frame();

        let r = expr.eval(&f);

        let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
        assert_eq!(err.to_string(), "#<env-error \"unbound variable: x\">");
    }

    mod program {
        use super::*;

        #[test]
        fn empty() {
            let prg = Sequence::new([]);
            let env = TestEnv::default();
            let f = env.new_frame();

            let r = prg.eval(&f);

            let v = ok_or_fail!(r);
            assert!(matches!(v, Value::Unspecified));
        }

        #[test]
        fn simple() {
            let txt = make_textline().into();
            let prg = Sequence::new([
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
            let env = TestEnv::default();
            let f = env.new_frame();

            let r = prg.eval(&f);

            let v = ok_or_fail!(r);
            assert!(matches!(v, Value::Character('b')));
        }

        #[test]
        fn multiple_procedures() {
            let txt = make_textline().into();
            let env = TestEnv::default();
            let prg = Sequence::new([
                ExprCtx {
                    span: 0..5,
                    txt: Rc::clone(&txt),
                }
                .into_expr(ExpressionKind::Call {
                    proc: Expression::variable(
                        env.symbols.get("foo"),
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
                        env.symbols.get("baz"),
                        ExprCtx {
                            span: 7..10,
                            txt: Rc::clone(&txt),
                        },
                    )
                    .into(),
                    args: [].into(),
                }),
            ]);
            env.binding.bind(
                env.symbols.get("foo"),
                Value::Intrinsic(
                    Intrinsic {
                        arity: 0..0,
                        def: |_, f| {
                            let sym = SymbolTable::default();
                            f.scope.bind(sym.get("foo_called"), Value::Boolean(true));
                            Ok(Value::Symbol(sym.get("bar")))
                        },
                        name: env.symbols.get("foo"),
                    }
                    .into(),
                ),
            );
            env.binding.bind(
                env.symbols.get("baz"),
                Value::Intrinsic(
                    Intrinsic {
                        arity: 0..0,
                        def: |_, _| Ok(Value::Character('a')),
                        name: env.symbols.get("baz"),
                    }
                    .into(),
                ),
            );
            let f = env.new_frame();

            let r = prg.eval(&f);

            let v = ok_or_fail!(r);
            assert!(matches!(v, Value::Character('a')));
            assert!(f.scope.lookup("foo_called").is_some());
        }

        #[test]
        fn exception_interrupts_execution() {
            let txt = make_textline().into();
            let env = TestEnv::default();
            let prg = Sequence::new([
                ExprCtx {
                    span: 0..18,
                    txt: Rc::clone(&txt),
                }
                .into_expr(ExpressionKind::Call {
                    proc: Expression::variable(
                        env.symbols.get("foo"),
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
                        env.symbols.get("fail"),
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
                        env.symbols.get("baz"),
                        ExprCtx {
                            span: 14..17,
                            txt: Rc::clone(&txt),
                        },
                    )
                    .into(),
                    args: [].into(),
                }),
            ]);
            env.binding.bind(
                env.symbols.get("foo"),
                Value::Intrinsic(
                    Intrinsic {
                        arity: 0..0,
                        def: |_, f| {
                            let sym = SymbolTable::default();
                            f.scope.bind(sym.get("foo_called"), Value::Boolean(true));
                            Ok(Value::Symbol(sym.get("bar")))
                        },
                        name: env.symbols.get("foo"),
                    }
                    .into(),
                ),
            );
            env.binding.bind(
                env.symbols.get("fail"),
                Value::Intrinsic(
                    Intrinsic {
                        arity: 0..0,
                        def: |_, _| Err(Condition::system_error("oh no").into()),
                        name: env.symbols.get("baz"),
                    }
                    .into(),
                ),
            );
            env.binding.bind(
                env.symbols.get("baz"),
                Value::Intrinsic(
                    Intrinsic {
                        arity: 0..0,
                        def: |_, f| {
                            let sym = SymbolTable::default();
                            f.scope.bind(sym.get("baz_called"), Value::Boolean(true));
                            Ok(Value::Character('a'))
                        },
                        name: env.symbols.get("baz"),
                    }
                    .into(),
                ),
            );
            let f = env.new_frame();

            let r = prg.eval(&f);

            let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
            assert_eq!(err.to_string(), "#<sys-error \"oh no\">");
            assert!(f.scope.lookup("foo_called").is_some());
            assert!(f.scope.lookup("baz_called").is_none());
        }
    }

    mod forms {
        use super::*;
        use crate::testutil::{procedure_body, some_or_fail};

        #[test]
        fn call_no_args() {
            let txt = make_textline().into();
            let env = TestEnv::default();
            let expr = ExprCtx {
                span: 0..5,
                txt: Rc::clone(&txt),
            }
            .into_expr(ExpressionKind::Call {
                proc: Expression::variable(
                    env.symbols.get("foo"),
                    ExprCtx {
                        span: 1..4,
                        txt: Rc::clone(&txt),
                    },
                )
                .into(),
                args: [].into(),
            });
            env.binding.bind(
                env.symbols.get("foo"),
                Value::Intrinsic(
                    Intrinsic {
                        arity: 0..0,
                        def: |_, _| Ok(Value::String("bar".into())),
                        name: env.symbols.get("foo"),
                    }
                    .into(),
                ),
            );
            let f = env.new_frame();

            let r = expr.eval(&f);

            let v = ok_or_fail!(r);
            assert!(matches!(v, Value::String(s) if s.as_ref() == "bar"));
        }

        #[test]
        fn call_with_args() {
            let txt = make_textline().into();
            let env = TestEnv::default();
            let expr = ExprCtx {
                span: 0..18,
                txt: Rc::clone(&txt),
            }
            .into_expr(ExpressionKind::Call {
                proc: Expression::variable(
                    env.symbols.get("foo"),
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
            env.binding.bind(
                env.symbols.get("foo"),
                Value::Intrinsic(
                    Intrinsic {
                        arity: 3..3,
                        def: |args, _| {
                            let s = args
                                .iter()
                                .map(|v| {
                                    let Value::String(s) = v else {
                                        unreachable!();
                                    };
                                    s.clone()
                                })
                                .collect::<Vec<_>>()
                                .join(", ");
                            Ok(Value::string(s))
                        },
                        name: env.symbols.get("foo"),
                    }
                    .into(),
                ),
            );
            let f = env.new_frame();

            let r = expr.eval(&f);

            let v = ok_or_fail!(r);
            assert!(matches!(v, Value::String(s) if s.as_ref() == "one, two, three"));
        }

        #[test]
        fn call_unbound_procedure() {
            let txt = make_textline().into();
            let env = TestEnv::default();
            let expr = ExprCtx {
                span: 0..5,
                txt: Rc::clone(&txt),
            }
            .into_expr(ExpressionKind::Call {
                proc: Expression::variable(
                    env.symbols.get("foo"),
                    ExprCtx {
                        span: 1..4,
                        txt: Rc::clone(&txt),
                    },
                )
                .into(),
                args: [].into(),
            });
            let f = env.new_frame();

            let r = expr.eval(&f);

            let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
            assert_eq!(err.to_string(), "#<env-error \"unbound variable: foo\">");
        }

        #[test]
        fn call_not_a_procedure() {
            let txt = make_textline().into();
            let env = TestEnv::default();
            let expr = ExprCtx {
                span: 0..5,
                txt: Rc::clone(&txt),
            }
            .into_expr(ExpressionKind::Call {
                proc: Expression::variable(
                    env.symbols.get("foo"),
                    ExprCtx {
                        span: 1..4,
                        txt: Rc::clone(&txt),
                    },
                )
                .into(),
                args: [].into(),
            });
            env.binding
                .bind(env.symbols.get("foo"), Value::string("foo"));
            let f = env.new_frame();

            let r = expr.eval(&f);

            let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
            assert_eq!(
                err.to_string(),
                "#<env-error \"expected procedure, got: string\">"
            );
        }

        #[test]
        fn call_too_many_args() {
            let txt = make_textline().into();
            let env = TestEnv::default();
            let expr = ExprCtx {
                span: 0..7,
                txt: Rc::clone(&txt),
            }
            .into_expr(ExpressionKind::Call {
                proc: Expression::variable(
                    env.symbols.get("foo"),
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
            env.binding.bind(
                env.symbols.get("foo"),
                Value::Intrinsic(
                    Intrinsic {
                        arity: 0..0,
                        def: |_, _| Ok(Value::String("bar".into())),
                        name: env.symbols.get("foo"),
                    }
                    .into(),
                ),
            );
            let f = env.new_frame();

            let r = expr.eval(&f);

            let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
            assert_eq!(
                err.to_string(),
                "#<env-error \"foo arity mismatch - expected: 0, got: 1\">"
            );
        }

        #[test]
        fn call_too_few_args() {
            let txt = make_textline().into();
            let env = TestEnv::default();
            let expr = ExprCtx {
                span: 0..7,
                txt: Rc::clone(&txt),
            }
            .into_expr(ExpressionKind::Call {
                proc: Expression::variable(
                    env.symbols.get("foo"),
                    ExprCtx {
                        span: 1..4,
                        txt: Rc::clone(&txt),
                    },
                )
                .into(),
                args: [].into(),
            });
            env.binding.bind(
                env.symbols.get("foo"),
                Value::Intrinsic(
                    Intrinsic {
                        arity: 1..1,
                        def: |_, _| Ok(Value::String("bar".into())),
                        name: env.symbols.get("foo"),
                    }
                    .into(),
                ),
            );
            let f = env.new_frame();

            let r = expr.eval(&f);

            let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
            assert_eq!(
                err.to_string(),
                "#<env-error \"foo arity mismatch - expected: 1, got: 0\">"
            );
        }

        #[test]
        fn call_too_few_args_variable_arity() {
            let txt = make_textline().into();
            let env = TestEnv::default();
            let expr = ExprCtx {
                span: 0..7,
                txt: Rc::clone(&txt),
            }
            .into_expr(ExpressionKind::Call {
                proc: Expression::variable(
                    env.symbols.get("foo"),
                    ExprCtx {
                        span: 1..4,
                        txt: Rc::clone(&txt),
                    },
                )
                .into(),
                args: [].into(),
            });
            env.binding.bind(
                env.symbols.get("foo"),
                Value::Intrinsic(
                    Intrinsic {
                        arity: 1..2,
                        def: |_, _| Ok(Value::String("bar".into())),
                        name: env.symbols.get("foo"),
                    }
                    .into(),
                ),
            );
            let f = env.new_frame();

            let r = expr.eval(&f);

            let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
            assert_eq!(
                err.to_string(),
                "#<env-error \"foo arity mismatch - expected at least: 1, got: 0\">"
            );
        }

        #[test]
        fn call_args_eval_failure() {
            let txt = make_textline().into();
            let env = TestEnv::default();
            let expr = ExprCtx {
                span: 0..13,
                txt: Rc::clone(&txt),
            }
            .into_expr(ExpressionKind::Call {
                proc: Expression::variable(
                    env.symbols.get("foo"),
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
                        env.symbols.get("x"),
                        ExprCtx {
                            span: 7..8,
                            txt: Rc::clone(&txt),
                        },
                    )
                    .into(),
                    Expression::variable(
                        env.symbols.get("y"),
                        ExprCtx {
                            span: 9..10,
                            txt: Rc::clone(&txt),
                        },
                    )
                    .into(),
                    Expression::variable(
                        env.symbols.get("z"),
                        ExprCtx {
                            span: 11..12,
                            txt: Rc::clone(&txt),
                        },
                    )
                    .into(),
                ]
                .into(),
            });
            env.binding.bind(
                env.symbols.get("foo"),
                Value::Intrinsic(
                    Intrinsic {
                        arity: 4..4,
                        def: |_, f| {
                            let sym = SymbolTable::default();
                            f.scope.bind(sym.get("foo_called"), Value::Boolean(true));
                            Ok(Value::Symbol(sym.get("bar")))
                        },
                        name: env.symbols.get("foo"),
                    }
                    .into(),
                ),
            );
            env.binding
                .bind(env.symbols.get("y"), Value::string("beef"));
            let f = env.new_frame();

            let r = expr.eval(&f);

            // NOTE: missing variable "z" is not hit
            let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
            assert_eq!(err.to_string(), "#<env-error \"unbound variable: x\">");
            assert!(f.scope.lookup("foo_called").is_none());
        }

        #[test]
        fn define_with_expr() {
            let txt = make_textline().into();
            let env = TestEnv::default();
            let expr = ExprCtx {
                span: 0..10,
                txt: Rc::clone(&txt),
            }
            .into_expr(ExpressionKind::Define {
                name: env.symbols.get("foo"),
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
            let f = env.new_frame();

            let r = expr.eval(&f);

            let v = ok_or_fail!(r);
            assert!(matches!(v, Value::Unspecified));
            assert!(matches!(f.scope.lookup("foo"), Some(Value::String(s)) if s.as_ref() == "one"))
        }

        #[test]
        fn define_names_procedure() {
            let txt = make_textline().into();
            let env = TestEnv::default();
            let expr = ExprCtx {
                span: 0..10,
                txt: Rc::clone(&txt),
            }
            .into_expr(ExpressionKind::Define {
                name: env.symbols.get("foo"),
                expr: Some(
                    Expression::lambda(
                        ok_or_fail!(Lambda::new([], None, empty_procedure_body())),
                        ExprCtx {
                            span: 5..8,
                            txt: Rc::clone(&txt),
                        },
                    )
                    .into(),
                ),
            });
            let f = env.new_frame();

            let r = expr.eval(&f);

            let v = ok_or_fail!(r);
            assert!(matches!(v, Value::Unspecified));
            assert!(
                matches!(f.scope.lookup("foo"), Some(Value::Procedure(p)) if some_or_fail!(p.name()) == "foo")
            );
        }

        #[test]
        fn define_no_expr() {
            let txt = make_textline().into();
            let env = TestEnv::default();
            let expr = ExprCtx {
                span: 0..10,
                txt: Rc::clone(&txt),
            }
            .into_expr(ExpressionKind::Define {
                name: env.symbols.get("foo"),
                expr: None,
            });
            let f = env.new_frame();

            let r = expr.eval(&f);

            let v = ok_or_fail!(r);
            assert!(matches!(v, Value::Unspecified));
            assert!(matches!(f.scope.lookup("foo"), Some(Value::Unspecified)));
        }

        #[test]
        fn define_eval_failure() {
            let txt = make_textline().into();
            let env = TestEnv::default();
            let expr = ExprCtx {
                span: 0..7,
                txt: Rc::clone(&txt),
            }
            .into_expr(ExpressionKind::Define {
                name: env.symbols.get("foo"),
                expr: Some(
                    Expression::variable(
                        env.symbols.get("x"),
                        ExprCtx {
                            span: 5..6,
                            txt: Rc::clone(&txt),
                        },
                    )
                    .into(),
                ),
            });
            let f = env.new_frame();

            let r = expr.eval(&f);

            let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
            assert_eq!(err.to_string(), "#<env-error \"unbound variable: x\">");
            assert!(f.scope.lookup("foo").is_none());
        }

        #[test]
        fn set_with_expr() {
            let txt = make_textline().into();
            let env = TestEnv::default();
            let expr = ExprCtx {
                span: 0..10,
                txt: Rc::clone(&txt),
            }
            .into_expr(ExpressionKind::Set {
                var: env.symbols.get("foo"),
                expr: Expression::string(
                    "one",
                    ExprCtx {
                        span: 5..8,
                        txt: Rc::clone(&txt),
                    },
                )
                .into(),
            });
            env.binding.bind(env.symbols.get("foo"), Value::Unspecified);
            let f = env.new_frame();

            let r = expr.eval(&f);

            let v = ok_or_fail!(r);
            assert!(matches!(v, Value::Unspecified));
            assert!(matches!(f.scope.lookup("foo"), Some(Value::String(s)) if s.as_ref() == "one"));
        }

        #[test]
        fn set_names_procedure() {
            let txt = make_textline().into();
            let env = TestEnv::default();
            let expr = ExprCtx {
                span: 0..10,
                txt: Rc::clone(&txt),
            }
            .into_expr(ExpressionKind::Set {
                var: env.symbols.get("foo"),
                expr: Expression::lambda(
                    ok_or_fail!(Lambda::new([], None, empty_procedure_body())),
                    ExprCtx {
                        span: 5..8,
                        txt: Rc::clone(&txt),
                    },
                )
                .into(),
            });
            env.binding.bind(env.symbols.get("foo"), Value::Unspecified);
            let f = env.new_frame();

            let r = expr.eval(&f);

            let v = ok_or_fail!(r);
            assert!(matches!(v, Value::Unspecified));
            assert!(
                matches!(f.scope.lookup("foo"), Some(Value::Procedure(p)) if some_or_fail!(p.name()) == "foo")
            );
        }

        #[test]
        fn set_with_expr_in_parent_scope() {
            let txt = make_textline().into();
            let env = TestEnv::default();
            let expr = ExprCtx {
                span: 0..10,
                txt: Rc::clone(&txt),
            }
            .into_expr(ExpressionKind::Set {
                var: env.symbols.get("foo"),
                expr: Expression::string(
                    "one",
                    ExprCtx {
                        span: 5..8,
                        txt: Rc::clone(&txt),
                    },
                )
                .into(),
            });
            env.binding.bind(env.symbols.get("foo"), Value::Unspecified);
            let f = env.new_frame();
            let call_f = f.new_child(Rc::clone(&env.binding));

            let r = expr.eval(&call_f);

            let v = ok_or_fail!(r);
            assert!(matches!(v, Value::Unspecified));
            assert!(
                matches!(call_f.scope.lookup("foo"), Some(Value::String(s)) if s.as_ref() == "one")
            );
            assert!(
                matches!(env.binding.lookup("foo"), Some(Value::String(s)) if s.as_ref() == "one")
            );
            assert!(call_f.scope.sorted_bindings().is_empty());
        }

        #[test]
        fn set_eval_failure() {
            let txt = make_textline().into();
            let env = TestEnv::default();
            let expr = ExprCtx {
                span: 0..7,
                txt: Rc::clone(&txt),
            }
            .into_expr(ExpressionKind::Set {
                var: env.symbols.get("foo"),
                expr: Expression::variable(
                    env.symbols.get("x"),
                    ExprCtx {
                        span: 5..6,
                        txt: Rc::clone(&txt),
                    },
                )
                .into(),
            });
            env.binding.bind(env.symbols.get("foo"), Value::Unspecified);
            let f = env.new_frame();

            let r = expr.eval(&f);

            let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
            assert_eq!(err.to_string(), "#<env-error \"unbound variable: x\">");
            assert!(matches!(f.scope.lookup("foo"), Some(Value::Unspecified)));
        }

        #[test]
        fn set_eval_no_binding() {
            let txt = make_textline().into();
            let env = TestEnv::default();
            let expr = ExprCtx {
                span: 0..7,
                txt: Rc::clone(&txt),
            }
            .into_expr(ExpressionKind::Set {
                var: env.symbols.get("foo"),
                expr: Expression::variable(
                    env.symbols.get("x"),
                    ExprCtx {
                        span: 5..6,
                        txt: Rc::clone(&txt),
                    },
                )
                .into(),
            });
            let f = env.new_frame();

            let r = expr.eval(&f);

            let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
            assert_eq!(err.to_string(), "#<env-error \"unbound variable: foo\">");
            assert!(f.scope.lookup("foo").is_none());
        }

        #[test]
        fn if_consequent() {
            let txt = make_textline().into();
            let env = TestEnv::default();
            let expr = ExprCtx {
                span: 0..13,
                txt: Rc::clone(&txt),
            }
            .into_expr(ExpressionKind::If {
                test: ExprCtx {
                    span: 4..6,
                    txt: Rc::clone(&txt),
                }
                .into_expr(ExpressionKind::Literal(Value::Boolean(true)))
                .into(),
                con: Expression::symbol(
                    env.symbols.get("a"),
                    ExprCtx {
                        span: 7..9,
                        txt: Rc::clone(&txt),
                    },
                )
                .into(),
                alt: Some(
                    Expression::symbol(
                        env.symbols.get("b"),
                        ExprCtx {
                            span: 10..12,
                            txt: Rc::clone(&txt),
                        },
                    )
                    .into(),
                ),
            });
            let f = env.new_frame();

            let r = expr.eval(&f);

            let v = ok_or_fail!(r);
            assert_eq!(v.to_string(), "a");
        }

        #[test]
        fn if_alternate() {
            let txt = make_textline().into();
            let env = TestEnv::default();
            let expr = ExprCtx {
                span: 0..13,
                txt: Rc::clone(&txt),
            }
            .into_expr(ExpressionKind::If {
                test: ExprCtx {
                    span: 4..6,
                    txt: Rc::clone(&txt),
                }
                .into_expr(ExpressionKind::Literal(Value::Boolean(false)))
                .into(),
                con: Expression::symbol(
                    env.symbols.get("a"),
                    ExprCtx {
                        span: 7..9,
                        txt: Rc::clone(&txt),
                    },
                )
                .into(),
                alt: Some(
                    Expression::symbol(
                        env.symbols.get("b"),
                        ExprCtx {
                            span: 10..12,
                            txt: Rc::clone(&txt),
                        },
                    )
                    .into(),
                ),
            });
            let f = env.new_frame();

            let r = expr.eval(&f);

            let v = ok_or_fail!(r);
            assert_eq!(v.to_string(), "b");
        }

        #[test]
        fn if_no_alternate() {
            let txt = make_textline().into();
            let env = TestEnv::default();
            let expr = ExprCtx {
                span: 0..10,
                txt: Rc::clone(&txt),
            }
            .into_expr(ExpressionKind::If {
                test: ExprCtx {
                    span: 4..6,
                    txt: Rc::clone(&txt),
                }
                .into_expr(ExpressionKind::Literal(Value::Boolean(false)))
                .into(),
                con: Expression::symbol(
                    env.symbols.get("a"),
                    ExprCtx {
                        span: 7..9,
                        txt: Rc::clone(&txt),
                    },
                )
                .into(),
                alt: None,
            });
            let f = env.new_frame();

            let r = expr.eval(&f);

            let v = ok_or_fail!(r);
            assert!(matches!(v, Value::Unspecified));
        }

        #[test]
        fn if_eval_failure() {
            let txt = make_textline().into();
            let env = TestEnv::default();
            let expr = ExprCtx {
                span: 0..10,
                txt: Rc::clone(&txt),
            }
            .into_expr(ExpressionKind::If {
                test: ExprCtx {
                    span: 4..6,
                    txt: Rc::clone(&txt),
                }
                .into_expr(ExpressionKind::Literal(Value::Boolean(true)))
                .into(),
                con: Expression::variable(
                    env.symbols.get("x"),
                    ExprCtx {
                        span: 7..9,
                        txt: Rc::clone(&txt),
                    },
                )
                .into(),
                alt: None,
            });
            let f = env.new_frame();

            let r = expr.eval(&f);

            let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
            assert_eq!(err.to_string(), "#<env-error \"unbound variable: x\">");
        }

        #[test]
        fn if_invalid_expr_not_evaluated() {
            let txt = make_textline().into();
            let env = TestEnv::default();
            let expr = ExprCtx {
                span: 0..13,
                txt: Rc::clone(&txt),
            }
            .into_expr(ExpressionKind::If {
                test: ExprCtx {
                    span: 4..6,
                    txt: Rc::clone(&txt),
                }
                .into_expr(ExpressionKind::Literal(Value::Boolean(false)))
                .into(),
                con: Expression::variable(
                    env.symbols.get("x"),
                    ExprCtx {
                        span: 7..9,
                        txt: Rc::clone(&txt),
                    },
                )
                .into(),
                alt: Some(
                    Expression::symbol(
                        env.symbols.get("b"),
                        ExprCtx {
                            span: 10..12,
                            txt: Rc::clone(&txt),
                        },
                    )
                    .into(),
                ),
            });
            let f = env.new_frame();

            let r = expr.eval(&f);

            let v = ok_or_fail!(r);
            assert_eq!(v.to_string(), "b");
        }

        #[test]
        fn lambda_to_proc() {
            let txt = make_textline().into();
            let env = TestEnv::default();
            let lm = ok_or_fail!(Lambda::new(
                [],
                None,
                procedure_body([TokenKind::Identifier("foo".to_owned())])
            ))
            .into();
            let expr = Expression::lambda(
                Rc::clone(&lm),
                ExprCtx {
                    span: 0..10,
                    txt: Rc::clone(&txt),
                },
            );
            env.binding
                .bind(env.symbols.get("foo"), Value::string("myval"));
            let f = env.new_frame();

            let r = expr.eval(&f);

            let v = ok_or_fail!(r);
            assert!(matches!(v, Value::Procedure(_)));
            let Value::Procedure(proc) = v else {
                unreachable!();
            };
            let new_env = TestEnv::default();
            let v = ok_or_fail!(proc.apply(&[], &new_env.new_frame()));
            assert!(matches!(v, Value::String(s) if s.as_ref() == "myval"));
        }
    }
}

mod error {
    use super::*;

    #[test]
    fn display_invalid_bytevector_item() {
        let sym = SymbolTable::default();
        let err = ExprCtx {
            span: 0..5,
            txt: make_textline().into(),
        }
        .into_error(ExpressionErrorKind::ByteVectorInvalidItem(
            ExpressionKind::Variable(sym.get("foobar")),
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
        let sym = SymbolTable::default();
        let err = ExprCtx {
            span: 0..5,
            txt: make_textline().into(),
        }
        .into_error(ExpressionErrorKind::VectorInvalidItem(
            ExpressionKind::Variable(sym.get("foobar")),
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
        let sym = SymbolTable::default();
        let err = ExprCtx {
            span: 0..5,
            txt: Rc::clone(&txt),
        }
        .into_error(ExpressionErrorKind::DatumInvalid(ExpressionKind::Call {
            proc: Expression::variable(
                sym.get("foo"),
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
    fn display_invalid_if() {
        let txt = make_textline().into();
        let err = ExprCtx {
            span: 0..5,
            txt: Rc::clone(&txt),
        }
        .into_error(ExpressionErrorKind::IfInvalid);

        assert_eq!(
            err.to_string(),
            "invalid form, expected: (if <test> <consequent> [alternate])"
        );
    }

    #[test]
    fn display_invalid_lambda() {
        let txt = make_textline().into();
        let err = ExprCtx {
            span: 0..5,
            txt: Rc::clone(&txt),
        }
        .into_error(ExpressionErrorKind::LambdaInvalid);

        assert_eq!(
            err.to_string(),
            "invalid form, expected: (lambda <formals> <body>)"
        );
    }

    #[test]
    fn display_invalid_lambda_signature() {
        let txt = make_textline().into();
        let err = ExprCtx {
            span: 0..5,
            txt: Rc::clone(&txt),
        }
        .into_error(ExpressionErrorKind::LambdaInvalidSignature);

        assert_eq!(err.to_string(), "invalid formals syntax");
    }

    #[test]
    fn display_invalid_lambda_formals() {
        let txt = make_textline().into();
        let err = ExprCtx {
            span: 0..5,
            txt: Rc::clone(&txt),
        }
        .into_error(ExpressionErrorKind::LambdaInvalidFormal(
            InvalidFormal::MaxFormals,
        ));

        assert_eq!(
            err.to_string(),
            "lambda definition exceeds formal arguments limit: 256"
        );
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
