use super::*;
use crate::eval::InvalidFormal;

#[test]
fn end() {
    let txt = make_textline().into();
    let env = TestEnv::default();
    let mut seq = vec![
        Expression::variable(
            env.symbols.get("+"),
            ExprCtx {
                span: 0..1,
                txt: Rc::clone(&txt),
            },
        ),
        ExprCtx {
            span: 1..4,
            txt: Rc::clone(&txt),
        }
        .into_expr(ExpressionKind::Literal(Value::real(4))),
        ExprCtx {
            span: 4..6,
            txt: Rc::clone(&txt),
        }
        .into_expr(ExpressionKind::Literal(Value::real(5))),
    ];
    let token = Token {
        kind: TokenKind::ParenRight,
        span: 6..7,
    };
    let ns = env.new_namespace();

    let f = SyntacticForm::Call.parse_list(&mut seq, token, &txt, &ns);

    assert!(matches!(
        f,
        ParseFlow::Break(ParseBreak::Complete(ExprEnd { lineno: 1, pos: 7 }))
    ));
    assert_eq!(seq.len(), 3);
}

#[test]
fn nested_list() {
    let txt = make_textline().into();
    let env = TestEnv::default();
    let mut seq = vec![
        Expression::variable(
            env.symbols.get("+"),
            ExprCtx {
                span: 0..1,
                txt: Rc::clone(&txt),
            },
        ),
        ExprCtx {
            span: 1..4,
            txt: Rc::clone(&txt),
        }
        .into_expr(ExpressionKind::Literal(Value::real(4))),
        ExprCtx {
            span: 4..6,
            txt: Rc::clone(&txt),
        }
        .into_expr(ExpressionKind::Literal(Value::real(5))),
    ];
    let token = Token {
        kind: TokenKind::ParenLeft,
        span: 6..7,
    };
    let ns = env.new_namespace();

    let f = SyntacticForm::Call.parse_list(&mut seq, token, &txt, &ns);

    assert!(matches!(
        f,
        ParseFlow::Break(ParseBreak::New(ParseNew {
            mode: ParseMode::List { form: SyntacticForm::Call, seq },
            start: 6
        })) if seq.is_empty()
    ));
    assert_eq!(seq.len(), 3);
}

#[test]
fn empty() {
    let mut seq = Vec::new();
    let token = Token {
        kind: TokenKind::ParenRight,
        span: 4..5,
    };
    let txt = make_textline().into();
    let env = TestEnv::default();
    let ns = env.new_namespace();

    let f = SyntacticForm::Call.parse_list(&mut seq, token, &txt, &ns);

    assert!(matches!(
        f,
        ParseFlow::Break(ParseBreak::Complete(ExprEnd { lineno: 1, pos: 5 }))
    ));
    assert!(seq.is_empty());
}

#[test]
fn expression_item() {
    let txt = make_textline().into();
    let env = TestEnv::default();
    let mut seq = vec![
        Expression::variable(
            env.symbols.get("+"),
            ExprCtx {
                span: 0..1,
                txt: Rc::clone(&txt),
            },
        ),
        ExprCtx {
            span: 1..4,
            txt: Rc::clone(&txt),
        }
        .into_expr(ExpressionKind::Literal(Value::real(4))),
        ExprCtx {
            span: 4..6,
            txt: Rc::clone(&txt),
        }
        .into_expr(ExpressionKind::Literal(Value::real(5))),
    ];
    let token = Token {
        kind: TokenKind::Number(Number::real(10)),
        span: 6..7,
    };
    let ns = env.new_namespace();

    let f = SyntacticForm::Call.parse_list(&mut seq, token, &txt, &ns);

    assert!(matches!(f, ParseFlow::Continue(())));
    assert_eq!(seq.len(), 4);
    assert!(matches!(
        &seq[3],
        Expression {
            ctx: ExprCtx { span: TxtSpan { start: 6, end: 7 }, txt: line },
            kind: ExpressionKind::Literal(Value::Number(n)),
        } if n.to_string() == "10" && Rc::ptr_eq(&txt, &line)
    ));
}

#[test]
fn start_dotted_pair() {
    let txt = make_textline().into();
    let mut seq = vec![
        ExprCtx {
            span: 1..4,
            txt: Rc::clone(&txt),
        }
        .into_expr(ExpressionKind::Literal(Value::real(4))),
    ];
    let token = Token {
        kind: TokenKind::PairJoiner,
        span: 5..6,
    };
    let env = TestEnv::default();
    let ns = env.new_namespace();
    let mut frm = SyntacticForm::Datum;

    let f = frm.parse_list(&mut seq, token, &txt, &ns);

    assert!(matches!(frm, SyntacticForm::PairOpen));
    assert!(matches!(f, ParseFlow::Continue(())));
    assert_eq!(seq.len(), 1);
}

#[test]
fn start_dotted_pair_missing_first_element() {
    let txt = make_textline().into();
    let mut seq = vec![];
    let token = Token {
        kind: TokenKind::PairJoiner,
        span: 1..2,
    };
    let env = TestEnv::default();
    let ns = env.new_namespace();
    let mut frm = SyntacticForm::Datum;

    let f = frm.parse_list(&mut seq, token, &txt, &ns);

    assert!(matches!(frm, SyntacticForm::Datum));
    assert!(matches!(
        f,
        ParseFlow::Break(ParseBreak::Err {
            err: ExpressionError {
                ctx: ExprCtx { span: TxtSpan { start: 1, end: 2 }, txt: line },
                kind: ExpressionErrorKind::PairIncomplete,
            },
            flow: ParseErrFlow::Continue(()),
        }) if Rc::ptr_eq(&line, &txt)
    ));
    assert!(seq.is_empty());
}

#[test]
fn dotted_pair_in_non_datum() {
    let txt = make_textline().into();
    let mut seq = vec![
        ExprCtx {
            span: 1..4,
            txt: Rc::clone(&txt),
        }
        .into_expr(ExpressionKind::Literal(Value::real(4))),
    ];
    let token = Token {
        kind: TokenKind::PairJoiner,
        span: 5..6,
    };
    let env = TestEnv::default();
    let ns = env.new_namespace();

    let f = SyntacticForm::Call.parse_list(&mut seq, token, &txt, &ns);

    assert!(matches!(
        f,
        ParseFlow::Break(ParseBreak::Err {
            err: ExpressionError {
                ctx: ExprCtx { span: TxtSpan { start: 5, end: 6 }, txt: line },
                kind: ExpressionErrorKind::PairUnexpected,
            },
            flow: ParseErrFlow::Continue(()),
        }) if Rc::ptr_eq(&line, &txt)
    ));
    assert_eq!(seq.len(), 1);
}

#[test]
fn close_dotted_pair() {
    let txt = make_textline().into();
    let mut seq = vec![
        ExprCtx {
            span: 1..4,
            txt: Rc::clone(&txt),
        }
        .into_expr(ExpressionKind::Literal(Value::real(4))),
    ];
    let token = Token {
        kind: TokenKind::Identifier("foo".to_owned()),
        span: 5..8,
    };
    let env = TestEnv::default();
    let ns = env.new_namespace();
    let mut frm = SyntacticForm::PairOpen;

    let f = frm.parse_list(&mut seq, token, &txt, &ns);

    assert!(matches!(frm, SyntacticForm::PairClosed));
    assert!(matches!(f, ParseFlow::Continue(())));
    assert_eq!(seq.len(), 2);
    assert!(matches!(
        &seq[1],
        Expression {
            ctx: ExprCtx { span: TxtSpan { start: 5, end: 8 }, txt: line },
            kind: ExpressionKind::Literal(Value::Symbol(s)),
        } if s.as_ref() == "foo" && Rc::ptr_eq(&txt, &line)
    ));
}

#[test]
fn open_dotted_pair_does_nothing_if_no_expr() {
    let txt = make_textline().into();
    let mut seq = vec![
        ExprCtx {
            span: 1..4,
            txt: Rc::clone(&txt),
        }
        .into_expr(ExpressionKind::Literal(Value::real(4))),
    ];
    let token = Token {
        kind: TokenKind::Comment,
        span: 5..8,
    };
    let env = TestEnv::default();
    let ns = env.new_namespace();
    let mut frm = SyntacticForm::PairOpen;

    let f = frm.parse_list(&mut seq, token, &txt, &ns);

    assert!(matches!(frm, SyntacticForm::PairOpen));
    assert!(matches!(f, ParseFlow::Continue(())));
    assert_eq!(seq.len(), 1);
}

#[test]
fn closed_dotted_pair_does_nothing_if_no_expr() {
    let txt = make_textline().into();
    let mut seq = vec![
        ExprCtx {
            span: 1..4,
            txt: Rc::clone(&txt),
        }
        .into_expr(ExpressionKind::Literal(Value::real(4))),
    ];
    let token = Token {
        kind: TokenKind::Comment,
        span: 5..8,
    };
    let env = TestEnv::default();
    let ns = env.new_namespace();
    let mut frm = SyntacticForm::PairClosed;

    let f = frm.parse_list(&mut seq, token, &txt, &ns);

    assert!(matches!(frm, SyntacticForm::PairClosed));
    assert!(matches!(f, ParseFlow::Continue(())));
    assert_eq!(seq.len(), 1);
}

#[test]
fn open_dotted_pair_hits_end_of_list() {
    let txt = make_textline().into();
    let mut seq = vec![
        ExprCtx {
            span: 1..4,
            txt: Rc::clone(&txt),
        }
        .into_expr(ExpressionKind::Literal(Value::real(4))),
    ];
    let token = Token {
        kind: TokenKind::ParenRight,
        span: 5..6,
    };
    let env = TestEnv::default();
    let ns = env.new_namespace();
    let mut frm = SyntacticForm::PairOpen;

    let f = frm.parse_list(&mut seq, token, &txt, &ns);

    assert!(matches!(frm, SyntacticForm::PairOpen));
    assert!(matches!(
        f,
        ParseFlow::Break(ParseBreak::Err {
            err: ExpressionError {
                ctx: ExprCtx { span: TxtSpan { start: 5, end: 6 }, txt: line },
                kind: ExpressionErrorKind::PairUnterminated,
            },
            flow: ParseErrFlow::Break(ParseErrBreak::FailedNode),
        }) if Rc::ptr_eq(&line, &txt)
    ));
    assert_eq!(seq.len(), 1);
}

#[test]
fn double_dotted_open_pair() {
    let txt = make_textline().into();
    let mut seq = vec![
        ExprCtx {
            span: 1..4,
            txt: Rc::clone(&txt),
        }
        .into_expr(ExpressionKind::Literal(Value::real(4))),
    ];
    let token = Token {
        kind: TokenKind::PairJoiner,
        span: 5..6,
    };
    let env = TestEnv::default();
    let ns = env.new_namespace();
    let mut frm = SyntacticForm::PairOpen;

    let f = frm.parse_list(&mut seq, token, &txt, &ns);

    assert!(matches!(frm, SyntacticForm::Datum));
    assert!(matches!(
        f,
        ParseFlow::Break(ParseBreak::Err {
            err: ExpressionError {
                ctx: ExprCtx { span: TxtSpan { start: 5, end: 6 }, txt: line },
                kind: ExpressionErrorKind::PairUnterminated,
            },
            flow: ParseErrFlow::Continue(()),
        }) if Rc::ptr_eq(&line, &txt)
    ));
    assert_eq!(seq.len(), 1);
}

#[test]
fn double_dotted_closed_pair() {
    let txt = make_textline().into();
    let mut seq = vec![
        ExprCtx {
            span: 1..4,
            txt: Rc::clone(&txt),
        }
        .into_expr(ExpressionKind::Literal(Value::real(4))),
    ];
    let token = Token {
        kind: TokenKind::PairJoiner,
        span: 5..6,
    };
    let env = TestEnv::default();
    let ns = env.new_namespace();
    let mut frm = SyntacticForm::PairClosed;

    let f = frm.parse_list(&mut seq, token, &txt, &ns);

    assert!(matches!(frm, SyntacticForm::Datum));
    assert!(matches!(
        f,
        ParseFlow::Break(ParseBreak::Err {
            err: ExpressionError {
                ctx: ExprCtx { span: TxtSpan { start: 5, end: 6 }, txt: line },
                kind: ExpressionErrorKind::PairUnterminated,
            },
            flow: ParseErrFlow::Continue(()),
        }) if Rc::ptr_eq(&line, &txt)
    ));
    assert_eq!(seq.len(), 1);
}

#[test]
fn invalid_token() {
    let txt = make_textline().into();
    let env = TestEnv::default();
    let mut seq = vec![
        Expression::variable(
            env.symbols.get("+"),
            ExprCtx {
                span: 0..1,
                txt: Rc::clone(&txt),
            },
        ),
        ExprCtx {
            span: 1..4,
            txt: Rc::clone(&txt),
        }
        .into_expr(ExpressionKind::Literal(Value::real(4))),
        ExprCtx {
            span: 4..6,
            txt: Rc::clone(&txt),
        }
        .into_expr(ExpressionKind::Literal(Value::real(5))),
    ];
    let token = Token {
        kind: TokenKind::StringDiscard,
        span: 6..7,
    };
    let ns = env.new_namespace();

    let f = SyntacticForm::Call.parse_list(&mut seq, token, &txt, &ns);

    assert!(matches!(
        f,
        ParseFlow::Break(ParseBreak::Err {
            err: ExpressionError {
                ctx: ExprCtx { span: TxtSpan { start: 6, end: 7 }, txt: line },
                kind: ExpressionErrorKind::SeqInvalid(TokenKind::StringDiscard),
            },
            flow: ParseErrFlow::Break(ParseErrBreak::InvalidTokenStream),
        }) if Rc::ptr_eq(&line, &txt)
    ));
    assert_eq!(seq.len(), 3);
}

#[test]
fn into_procedure_call() {
    // (+ 4 5)
    let txt = make_textline().into();
    let env = TestEnv::default();
    let p = ExprNode {
        ctx: ExprCtx {
            span: 0..6,
            txt: Rc::clone(&txt),
        },
        mode: ParseMode::List {
            form: SyntacticForm::Call,
            seq: vec![
                Expression::variable(
                    env.symbols.get("+"),
                    ExprCtx {
                        span: 0..1,
                        txt: Rc::clone(&txt),
                    },
                ),
                ExprCtx {
                    span: 1..4,
                    txt: Rc::clone(&txt),
                }
                .into_expr(ExpressionKind::Literal(Value::real(4))),
                ExprCtx {
                    span: 4..6,
                    txt: Rc::clone(&txt),
                }
                .into_expr(ExpressionKind::Literal(Value::real(5))),
            ],
        },
    };
    let ns = env.new_namespace();

    let r = p.try_into_expr(&ns);

    let expr = some_or_fail!(ok_or_fail!(r));
    assert!(matches!(
        expr,
        Expression {
            ctx: ExprCtx { span: TxtSpan { start: 0, end: 6 }, txt: line },
            kind: ExpressionKind::Call { .. },
        } if Rc::ptr_eq(&txt, &line)
    ));
    let ExpressionKind::Call { args, proc } = expr.kind else {
        unreachable!();
    };
    assert!(matches!(
        proc.as_ref(),
        Expression {
            ctx: ExprCtx { span: TxtSpan { start: 0, end: 1 }, txt: line },
            kind: ExpressionKind::Variable(s),
        } if s.as_ref() == "+" && Rc::ptr_eq(&txt, &line)
    ));
    assert_eq!(args.len(), 2);
    assert!(matches!(
        &args[0],
        Expression {
            ctx: ExprCtx { span: TxtSpan { start: 1, end: 4 }, txt: line },
            kind: ExpressionKind::Literal(Value::Number(n)),
        } if n.to_string() == "4" && Rc::ptr_eq(&txt, &line)
    ));
    assert!(matches!(
        &args[1],
        Expression {
            ctx: ExprCtx { span: TxtSpan { start: 4, end: 6 }, txt: line },
            kind: ExpressionKind::Literal(Value::Number(n)),
        } if n.to_string() == "5" && Rc::ptr_eq(&txt, &line)
    ));
}

#[test]
fn into_empty_procedure_call() {
    // ()
    let txt = make_textline().into();
    let p = ExprNode {
        ctx: ExprCtx {
            span: 0..8,
            txt: Rc::clone(&txt),
        },
        mode: ParseMode::List {
            form: SyntacticForm::Call,
            seq: vec![],
        },
    };
    let env = TestEnv::default();
    let ns = env.new_namespace();

    let r = p.try_into_expr(&ns);

    let errs = err_or_fail!(r);
    assert_eq!(errs.len(), 1);
    assert!(matches!(
        &errs[0],
        ExpressionError {
            ctx: ExprCtx { span: TxtSpan { start: 0, end: 8 }, txt: line },
            kind: ExpressionErrorKind::ProcedureEmpty,
        } if Rc::ptr_eq(&txt, &line)
    ));
}

#[test]
fn into_quote_apply() {
    // (quote foo)
    let txt = make_textline().into();
    let env = TestEnv::default();
    let p = ExprNode {
        ctx: ExprCtx {
            span: 0..10,
            txt: Rc::clone(&txt),
        },
        mode: ParseMode::List {
            form: SyntacticForm::Quote,
            seq: vec![Expression::symbol(
                env.symbols.get("foo"),
                ExprCtx {
                    span: 6..9,
                    txt: Rc::clone(&txt),
                },
            )],
        },
    };
    let ns = env.new_namespace();

    let r = p.try_into_expr(&ns);

    let expr = some_or_fail!(ok_or_fail!(r));
    assert!(matches!(
        expr,
        Expression {
            ctx: ExprCtx { span: TxtSpan { start: 6, end: 9 }, txt: line },
            kind: ExpressionKind::Literal(Value::Symbol(s)),
        } if Rc::ptr_eq(&txt, &line) && s.as_ref() == "foo"
    ));
}

#[test]
fn into_empty_quote_apply() {
    // (quote)
    let txt = make_textline().into();
    let p = ExprNode {
        ctx: ExprCtx {
            span: 0..7,
            txt: Rc::clone(&txt),
        },
        mode: ParseMode::List {
            form: SyntacticForm::Quote,
            seq: vec![],
        },
    };
    let env = TestEnv::default();
    let ns = env.new_namespace();

    let r = p.try_into_expr(&ns);

    let errs = err_or_fail!(r);
    assert_eq!(errs.len(), 1);
    assert!(matches!(
        &errs[0],
        ExpressionError {
            ctx: ExprCtx { span: TxtSpan { start: 0, end: 7 }, txt: line },
            kind: ExpressionErrorKind::QuoteInvalid,
        } if Rc::ptr_eq(&txt, &line)
    ));
}

#[test]
fn into_quote_apply_too_many_args() {
    // (quote foo bar)
    let txt = make_textline().into();
    let env = TestEnv::default();
    let p = ExprNode {
        ctx: ExprCtx {
            span: 0..14,
            txt: Rc::clone(&txt),
        },
        mode: ParseMode::List {
            form: SyntacticForm::Quote,
            seq: vec![
                Expression::symbol(
                    env.symbols.get("foo"),
                    ExprCtx {
                        span: 6..9,
                        txt: Rc::clone(&txt),
                    },
                ),
                Expression::symbol(
                    env.symbols.get("bar"),
                    ExprCtx {
                        span: 10..13,
                        txt: Rc::clone(&txt),
                    },
                ),
            ],
        },
    };
    let ns = env.new_namespace();

    let r = p.try_into_expr(&ns);

    let errs = err_or_fail!(r);
    assert_eq!(errs.len(), 1);
    assert!(matches!(
        &errs[0],
        ExpressionError {
            ctx: ExprCtx { span: TxtSpan { start: 0, end: 14 }, txt: line },
            kind: ExpressionErrorKind::QuoteInvalid,
        } if Rc::ptr_eq(&txt, &line)
    ));
}

#[test]
fn into_datum_list() {
    // '(+ 4 5)
    let txt = make_textline().into();
    let env = TestEnv::default();
    let p = ExprNode {
        ctx: ExprCtx {
            span: 0..6,
            txt: Rc::clone(&txt),
        },
        mode: ParseMode::List {
            form: SyntacticForm::Datum,
            seq: vec![
                Expression::symbol(
                    env.symbols.get("+"),
                    ExprCtx {
                        span: 0..1,
                        txt: Rc::clone(&txt),
                    },
                ),
                ExprCtx {
                    span: 1..4,
                    txt: Rc::clone(&txt),
                }
                .into_expr(ExpressionKind::Literal(Value::real(4))),
                ExprCtx {
                    span: 4..6,
                    txt: Rc::clone(&txt),
                }
                .into_expr(ExpressionKind::Literal(Value::real(5))),
            ],
        },
    };
    let ns = env.new_namespace();

    let r = p.try_into_expr(&ns);

    let expr = some_or_fail!(ok_or_fail!(r));
    assert!(matches!(
        expr,
        Expression {
            ctx: ExprCtx { span: TxtSpan { start: 0, end: 6 }, txt: line },
            kind: ExpressionKind::Literal(Value::Pair(Some(_))),
        } if Rc::ptr_eq(&txt, &line)
    ));
    let value = extract_or_fail!(expr.kind, ExpressionKind::Literal);
    assert_eq!(value.to_string(), "(+ 4 5)");
}

#[test]
fn into_empty_datum_list() {
    // '()
    let txt = make_textline().into();
    let p = ExprNode {
        ctx: ExprCtx {
            span: 0..2,
            txt: Rc::clone(&txt),
        },
        mode: ParseMode::List {
            form: SyntacticForm::Datum,
            seq: Vec::new(),
        },
    };
    let env = TestEnv::default();
    let ns = env.new_namespace();

    let r = p.try_into_expr(&ns);

    let expr = some_or_fail!(ok_or_fail!(r));
    assert!(matches!(
        expr,
        Expression {
            ctx: ExprCtx { span: TxtSpan { start: 0, end: 2 }, txt: line },
            kind: ExpressionKind::Literal(Value::Pair(None)),
        } if Rc::ptr_eq(&txt, &line)
    ));
    let value = extract_or_fail!(expr.kind, ExpressionKind::Literal);
    assert_eq!(value.to_string(), "()");
}

#[test]
fn into_invalid_datum_list() {
    // '(<unquoted +> 4 5)
    let txt = make_textline().into();
    let env = TestEnv::default();
    let p = ExprNode {
        ctx: ExprCtx {
            span: 0..8,
            txt: Rc::clone(&txt),
        },
        mode: ParseMode::List {
            form: SyntacticForm::Datum,
            seq: vec![
                Expression::variable(
                    env.symbols.get("+"),
                    ExprCtx {
                        span: 1..2,
                        txt: Rc::clone(&txt),
                    },
                ),
                ExprCtx {
                    span: 2..5,
                    txt: Rc::clone(&txt),
                }
                .into_expr(ExpressionKind::Literal(Value::real(4))),
                ExprCtx {
                    span: 5..7,
                    txt: Rc::clone(&txt),
                }
                .into_expr(ExpressionKind::Literal(Value::real(5))),
            ],
        },
    };
    let ns = env.new_namespace();

    let r = p.try_into_expr(&ns);

    let errs = err_or_fail!(r);
    assert_eq!(errs.len(), 1);
    assert!(matches!(
        &errs[0],
        ExpressionError {
            ctx: ExprCtx { span: TxtSpan { start: 1, end: 2 }, txt: line },
            kind: ExpressionErrorKind::DatumInvalid(ExpressionKind::Variable(s)),
        } if Rc::ptr_eq(&txt, &line) && s.as_ref() == "+"
    ));
}

#[test]
fn into_pair() {
    // '(4 . 5)
    let txt = make_textline().into();
    let p = ExprNode {
        ctx: ExprCtx {
            span: 0..8,
            txt: Rc::clone(&txt),
        },
        mode: ParseMode::List {
            form: SyntacticForm::PairClosed,
            seq: vec![
                ExprCtx {
                    span: 1..2,
                    txt: Rc::clone(&txt),
                }
                .into_expr(ExpressionKind::Literal(Value::real(4))),
                ExprCtx {
                    span: 6..7,
                    txt: Rc::clone(&txt),
                }
                .into_expr(ExpressionKind::Literal(Value::real(5))),
            ],
        },
    };
    let env = TestEnv::default();
    let ns = env.new_namespace();

    let r = p.try_into_expr(&ns);

    let expr = some_or_fail!(ok_or_fail!(r));
    assert!(matches!(
        expr,
        Expression {
            ctx: ExprCtx { span: TxtSpan { start: 0, end: 8 }, txt: line },
            kind: ExpressionKind::Literal(Value::Pair(Some(_))),
        } if Rc::ptr_eq(&txt, &line)
    ));
    let value = extract_or_fail!(expr.kind, ExpressionKind::Literal);
    assert_eq!(value.to_string(), "(4 . 5)");
}

#[test]
fn invalid_into_open_pair() {
    // '(4 . )
    let txt = make_textline().into();
    let p = ExprNode {
        ctx: ExprCtx {
            span: 0..8,
            txt: Rc::clone(&txt),
        },
        mode: ParseMode::List {
            form: SyntacticForm::PairOpen,
            seq: vec![
                ExprCtx {
                    span: 1..2,
                    txt: Rc::clone(&txt),
                }
                .into_expr(ExpressionKind::Literal(Value::real(4))),
            ],
        },
    };
    let env = TestEnv::default();
    let ns = env.new_namespace();

    let r = p.try_into_expr(&ns);

    let errs = err_or_fail!(r);
    assert_eq!(errs.len(), 1);
    assert!(matches!(
        &errs[0],
        ExpressionError {
            ctx: ExprCtx { span: TxtSpan { start: 0, end: 8 }, txt: line },
            kind: ExpressionErrorKind::PairUnterminated,
        } if Rc::ptr_eq(&txt, &line)
    ));
}

#[test]
fn into_define_variable() {
    // (define foo "bar")
    let txt = make_textline().into();
    let env = TestEnv::default();
    let p = ExprNode {
        ctx: ExprCtx {
            span: 0..18,
            txt: Rc::clone(&txt),
        },
        mode: ParseMode::List {
            form: SyntacticForm::Define,
            seq: vec![
                Expression::symbol(
                    env.symbols.get("foo"),
                    ExprCtx {
                        span: 8..11,
                        txt: Rc::clone(&txt),
                    },
                ),
                Expression::string(
                    "bar",
                    ExprCtx {
                        span: 12..17,
                        txt: Rc::clone(&txt),
                    },
                ),
            ],
        },
    };
    let ns = env.new_namespace();

    let r = p.try_into_expr(&ns);

    let expr = some_or_fail!(ok_or_fail!(r));
    assert!(matches!(
        expr,
        Expression {
            ctx: ExprCtx { span: TxtSpan { start: 0, end: 18 }, txt: line },
            kind: ExpressionKind::Define { .. },
        } if Rc::ptr_eq(&txt, &line)
    ));
    let ExpressionKind::Define { name, expr } = expr.kind else {
        unreachable!();
    };
    assert_eq!(name.as_ref(), "foo");
    let val_expr = some_or_fail!(expr);
    let val = extract_or_fail!(val_expr.kind, ExpressionKind::Literal);
    assert_eq!(val.to_string(), "\"bar\"");
}

#[test]
fn into_define_lambda() {
    // (define (foo x y) (+ x y))
    let txt = make_textline().into();
    let env = TestEnv::default();
    let p = ExprNode {
        ctx: ExprCtx {
            span: 0..26,
            txt: Rc::clone(&txt),
        },
        mode: ParseMode::List {
            form: SyntacticForm::Define,
            seq: vec![
                ExprCtx {
                    span: 8..17,
                    txt: Rc::clone(&txt),
                }
                .into_expr(ExpressionKind::Literal(zlist![
                    Value::Symbol(env.symbols.get("foo")),
                    Value::Symbol(env.symbols.get("x")),
                    Value::Symbol(env.symbols.get("y"))
                ])),
                ExprCtx {
                    span: 18..25,
                    txt: Rc::clone(&txt),
                }
                .into_expr(ExpressionKind::Call {
                    proc: Expression::variable(
                        env.symbols.get("+"),
                        ExprCtx {
                            span: 19..20,
                            txt: Rc::clone(&txt),
                        },
                    )
                    .into(),
                    args: [
                        Expression::variable(
                            env.symbols.get("x"),
                            ExprCtx {
                                span: 21..22,
                                txt: Rc::clone(&txt),
                            },
                        ),
                        Expression::variable(
                            env.symbols.get("y"),
                            ExprCtx {
                                span: 23..24,
                                txt: Rc::clone(&txt),
                            },
                        ),
                    ]
                    .into(),
                }),
            ],
        },
    };
    let ns = env.new_namespace();

    let r = p.try_into_expr(&ns);

    let expr = some_or_fail!(ok_or_fail!(r));
    assert!(matches!(
        expr,
        Expression {
            ctx: ExprCtx { span: TxtSpan { start: 0, end: 26 }, txt: line },
            kind: ExpressionKind::Define { .. },
        } if Rc::ptr_eq(&txt, &line)
    ));
    let ExpressionKind::Define { name, expr } = expr.kind else {
        unreachable!();
    };
    assert_eq!(name.as_ref(), "foo");
    let val_expr = some_or_fail!(expr);
    let val = extract_or_fail!(val_expr.kind, ExpressionKind::Literal);
    assert_eq!(val.to_string(), "\"bar\"");
}

#[test]
fn into_define_lambda_multi_expression_body() {
    // (define (foo x y) (+ x y) (display "foo") 'a)
    let txt = make_textline().into();
    let env = TestEnv::default();
    let p = ExprNode {
        ctx: ExprCtx {
            span: 0..45,
            txt: Rc::clone(&txt),
        },
        mode: ParseMode::List {
            form: SyntacticForm::Define,
            seq: vec![
                ExprCtx {
                    span: 8..17,
                    txt: Rc::clone(&txt),
                }
                .into_expr(ExpressionKind::Literal(zlist![
                    Value::Symbol(env.symbols.get("foo")),
                    Value::Symbol(env.symbols.get("x")),
                    Value::Symbol(env.symbols.get("y"))
                ])),
                ExprCtx {
                    span: 18..25,
                    txt: Rc::clone(&txt),
                }
                .into_expr(ExpressionKind::Call {
                    proc: Expression::variable(
                        env.symbols.get("+"),
                        ExprCtx {
                            span: 19..20,
                            txt: Rc::clone(&txt),
                        },
                    )
                    .into(),
                    args: [
                        Expression::variable(
                            env.symbols.get("x"),
                            ExprCtx {
                                span: 21..22,
                                txt: Rc::clone(&txt),
                            },
                        ),
                        Expression::variable(
                            env.symbols.get("y"),
                            ExprCtx {
                                span: 23..24,
                                txt: Rc::clone(&txt),
                            },
                        ),
                    ]
                    .into(),
                }),
                ExprCtx {
                    span: 26..41,
                    txt: Rc::clone(&txt),
                }
                .into_expr(ExpressionKind::Call {
                    proc: Expression::variable(
                        env.symbols.get("display"),
                        ExprCtx {
                            span: 27..34,
                            txt: Rc::clone(&txt),
                        },
                    )
                    .into(),
                    args: [Expression::string(
                        "foo",
                        ExprCtx {
                            span: 35..40,
                            txt: Rc::clone(&txt),
                        },
                    )]
                    .into(),
                }),
                Expression::symbol(
                    env.symbols.get("a"),
                    ExprCtx {
                        span: 42..44,
                        txt: Rc::clone(&txt),
                    },
                ),
            ],
        },
    };
    let ns = env.new_namespace();

    let r = p.try_into_expr(&ns);

    let expr = some_or_fail!(ok_or_fail!(r));
    assert!(matches!(
        expr,
        Expression {
            ctx: ExprCtx { span: TxtSpan { start: 0, end: 26 }, txt: line },
            kind: ExpressionKind::Define { .. },
        } if Rc::ptr_eq(&txt, &line)
    ));
    let ExpressionKind::Define { name, expr } = expr.kind else {
        unreachable!();
    };
    assert_eq!(name.as_ref(), "foo");
    let val_expr = some_or_fail!(expr);
    let val = extract_or_fail!(val_expr.kind, ExpressionKind::Literal);
    assert_eq!(val.to_string(), "\"bar\"");
}

#[test]
fn into_define_parameterless_lambda() {
    // (define (foo) 123)
    let txt = make_textline().into();
    let env = TestEnv::default();
    let p = ExprNode {
        ctx: ExprCtx {
            span: 0..18,
            txt: Rc::clone(&txt),
        },
        mode: ParseMode::List {
            form: SyntacticForm::Define,
            seq: vec![
                ExprCtx {
                    span: 8..13,
                    txt: Rc::clone(&txt),
                }
                .into_expr(ExpressionKind::Literal(zlist![Value::Symbol(
                    env.symbols.get("foo")
                )])),
                ExprCtx {
                    span: 14..17,
                    txt: Rc::clone(&txt),
                }
                .into_expr(ExpressionKind::Literal(Value::Number(Number::real(123)))),
            ],
        },
    };
    let ns = env.new_namespace();

    let r = p.try_into_expr(&ns);

    let expr = some_or_fail!(ok_or_fail!(r));
    assert!(matches!(
        expr,
        Expression {
            ctx: ExprCtx { span: TxtSpan { start: 0, end: 18 }, txt: line },
            kind: ExpressionKind::Define { .. },
        } if Rc::ptr_eq(&txt, &line)
    ));
    let ExpressionKind::Define { name, expr } = expr.kind else {
        unreachable!();
    };
    assert_eq!(name.as_ref(), "foo");
    let val_expr = some_or_fail!(expr);
    let val = extract_or_fail!(val_expr.kind, ExpressionKind::Literal);
    assert_eq!(val.to_string(), "\"bar\"");
}

#[test]
fn into_define_variadic_lambda() {
    // (define (foo . x) (cdr x))
    let txt = make_textline().into();
    let env = TestEnv::default();
    let p = ExprNode {
        ctx: ExprCtx {
            span: 0..26,
            txt: Rc::clone(&txt),
        },
        mode: ParseMode::List {
            form: SyntacticForm::Define,
            seq: vec![
                ExprCtx {
                    span: 8..17,
                    txt: Rc::clone(&txt),
                }
                .into_expr(ExpressionKind::Literal(Value::cons(
                    Value::Symbol(env.symbols.get("foo")),
                    Value::Symbol(env.symbols.get("x")),
                ))),
                ExprCtx {
                    span: 18..25,
                    txt: Rc::clone(&txt),
                }
                .into_expr(ExpressionKind::Call {
                    proc: Expression::variable(
                        env.symbols.get("cdr"),
                        ExprCtx {
                            span: 19..22,
                            txt: Rc::clone(&txt),
                        },
                    )
                    .into(),
                    args: [Expression::variable(
                        env.symbols.get("x"),
                        ExprCtx {
                            span: 23..24,
                            txt: Rc::clone(&txt),
                        },
                    )]
                    .into(),
                }),
            ],
        },
    };
    let ns = env.new_namespace();

    let r = p.try_into_expr(&ns);

    let expr = some_or_fail!(ok_or_fail!(r));
    assert!(matches!(
        expr,
        Expression {
            ctx: ExprCtx { span: TxtSpan { start: 0, end: 26 }, txt: line },
            kind: ExpressionKind::Define { .. },
        } if Rc::ptr_eq(&txt, &line)
    ));
    let ExpressionKind::Define { name, expr } = expr.kind else {
        unreachable!();
    };
    assert_eq!(name.as_ref(), "foo");
    let val_expr = some_or_fail!(expr);
    let val = extract_or_fail!(val_expr.kind, ExpressionKind::Literal);
    assert_eq!(val.to_string(), "\"bar\"");
}

#[test]
fn into_define_rest_lambda() {
    // (define (foo x . y) (display x y))
    let txt = make_textline().into();
    let env = TestEnv::default();
    let p = ExprNode {
        ctx: ExprCtx {
            span: 0..34,
            txt: Rc::clone(&txt),
        },
        mode: ParseMode::List {
            form: SyntacticForm::Define,
            seq: vec![
                ExprCtx {
                    span: 8..19,
                    txt: Rc::clone(&txt),
                }
                .into_expr(ExpressionKind::Literal(Value::cons(
                    Value::Symbol(env.symbols.get("foo")),
                    Value::cons(
                        Value::Symbol(env.symbols.get("x")),
                        Value::Symbol(env.symbols.get("y")),
                    ),
                ))),
                ExprCtx {
                    span: 20..33,
                    txt: Rc::clone(&txt),
                }
                .into_expr(ExpressionKind::Call {
                    proc: Expression::variable(
                        env.symbols.get("display"),
                        ExprCtx {
                            span: 21..28,
                            txt: Rc::clone(&txt),
                        },
                    )
                    .into(),
                    args: [
                        Expression::variable(
                            env.symbols.get("x"),
                            ExprCtx {
                                span: 29..30,
                                txt: Rc::clone(&txt),
                            },
                        ),
                        Expression::variable(
                            env.symbols.get("y"),
                            ExprCtx {
                                span: 31..32,
                                txt: Rc::clone(&txt),
                            },
                        ),
                    ]
                    .into(),
                }),
            ],
        },
    };
    let ns = env.new_namespace();

    let r = p.try_into_expr(&ns);

    let expr = some_or_fail!(ok_or_fail!(r));
    assert!(matches!(
        expr,
        Expression {
            ctx: ExprCtx { span: TxtSpan { start: 0, end: 34 }, txt: line },
            kind: ExpressionKind::Define { .. },
        } if Rc::ptr_eq(&txt, &line)
    ));
    let ExpressionKind::Define { name, expr } = expr.kind else {
        unreachable!();
    };
    assert_eq!(name.as_ref(), "foo");
    let val_expr = some_or_fail!(expr);
    let val = extract_or_fail!(val_expr.kind, ExpressionKind::Literal);
    assert_eq!(val.to_string(), "\"bar\"");
}

#[test]
fn into_define_variable_no_value() {
    // (define foo)
    let txt = make_textline().into();
    let env = TestEnv::default();
    let p = ExprNode {
        ctx: ExprCtx {
            span: 0..12,
            txt: Rc::clone(&txt),
        },
        mode: ParseMode::List {
            form: SyntacticForm::Define,
            seq: vec![Expression::symbol(
                env.symbols.get("foo"),
                ExprCtx {
                    span: 8..11,
                    txt: Rc::clone(&txt),
                },
            )],
        },
    };
    let ns = env.new_namespace();

    let r = p.try_into_expr(&ns);

    let expr = some_or_fail!(ok_or_fail!(r));
    assert!(matches!(
        expr,
        Expression {
            ctx: ExprCtx { span: TxtSpan { start: 0, end: 12 }, txt: line },
            kind: ExpressionKind::Define { .. },
        } if Rc::ptr_eq(&txt, &line)
    ));
    let ExpressionKind::Define { name, expr } = expr.kind else {
        unreachable!();
    };
    assert_eq!(name.as_ref(), "foo");
    assert!(expr.is_none());
}

#[test]
fn into_define_not_identifier_expr() {
    // (define <unquoted (myproc)> "bar")
    let txt = make_textline().into();
    let env = TestEnv::default();
    let p = ExprNode {
        ctx: ExprCtx {
            span: 0..23,
            txt: Rc::clone(&txt),
        },
        mode: ParseMode::List {
            form: SyntacticForm::Define,
            seq: vec![
                ExprCtx {
                    span: 8..16,
                    txt: Rc::clone(&txt),
                }
                .into_expr(ExpressionKind::Call {
                    proc: Expression::variable(
                        env.symbols.get("myproc"),
                        ExprCtx {
                            span: 9..15,
                            txt: Rc::clone(&txt),
                        },
                    )
                    .into(),
                    args: [].into(),
                }),
                Expression::string(
                    "bar",
                    ExprCtx {
                        span: 17..22,
                        txt: Rc::clone(&txt),
                    },
                ),
            ],
        },
    };
    let ns = env.new_namespace();

    let r = p.try_into_expr(&ns);

    let errs = err_or_fail!(r);
    assert_eq!(errs.len(), 1);
    assert!(matches!(
        &errs[0],
        ExpressionError {
            ctx: ExprCtx { span: TxtSpan { start: 0, end: 23 }, txt: line },
            kind: ExpressionErrorKind::DefineInvalid,
        } if Rc::ptr_eq(&txt, &line)
    ));
}

#[test]
fn into_empty_define() {
    // (define)
    let txt = make_textline().into();
    let p = ExprNode {
        ctx: ExprCtx {
            span: 0..8,
            txt: Rc::clone(&txt),
        },
        mode: ParseMode::List {
            form: SyntacticForm::Define,
            seq: vec![],
        },
    };
    let env = TestEnv::default();
    let ns = env.new_namespace();

    let r = p.try_into_expr(&ns);

    let errs = err_or_fail!(r);
    assert_eq!(errs.len(), 1);
    assert!(matches!(
        &errs[0],
        ExpressionError {
            ctx: ExprCtx { span: TxtSpan { start: 0, end: 8 }, txt: line },
            kind: ExpressionErrorKind::DefineInvalid,
        } if Rc::ptr_eq(&txt, &line)
    ));
}

#[test]
fn into_define_too_many_args() {
    // (define foo "bar" "baz")
    let txt = make_textline().into();
    let env = TestEnv::default();
    let p = ExprNode {
        ctx: ExprCtx {
            span: 0..24,
            txt: Rc::clone(&txt),
        },
        mode: ParseMode::List {
            form: SyntacticForm::Define,
            seq: vec![
                Expression::symbol(
                    env.symbols.get("foo"),
                    ExprCtx {
                        span: 8..11,
                        txt: Rc::clone(&txt),
                    },
                ),
                Expression::string(
                    "bar",
                    ExprCtx {
                        span: 12..17,
                        txt: Rc::clone(&txt),
                    },
                ),
                Expression::string(
                    "baz",
                    ExprCtx {
                        span: 18..23,
                        txt: Rc::clone(&txt),
                    },
                ),
            ],
        },
    };
    let ns = env.new_namespace();

    let r = p.try_into_expr(&ns);

    let errs = err_or_fail!(r);
    assert_eq!(errs.len(), 1);
    assert!(matches!(
        &errs[0],
        ExpressionError {
            ctx: ExprCtx { span: TxtSpan { start: 0, end: 24 }, txt: line },
            kind: ExpressionErrorKind::DefineInvalid,
        } if Rc::ptr_eq(&txt, &line)
    ));
}

#[test]
fn into_define_lambda_no_body() {
    // (define (foo))
    let txt = make_textline().into();
    let env = TestEnv::default();
    let p = ExprNode {
        ctx: ExprCtx {
            span: 0..14,
            txt: Rc::clone(&txt),
        },
        mode: ParseMode::List {
            form: SyntacticForm::Define,
            seq: vec![
                ExprCtx {
                    span: 8..13,
                    txt: Rc::clone(&txt),
                }
                .into_expr(ExpressionKind::Literal(zlist![Value::Symbol(
                    env.symbols.get("foo")
                )])),
            ],
        },
    };
    let ns = env.new_namespace();

    let r = p.try_into_expr(&ns);

    let errs = err_or_fail!(r);
    assert_eq!(errs.len(), 1);
    assert!(matches!(
        &errs[0],
        ExpressionError {
            ctx: ExprCtx { span: TxtSpan { start: 0, end: 14 }, txt: line },
            kind: ExpressionErrorKind::DefineInvalid,
        } if Rc::ptr_eq(&txt, &line)
    ));
}

#[test]
fn into_define_lambda_invalid_name() {
    // (define ("foo" x) x)
    let txt = make_textline().into();
    let env = TestEnv::default();
    let p = ExprNode {
        ctx: ExprCtx {
            span: 0..20,
            txt: Rc::clone(&txt),
        },
        mode: ParseMode::List {
            form: SyntacticForm::Define,
            seq: vec![
                ExprCtx {
                    span: 8..17,
                    txt: Rc::clone(&txt),
                }
                .into_expr(ExpressionKind::Literal(zlist![
                    Value::string("foo"),
                    Value::Symbol(env.symbols.get("x")),
                ])),
                Expression::variable(
                    env.symbols.get("x"),
                    ExprCtx {
                        span: 18..19,
                        txt: Rc::clone(&txt),
                    },
                ),
            ],
        },
    };
    let ns = env.new_namespace();

    let r = p.try_into_expr(&ns);

    let errs = err_or_fail!(r);
    assert_eq!(errs.len(), 1);
    assert!(matches!(
        &errs[0],
        ExpressionError {
            ctx: ExprCtx { span: TxtSpan { start: 0, end: 14 }, txt: line },
            kind: ExpressionErrorKind::DefineInvalid,
        } if Rc::ptr_eq(&txt, &line)
    ));
}

#[test]
fn into_define_lambda_invalid_formals() {
    // (define (foo "x") 'a)
    let txt = make_textline().into();
    let env = TestEnv::default();
    let p = ExprNode {
        ctx: ExprCtx {
            span: 0..21,
            txt: Rc::clone(&txt),
        },
        mode: ParseMode::List {
            form: SyntacticForm::Define,
            seq: vec![
                ExprCtx {
                    span: 8..17,
                    txt: Rc::clone(&txt),
                }
                .into_expr(ExpressionKind::Literal(zlist![
                    Value::Symbol(env.symbols.get("foo")),
                    Value::string("x"),
                ])),
                Expression::variable(
                    env.symbols.get("a"),
                    ExprCtx {
                        span: 18..20,
                        txt: Rc::clone(&txt),
                    },
                ),
            ],
        },
    };
    let ns = env.new_namespace();

    let r = p.try_into_expr(&ns);

    let errs = err_or_fail!(r);
    assert_eq!(errs.len(), 1);
    assert!(matches!(
        &errs[0],
        ExpressionError {
            ctx: ExprCtx { span: TxtSpan { start: 0, end: 14 }, txt: line },
            kind: ExpressionErrorKind::DefineInvalid,
        } if Rc::ptr_eq(&txt, &line)
    ));
}

#[test]
fn into_define_lambda_empty_formals() {
    // (define () 'a)
    let txt = make_textline().into();
    let env = TestEnv::default();
    let p = ExprNode {
        ctx: ExprCtx {
            span: 0..14,
            txt: Rc::clone(&txt),
        },
        mode: ParseMode::List {
            form: SyntacticForm::Define,
            seq: vec![
                ExprCtx {
                    span: 8..10,
                    txt: Rc::clone(&txt),
                }
                .into_expr(ExpressionKind::Literal(zlist![])),
                Expression::variable(
                    env.symbols.get("a"),
                    ExprCtx {
                        span: 11..13,
                        txt: Rc::clone(&txt),
                    },
                ),
            ],
        },
    };
    let ns = env.new_namespace();

    let r = p.try_into_expr(&ns);

    let errs = err_or_fail!(r);
    assert_eq!(errs.len(), 1);
    assert!(matches!(
        &errs[0],
        ExpressionError {
            ctx: ExprCtx { span: TxtSpan { start: 0, end: 17 }, txt: line },
            kind: ExpressionErrorKind::DefineInvalid,
        } if Rc::ptr_eq(&txt, &line)
    ));
}

#[test]
fn into_define_lambda_empty_body() {
    // (define (foo) ())
    let txt = make_textline().into();
    let env = TestEnv::default();
    let p = ExprNode {
        ctx: ExprCtx {
            span: 0..17,
            txt: Rc::clone(&txt),
        },
        mode: ParseMode::List {
            form: SyntacticForm::Define,
            seq: vec![
                ExprCtx {
                    span: 8..13,
                    txt: Rc::clone(&txt),
                }
                .into_expr(ExpressionKind::Literal(zlist![Value::Symbol(
                    env.symbols.get("foo")
                )])),
            ],
        },
    };
    let ns = env.new_namespace();

    let r = p.try_into_expr(&ns);

    let errs = err_or_fail!(r);
    assert_eq!(errs.len(), 1);
    assert!(matches!(
        &errs[0],
        ExpressionError {
            ctx: ExprCtx { span: TxtSpan { start: 0, end: 17 }, txt: line },
            kind: ExpressionErrorKind::DefineInvalid,
        } if Rc::ptr_eq(&txt, &line)
    ));
    todo!("this test is incomplete, it may need to be a syntax test");
}

#[test]
fn into_set_variable() {
    // (set! foo "bar")
    let txt = make_textline().into();
    let env = TestEnv::default();
    let p = ExprNode {
        ctx: ExprCtx {
            span: 0..18,
            txt: Rc::clone(&txt),
        },
        mode: ParseMode::List {
            form: SyntacticForm::Set,
            seq: vec![
                Expression::variable(
                    env.symbols.get("foo"),
                    ExprCtx {
                        span: 8..11,
                        txt: Rc::clone(&txt),
                    },
                ),
                Expression::string(
                    "bar",
                    ExprCtx {
                        span: 12..17,
                        txt: Rc::clone(&txt),
                    },
                ),
            ],
        },
    };
    let ns = env.new_namespace();

    let r = p.try_into_expr(&ns);

    let expr = some_or_fail!(ok_or_fail!(r));
    assert!(matches!(
        expr,
        Expression {
            ctx: ExprCtx { span: TxtSpan { start: 0, end: 18 }, txt: line },
            kind: ExpressionKind::Set { .. },
        } if Rc::ptr_eq(&txt, &line)
    ));
    let ExpressionKind::Set { var, expr } = expr.kind else {
        unreachable!();
    };
    assert_eq!(var.as_ref(), "foo");
    let val = extract_or_fail!(expr.kind, ExpressionKind::Literal);
    assert_eq!(val.to_string(), "\"bar\"");
}

#[test]
fn into_set_not_variable_expr() {
    // (set! (myproc) "bar")
    let txt = make_textline().into();
    let env = TestEnv::default();
    let p = ExprNode {
        ctx: ExprCtx {
            span: 0..23,
            txt: Rc::clone(&txt),
        },
        mode: ParseMode::List {
            form: SyntacticForm::Set,
            seq: vec![
                ExprCtx {
                    span: 8..16,
                    txt: Rc::clone(&txt),
                }
                .into_expr(ExpressionKind::Call {
                    proc: Expression::variable(
                        env.symbols.get("myproc"),
                        ExprCtx {
                            span: 9..15,
                            txt: Rc::clone(&txt),
                        },
                    )
                    .into(),
                    args: [].into(),
                }),
                Expression::string(
                    "bar",
                    ExprCtx {
                        span: 17..22,
                        txt: Rc::clone(&txt),
                    },
                ),
            ],
        },
    };
    let ns = env.new_namespace();

    let r = p.try_into_expr(&ns);

    let errs = err_or_fail!(r);
    assert_eq!(errs.len(), 1);
    assert!(matches!(
        &errs[0],
        ExpressionError {
            ctx: ExprCtx { span: TxtSpan { start: 0, end: 23 }, txt: line },
            kind: ExpressionErrorKind::SetInvalid,
        } if Rc::ptr_eq(&txt, &line)
    ));
}

#[test]
fn into_set_too_few_args() {
    // (set! foo)
    let txt = make_textline().into();
    let env = TestEnv::default();
    let p = ExprNode {
        ctx: ExprCtx {
            span: 0..25,
            txt: Rc::clone(&txt),
        },
        mode: ParseMode::List {
            form: SyntacticForm::Set,
            seq: vec![Expression::variable(
                env.symbols.get("foo"),
                ExprCtx {
                    span: 8..11,
                    txt: Rc::clone(&txt),
                },
            )],
        },
    };
    let ns = env.new_namespace();

    let r = p.try_into_expr(&ns);

    let errs = err_or_fail!(r);
    assert_eq!(errs.len(), 1);
    assert!(matches!(
        &errs[0],
        ExpressionError {
            ctx: ExprCtx { span: TxtSpan { start: 0, end: 25 }, txt: line },
            kind: ExpressionErrorKind::SetInvalid,
        } if Rc::ptr_eq(&txt, &line)
    ));
}

#[test]
fn into_set_too_many_args() {
    // (set! foo "bar" "baz")
    let txt = make_textline().into();
    let env = TestEnv::default();
    let p = ExprNode {
        ctx: ExprCtx {
            span: 0..25,
            txt: Rc::clone(&txt),
        },
        mode: ParseMode::List {
            form: SyntacticForm::Set,
            seq: vec![
                Expression::variable(
                    env.symbols.get("foo"),
                    ExprCtx {
                        span: 8..11,
                        txt: Rc::clone(&txt),
                    },
                ),
                Expression::string(
                    "bar",
                    ExprCtx {
                        span: 12..17,
                        txt: Rc::clone(&txt),
                    },
                ),
                Expression::string(
                    "baz",
                    ExprCtx {
                        span: 18..23,
                        txt: Rc::clone(&txt),
                    },
                ),
            ],
        },
    };
    let ns = env.new_namespace();

    let r = p.try_into_expr(&ns);

    let errs = err_or_fail!(r);
    assert_eq!(errs.len(), 1);
    assert!(matches!(
        &errs[0],
        ExpressionError {
            ctx: ExprCtx { span: TxtSpan { start: 0, end: 25 }, txt: line },
            kind: ExpressionErrorKind::SetInvalid,
        } if Rc::ptr_eq(&txt, &line)
    ));
}

#[test]
fn into_if_consequent() {
    // (if #t "bar")
    let txt = make_textline().into();
    let p = ExprNode {
        ctx: ExprCtx {
            span: 0..13,
            txt: Rc::clone(&txt),
        },
        mode: ParseMode::List {
            form: SyntacticForm::If,
            seq: vec![
                ExprCtx {
                    span: 4..6,
                    txt: Rc::clone(&txt),
                }
                .into_expr(ExpressionKind::Literal(Value::Boolean(true))),
                Expression::string(
                    "bar",
                    ExprCtx {
                        span: 7..12,
                        txt: Rc::clone(&txt),
                    },
                ),
            ],
        },
    };
    let env = TestEnv::default();
    let ns = env.new_namespace();

    let r = p.try_into_expr(&ns);

    let expr = some_or_fail!(ok_or_fail!(r));
    assert!(matches!(
        expr,
        Expression {
            ctx: ExprCtx { span: TxtSpan { start: 0, end: 13 }, txt: line },
            kind: ExpressionKind::If { .. },
        } if Rc::ptr_eq(&txt, &line)
    ));
    let ExpressionKind::If { test, con, alt } = expr.kind else {
        unreachable!();
    };
    assert_eq!(
        extract_or_fail!(test.kind, ExpressionKind::Literal).to_string(),
        "#t"
    );
    assert_eq!(
        extract_or_fail!(con.kind, ExpressionKind::Literal).to_string(),
        "\"bar\""
    );
    assert!(alt.is_none());
}

#[test]
fn into_if_consequent_alternate() {
    // (if #t "bar" "foo")
    let txt = make_textline().into();
    let env = TestEnv::default();
    let p = ExprNode {
        ctx: ExprCtx {
            span: 0..17,
            txt: Rc::clone(&txt),
        },
        mode: ParseMode::List {
            form: SyntacticForm::If,
            seq: vec![
                ExprCtx {
                    span: 4..6,
                    txt: Rc::clone(&txt),
                }
                .into_expr(ExpressionKind::Literal(Value::Boolean(true))),
                Expression::string(
                    "bar",
                    ExprCtx {
                        span: 7..12,
                        txt: Rc::clone(&txt),
                    },
                ),
                Expression::symbol(
                    env.symbols.get("foo"),
                    ExprCtx {
                        span: 13..16,
                        txt: Rc::clone(&txt),
                    },
                ),
            ],
        },
    };
    let ns = env.new_namespace();

    let r = p.try_into_expr(&ns);

    let expr = some_or_fail!(ok_or_fail!(r));
    assert!(matches!(
        expr,
        Expression {
            ctx: ExprCtx { span: TxtSpan { start: 0, end: 17 }, txt: line },
            kind: ExpressionKind::If { .. },
        } if Rc::ptr_eq(&txt, &line)
    ));
    let ExpressionKind::If { test, con, alt } = expr.kind else {
        unreachable!();
    };
    assert_eq!(
        extract_or_fail!(test.kind, ExpressionKind::Literal).to_string(),
        "#t"
    );
    assert_eq!(
        extract_or_fail!(con.kind, ExpressionKind::Literal).to_string(),
        "\"bar\""
    );
    let alt = some_or_fail!(alt);
    assert_eq!(
        extract_or_fail!(alt.kind, ExpressionKind::Literal).to_string(),
        "foo"
    );
}

#[test]
fn into_if_too_few_args() {
    // (if #t)
    let txt = make_textline().into();
    let p = ExprNode {
        ctx: ExprCtx {
            span: 0..7,
            txt: Rc::clone(&txt),
        },
        mode: ParseMode::List {
            form: SyntacticForm::If,
            seq: vec![
                ExprCtx {
                    span: 4..6,
                    txt: Rc::clone(&txt),
                }
                .into_expr(ExpressionKind::Literal(Value::Boolean(true))),
            ],
        },
    };
    let env = TestEnv::default();
    let ns = env.new_namespace();

    let r = p.try_into_expr(&ns);

    let errs = err_or_fail!(r);
    assert_eq!(errs.len(), 1);
    assert!(matches!(
        &errs[0],
        ExpressionError {
            ctx: ExprCtx { span: TxtSpan { start: 0, end: 7 }, txt: line },
            kind: ExpressionErrorKind::IfInvalid,
        } if Rc::ptr_eq(&txt, &line)
    ));
}

#[test]
fn into_if_too_many_args() {
    // (if #t "bar" "foo" "beef")
    let txt = make_textline().into();
    let env = TestEnv::default();
    let p = ExprNode {
        ctx: ExprCtx {
            span: 0..22,
            txt: Rc::clone(&txt),
        },
        mode: ParseMode::List {
            form: SyntacticForm::If,
            seq: vec![
                ExprCtx {
                    span: 4..6,
                    txt: Rc::clone(&txt),
                }
                .into_expr(ExpressionKind::Literal(Value::Boolean(true))),
                Expression::string(
                    "bar",
                    ExprCtx {
                        span: 7..12,
                        txt: Rc::clone(&txt),
                    },
                ),
                Expression::symbol(
                    env.symbols.get("foo"),
                    ExprCtx {
                        span: 13..16,
                        txt: Rc::clone(&txt),
                    },
                ),
                Expression::symbol(
                    env.symbols.get("beef"),
                    ExprCtx {
                        span: 17..21,
                        txt: Rc::clone(&txt),
                    },
                ),
            ],
        },
    };
    let ns = env.new_namespace();

    let r = p.try_into_expr(&ns);

    let errs = err_or_fail!(r);
    assert_eq!(errs.len(), 1);
    assert!(matches!(
        &errs[0],
        ExpressionError {
            ctx: ExprCtx { span: TxtSpan { start: 0, end: 22 }, txt: line },
            kind: ExpressionErrorKind::IfInvalid,
        } if Rc::ptr_eq(&txt, &line)
    ));
}

#[test]
fn into_lambda_fixed_arguments() {
    // (lambda (x y) (+ x y))
    let txt = make_textline().into();
    let env = TestEnv::default();
    let p = ExprNode {
        ctx: ExprCtx {
            span: 0..22,
            txt: Rc::clone(&txt),
        },
        mode: ParseMode::List {
            form: SyntacticForm::Lambda,
            seq: vec![
                ExprCtx {
                    span: 8..13,
                    txt: Rc::clone(&txt),
                }
                .into_expr(ExpressionKind::Literal(zlist![
                    Value::Symbol(env.symbols.get("x")),
                    Value::Symbol(env.symbols.get("y"))
                ])),
                ExprCtx {
                    span: 14..21,
                    txt: Rc::clone(&txt),
                }
                .into_expr(ExpressionKind::Call {
                    proc: Expression::variable(
                        env.symbols.get("+"),
                        ExprCtx {
                            span: 15..16,
                            txt: Rc::clone(&txt),
                        },
                    )
                    .into(),
                    args: [
                        Expression::variable(
                            env.symbols.get("x"),
                            ExprCtx {
                                span: 17..18,
                                txt: Rc::clone(&txt),
                            },
                        ),
                        Expression::variable(
                            env.symbols.get("y"),
                            ExprCtx {
                                span: 19..20,
                                txt: Rc::clone(&txt),
                            },
                        ),
                    ]
                    .into(),
                }),
            ],
        },
    };
    let ns = env.new_namespace();

    let r = p.try_into_expr(&ns);

    let expr = some_or_fail!(ok_or_fail!(r));
    assert!(matches!(
        expr,
        Expression {
            ctx: ExprCtx { span: TxtSpan { start: 0, end: 22 }, txt: line },
            kind: ExpressionKind::Lambda(_),
        } if Rc::ptr_eq(&txt, &line)
    ));
    let ExpressionKind::Lambda(lm) = expr.kind else {
        unreachable!();
    };
    assert_eq!(lm.to_string(), " (x y)");
}

#[test]
fn into_lambda_simple_body() {
    // (lambda (x) x)
    let txt = make_textline().into();
    let env = TestEnv::default();
    let p = ExprNode {
        ctx: ExprCtx {
            span: 0..14,
            txt: Rc::clone(&txt),
        },
        mode: ParseMode::List {
            form: SyntacticForm::Lambda,
            seq: vec![
                ExprCtx {
                    span: 8..11,
                    txt: Rc::clone(&txt),
                }
                .into_expr(ExpressionKind::Literal(zlist![Value::Symbol(
                    env.symbols.get("x")
                )])),
                Expression::variable(
                    env.symbols.get("x"),
                    ExprCtx {
                        span: 12..13,
                        txt: Rc::clone(&txt),
                    },
                ),
            ],
        },
    };
    let ns = env.new_namespace();

    let r = p.try_into_expr(&ns);

    let expr = some_or_fail!(ok_or_fail!(r));
    assert!(matches!(
        expr,
        Expression {
            ctx: ExprCtx { span: TxtSpan { start: 0, end: 14 }, txt: line },
            kind: ExpressionKind::Lambda(_),
        } if Rc::ptr_eq(&txt, &line)
    ));
    let ExpressionKind::Lambda(lm) = expr.kind else {
        unreachable!();
    };
    assert_eq!(lm.to_string(), " (x)");
}

#[test]
fn into_lambda_no_arguments() {
    // (lambda () 'a)
    let txt = make_textline().into();
    let env = TestEnv::default();
    let p = ExprNode {
        ctx: ExprCtx {
            span: 0..14,
            txt: Rc::clone(&txt),
        },
        mode: ParseMode::List {
            form: SyntacticForm::Lambda,
            seq: vec![
                ExprCtx {
                    span: 8..10,
                    txt: Rc::clone(&txt),
                }
                .into_expr(ExpressionKind::Literal(zlist![])),
                Expression::symbol(
                    env.symbols.get("a"),
                    ExprCtx {
                        span: 11..13,
                        txt: Rc::clone(&txt),
                    },
                ),
            ],
        },
    };
    let ns = env.new_namespace();

    let r = p.try_into_expr(&ns);

    let expr = some_or_fail!(ok_or_fail!(r));
    assert!(matches!(
        expr,
        Expression {
            ctx: ExprCtx { span: TxtSpan { start: 0, end: 14 }, txt: line },
            kind: ExpressionKind::Lambda(_),
        } if Rc::ptr_eq(&txt, &line)
    ));
    let ExpressionKind::Lambda(lm) = expr.kind else {
        unreachable!();
    };
    assert_eq!(lm.to_string(), "");
}

#[test]
fn into_lambda_variadic() {
    // (lambda x x)
    let txt = make_textline().into();
    let env = TestEnv::default();
    let p = ExprNode {
        ctx: ExprCtx {
            span: 0..12,
            txt: Rc::clone(&txt),
        },
        mode: ParseMode::List {
            form: SyntacticForm::Lambda,
            seq: vec![
                Expression::symbol(
                    env.symbols.get("x"),
                    ExprCtx {
                        span: 8..9,
                        txt: Rc::clone(&txt),
                    },
                ),
                Expression::variable(
                    env.symbols.get("x"),
                    ExprCtx {
                        span: 10..11,
                        txt: Rc::clone(&txt),
                    },
                ),
            ],
        },
    };
    let ns = env.new_namespace();

    let r = p.try_into_expr(&ns);

    let expr = some_or_fail!(ok_or_fail!(r));
    assert!(matches!(
        expr,
        Expression {
            ctx: ExprCtx { span: TxtSpan { start: 0, end: 12 }, txt: line },
            kind: ExpressionKind::Lambda(_),
        } if Rc::ptr_eq(&txt, &line)
    ));
    let ExpressionKind::Lambda(lm) = expr.kind else {
        unreachable!();
    };
    assert_eq!(lm.to_string(), " x…");
}

#[test]
fn into_lambda_rest() {
    // (lambda (x y . z) z)
    let txt = make_textline().into();
    let env = TestEnv::default();
    let p = ExprNode {
        ctx: ExprCtx {
            span: 0..20,
            txt: Rc::clone(&txt),
        },
        mode: ParseMode::List {
            form: SyntacticForm::Lambda,
            seq: vec![
                ExprCtx {
                    span: 8..17,
                    txt: Rc::clone(&txt),
                }
                .into_expr(ExpressionKind::Literal(Value::cons(
                    Value::Symbol(env.symbols.get("x")),
                    Value::cons(
                        Value::Symbol(env.symbols.get("y")),
                        Value::Symbol(env.symbols.get("z")),
                    ),
                ))),
                Expression::variable(
                    env.symbols.get("z"),
                    ExprCtx {
                        span: 18..19,
                        txt: Rc::clone(&txt),
                    },
                ),
            ],
        },
    };
    let ns = env.new_namespace();

    let r = p.try_into_expr(&ns);

    let expr = some_or_fail!(ok_or_fail!(r));
    assert!(matches!(
        expr,
        Expression {
            ctx: ExprCtx { span: TxtSpan { start: 0, end: 20 }, txt: line },
            kind: ExpressionKind::Lambda(_),
        } if Rc::ptr_eq(&txt, &line)
    ));
    let ExpressionKind::Lambda(lm) = expr.kind else {
        unreachable!();
    };
    assert_eq!(lm.to_string(), " (x y z…)");
}

#[test]
fn into_lambda_multiple_expression_body() {
    // (lambda x x 'b)
    let txt = make_textline().into();
    let env = TestEnv::default();
    let p = ExprNode {
        ctx: ExprCtx {
            span: 0..15,
            txt: Rc::clone(&txt),
        },
        mode: ParseMode::List {
            form: SyntacticForm::Lambda,
            seq: vec![
                Expression::symbol(
                    env.symbols.get("x"),
                    ExprCtx {
                        span: 8..9,
                        txt: Rc::clone(&txt),
                    },
                ),
                Expression::variable(
                    env.symbols.get("x"),
                    ExprCtx {
                        span: 10..11,
                        txt: Rc::clone(&txt),
                    },
                ),
                Expression::symbol(
                    env.symbols.get("b"),
                    ExprCtx {
                        span: 12..14,
                        txt: Rc::clone(&txt),
                    },
                ),
            ],
        },
    };
    let ns = env.new_namespace();

    let r = p.try_into_expr(&ns);

    let expr = some_or_fail!(ok_or_fail!(r));
    assert!(matches!(
        expr,
        Expression {
            ctx: ExprCtx { span: TxtSpan { start: 0, end: 15 }, txt: line },
            kind: ExpressionKind::Lambda(_),
        } if Rc::ptr_eq(&txt, &line)
    ));
    let ExpressionKind::Lambda(lm) = expr.kind else {
        unreachable!();
    };
    assert_eq!(lm.to_string(), " x…");
}

#[test]
fn into_lambda_not_identifier_expr() {
    // (lambda (1) 'a)
    let txt = make_textline().into();
    let env = TestEnv::default();
    let p = ExprNode {
        ctx: ExprCtx {
            span: 0..15,
            txt: Rc::clone(&txt),
        },
        mode: ParseMode::List {
            form: SyntacticForm::Lambda,
            seq: vec![
                ExprCtx {
                    span: 8..10,
                    txt: Rc::clone(&txt),
                }
                .into_expr(ExpressionKind::Literal(zlist![Value::Number(
                    Number::real(1)
                )])),
                Expression::symbol(
                    env.symbols.get("a"),
                    ExprCtx {
                        span: 12..14,
                        txt: Rc::clone(&txt),
                    },
                ),
            ],
        },
    };
    let ns = env.new_namespace();

    let r = p.try_into_expr(&ns);

    let errs = err_or_fail!(r);
    assert_eq!(errs.len(), 1);
    assert!(matches!(
        &errs[0],
        ExpressionError {
            ctx: ExprCtx { span: TxtSpan { start: 8, end: 10 }, txt: line },
            kind: ExpressionErrorKind::LambdaInvalidSignature,
        } if Rc::ptr_eq(&txt, &line)
    ));
}

#[test]
fn into_lambda_too_few_args() {
    // (lambda x)
    let txt = make_textline().into();
    let env = TestEnv::default();
    let p = ExprNode {
        ctx: ExprCtx {
            span: 0..10,
            txt: Rc::clone(&txt),
        },
        mode: ParseMode::List {
            form: SyntacticForm::Lambda,
            seq: vec![Expression::symbol(
                env.symbols.get("x"),
                ExprCtx {
                    span: 8..9,
                    txt: Rc::clone(&txt),
                },
            )],
        },
    };
    let ns = env.new_namespace();

    let r = p.try_into_expr(&ns);

    let errs = err_or_fail!(r);
    assert_eq!(errs.len(), 1);
    assert!(matches!(
        &errs[0],
        ExpressionError {
            ctx: ExprCtx { span: TxtSpan { start: 0, end: 10 }, txt: line },
            kind: ExpressionErrorKind::LambdaInvalid,
        } if Rc::ptr_eq(&txt, &line)
    ));
}

#[test]
fn into_lambda_duplicate_args() {
    // (lambda (x y x) (+ x y))
    let txt = make_textline().into();
    let env = TestEnv::default();
    let p = ExprNode {
        ctx: ExprCtx {
            span: 0..24,
            txt: Rc::clone(&txt),
        },
        mode: ParseMode::List {
            form: SyntacticForm::Lambda,
            seq: vec![
                ExprCtx {
                    span: 8..15,
                    txt: Rc::clone(&txt),
                }
                .into_expr(ExpressionKind::Literal(zlist![
                    Value::Symbol(env.symbols.get("x")),
                    Value::Symbol(env.symbols.get("y")),
                    Value::Symbol(env.symbols.get("x")),
                ])),
                ExprCtx {
                    span: 16..23,
                    txt: Rc::clone(&txt),
                }
                .into_expr(ExpressionKind::Call {
                    proc: Expression::variable(
                        env.symbols.get("+"),
                        ExprCtx {
                            span: 17..18,
                            txt: Rc::clone(&txt),
                        },
                    )
                    .into(),
                    args: [
                        Expression::variable(
                            env.symbols.get("x"),
                            ExprCtx {
                                span: 19..20,
                                txt: Rc::clone(&txt),
                            },
                        ),
                        Expression::variable(
                            env.symbols.get("y"),
                            ExprCtx {
                                span: 21..22,
                                txt: Rc::clone(&txt),
                            },
                        ),
                    ]
                    .into(),
                }),
            ],
        },
    };
    let ns = env.new_namespace();

    let r = p.try_into_expr(&ns);

    let errs = err_or_fail!(r);
    assert_eq!(errs.len(), 1);
    assert!(matches!(
        &errs[0],
        ExpressionError {
            ctx: ExprCtx { span: TxtSpan { start: 8, end: 15 }, txt: line },
            kind: ExpressionErrorKind::LambdaInvalidFormal(InvalidFormal::DuplicateFormal(n)),
        } if Rc::ptr_eq(&txt, &line) && n.as_ref() == "x"
    ));
}

#[test]
fn into_lambda_too_many_formals() {
    // (lambda (257 arguments...) 'a)
    let txt = make_textline().into();
    let env = TestEnv::default();
    let args = (0..256)
        .into_iter()
        .map(|i| Value::Symbol(env.symbols.get(format!("a{i}"))))
        .collect::<Vec<_>>();
    let p = ExprNode {
        ctx: ExprCtx {
            span: 0..10,
            txt: Rc::clone(&txt),
        },
        mode: ParseMode::List {
            form: SyntacticForm::Lambda,
            seq: vec![
                ExprCtx {
                    span: 8..15,
                    txt: Rc::clone(&txt),
                }
                .into_expr(ExpressionKind::Literal(Value::list(args))),
                Expression::symbol(
                    env.symbols.get("a"),
                    ExprCtx {
                        span: 12..14,
                        txt: Rc::clone(&txt),
                    },
                ),
            ],
        },
    };
    let ns = env.new_namespace();

    let r = p.try_into_expr(&ns);

    let errs = err_or_fail!(r);
    assert_eq!(errs.len(), 1);
    assert!(matches!(
        &errs[0],
        ExpressionError {
            ctx: ExprCtx { span: TxtSpan { start: 8, end: 15 }, txt: line },
            kind: ExpressionErrorKind::LambdaInvalidFormal(InvalidFormal::MaxFormals),
        } if Rc::ptr_eq(&txt, &line)
    ));
}

#[test]
fn into_lambda_too_many_formals_with_rest() {
    // (lambda (256 arguments... . rest) 'a)
    let txt = make_textline().into();
    let env = TestEnv::default();
    let args = (0..256)
        .into_iter()
        .map(|i| Value::Symbol(env.symbols.get(format!("a{i}"))))
        .collect::<Vec<_>>();
    let p = ExprNode {
        ctx: ExprCtx {
            span: 0..10,
            txt: Rc::clone(&txt),
        },
        mode: ParseMode::List {
            form: SyntacticForm::Lambda,
            seq: vec![
                ExprCtx {
                    span: 8..15,
                    txt: Rc::clone(&txt),
                }
                .into_expr(ExpressionKind::Literal(Value::improper_list(args))),
                Expression::symbol(
                    env.symbols.get("a"),
                    ExprCtx {
                        span: 12..14,
                        txt: Rc::clone(&txt),
                    },
                ),
            ],
        },
    };
    let ns = env.new_namespace();

    let r = p.try_into_expr(&ns);

    let errs = err_or_fail!(r);
    assert_eq!(errs.len(), 1);
    assert!(matches!(
        &errs[0],
        ExpressionError {
            ctx: ExprCtx { span: TxtSpan { start: 8, end: 15 }, txt: line },
            kind: ExpressionErrorKind::LambdaInvalidFormal(InvalidFormal::MaxFormals),
        } if Rc::ptr_eq(&txt, &line)
    ));
}

mod merge {
    use super::*;

    #[test]
    fn list_merge() {
        let txt = make_textline().into();
        let env = TestEnv::default();
        let mut p = ExprNode {
            ctx: ExprCtx {
                span: 0..3,
                txt: Rc::clone(&txt),
            },
            mode: ParseMode::List {
                form: SyntacticForm::Call,
                seq: vec![Expression::variable(
                    env.symbols.get("+"),
                    ExprCtx {
                        span: 0..3,
                        txt: Rc::clone(&txt),
                    },
                )],
            },
        };
        let other = ExprNode {
            ctx: ExprCtx {
                span: 3..6,
                txt: Rc::clone(&txt),
            },
            mode: ParseMode::StringLiteral("foo".to_owned()),
        };
        let ns = env.new_namespace();

        let r = p.merge(other, &ns);

        assert!(matches!(r, Ok(MergeFlow::Continue(()))));
        assert!(matches!(
            p.mode,
            ParseMode::List {
                form: SyntacticForm::Call,
                ..
            }
        ));
        let ParseMode::List { seq, .. } = p.mode else {
            unreachable!();
        };
        assert_eq!(seq.len(), 2);
        assert!(matches!(
            &seq[1],
            Expression {
                ctx: ExprCtx {
                    span: TxtSpan { start: 3, end: 6 },
                    txt: line
                },
                kind: ExpressionKind::Literal(Value::String(s)),
            } if s.as_ref() == "foo" && Rc::ptr_eq(&txt, &line)
        ));
    }

    #[test]
    fn pair_merge() {
        let txt = make_textline().into();
        let env = TestEnv::default();
        let mut p = ExprNode {
            ctx: ExprCtx {
                span: 0..3,
                txt: Rc::clone(&txt),
            },
            mode: ParseMode::List {
                form: SyntacticForm::PairOpen,
                seq: vec![
                    ExprCtx {
                        span: 0..2,
                        txt: Rc::clone(&txt),
                    }
                    .into_expr(ExpressionKind::Literal(Value::Symbol(env.symbols.get("a")))),
                ],
            },
        };
        let other = ExprNode {
            ctx: ExprCtx {
                span: 3..6,
                txt: Rc::clone(&txt),
            },
            mode: ParseMode::Identifier {
                name: "foo".to_owned(),
                quoted: true,
            },
        };
        let ns = env.new_namespace();

        let r = p.merge(other, &ns);

        assert!(matches!(r, Ok(MergeFlow::Continue(()))));
        assert!(matches!(
            p.mode,
            ParseMode::List {
                form: SyntacticForm::PairClosed,
                ..
            }
        ));
        let ParseMode::List { seq, .. } = p.mode else {
            unreachable!();
        };
        assert_eq!(seq.len(), 2);
        assert!(matches!(
            &seq[1],
            Expression {
                ctx: ExprCtx {
                    span: TxtSpan { start: 3, end: 6 },
                    txt: line
                },
                kind: ExpressionKind::Literal(Value::Symbol(s)),
            } if s.as_ref() == "foo" && Rc::ptr_eq(&txt, &line)
        ));
    }

    #[test]
    fn pair_merge_does_nothing_if_no_expr() {
        let txt = make_textline().into();
        let env = TestEnv::default();
        let mut p = ExprNode {
            ctx: ExprCtx {
                span: 0..3,
                txt: Rc::clone(&txt),
            },
            mode: ParseMode::List {
                form: SyntacticForm::PairOpen,
                seq: vec![
                    ExprCtx {
                        span: 0..2,
                        txt: Rc::clone(&txt),
                    }
                    .into_expr(ExpressionKind::Literal(Value::Symbol(env.symbols.get("a")))),
                ],
            },
        };
        let other = ExprNode {
            ctx: ExprCtx {
                span: 3..6,
                txt: Rc::clone(&txt),
            },
            mode: ParseMode::CommentDatum(Some(
                ExprCtx {
                    span: 2..4,
                    txt: Rc::clone(&txt),
                }
                .into_expr(ExpressionKind::Literal(Value::Symbol(env.symbols.get("b")))),
            )),
        };
        let ns = env.new_namespace();

        let r = p.merge(other, &ns);

        assert!(matches!(r, Ok(MergeFlow::Continue(()))));
        assert!(matches!(
            p.mode,
            ParseMode::List {
                form: SyntacticForm::PairOpen,
                ..
            }
        ));
        let ParseMode::List { seq, .. } = p.mode else {
            unreachable!();
        };
        assert_eq!(seq.len(), 1);
    }

    #[test]
    fn pair_invalid_merge() {
        let txt = make_textline().into();
        let env = TestEnv::default();
        let mut p = ExprNode {
            ctx: ExprCtx {
                span: 0..3,
                txt: Rc::clone(&txt),
            },
            mode: ParseMode::List {
                form: SyntacticForm::PairClosed,
                seq: vec![
                    ExprCtx {
                        span: 0..2,
                        txt: Rc::clone(&txt),
                    }
                    .into_expr(ExpressionKind::Literal(Value::Symbol(env.symbols.get("a")))),
                    ExprCtx {
                        span: 2..3,
                        txt: Rc::clone(&txt),
                    }
                    .into_expr(ExpressionKind::Literal(Value::Symbol(env.symbols.get("b")))),
                ],
            },
        };
        let other = ExprNode {
            ctx: ExprCtx {
                span: 3..6,
                txt: Rc::clone(&txt),
            },
            mode: ParseMode::Identifier {
                name: "foo".to_owned(),
                quoted: true,
            },
        };
        let ns = env.new_namespace();

        let r = p.merge(other, &ns);

        let errs = extract_or_fail!(err_or_fail!(r), ParserError::Syntax).0;
        assert_eq!(errs.len(), 1);
        assert!(matches!(
            &errs[0],
            ExpressionError {
                ctx: ExprCtx {
                    span: TxtSpan { start: 0, end: 3 },
                    txt: line
                },
                kind: ExpressionErrorKind::PairUnterminated,
            } if Rc::ptr_eq(&txt, &line)
        ));
    }

    #[test]
    fn list_does_not_allow_define_merge() {
        let txt = make_textline().into();
        let env = TestEnv::default();
        let mut p = ExprNode {
            ctx: ExprCtx {
                span: 0..17,
                txt: Rc::clone(&txt),
            },
            mode: ParseMode::List {
                form: SyntacticForm::Call,
                seq: vec![Expression::variable(
                    env.symbols.get("+"),
                    ExprCtx {
                        span: 0..3,
                        txt: Rc::clone(&txt),
                    },
                )],
            },
        };
        let other = ExprNode {
            ctx: ExprCtx {
                span: 3..15,
                txt: Rc::clone(&txt),
            },
            mode: ParseMode::List {
                form: SyntacticForm::Define,
                seq: vec![
                    Expression::symbol(
                        env.symbols.get("foo"),
                        ExprCtx {
                            span: 4..7,
                            txt: Rc::clone(&txt),
                        },
                    ),
                    Expression::string(
                        "bar",
                        ExprCtx {
                            span: 9..14,
                            txt: Rc::clone(&txt),
                        },
                    ),
                ],
            },
        };
        let ns = env.new_namespace();

        let r = p.merge(other, &ns);

        let errs = extract_or_fail!(err_or_fail!(r), ParserError::Syntax).0;
        assert_eq!(errs.len(), 1);
        assert!(matches!(
            &errs[0],
            ExpressionError {
                ctx: ExprCtx {
                    span: TxtSpan { start: 3, end: 15 },
                    txt: line
                },
                kind: ExpressionErrorKind::DefineNotAllowed,
            } if Rc::ptr_eq(&txt, &line)
        ));
    }
}
