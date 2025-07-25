use super::*;
use crate::eval::InvalidFormal;

#[test]
fn end() {
    let txt = make_textline().into();
    let mut seq = vec![
        Expression::variable(
            "+".into(),
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
    let mut env = TestEnv::default();
    let mut ns = env.new_namespace();

    let f = SyntacticForm::Call.parse_list(&mut seq, token, &txt, &mut ns);

    assert!(matches!(
        f,
        ParseFlow::Break(ParseBreak::Complete(ExprEnd { lineno: 1, pos: 7 }))
    ));
    assert_eq!(seq.len(), 3);
}

#[test]
fn nested_list() {
    let txt = make_textline().into();
    let mut seq = vec![
        Expression::variable(
            "+".into(),
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
    let mut env = TestEnv::default();
    let mut ns = env.new_namespace();

    let f = SyntacticForm::Call.parse_list(&mut seq, token, &txt, &mut ns);

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
    let mut env = TestEnv::default();
    let mut ns = env.new_namespace();

    let f = SyntacticForm::Call.parse_list(&mut seq, token, &txt, &mut ns);

    assert!(matches!(
        f,
        ParseFlow::Break(ParseBreak::Complete(ExprEnd { lineno: 1, pos: 5 }))
    ));
    assert!(seq.is_empty());
}

#[test]
fn expression_item() {
    let txt = make_textline().into();
    let mut seq = vec![
        Expression::variable(
            "+".into(),
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
    let mut env = TestEnv::default();
    let mut ns = env.new_namespace();

    let f = SyntacticForm::Call.parse_list(&mut seq, token, &txt, &mut ns);

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
    let mut env = TestEnv::default();
    let mut ns = env.new_namespace();
    let mut frm = SyntacticForm::Datum;

    let f = frm.parse_list(&mut seq, token, &txt, &mut ns);

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
    let mut env = TestEnv::default();
    let mut ns = env.new_namespace();
    let mut frm = SyntacticForm::Datum;

    let f = frm.parse_list(&mut seq, token, &txt, &mut ns);

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
    let mut env = TestEnv::default();
    let mut ns = env.new_namespace();

    let f = SyntacticForm::Call.parse_list(&mut seq, token, &txt, &mut ns);

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
    let mut env = TestEnv::default();
    let mut ns = env.new_namespace();
    let mut frm = SyntacticForm::PairOpen;

    let f = frm.parse_list(&mut seq, token, &txt, &mut ns);

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
    let mut env = TestEnv::default();
    let mut ns = env.new_namespace();
    let mut frm = SyntacticForm::PairOpen;

    let f = frm.parse_list(&mut seq, token, &txt, &mut ns);

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
    let mut env = TestEnv::default();
    let mut ns = env.new_namespace();
    let mut frm = SyntacticForm::PairClosed;

    let f = frm.parse_list(&mut seq, token, &txt, &mut ns);

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
    let mut env = TestEnv::default();
    let mut ns = env.new_namespace();
    let mut frm = SyntacticForm::PairOpen;

    let f = frm.parse_list(&mut seq, token, &txt, &mut ns);

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
    let mut env = TestEnv::default();
    let mut ns = env.new_namespace();
    let mut frm = SyntacticForm::PairOpen;

    let f = frm.parse_list(&mut seq, token, &txt, &mut ns);

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
    let mut env = TestEnv::default();
    let mut ns = env.new_namespace();
    let mut frm = SyntacticForm::PairClosed;

    let f = frm.parse_list(&mut seq, token, &txt, &mut ns);

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
    let mut seq = vec![
        Expression::variable(
            "+".into(),
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
    let mut env = TestEnv::default();
    let mut ns = env.new_namespace();

    let f = SyntacticForm::Call.parse_list(&mut seq, token, &txt, &mut ns);

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
    let txt = make_textline().into();
    // (+ 4 5)
    let p = ExprNode {
        ctx: ExprCtx {
            span: 0..6,
            txt: Rc::clone(&txt),
        },
        mode: ParseMode::List {
            form: SyntacticForm::Call,
            seq: vec![
                Expression::variable(
                    "+".into(),
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
    let mut env = TestEnv::default();
    let mut ns = env.new_namespace();

    let r = p.try_into_expr(&mut ns);

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
    let txt = make_textline().into();
    // ()
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
    let mut env = TestEnv::default();
    let mut ns = env.new_namespace();

    let r = p.try_into_expr(&mut ns);

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
    let txt = make_textline().into();
    // (quote foo)
    let p = ExprNode {
        ctx: ExprCtx {
            span: 0..10,
            txt: Rc::clone(&txt),
        },
        mode: ParseMode::List {
            form: SyntacticForm::Quote,
            seq: vec![Expression::symbol(
                "foo".into(),
                ExprCtx {
                    span: 6..9,
                    txt: Rc::clone(&txt),
                },
            )],
        },
    };
    let mut env = TestEnv::default();
    let mut ns = env.new_namespace();

    let r = p.try_into_expr(&mut ns);

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
    let txt = make_textline().into();
    // (quote)
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
    let mut env = TestEnv::default();
    let mut ns = env.new_namespace();

    let r = p.try_into_expr(&mut ns);

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
    let txt = make_textline().into();
    // (quote foo bar)
    let p = ExprNode {
        ctx: ExprCtx {
            span: 0..14,
            txt: Rc::clone(&txt),
        },
        mode: ParseMode::List {
            form: SyntacticForm::Quote,
            seq: vec![
                Expression::symbol(
                    "foo".into(),
                    ExprCtx {
                        span: 6..9,
                        txt: Rc::clone(&txt),
                    },
                ),
                Expression::symbol(
                    "bar".into(),
                    ExprCtx {
                        span: 10..13,
                        txt: Rc::clone(&txt),
                    },
                ),
            ],
        },
    };
    let mut env = TestEnv::default();
    let mut ns = env.new_namespace();

    let r = p.try_into_expr(&mut ns);

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
    let txt = make_textline().into();
    // '(+ 4 5)
    let p = ExprNode {
        ctx: ExprCtx {
            span: 0..6,
            txt: Rc::clone(&txt),
        },
        mode: ParseMode::List {
            form: SyntacticForm::Datum,
            seq: vec![
                Expression::symbol(
                    "+".into(),
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
    let mut env = TestEnv::default();
    let mut ns = env.new_namespace();

    let r = p.try_into_expr(&mut ns);

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
    let txt = make_textline().into();
    // '()
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
    let mut env = TestEnv::default();
    let mut ns = env.new_namespace();

    let r = p.try_into_expr(&mut ns);

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
    let txt = make_textline().into();
    // '(<unquoted +> 4 5)
    let p = ExprNode {
        ctx: ExprCtx {
            span: 0..8,
            txt: Rc::clone(&txt),
        },
        mode: ParseMode::List {
            form: SyntacticForm::Datum,
            seq: vec![
                Expression::variable(
                    "+".into(),
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
    let mut env = TestEnv::default();
    let mut ns = env.new_namespace();

    let r = p.try_into_expr(&mut ns);

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
    let txt = make_textline().into();
    // '(4 . 5)
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
    let mut env = TestEnv::default();
    let mut ns = env.new_namespace();

    let r = p.try_into_expr(&mut ns);

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
    let txt = make_textline().into();
    // '(4 . )
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
    let mut env = TestEnv::default();
    let mut ns = env.new_namespace();

    let r = p.try_into_expr(&mut ns);

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
    let txt = make_textline().into();
    // (define foo "bar")
    let p = ExprNode {
        ctx: ExprCtx {
            span: 0..18,
            txt: Rc::clone(&txt),
        },
        mode: ParseMode::List {
            form: SyntacticForm::Define,
            seq: vec![
                Expression::symbol(
                    "foo".into(),
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
    let mut env = TestEnv::default();
    let mut ns = env.new_namespace();

    let r = p.try_into_expr(&mut ns);

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
#[ignore = "define lambda not implemented"]
fn into_define_lambda() {
    let txt = make_textline().into();
    // (define (foo x y) (+ x y))
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
                    Value::Symbol("foo".into()),
                    Value::Symbol("x".into()),
                    Value::Symbol("y".into())
                ])),
                ExprCtx {
                    span: 9..12,
                    txt: Rc::clone(&txt),
                }
                .into_expr(ExpressionKind::Call {
                    proc: Expression::variable(
                        "+".into(),
                        ExprCtx {
                            span: 19..20,
                            txt: Rc::clone(&txt),
                        },
                    )
                    .into(),
                    args: [
                        Expression::variable(
                            "x".into(),
                            ExprCtx {
                                span: 21..22,
                                txt: Rc::clone(&txt),
                            },
                        ),
                        Expression::variable(
                            "y".into(),
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
    let mut env = TestEnv::default();
    let mut ns = env.new_namespace();

    let r = p.try_into_expr(&mut ns);

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
#[ignore = "define lambda not implemented"]
fn into_define_parameterless_lambda() {
    let txt = make_textline().into();
    // (define (foo) 123)
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
                .into_expr(ExpressionKind::Literal(zlist![Value::Symbol("foo".into())])),
                ExprCtx {
                    span: 14..17,
                    txt: Rc::clone(&txt),
                }
                .into_expr(ExpressionKind::Literal(Value::Number(Number::real(123)))),
            ],
        },
    };
    let mut env = TestEnv::default();
    let mut ns = env.new_namespace();

    let r = p.try_into_expr(&mut ns);

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
#[ignore = "define lambda not implemented"]
fn into_define_variadic_lambda() {
    let txt = make_textline().into();
    // (define (foo . x) (cdr x))
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
                    Value::Symbol("foo".into()),
                    Value::Symbol("x".into()),
                ))),
                ExprCtx {
                    span: 18..25,
                    txt: Rc::clone(&txt),
                }
                .into_expr(ExpressionKind::Call {
                    proc: Expression::variable(
                        "cdr".into(),
                        ExprCtx {
                            span: 19..22,
                            txt: Rc::clone(&txt),
                        },
                    )
                    .into(),
                    args: [Expression::variable(
                        "x".into(),
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
    let mut env = TestEnv::default();
    let mut ns = env.new_namespace();

    let r = p.try_into_expr(&mut ns);

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
#[ignore = "define lambda not implemented"]
fn into_define_rest_lambda() {
    let txt = make_textline().into();
    // (define (foo x . y) (display x y))
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
                    Value::Symbol("foo".into()),
                    Value::cons(Value::Symbol("x".into()), Value::Symbol("y".into())),
                ))),
                ExprCtx {
                    span: 20..33,
                    txt: Rc::clone(&txt),
                }
                .into_expr(ExpressionKind::Call {
                    proc: Expression::variable(
                        "display".into(),
                        ExprCtx {
                            span: 21..28,
                            txt: Rc::clone(&txt),
                        },
                    )
                    .into(),
                    args: [
                        Expression::variable(
                            "x".into(),
                            ExprCtx {
                                span: 29..30,
                                txt: Rc::clone(&txt),
                            },
                        ),
                        Expression::variable(
                            "y".into(),
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
    let mut env = TestEnv::default();
    let mut ns = env.new_namespace();

    let r = p.try_into_expr(&mut ns);

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
    let txt = make_textline().into();
    // (define foo)
    let p = ExprNode {
        ctx: ExprCtx {
            span: 0..12,
            txt: Rc::clone(&txt),
        },
        mode: ParseMode::List {
            form: SyntacticForm::Define,
            seq: vec![Expression::symbol(
                "foo".into(),
                ExprCtx {
                    span: 8..11,
                    txt: Rc::clone(&txt),
                },
            )],
        },
    };
    let mut env = TestEnv::default();
    let mut ns = env.new_namespace();

    let r = p.try_into_expr(&mut ns);

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
    let txt = make_textline().into();
    // (define <unquoted (myproc)> "bar")
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
                        "myproc".into(),
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
    let mut env = TestEnv::default();
    let mut ns = env.new_namespace();

    let r = p.try_into_expr(&mut ns);

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
    let txt = make_textline().into();
    // (define)
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
    let mut env = TestEnv::default();
    let mut ns = env.new_namespace();

    let r = p.try_into_expr(&mut ns);

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
    let txt = make_textline().into();
    // (define foo "bar" "baz")
    let p = ExprNode {
        ctx: ExprCtx {
            span: 0..24,
            txt: Rc::clone(&txt),
        },
        mode: ParseMode::List {
            form: SyntacticForm::Define,
            seq: vec![
                Expression::symbol(
                    "foo".into(),
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
    let mut env = TestEnv::default();
    let mut ns = env.new_namespace();

    let r = p.try_into_expr(&mut ns);

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
#[ignore = "define lambda not implemented"]
fn into_define_lambda_no_body() {
    let txt = make_textline().into();
    // (define (foo))
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
                .into_expr(ExpressionKind::Literal(zlist![Value::Symbol("foo".into())])),
            ],
        },
    };
    let mut env = TestEnv::default();
    let mut ns = env.new_namespace();

    let r = p.try_into_expr(&mut ns);

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
#[ignore = "define lambda not implemented"]
fn into_define_lambda_empty_body() {
    let txt = make_textline().into();
    // (define (foo) ())
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
                .into_expr(ExpressionKind::Literal(zlist![Value::Symbol("foo".into())])),
            ],
            // TODO: what goes here
        },
    };
    let mut env = TestEnv::default();
    let mut ns = env.new_namespace();

    let r = p.try_into_expr(&mut ns);

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
fn into_set_variable() {
    let txt = make_textline().into();
    // (set! foo "bar")
    let p = ExprNode {
        ctx: ExprCtx {
            span: 0..18,
            txt: Rc::clone(&txt),
        },
        mode: ParseMode::List {
            form: SyntacticForm::Set,
            seq: vec![
                Expression::variable(
                    "foo".into(),
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
    let mut env = TestEnv::default();
    let mut ns = env.new_namespace();

    let r = p.try_into_expr(&mut ns);

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
    let txt = make_textline().into();
    // (set! (myproc) "bar")
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
                        "myproc".into(),
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
    let mut env = TestEnv::default();
    let mut ns = env.new_namespace();

    let r = p.try_into_expr(&mut ns);

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
    let txt = make_textline().into();
    // (set! foo)
    let p = ExprNode {
        ctx: ExprCtx {
            span: 0..25,
            txt: Rc::clone(&txt),
        },
        mode: ParseMode::List {
            form: SyntacticForm::Set,
            seq: vec![Expression::variable(
                "foo".into(),
                ExprCtx {
                    span: 8..11,
                    txt: Rc::clone(&txt),
                },
            )],
        },
    };
    let mut env = TestEnv::default();
    let mut ns = env.new_namespace();

    let r = p.try_into_expr(&mut ns);

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
    let txt = make_textline().into();
    // (set! foo "bar" "baz")
    let p = ExprNode {
        ctx: ExprCtx {
            span: 0..25,
            txt: Rc::clone(&txt),
        },
        mode: ParseMode::List {
            form: SyntacticForm::Set,
            seq: vec![
                Expression::variable(
                    "foo".into(),
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
    let mut env = TestEnv::default();
    let mut ns = env.new_namespace();

    let r = p.try_into_expr(&mut ns);

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
    let txt = make_textline().into();
    // (if #t "bar")
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
    let mut env = TestEnv::default();
    let mut ns = env.new_namespace();

    let r = p.try_into_expr(&mut ns);

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
    let txt = make_textline().into();
    // (if #t "bar" "foo")
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
                    "foo".into(),
                    ExprCtx {
                        span: 13..16,
                        txt: Rc::clone(&txt),
                    },
                ),
            ],
        },
    };
    let mut env = TestEnv::default();
    let mut ns = env.new_namespace();

    let r = p.try_into_expr(&mut ns);

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
    let txt = make_textline().into();
    // (if #t)
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
    let mut env = TestEnv::default();
    let mut ns = env.new_namespace();

    let r = p.try_into_expr(&mut ns);

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
    let txt = make_textline().into();
    // (if #t "bar" "foo" "beef")
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
                    "foo".into(),
                    ExprCtx {
                        span: 13..16,
                        txt: Rc::clone(&txt),
                    },
                ),
                Expression::symbol(
                    "beef".into(),
                    ExprCtx {
                        span: 17..21,
                        txt: Rc::clone(&txt),
                    },
                ),
            ],
        },
    };
    let mut env = TestEnv::default();
    let mut ns = env.new_namespace();

    let r = p.try_into_expr(&mut ns);

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
                    Value::Symbol("x".into()),
                    Value::Symbol("y".into())
                ])),
                ExprCtx {
                    span: 14..21,
                    txt: Rc::clone(&txt),
                }
                .into_expr(ExpressionKind::Call {
                    proc: Expression::variable(
                        "+".into(),
                        ExprCtx {
                            span: 15..16,
                            txt: Rc::clone(&txt),
                        },
                    )
                    .into(),
                    args: [
                        Expression::variable(
                            "x".into(),
                            ExprCtx {
                                span: 17..18,
                                txt: Rc::clone(&txt),
                            },
                        ),
                        Expression::variable(
                            "y".into(),
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
    let mut env = TestEnv::default();
    let mut ns = env.new_namespace();

    let r = p.try_into_expr(&mut ns);

    let expr = some_or_fail!(ok_or_fail!(r));
    assert!(matches!(
        expr,
        Expression {
            ctx: ExprCtx { span: TxtSpan { start: 0, end: 22 }, txt: line },
            kind: ExpressionKind::Literal(_),
        } if Rc::ptr_eq(&txt, &line)
    ));
    let ExpressionKind::Literal(val) = expr.kind else {
        unreachable!();
    };
    assert_eq!(val.to_string(), "#<procedure (x y)>");
}

#[test]
fn into_lambda_simple_body() {
    // (lambda (x) x)
    let txt = make_textline().into();
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
                .into_expr(ExpressionKind::Literal(zlist![Value::Symbol("x".into())])),
                Expression::variable(
                    "x".into(),
                    ExprCtx {
                        span: 12..13,
                        txt: Rc::clone(&txt),
                    },
                ),
            ],
        },
    };
    let mut env = TestEnv::default();
    let mut ns = env.new_namespace();

    let r = p.try_into_expr(&mut ns);

    let expr = some_or_fail!(ok_or_fail!(r));
    assert!(matches!(
        expr,
        Expression {
            ctx: ExprCtx { span: TxtSpan { start: 0, end: 14 }, txt: line },
            kind: ExpressionKind::Literal(_),
        } if Rc::ptr_eq(&txt, &line)
    ));
    let ExpressionKind::Literal(val) = expr.kind else {
        unreachable!();
    };
    assert_eq!(val.to_string(), "#<procedure (x)>");
}

#[test]
fn into_lambda_no_arguments() {
    // (lambda () 'a)
    let txt = make_textline().into();
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
                    "a".into(),
                    ExprCtx {
                        span: 11..13,
                        txt: Rc::clone(&txt),
                    },
                ),
            ],
        },
    };
    let mut env = TestEnv::default();
    let mut ns = env.new_namespace();

    let r = p.try_into_expr(&mut ns);

    let expr = some_or_fail!(ok_or_fail!(r));
    assert!(matches!(
        expr,
        Expression {
            ctx: ExprCtx { span: TxtSpan { start: 0, end: 14 }, txt: line },
            kind: ExpressionKind::Literal(_),
        } if Rc::ptr_eq(&txt, &line)
    ));
    let ExpressionKind::Literal(val) = expr.kind else {
        unreachable!();
    };
    assert_eq!(val.to_string(), "#<procedure>");
}

#[test]
fn into_lambda_variadic() {
    // (lambda x x)
    let txt = make_textline().into();
    let p = ExprNode {
        ctx: ExprCtx {
            span: 0..12,
            txt: Rc::clone(&txt),
        },
        mode: ParseMode::List {
            form: SyntacticForm::Lambda,
            seq: vec![
                Expression::symbol(
                    "x".into(),
                    ExprCtx {
                        span: 8..9,
                        txt: Rc::clone(&txt),
                    },
                ),
                Expression::variable(
                    "x".into(),
                    ExprCtx {
                        span: 10..11,
                        txt: Rc::clone(&txt),
                    },
                ),
            ],
        },
    };
    let mut env = TestEnv::default();
    let mut ns = env.new_namespace();

    let r = p.try_into_expr(&mut ns);

    let expr = some_or_fail!(ok_or_fail!(r));
    assert!(matches!(
        expr,
        Expression {
            ctx: ExprCtx { span: TxtSpan { start: 0, end: 12 }, txt: line },
            kind: ExpressionKind::Literal(_),
        } if Rc::ptr_eq(&txt, &line)
    ));
    let ExpressionKind::Literal(val) = expr.kind else {
        unreachable!();
    };
    assert_eq!(val.to_string(), "#<procedure x…>");
}

#[test]
fn into_lambda_rest() {
    // (lambda (x y . z) z)
    let txt = make_textline().into();
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
                    Value::Symbol("x".into()),
                    Value::cons(Value::Symbol("y".into()), Value::Symbol("z".into())),
                ))),
                Expression::variable(
                    "z".into(),
                    ExprCtx {
                        span: 18..19,
                        txt: Rc::clone(&txt),
                    },
                ),
            ],
        },
    };
    let mut env = TestEnv::default();
    let mut ns = env.new_namespace();

    let r = p.try_into_expr(&mut ns);

    let expr = some_or_fail!(ok_or_fail!(r));
    assert!(matches!(
        expr,
        Expression {
            ctx: ExprCtx { span: TxtSpan { start: 0, end: 20 }, txt: line },
            kind: ExpressionKind::Literal(_),
        } if Rc::ptr_eq(&txt, &line)
    ));
    let ExpressionKind::Literal(val) = expr.kind else {
        unreachable!();
    };
    assert_eq!(val.to_string(), "#<procedure (x y z…)>");
}

#[test]
fn into_lambda_multiple_expression_body() {
    // (lambda x x 'b)
    let txt = make_textline().into();
    let p = ExprNode {
        ctx: ExprCtx {
            span: 0..15,
            txt: Rc::clone(&txt),
        },
        mode: ParseMode::List {
            form: SyntacticForm::Lambda,
            seq: vec![
                Expression::symbol(
                    "x".into(),
                    ExprCtx {
                        span: 8..9,
                        txt: Rc::clone(&txt),
                    },
                ),
                Expression::variable(
                    "x".into(),
                    ExprCtx {
                        span: 10..11,
                        txt: Rc::clone(&txt),
                    },
                ),
                Expression::symbol(
                    "b".into(),
                    ExprCtx {
                        span: 12..14,
                        txt: Rc::clone(&txt),
                    },
                ),
            ],
        },
    };
    let mut env = TestEnv::default();
    let mut ns = env.new_namespace();

    let r = p.try_into_expr(&mut ns);

    let expr = some_or_fail!(ok_or_fail!(r));
    assert!(matches!(
        expr,
        Expression {
            ctx: ExprCtx { span: TxtSpan { start: 0, end: 15 }, txt: line },
            kind: ExpressionKind::Literal(_),
        } if Rc::ptr_eq(&txt, &line)
    ));
    let ExpressionKind::Literal(val) = expr.kind else {
        unreachable!();
    };
    assert_eq!(val.to_string(), "#<procedure x…>");
}

#[test]
fn into_lambda_not_identifier_expr() {
    // (lambda (1) 'a)
    let txt = make_textline().into();
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
                    "a".into(),
                    ExprCtx {
                        span: 12..14,
                        txt: Rc::clone(&txt),
                    },
                ),
            ],
        },
    };
    let mut env = TestEnv::default();
    let mut ns = env.new_namespace();

    let r = p.try_into_expr(&mut ns);

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
    let p = ExprNode {
        ctx: ExprCtx {
            span: 0..10,
            txt: Rc::clone(&txt),
        },
        mode: ParseMode::List {
            form: SyntacticForm::Lambda,
            seq: vec![Expression::symbol(
                "x".into(),
                ExprCtx {
                    span: 8..9,
                    txt: Rc::clone(&txt),
                },
            )],
        },
    };
    let mut env = TestEnv::default();
    let mut ns = env.new_namespace();

    let r = p.try_into_expr(&mut ns);

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
                    Value::Symbol("x".into()),
                    Value::Symbol("y".into()),
                    Value::Symbol("x".into()),
                ])),
                ExprCtx {
                    span: 16..23,
                    txt: Rc::clone(&txt),
                }
                .into_expr(ExpressionKind::Call {
                    proc: Expression::variable(
                        "+".into(),
                        ExprCtx {
                            span: 17..18,
                            txt: Rc::clone(&txt),
                        },
                    )
                    .into(),
                    args: [
                        Expression::variable(
                            "x".into(),
                            ExprCtx {
                                span: 19..20,
                                txt: Rc::clone(&txt),
                            },
                        ),
                        Expression::variable(
                            "y".into(),
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
    let mut env = TestEnv::default();
    let mut ns = env.new_namespace();

    let r = p.try_into_expr(&mut ns);

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
    let args = (0..256)
        .into_iter()
        .map(|i| Value::Symbol(format!("a{i}").into()))
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
                    "a".into(),
                    ExprCtx {
                        span: 12..14,
                        txt: Rc::clone(&txt),
                    },
                ),
            ],
        },
    };
    let mut env = TestEnv::default();
    let mut ns = env.new_namespace();

    let r = p.try_into_expr(&mut ns);

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
    let args = (0..256)
        .into_iter()
        .map(|i| Value::Symbol(format!("a{i}").into()))
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
                    "a".into(),
                    ExprCtx {
                        span: 12..14,
                        txt: Rc::clone(&txt),
                    },
                ),
            ],
        },
    };
    let mut env = TestEnv::default();
    let mut ns = env.new_namespace();

    let r = p.try_into_expr(&mut ns);

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
        let mut p = ExprNode {
            ctx: ExprCtx {
                span: 0..3,
                txt: Rc::clone(&txt),
            },
            mode: ParseMode::List {
                form: SyntacticForm::Call,
                seq: vec![Expression::variable(
                    "+".into(),
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
        let mut env = TestEnv::default();
        let mut ns = env.new_namespace();

        let r = p.merge(other, &mut ns);

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
                    .into_expr(ExpressionKind::Literal(Value::Symbol("a".into()))),
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
        let mut env = TestEnv::default();
        let mut ns = env.new_namespace();

        let r = p.merge(other, &mut ns);

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
                    .into_expr(ExpressionKind::Literal(Value::Symbol("a".into()))),
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
                .into_expr(ExpressionKind::Literal(Value::Symbol("b".into()))),
            )),
        };
        let mut env = TestEnv::default();
        let mut ns = env.new_namespace();

        let r = p.merge(other, &mut ns);

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
                    .into_expr(ExpressionKind::Literal(Value::Symbol("a".into()))),
                    ExprCtx {
                        span: 2..3,
                        txt: Rc::clone(&txt),
                    }
                    .into_expr(ExpressionKind::Literal(Value::Symbol("b".into()))),
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
        let mut env = TestEnv::default();
        let mut ns = env.new_namespace();

        let r = p.merge(other, &mut ns);

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
        let mut p = ExprNode {
            ctx: ExprCtx {
                span: 0..17,
                txt: Rc::clone(&txt),
            },
            mode: ParseMode::List {
                form: SyntacticForm::Call,
                seq: vec![Expression::variable(
                    "+".into(),
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
                        "foo".into(),
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
        let mut env = TestEnv::default();
        let mut ns = env.new_namespace();

        let r = p.merge(other, &mut ns);

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
