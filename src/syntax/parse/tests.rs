use super::*;
use crate::{
    testutil::{err_or_fail, extract_or_fail, make_textline, ok_or_fail, some_or_fail},
    value::Value,
};
use std::ops::Range;

mod expr {
    use super::*;
    use crate::number::Real;

    #[test]
    fn constant() {
        let token = Token {
            kind: TokenKind::Constant(Constant::Boolean(true)),
            span: 0..3,
        };
        let txt = make_textline().into();

        let f = parse_expr(token, &txt);

        assert!(matches!(
            f,
            ExprFlow::Continue(Some(
                Expression {
                ctx: ExprCtx { span: Range { start: 0, end: 3 }, txt: line },
                kind: ExpressionKind::Literal(Value::Constant(Constant::Boolean(true))),
            })) if Rc::ptr_eq(&txt, &line)
        ));
    }

    #[test]
    fn imaginary() {
        let token = Token {
            kind: TokenKind::Imaginary(Real::Float(1.2)),
            span: 0..3,
        };
        let txt = make_textline().into();

        let f = parse_expr(token, &txt);

        assert!(matches!(
            f,
            ExprFlow::Continue(Some(
                Expression {
                ctx: ExprCtx { span: Range { start: 0, end: 3 }, txt: line },
                kind: ExpressionKind::Literal(Value::Constant(Constant::Number(n))),
            })) if n.as_datum().to_string() == "+1.2i" && Rc::ptr_eq(&txt, &line)
        ));
    }

    #[test]
    fn start_string() {
        let token = Token {
            kind: TokenKind::StringBegin {
                s: "start".to_owned(),
                line_cont: false,
            },
            span: 3..8,
        };
        let txt = make_textline().into();

        let f = parse_expr(token, &txt);

        assert!(matches!(
            f,
            ExprFlow::Break(ParseBreak::New(
                ParseNew {
                    mode: ParseMode::StringLiteral(s),
                    start: 3
                }
            )) if s == "start\n"
        ));
    }

    #[test]
    fn start_string_line_cont() {
        let token = Token {
            kind: TokenKind::StringBegin {
                s: "start".to_owned(),
                line_cont: true,
            },
            span: 3..8,
        };
        let txt = make_textline().into();

        let f = parse_expr(token, &txt);

        assert!(matches!(
            f,
            ExprFlow::Break(ParseBreak::New(
                ParseNew {
                    mode: ParseMode::StringLiteral(s),
                    start: 3
                }
            )) if s == "start"
        ));
    }

    #[test]
    fn invalid() {
        let token = Token {
            kind: TokenKind::StringEnd("foo".to_owned()),
            span: 0..3,
        };
        let txt = make_textline().into();

        let f = parse_expr(token, &txt);

        assert!(matches!(
            f,
            ExprFlow::Break(ParseBreak::Err {
                err: ExpressionError {
                    ctx: ExprCtx { span: Range { start: 0, end: 3 }, txt: line },
                    kind: ExpressionErrorKind::SeqInvalid(TokenKind::StringEnd(_)),
                },
                flow: ParseErrFlow::Break(ParseErrBreak::InvalidTokenStream),
            }) if Rc::ptr_eq(&line, &txt)
        ));
    }

    #[test]
    fn comment() {
        let token = Token {
            kind: TokenKind::Comment,
            span: 0..3,
        };
        let txt = make_textline().into();

        let f = parse_expr(token, &txt);

        assert!(matches!(f, ExprFlow::Continue(None)));
    }

    #[test]
    fn comment_begin() {
        let token = Token {
            kind: TokenKind::CommentBlockBegin { depth: 0 },
            span: 3..6,
        };
        let txt = make_textline().into();

        let f = parse_expr(token, &txt);

        assert!(matches!(
            f,
            ExprFlow::Break(ParseBreak::New(ParseNew {
                mode: ParseMode::CommentBlock,
                start: 3
            }))
        ));
    }

    #[test]
    fn start_bytevector() {
        let token = Token {
            kind: TokenKind::ByteVector,
            span: 3..6,
        };
        let txt = make_textline().into();

        let f = parse_expr(token, &txt);

        assert!(matches!(
            f,
            ExprFlow::Break(ParseBreak::New(ParseNew {
                mode: ParseMode::ByteVector(vec),
                start: 3
            })) if vec.is_empty()
        ));
    }

    #[test]
    fn identifier() {
        let token = Token {
            kind: TokenKind::Identifier("myproc".to_owned()),
            span: 0..6,
        };
        let txt = make_textline().into();

        let f = parse_expr(token, &txt);

        assert!(matches!(
            f,
            ExprFlow::Continue(Some(
                Expression {
                ctx: ExprCtx { span: Range { start: 0, end: 6 }, txt: line },
                kind: ExpressionKind::Identifier(s),
            })) if &*s == "myproc" && Rc::ptr_eq(&txt, &line)
        ));
    }

    #[test]
    fn empty_identifier() {
        let token = Token {
            kind: TokenKind::Identifier("".to_owned()),
            span: 0..0,
        };
        let txt = make_textline().into();

        let f = parse_expr(token, &txt);

        assert!(matches!(
            f,
            ExprFlow::Continue(Some(
                Expression {
                ctx: ExprCtx { span: Range { start: 0, end: 0 }, txt: line },
                kind: ExpressionKind::Identifier(s),
            })) if &*s == "" && Rc::ptr_eq(&txt, &line)
        ));
    }

    #[test]
    fn start_identifier() {
        let token = Token {
            kind: TokenKind::IdentifierBegin("start".to_owned()),
            span: 3..8,
        };
        let txt = make_textline().into();

        let f = parse_expr(token, &txt);

        assert!(matches!(
            f,
            ExprFlow::Break(ParseBreak::New(
                ParseNew {
                    mode: ParseMode::Identifier(s),
                    start: 3
                }
            )) if s == "start\n"
        ));
    }

    #[test]
    fn start_list() {
        let token = Token {
            kind: TokenKind::ParenLeft,
            span: 1..2,
        };
        let txt = make_textline().into();

        let f = parse_expr(token, &txt);

        assert!(matches!(
            f,
            ExprFlow::Break(ParseBreak::New(
                ParseNew {
                    mode: ParseMode::List(vec),
                    start: 1
                }
            )) if vec.is_empty()
        ));
    }

    #[test]
    fn start_comment_datum() {
        let token = Token {
            kind: TokenKind::CommentDatum,
            span: 1..2,
        };
        let txt = make_textline().into();

        let f = parse_expr(token, &txt);

        assert!(matches!(
            f,
            ExprFlow::Break(ParseBreak::New(ParseNew {
                mode: ParseMode::CommentDatum(None),
                start: 1
            }))
        ));
    }
}

mod bytevector {
    use super::*;
    use crate::number::ByteConversionError;

    #[test]
    fn byte() {
        let txt = make_textline().into();
        let seq = vec![Expression::constant(
            Constant::Number(Number::real(24)),
            ExprCtx {
                span: 0..3,
                txt: Rc::clone(&txt),
            },
        )];
        let node = ExprNode {
            ctx: ExprCtx {
                span: 0..3,
                txt: Rc::clone(&txt),
            },
            mode: ParseMode::ByteVector(seq),
        };

        let r: Result<Option<Expression>, _> = node.try_into();

        let expr = some_or_fail!(ok_or_fail!(r));
        assert!(matches!(
            expr,
            Expression {
                ctx: ExprCtx { span: Range { start: 0, end: 3 }, txt: line },
                kind: ExpressionKind::Literal(Value::ByteVector(_)),
            } if Rc::ptr_eq(&txt, &line)
        ));
        let bv = extract_or_fail!(
            extract_or_fail!(expr.kind, ExpressionKind::Literal),
            Value::ByteVector
        );
        assert_eq!(bv.len(), 1);
        assert_eq!(bv[0], 24);
    }

    #[test]
    fn multiple_bytes() {
        let txt = make_textline().into();
        let seq = vec![
            Expression::constant(
                Constant::Number(Number::real(24)),
                ExprCtx {
                    span: 0..3,
                    txt: Rc::clone(&txt),
                },
            ),
            Expression::constant(
                Constant::Number(Number::real(25)),
                ExprCtx {
                    span: 3..6,
                    txt: Rc::clone(&txt),
                },
            ),
            Expression::constant(
                Constant::Number(Number::real(26)),
                ExprCtx {
                    span: 6..9,
                    txt: Rc::clone(&txt),
                },
            ),
        ];
        let node = ExprNode {
            ctx: ExprCtx {
                span: 0..9,
                txt: Rc::clone(&txt),
            },
            mode: ParseMode::ByteVector(seq),
        };

        let r: Result<Option<Expression>, _> = node.try_into();

        let expr = some_or_fail!(ok_or_fail!(r));
        assert!(matches!(
            expr,
            Expression {
                ctx: ExprCtx { span: Range { start: 0, end: 9 }, txt: line },
                kind: ExpressionKind::Literal(Value::ByteVector(_)),
            } if Rc::ptr_eq(&txt, &line)
        ));
        let bv = extract_or_fail!(
            extract_or_fail!(expr.kind, ExpressionKind::Literal),
            Value::ByteVector
        );
        assert_eq!(bv.len(), 3);
        assert_eq!(bv[0], 24);
        assert_eq!(bv[1], 25);
        assert_eq!(bv[2], 26);
    }

    #[test]
    fn empty() {
        let txt = make_textline().into();
        let node = ExprNode {
            ctx: ExprCtx {
                span: 0..3,
                txt: Rc::clone(&txt),
            },
            mode: ParseMode::ByteVector(Vec::new()),
        };

        let r: Result<Option<Expression>, _> = node.try_into();

        let expr = some_or_fail!(ok_or_fail!(r));
        assert!(matches!(
            expr,
            Expression {
                ctx: ExprCtx { span: Range { start: 0, end: 3 }, txt: line },
                kind: ExpressionKind::Literal(Value::ByteVector(_)),
            } if Rc::ptr_eq(&txt, &line)
        ));
        let bv = extract_or_fail!(
            extract_or_fail!(expr.kind, ExpressionKind::Literal),
            Value::ByteVector
        );
        assert_eq!(bv.len(), 0);
    }

    #[test]
    fn invalid_item() {
        let txt = make_textline().into();
        let seq = vec![
            Expression::constant(
                Constant::Number(Number::real(24)),
                ExprCtx {
                    span: 0..3,
                    txt: Rc::clone(&txt),
                },
            ),
            Expression {
                ctx: ExprCtx {
                    span: 3..6,
                    txt: Rc::clone(&txt),
                },
                kind: ExpressionKind::Identifier("foo".into()),
            },
            Expression::constant(
                Constant::Number(Number::real(26)),
                ExprCtx {
                    span: 6..9,
                    txt: Rc::clone(&txt),
                },
            ),
        ];
        let node = ExprNode {
            ctx: ExprCtx {
                span: 0..9,
                txt: Rc::clone(&txt),
            },
            mode: ParseMode::ByteVector(seq),
        };

        let r: Result<Option<Expression>, _> = node.try_into();

        let errs = err_or_fail!(r);
        assert_eq!(errs.len(), 1);
        assert!(matches!(
            &errs[0],
            ExpressionError {
                ctx: ExprCtx { span: Range { start: 3, end: 6 }, txt: line },
                kind: ExpressionErrorKind::ByteVectorInvalidItem(ExpressionKind::Identifier(s)),
            } if &**s == "foo" && Rc::ptr_eq(&txt, &line)
        ));
    }

    #[test]
    fn invalid_items() {
        let txt = make_textline().into();
        let seq = vec![
            Expression::constant(
                Constant::Number(Number::real(24)),
                ExprCtx {
                    span: 0..3,
                    txt: Rc::clone(&txt),
                },
            ),
            Expression {
                ctx: ExprCtx {
                    span: 3..6,
                    txt: Rc::clone(&txt),
                },
                kind: ExpressionKind::Identifier("foo".into()),
            },
            Expression::constant(
                Constant::Number(Number::real(26)),
                ExprCtx {
                    span: 6..9,
                    txt: Rc::clone(&txt),
                },
            ),
            Expression::constant(
                Constant::Number(Number::real(1.2)),
                ExprCtx {
                    span: 9..12,
                    txt: Rc::clone(&txt),
                },
            ),
        ];
        let node = ExprNode {
            ctx: ExprCtx {
                span: 0..12,
                txt: Rc::clone(&txt),
            },
            mode: ParseMode::ByteVector(seq),
        };

        let r: Result<Option<Expression>, _> = node.try_into();

        let errs = err_or_fail!(r);
        assert_eq!(errs.len(), 2);
        assert!(matches!(
            &errs[0],
            ExpressionError {
                ctx: ExprCtx { span: Range { start: 3, end: 6 }, txt: line },
                kind: ExpressionErrorKind::ByteVectorInvalidItem(ExpressionKind::Identifier(s)),
            } if &**s == "foo" && Rc::ptr_eq(&txt, &line)
        ));
        assert!(matches!(
            &errs[1],
            ExpressionError {
                ctx: ExprCtx { span: Range { start: 9, end: 12 }, txt: line },
                kind: ExpressionErrorKind::ByteVectorInvalidNumber(ByteConversionError::InvalidType(s)),
            } if s == "floating-point" && Rc::ptr_eq(&txt, &line)
        ));
    }
}

mod identifier {
    use super::*;

    #[test]
    fn end() {
        let mut s = "start\n".to_owned();
        let token = Token {
            kind: TokenKind::IdentifierEnd("end".to_owned()),
            span: 0..4,
        };
        let txt = make_textline().into();

        let f = parse_verbatim_identifier(&mut s, token, &txt);

        assert!(matches!(
            f,
            ParseFlow::Break(ParseBreak::Complete(ExprEnd { lineno: 1, pos: 4 }))
        ));
        assert_eq!(s, "start\nend");
    }

    #[test]
    fn fragment() {
        let mut s = "start\n".to_owned();
        let token = Token {
            kind: TokenKind::IdentifierFragment("middle".to_owned()),
            span: 0..6,
        };
        let txt = make_textline().into();

        let f = parse_verbatim_identifier(&mut s, token, &txt);

        assert!(matches!(f, ParseFlow::Continue(())));
        assert_eq!(s, "start\nmiddle\n");
    }

    #[test]
    fn invalid() {
        let mut s = "start\n".to_owned();
        let token = Token {
            kind: TokenKind::ParenLeft,
            span: 4..5,
        };
        let txt = make_textline().into();

        let f = parse_verbatim_identifier(&mut s, token, &txt);

        assert!(matches!(
            f,
            ParseFlow::Break(ParseBreak::Err {
                err: ExpressionError {
                    ctx: ExprCtx { span: Range { start: 4, end: 5 }, txt: line },
                    kind: ExpressionErrorKind::IdentifierInvalid(TokenKind::ParenLeft),
                },
                flow: ParseErrFlow::Break(ParseErrBreak::InvalidTokenStream),
            }) if Rc::ptr_eq(&line, &txt)
        ));
        assert_eq!(s, "start\n");
    }

    #[test]
    fn node_into_expr() {
        let txt = make_textline().into();
        let p = ExprNode {
            ctx: ExprCtx {
                span: 0..3,
                txt: Rc::clone(&txt),
            },
            mode: ParseMode::Identifier("foo".to_owned()),
        };

        let r: Result<Option<Expression>, _> = p.try_into();

        let expr = some_or_fail!(ok_or_fail!(r));
        assert!(matches!(
            expr,
            Expression {
                ctx: ExprCtx { span: Range { start: 0, end: 3 }, txt: line },
                kind: ExpressionKind::Identifier(s),
            } if &*s == "foo" && Rc::ptr_eq(&txt, &line)
        ));
    }
}

mod sequence {
    use super::*;

    #[test]
    fn empty() {
        let mut seq = Vec::new();
        let token = Token {
            kind: TokenKind::Constant(Constant::Boolean(true)),
            span: 0..3,
        };
        let txt = make_textline().into();

        let f = parse_sequence(&mut seq, token, &txt);

        assert!(matches!(f, ParseFlow::Continue(())));
        assert_eq!(seq.len(), 1);
        assert!(matches!(
            &seq[0],
            Expression {
                ctx: ExprCtx { span: Range { start: 0, end: 3 }, txt: line },
                kind: ExpressionKind::Literal(Value::Constant(Constant::Boolean(true))),
            } if Rc::ptr_eq(&txt, &line)
        ));
    }

    #[test]
    fn non_empty() {
        let txt = make_textline().into();
        let mut seq = vec![
            Expression::constant(
                Constant::Number(Number::real(4)),
                ExprCtx {
                    span: 1..4,
                    txt: Rc::clone(&txt),
                },
            ),
            Expression::constant(
                Constant::Number(Number::real(5)),
                ExprCtx {
                    span: 4..6,
                    txt: Rc::clone(&txt),
                },
            ),
        ];
        let token = Token {
            kind: TokenKind::Constant(Constant::Boolean(true)),
            span: 6..9,
        };
        let txt = make_textline().into();

        let f = parse_sequence(&mut seq, token, &txt);

        assert!(matches!(f, ParseFlow::Continue(())));
        assert_eq!(seq.len(), 3);
        assert!(matches!(
            &seq[2],
            Expression {
                ctx: ExprCtx { span: Range { start: 6, end: 9 }, txt: line },
                kind: ExpressionKind::Literal(Value::Constant(Constant::Boolean(true))),
            } if Rc::ptr_eq(&txt, &line)
        ));
    }

    #[test]
    fn start_compound() {
        let mut seq = Vec::new();
        let token = Token {
            kind: TokenKind::ParenLeft,
            span: 1..2,
        };
        let txt = make_textline().into();

        let f = parse_sequence(&mut seq, token, &txt);

        assert!(matches!(
            f,
            ParseFlow::Break(ParseBreak::New(
                ParseNew {
                    mode: ParseMode::List(vec),
                    start: 1
                }
            )) if vec.is_empty()
        ));
        assert!(seq.is_empty());
    }

    #[test]
    fn invalid() {
        let mut seq = Vec::new();
        let token = Token {
            kind: TokenKind::StringEnd("foo".to_owned()),
            span: 0..3,
        };
        let txt = make_textline().into();

        let f = parse_sequence(&mut seq, token, &txt);

        assert!(matches!(
            f,
            ParseFlow::Break(ParseBreak::Err {
                err: ExpressionError {
                    ctx: ExprCtx { span: Range { start: 0, end: 3 }, txt: line },
                    kind: ExpressionErrorKind::SeqInvalid(TokenKind::StringEnd(_)),
                },
                flow: ParseErrFlow::Break(ParseErrBreak::InvalidTokenStream),
            }) if Rc::ptr_eq(&line, &txt)
        ));
        assert!(seq.is_empty());
    }
}

mod list {
    use super::*;

    #[test]
    fn end() {
        let txt = make_textline().into();
        let mut seq = vec![
            Expression {
                ctx: ExprCtx {
                    span: 0..1,
                    txt: Rc::clone(&txt),
                },
                kind: ExpressionKind::Identifier("+".into()),
            },
            Expression::constant(
                Constant::Number(Number::real(4)),
                ExprCtx {
                    span: 1..4,
                    txt: Rc::clone(&txt),
                },
            ),
            Expression::constant(
                Constant::Number(Number::real(5)),
                ExprCtx {
                    span: 4..6,
                    txt: Rc::clone(&txt),
                },
            ),
        ];
        let token = Token {
            kind: TokenKind::ParenRight,
            span: 6..7,
        };

        let f = parse_list(&mut seq, token, &txt);

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
            Expression {
                ctx: ExprCtx {
                    span: 0..1,
                    txt: Rc::clone(&txt),
                },
                kind: ExpressionKind::Identifier("+".into()),
            },
            Expression::constant(
                Constant::Number(Number::real(4)),
                ExprCtx {
                    span: 1..4,
                    txt: Rc::clone(&txt),
                },
            ),
            Expression::constant(
                Constant::Number(Number::real(5)),
                ExprCtx {
                    span: 4..6,
                    txt: Rc::clone(&txt),
                },
            ),
        ];
        let token = Token {
            kind: TokenKind::ParenLeft,
            span: 6..7,
        };

        let f = parse_list(&mut seq, token, &txt);

        assert!(matches!(
            f,
            ParseFlow::Break(ParseBreak::New(ParseNew {
                mode: ParseMode::List(vec),
                start: 6
            })) if vec.is_empty()
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

        let f = parse_list(&mut seq, token, &txt);

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
            Expression {
                ctx: ExprCtx {
                    span: 0..1,
                    txt: Rc::clone(&txt),
                },
                kind: ExpressionKind::Identifier("+".into()),
            },
            Expression::constant(
                Constant::Number(Number::real(4)),
                ExprCtx {
                    span: 1..4,
                    txt: Rc::clone(&txt),
                },
            ),
            Expression::constant(
                Constant::Number(Number::real(5)),
                ExprCtx {
                    span: 4..6,
                    txt: Rc::clone(&txt),
                },
            ),
        ];
        let token = Token {
            kind: TokenKind::Constant(Constant::Number(Number::real(10))),
            span: 6..7,
        };

        let f = parse_list(&mut seq, token, &txt);

        assert!(matches!(f, ParseFlow::Continue(())));
        assert_eq!(seq.len(), 4);
        assert!(matches!(
            &seq[3],
            Expression {
                ctx: ExprCtx { span: Range { start: 6, end: 7 }, txt: line },
                kind: ExpressionKind::Literal(Value::Constant(Constant::Number(n))),
            } if n.as_datum().to_string() == "10" && Rc::ptr_eq(&txt, &line)
        ));
    }

    #[test]
    fn invalid_token() {
        let txt = make_textline().into();
        let mut seq = vec![
            Expression {
                ctx: ExprCtx {
                    span: 0..1,
                    txt: Rc::clone(&txt),
                },
                kind: ExpressionKind::Identifier("+".into()),
            },
            Expression::constant(
                Constant::Number(Number::real(4)),
                ExprCtx {
                    span: 1..4,
                    txt: Rc::clone(&txt),
                },
            ),
            Expression::constant(
                Constant::Number(Number::real(5)),
                ExprCtx {
                    span: 4..6,
                    txt: Rc::clone(&txt),
                },
            ),
        ];
        let token = Token {
            kind: TokenKind::StringDiscard,
            span: 6..7,
        };

        let f = parse_list(&mut seq, token, &txt);

        assert!(matches!(
            f,
            ParseFlow::Break(ParseBreak::Err {
                err: ExpressionError {
                    ctx: ExprCtx { span: Range { start: 6, end: 7 }, txt: line },
                    kind: ExpressionErrorKind::SeqInvalid(TokenKind::StringDiscard),
                },
                flow: ParseErrFlow::Break(ParseErrBreak::InvalidTokenStream),
            }) if Rc::ptr_eq(&line, &txt)
        ));
        assert_eq!(seq.len(), 3);
    }

    #[test]
    fn node_into_procedure_call() {
        let txt = make_textline().into();
        let p = ExprNode {
            ctx: ExprCtx {
                span: 0..6,
                txt: Rc::clone(&txt),
            },
            mode: ParseMode::List(vec![
                Expression {
                    ctx: ExprCtx {
                        span: 0..1,
                        txt: Rc::clone(&txt),
                    },
                    kind: ExpressionKind::Identifier("+".into()),
                },
                Expression::constant(
                    Constant::Number(Number::real(4)),
                    ExprCtx {
                        span: 1..4,
                        txt: Rc::clone(&txt),
                    },
                ),
                Expression::constant(
                    Constant::Number(Number::real(5)),
                    ExprCtx {
                        span: 4..6,
                        txt: Rc::clone(&txt),
                    },
                ),
            ]),
        };

        let r: Result<Option<Expression>, _> = p.try_into();

        let expr = some_or_fail!(ok_or_fail!(r));
        assert!(matches!(
            expr,
            Expression {
                ctx: ExprCtx { span: Range { start: 0, end: 6 }, txt: line },
                kind: ExpressionKind::Call { .. },
            } if Rc::ptr_eq(&txt, &line)
        ));
        let ExpressionKind::Call { args, proc } = expr.kind else {
            unreachable!();
        };
        assert!(matches!(
            &*proc,
            Expression {
                ctx: ExprCtx { span: Range { start: 0, end: 1 }, txt: line },
                kind: ExpressionKind::Identifier(s),
            } if &**s == "+" && Rc::ptr_eq(&txt, &line)
        ));
        assert_eq!(args.len(), 2);
        assert!(matches!(
            &args[0],
            Expression {
                ctx: ExprCtx { span: Range { start: 1, end: 4 }, txt: line },
                kind: ExpressionKind::Literal(Value::Constant(Constant::Number(n))),
            } if n.as_datum().to_string() == "4" && Rc::ptr_eq(&txt, &line)
        ));
        assert!(matches!(
            &args[1],
            Expression {
                ctx: ExprCtx { span: Range { start: 4, end: 6 }, txt: line },
                kind: ExpressionKind::Literal(Value::Constant(Constant::Number(n))),
            } if n.as_datum().to_string() == "5" && Rc::ptr_eq(&txt, &line)
        ));
    }
}

mod string {
    use super::*;

    #[test]
    fn end() {
        let mut s = "start\n".to_owned();
        let token = Token {
            kind: TokenKind::StringEnd("end".to_owned()),
            span: 0..4,
        };
        let txt = make_textline().into();

        let f = parse_str(&mut s, token, &txt);

        assert!(matches!(
            f,
            ParseFlow::Break(ParseBreak::Complete(ExprEnd { lineno: 1, pos: 4 }))
        ));
        assert_eq!(s, "start\nend");
    }

    #[test]
    fn fragment() {
        let mut s = "start\n".to_owned();
        let token = Token {
            kind: TokenKind::StringFragment {
                s: "middle".to_owned(),
                line_cont: false,
            },
            span: 0..6,
        };
        let txt = make_textline().into();

        let f = parse_str(&mut s, token, &txt);

        assert!(matches!(f, ParseFlow::Continue(())));
        assert_eq!(s, "start\nmiddle\n");
    }

    #[test]
    fn fragment_line_cont() {
        let mut s = "start\n".to_owned();
        let token = Token {
            kind: TokenKind::StringFragment {
                s: "middle".to_owned(),
                line_cont: true,
            },
            span: 0..6,
        };
        let txt = make_textline().into();

        let f = parse_str(&mut s, token, &txt);

        assert!(matches!(f, ParseFlow::Continue(())));
        assert_eq!(s, "start\nmiddle");
    }

    #[test]
    fn invalid() {
        let mut s = "start\n".to_owned();
        let token = Token {
            kind: TokenKind::ParenLeft,
            span: 4..5,
        };
        let txt = make_textline().into();

        let f = parse_str(&mut s, token, &txt);

        assert!(matches!(
            f,
            ParseFlow::Break(ParseBreak::Err {
                err: ExpressionError {
                    ctx: ExprCtx { span: Range { start: 4, end: 5 }, txt: line },
                    kind: ExpressionErrorKind::StrInvalid(TokenKind::ParenLeft),
                },
                flow: ParseErrFlow::Break(ParseErrBreak::InvalidTokenStream),
            }) if Rc::ptr_eq(&line, &txt)
        ));
        assert_eq!(s, "start\n");
    }

    #[test]
    fn node_into_expr() {
        let txt = make_textline().into();
        let p = ExprNode {
            ctx: ExprCtx {
                span: 0..3,
                txt: Rc::clone(&txt),
            },
            mode: ParseMode::StringLiteral("foo".to_owned()),
        };

        let r: Result<Option<Expression>, _> = p.try_into();

        let expr = some_or_fail!(ok_or_fail!(r));
        assert!(matches!(
            expr,
            Expression {
                ctx: ExprCtx { span: Range { start: 0, end: 3 }, txt: line },
                kind: ExpressionKind::Literal(Value::Constant(Constant::String(s))),
            } if &*s == "foo" && Rc::ptr_eq(&txt, &line)
        ));
    }
}

mod comment {
    use super::*;

    #[test]
    fn block_end() {
        let token = Token {
            kind: TokenKind::CommentBlockEnd,
            span: 0..4,
        };
        let txt = make_textline().into();

        let f = parse_comment_block(token, &txt);

        assert!(matches!(
            f,
            ParseFlow::Break(ParseBreak::Complete(ExprEnd { lineno: 1, pos: 4 }))
        ));
    }

    #[test]
    fn block_fragment() {
        let token = Token {
            kind: TokenKind::CommentBlockFragment { depth: 0 },
            span: 0..4,
        };
        let txt = make_textline().into();

        let f = parse_comment_block(token, &txt);

        assert!(matches!(f, ParseFlow::Continue(())));
    }

    #[test]
    fn unexpected_new_block() {
        let token = Token {
            kind: TokenKind::CommentBlockBegin { depth: 1 },
            span: 0..4,
        };
        let txt = make_textline().into();

        let f = parse_comment_block(token, &txt);

        assert!(matches!(
            f,
            ParseFlow::Break(ParseBreak::Err {
                err: ExpressionError {
                    ctx: ExprCtx { span: Range { start: 0, end: 4 }, txt: line },
                    kind: ExpressionErrorKind::CommentBlockInvalid(
                        TokenKind::CommentBlockBegin { .. }
                    ),
                },
                flow: ParseErrFlow::Break(ParseErrBreak::InvalidTokenStream),
            }) if Rc::ptr_eq(&line, &txt)
        ));
    }

    #[test]
    fn block_invalid() {
        let token = Token {
            kind: TokenKind::ParenLeft,
            span: 0..1,
        };
        let txt = make_textline().into();

        let f = parse_comment_block(token, &txt);

        assert!(matches!(
            f,
            ParseFlow::Break(ParseBreak::Err {
                err: ExpressionError {
                    ctx: ExprCtx { span: Range { start: 0, end: 1 }, txt: line },
                    kind: ExpressionErrorKind::CommentBlockInvalid(TokenKind::ParenLeft),
                },
                flow: ParseErrFlow::Break(ParseErrBreak::InvalidTokenStream),
            }) if Rc::ptr_eq(&line, &txt)
        ));
    }

    #[test]
    fn block_into_expr() {
        let p = ExprNode {
            ctx: ExprCtx {
                span: 0..3,
                txt: make_textline().into(),
            },
            mode: ParseMode::CommentBlock,
        };

        let r: Result<Option<Expression>, _> = p.try_into();

        let o = ok_or_fail!(r);
        assert!(o.is_none());
    }

    #[test]
    fn datum_simple() {
        let token = Token {
            kind: TokenKind::Constant(Constant::Boolean(true)),
            span: 1..3,
        };
        let txt = make_textline().into();
        let mut inner = None;
        let ctx = ExprCtx {
            span: 0..1,
            txt: Rc::clone(&txt),
        };

        let f = parse_comment_datum(&mut inner, token, &txt, &ctx);

        assert!(matches!(
            f,
            ParseFlow::Break(ParseBreak::Complete(ExprEnd { lineno: 1, pos: 3 })),
        ));
        let expr = some_or_fail!(inner);
        assert!(matches!(expr, Expression {
                ctx: ExprCtx { span: Range { start: 1, end: 3 }, txt: line },
                kind: ExpressionKind::Literal(Value::Constant(Constant::Boolean(true)))
            } if Rc::ptr_eq(&txt, &line)
        ));
    }

    #[test]
    fn datum_compound() {
        let token = Token {
            kind: TokenKind::ParenLeft,
            span: 1..2,
        };
        let txt = make_textline().into();
        let mut inner = None;
        let ctx = ExprCtx {
            span: 0..1,
            txt: Rc::clone(&txt),
        };

        let f = parse_comment_datum(&mut inner, token, &txt, &ctx);

        assert!(matches!(
            f,
            ParseFlow::Break(ParseBreak::New(ParseNew {
                mode: ParseMode::List(v),
                start: 1
            })) if v.is_empty(),
        ));
        assert!(inner.is_none());
    }

    #[test]
    fn datum_no_expression() {
        let token = Token {
            kind: TokenKind::Comment,
            span: 1..6,
        };
        let txt = make_textline().into();
        let mut inner = None;
        let ctx = ExprCtx {
            span: 0..1,
            txt: Rc::clone(&txt),
        };

        let f = parse_comment_datum(&mut inner, token, &txt, &ctx);

        assert!(matches!(f, ParseFlow::Continue(()),));
        assert!(inner.is_none());
    }

    #[test]
    fn datum_invalid() {
        let token = Token {
            kind: TokenKind::StringDiscard,
            span: 1..2,
        };
        let txt = make_textline().into();
        let mut inner = None;
        let ctx = ExprCtx {
            span: 0..1,
            txt: Rc::clone(&txt),
        };

        let f = parse_comment_datum(&mut inner, token, &txt, &ctx);

        assert!(matches!(
            f,
            ParseFlow::Break(ParseBreak::Err{
                err: ExpressionError {
                    ctx: ExprCtx { span: Range { start: 1, end: 2 }, txt: line },
                    kind: ExpressionErrorKind::SeqInvalid(TokenKind::StringDiscard),
                },
                flow: ParseErrFlow::Break(ParseErrBreak::InvalidTokenStream),
            }) if Rc::ptr_eq(&txt, &line),
        ));
        assert!(inner.is_none());
    }

    #[test]
    fn datum_end_of_list() {
        let token = Token {
            kind: TokenKind::ParenRight,
            span: 1..2,
        };
        let txt = make_textline().into();
        let mut inner = None;
        let ctx = ExprCtx {
            span: 0..1,
            txt: Rc::clone(&txt),
        };

        let f = parse_comment_datum(&mut inner, token, &txt, &ctx);

        assert!(matches!(
            f,
            ParseFlow::Break(ParseBreak::Err{
                err: ExpressionError {
                    ctx: ExprCtx { span: Range { start: 0, end: 2 }, txt: line },
                    kind: ExpressionErrorKind::CommentDatumUnterminated,
                },
                flow: ParseErrFlow::Break(ParseErrBreak::FailedParser),
            }) if Rc::ptr_eq(&txt, &line),
        ));
        assert!(inner.is_none());
    }

    #[test]
    fn datum_into_expr() {
        let txt = make_textline().into();
        let p = ExprNode {
            ctx: ExprCtx {
                span: 0..2,
                txt: Rc::clone(&txt),
            },
            mode: ParseMode::CommentDatum(Some(Expression {
                ctx: ExprCtx {
                    span: 3..5,
                    txt: Rc::clone(&txt),
                },
                kind: ExpressionKind::Identifier("foo".into()),
            })),
        };

        let r: Result<Option<Expression>, _> = p.try_into();

        let o = ok_or_fail!(r);
        assert!(o.is_none());
    }

    #[test]
    fn datum_empty_into_expr() {
        let txt = make_textline().into();
        let p = ExprNode {
            ctx: ExprCtx {
                span: 0..2,
                txt: Rc::clone(&txt),
            },
            mode: ParseMode::CommentDatum(None),
        };

        let r: Result<Option<Expression>, _> = p.try_into();

        let errs = err_or_fail!(r);
        assert_eq!(errs.len(), 1);
        assert!(matches!(
            &errs[0],
            ExpressionError {
                ctx: ExprCtx { span: Range { start: 0, end: 2 }, txt },
                kind: ExpressionErrorKind::CommentDatumUnterminated,
            } if txt.lineno == 1
        ));
    }
}

mod program {
    use super::*;

    #[test]
    fn node_to_program() {
        let txt = make_textline().into();
        let p = ParseNode::Prg(vec![Expression::constant(
            Constant::Number(Number::real(24)),
            ExprCtx {
                span: 0..3,
                txt: Rc::clone(&txt),
            },
        )]);

        let r = p.try_into();

        let prg: Program = ok_or_fail!(r);
        let seq = prg.unwrap();
        assert_eq!(seq.len(), 1);
        assert!(matches!(
            &seq[0],
            Expression {
                ctx: ExprCtx { span: Range { start: 0, end: 3 }, txt: line },
                kind: ExpressionKind::Literal(Value::Constant(Constant::Number(n))),
            } if n.as_datum().to_string() == "24" && Rc::ptr_eq(&txt, &line)
        ));
    }

    #[test]
    fn parse_failure_node_to_program_error() {
        let p = ParseNode::InvalidParseTree(InvalidParseError::InvalidExprSource);

        let r: Result<Program, InvalidParseError> = p.try_into();

        let err = err_or_fail!(r);
        assert!(matches!(err, InvalidParseError::InvalidExprSource));
    }

    #[test]
    fn invalid_node_to_program_error() {
        let p = ParseNode::new(ParseMode::CommentBlock, 0, make_textline());

        let r: Result<Program, InvalidParseError> = p.try_into();

        let err = err_or_fail!(r);
        assert!(matches!(err, InvalidParseError::EndOfParse));
    }
}

mod merge {
    use super::*;

    #[test]
    fn prg_merge() {
        let txt = make_textline().into();
        let mut p = ParseNode::Prg(vec![Expression {
            ctx: ExprCtx {
                span: 0..1,
                txt: Rc::clone(&txt),
            },
            kind: ExpressionKind::Literal(Value::Constant(Constant::Boolean(true))),
        }]);
        let other = ExprNode {
            ctx: ExprCtx {
                span: 0..3,
                txt: Rc::clone(&txt),
            },
            mode: ParseMode::Identifier("foo".to_owned()),
        };

        let r = p.merge(other);

        assert!(matches!(r, Ok(MergeFlow::Continue(()))));
        let seq = extract_or_fail!(p, ParseNode::Prg);
        assert_eq!(seq.len(), 2);
        assert!(matches!(
            &seq[1],
            Expression {
                ctx: ExprCtx { span: Range { start: 0, end: 3 }, txt: line },
                kind: ExpressionKind::Identifier(s),
            } if &**s == "foo" && Rc::ptr_eq(&txt, &line)
        ));
    }

    #[test]
    fn prg_merge_fail() {
        let txt = make_textline().into();
        let mut p = ParseNode::prg();
        let other = ExprNode {
            ctx: ExprCtx {
                span: 0..3,
                txt: Rc::clone(&txt),
            },
            mode: ParseMode::ByteVector(vec![Expression {
                ctx: ExprCtx {
                    span: 0..3,
                    txt: Rc::clone(&txt),
                },
                kind: ExpressionKind::Identifier("foo".into()),
            }]),
        };

        let r = p.merge(other);

        let errs = extract_or_fail!(err_or_fail!(r), ParserError::Syntax).0;
        assert_eq!(errs.len(), 1);
        assert!(matches!(
            &errs[0],
            ExpressionError {
                ctx: ExprCtx { span: Range { start: 0, end: 3 }, txt: line },
                kind: ExpressionErrorKind::ByteVectorInvalidItem(ExpressionKind::Identifier(s)),
            } if &**s == "foo" && Rc::ptr_eq(&txt, &line)
        ));
    }

    #[test]
    fn invalid_merge() {
        let mut p = ParseNode::InvalidTokenStream;
        let other = ExprNode {
            ctx: ExprCtx {
                span: 0..3,
                txt: make_textline().into(),
            },
            mode: ParseMode::Identifier("foo".to_owned()),
        };

        let r = p.merge(other);

        assert!(r.is_ok());
    }

    #[test]
    fn commentblock_merge() {
        let txt = make_textline().into();
        let mut p = ExprNode {
            ctx: ExprCtx {
                span: 0..3,
                txt: Rc::clone(&txt),
            },
            mode: ParseMode::CommentBlock,
        };
        let other = ExprNode {
            ctx: ExprCtx {
                span: 0..3,
                txt: Rc::clone(&txt),
            },
            mode: ParseMode::Identifier("foo".to_owned()),
        };

        let r = p.merge(other);

        assert!(matches!(
            r,
            Err(ParserError::Invalid(InvalidParseError::InvalidExprTarget))
        ));
    }

    #[test]
    fn comment_datum_merge() {
        let txt = make_textline().into();
        let mut p = ExprNode {
            ctx: ExprCtx {
                span: 0..3,
                txt: Rc::clone(&txt),
            },
            mode: ParseMode::CommentDatum(None),
        };
        let other = ExprNode {
            ctx: ExprCtx {
                span: 0..4,
                txt: Rc::clone(&txt),
            },
            mode: ParseMode::List(vec![Expression {
                ctx: ExprCtx {
                    span: 1..3,
                    txt: Rc::clone(&txt),
                },
                kind: ExpressionKind::Identifier("foo".into()),
            }]),
        };

        let r = p.merge(other);

        assert!(false);

        assert!(matches!(
            r,
            Err(ParserError::Invalid(InvalidParseError::InvalidExprTarget))
        ));
    }

    #[test]
    fn identifier_merge() {
        let txt = make_textline().into();
        let mut p = ExprNode {
            ctx: ExprCtx {
                span: 0..3,
                txt: Rc::clone(&txt),
            },
            mode: ParseMode::Identifier("bar".to_owned()),
        };
        let other = ExprNode {
            ctx: ExprCtx {
                span: 0..3,
                txt: Rc::clone(&txt),
            },
            mode: ParseMode::Identifier("foo".to_owned()),
        };

        let r = p.merge(other);

        assert!(matches!(
            r,
            Err(ParserError::Invalid(InvalidParseError::InvalidExprTarget))
        ));
    }

    #[test]
    fn string_merge() {
        let txt = make_textline().into();
        let mut p = ExprNode {
            ctx: ExprCtx {
                span: 0..3,
                txt: Rc::clone(&txt),
            },
            mode: ParseMode::StringLiteral("bar".to_owned()),
        };
        let other = ExprNode {
            ctx: ExprCtx {
                span: 0..3,
                txt: Rc::clone(&txt),
            },
            mode: ParseMode::Identifier("foo".to_owned()),
        };

        let r = p.merge(other);

        assert!(matches!(
            r,
            Err(ParserError::Invalid(InvalidParseError::InvalidExprTarget))
        ));
    }

    #[test]
    fn bytevector_merge() {
        let txt = make_textline().into();
        let mut p = ExprNode {
            ctx: ExprCtx {
                span: 0..3,
                txt: Rc::clone(&txt),
            },
            mode: ParseMode::ByteVector(vec![Expression::constant(
                Constant::Number(Number::real(24)),
                ExprCtx {
                    span: 0..3,
                    txt: Rc::clone(&txt),
                },
            )]),
        };
        let other = ExprNode {
            ctx: ExprCtx {
                span: 3..6,
                txt: Rc::clone(&txt),
            },
            mode: ParseMode::Identifier("foo".to_owned()),
        };

        let r = p.merge(other);

        assert!(matches!(r, Ok(MergeFlow::Continue(()))));
        let seq = extract_or_fail!(p.mode, ParseMode::ByteVector);
        assert_eq!(seq.len(), 2);
        assert!(matches!(
            &seq[1],
            Expression {
                ctx: ExprCtx {
                    span: Range { start: 3, end: 6 },
                    txt: line
                },
                kind: ExpressionKind::Identifier(s),
            } if &**s == "foo" && Rc::ptr_eq(&txt, &line)
        ));
    }

    #[test]
    fn list_merge() {
        let txt = make_textline().into();
        let mut p = ExprNode {
            ctx: ExprCtx {
                span: 0..3,
                txt: Rc::clone(&txt),
            },
            mode: ParseMode::List(vec![Expression {
                ctx: ExprCtx {
                    span: 0..3,
                    txt: Rc::clone(&txt),
                },
                kind: ExpressionKind::Identifier("+".into()),
            }]),
        };
        let other = ExprNode {
            ctx: ExprCtx {
                span: 3..6,
                txt: Rc::clone(&txt),
            },
            mode: ParseMode::StringLiteral("foo".to_owned()),
        };

        let r = p.merge(other);

        assert!(matches!(r, Ok(MergeFlow::Continue(()))));
        let seq = extract_or_fail!(p.mode, ParseMode::List);
        assert_eq!(seq.len(), 2);
        assert!(matches!(
            &seq[1],
            Expression {
                ctx: ExprCtx {
                    span: Range { start: 3, end: 6 },
                    txt: line
                },
                kind: ExpressionKind::Literal(Value::Constant(Constant::String(s))),
            } if &**s == "foo" && Rc::ptr_eq(&txt, &line)
        ));
    }
}

mod nodeutil {
    use super::*;

    #[test]
    fn prg_no_continuation() {
        let p = ParseNode::prg();

        let o = p.into_continuation_unsupported();

        assert!(o.is_none());
    }

    #[test]
    fn str_continuation() {
        let p = ParseNode::new(
            ParseMode::StringLiteral("foo".to_owned()),
            3,
            make_textline(),
        );

        let o = p.into_continuation_unsupported();

        let err = some_or_fail!(o);
        assert!(matches!(
            &err,
            ExpressionError {
                ctx: ExprCtx { span: Range { start: 3, end: 19 }, txt },
                kind: ExpressionErrorKind::StrUnterminated,
            } if txt.lineno == 1
        ));
    }

    #[test]
    fn comment_block_continuation() {
        let p = ParseNode::new(ParseMode::CommentBlock, 3, make_textline());

        let o = p.into_continuation_unsupported();

        let err = some_or_fail!(o);
        assert!(matches!(
            &err,
            ExpressionError {
                ctx: ExprCtx { span: Range { start: 3, end: 19 }, txt },
                kind: ExpressionErrorKind::CommentBlockUnterminated,
            } if txt.lineno == 1
        ));
    }

    #[test]
    fn comment_datum_continuation() {
        let p = ParseNode::new(ParseMode::CommentDatum(None), 3, make_textline());

        let o = p.into_continuation_unsupported();

        let err = some_or_fail!(o);
        assert!(matches!(
            &err,
            ExpressionError {
                ctx: ExprCtx { span: Range { start: 3, end: 19 }, txt },
                kind: ExpressionErrorKind::CommentDatumUnterminated,
            } if txt.lineno == 1
        ));
    }

    #[test]
    fn identifier_continuation() {
        let p = ParseNode::new(
            ParseMode::Identifier("myproc".to_owned()),
            3,
            make_textline(),
        );

        let o = p.into_continuation_unsupported();

        let err = some_or_fail!(o);
        assert!(matches!(
            &err,
            ExpressionError {
                ctx: ExprCtx { span: Range { start: 3, end: 19 }, txt },
                kind: ExpressionErrorKind::IdentifierUnterminated,
            } if txt.lineno == 1
        ));
    }

    #[test]
    fn list_continuation() {
        let txt = make_textline().into();
        let p = ParseNode::new(
            ParseMode::List(vec![
                Expression {
                    ctx: ExprCtx {
                        span: 0..1,
                        txt: Rc::clone(&txt),
                    },
                    kind: ExpressionKind::Identifier("+".into()),
                },
                Expression::constant(
                    Constant::Number(Number::real(4)),
                    ExprCtx {
                        span: 1..4,
                        txt: Rc::clone(&txt),
                    },
                ),
                Expression::constant(
                    Constant::Number(Number::real(5)),
                    ExprCtx {
                        span: 4..6,
                        txt: Rc::clone(&txt),
                    },
                ),
            ]),
            3,
            Rc::clone(&txt),
        );

        let o = p.into_continuation_unsupported();

        let err = some_or_fail!(o);
        assert!(matches!(
            &err,
            ExpressionError {
                ctx: ExprCtx { span: Range { start: 3, end: 19 }, txt },
                kind: ExpressionErrorKind::ListUnterminated,
            } if txt.lineno == 1
        ));
    }

    #[test]
    fn unwrap_expr_node() {
        let txt = make_textline().into();
        let p = ParseNode::new(
            ParseMode::StringLiteral("foo".to_owned()),
            3,
            Rc::clone(&txt),
        );

        let o = p.into_expr_node(ExprEnd { lineno: 1, pos: 8 });

        let exp = some_or_fail!(o);
        assert!(matches!(
            exp,
            ExprNode {
                ctx: ExprCtx {
                    span: Range { start: 3, end: 8 },
                    txt: line
                },
                mode: ParseMode::StringLiteral(s)
            } if s == "foo" && Rc::ptr_eq(&txt, &line)
        ));
    }

    #[test]
    fn unwrap_expr_node_ended_on_different_line() {
        let txt = make_textline().into();
        let p = ParseNode::new(
            ParseMode::StringLiteral("foo".to_owned()),
            3,
            Rc::clone(&txt),
        );

        let o = p.into_expr_node(ExprEnd { lineno: 2, pos: 8 });

        let exp = some_or_fail!(o);
        assert!(matches!(
            exp,
            ExprNode {
                ctx: ExprCtx {
                    span: Range { start: 3, end: 19 },
                    txt: line
                },
                mode: ParseMode::StringLiteral(s)
            } if s == "foo" && Rc::ptr_eq(&txt, &line)
        ));
    }

    #[test]
    fn unwrap_other_node() {
        let p = ParseNode::prg();

        let o = p.into_expr_node(ExprEnd { lineno: 1, pos: 8 });

        assert!(o.is_none());
    }

    #[test]
    fn failed_parse() {
        let p = ParseNode::InvalidTokenStream;

        assert!(p.is_invalid_parse());
    }

    #[test]
    fn not_failed_parse() {
        let p = ParseNode::prg();

        assert!(!p.is_invalid_parse());
    }

    #[test]
    fn invalid_node_parse() {
        let mut p = ParseNode::InvalidTokenStream;
        let token = Token {
            kind: TokenKind::ParenLeft,
            span: 0..6,
        };
        let txt = make_textline().into();

        let r = p.parse(token, &txt);

        assert!(r.is_continue());
    }
}
