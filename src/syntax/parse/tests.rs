use super::*;
use crate::{testutil::make_textline, value::Value};
use std::ops::Range;

mod bytevector {
    use super::*;

    #[test]
    fn byte() {
        let mut seq = Vec::new();
        let token = Token {
            kind: TokenKind::Constant(Constant::Number(Number::real(16))),
            span: 3..5,
        };
        let txt = Rc::new(make_textline());

        let f = parse_list(&mut seq, token, &txt);

        assert!(matches!(f, ParseFlow::Continue(())));
        assert_eq!(seq.len(), 1);
        assert!(matches!(
            &seq[0],
            Expression {
                ctx: ExprCtx { span: Range { start: 3, end: 5 }, txt: line },
                kind: ExpressionKind::Literal(Value::Constant(Constant::Number(n))),
            }
            if n.as_datum().to_string() == "16" && Rc::ptr_eq(&txt, &line)
        ));
    }

    #[test]
    fn min_byte() {
        let mut seq = Vec::new();
        let token = Token {
            kind: TokenKind::Constant(Constant::Number(Number::real(0))),
            span: 3..5,
        };
        let txt = Rc::new(make_textline());

        let f = parse_list(&mut seq, token, &txt);

        assert!(matches!(f, ParseFlow::Continue(())));
        assert_eq!(seq.len(), 1);
        assert!(matches!(
            &seq[0],
            Expression {
                ctx: ExprCtx { span: Range { start: 3, end: 5 }, txt: line },
                kind: ExpressionKind::Literal(Value::Constant(Constant::Number(n))),
            }
            if n.as_datum().to_string() == "0" && Rc::ptr_eq(&txt, &line)
        ));
    }

    #[test]
    fn max_byte() {
        let mut seq = Vec::new();
        let token = Token {
            kind: TokenKind::Constant(Constant::Number(Number::real(255))),
            span: 3..5,
        };
        let txt = Rc::new(make_textline());

        let f = parse_list(&mut seq, token, &txt);

        assert!(matches!(f, ParseFlow::Continue(())));
        assert_eq!(seq.len(), 1);
        assert!(matches!(
            &seq[0],
            Expression {
                ctx: ExprCtx { span: Range { start: 3, end: 5 }, txt: line },
                kind: ExpressionKind::Literal(Value::Constant(Constant::Number(n))),
            }
            if n.as_datum().to_string() == "255" && Rc::ptr_eq(&txt, &line)
        ));
    }

    #[test]
    fn end() {
        let txt = make_textline().into();
        let mut seq = vec![
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
        let token = Token {
            kind: TokenKind::ParenRight,
            span: 9..10,
        };
        let txt = Rc::new(make_textline());

        let f = parse_list(&mut seq, token, &txt);

        assert!(matches!(f, ParseFlow::Break(ParseBreak::Complete)));
        assert_eq!(seq.len(), 3);
    }

    #[test]
    fn empty() {
        let mut seq = Vec::new();
        let token = Token {
            kind: TokenKind::ParenRight,
            span: 3..4,
        };
        let txt = Rc::new(make_textline());

        let f = parse_list(&mut seq, token, &txt);

        assert!(matches!(f, ParseFlow::Break(ParseBreak::Complete)));
        assert!(seq.is_empty());
    }
}

mod continuation {
    use super::*;
    use crate::testutil::{make_textline, some_or_fail};

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
        let txt = Rc::new(make_textline());

        let f = parse_verbatim_identifier(&mut s, token, &txt);

        assert!(matches!(f, ParseFlow::Break(ParseBreak::Complete)));
        assert_eq!(s, "start\nend");
    }

    #[test]
    fn fragment() {
        let mut s = "start\n".to_owned();
        let token = Token {
            kind: TokenKind::IdentifierFragment("middle".to_owned()),
            span: 0..6,
        };
        let txt = Rc::new(make_textline());

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
        let txt = Rc::new(make_textline());

        let f = parse_verbatim_identifier(&mut s, token, &txt);

        assert!(matches!(
            f,
            ParseFlow::Break(ParseBreak::Err(
                ExpressionError {
                    ctx: ExprCtx { span: Range { start: 4, end: 5 }, txt: line },
                    kind: ExpressionErrorKind::IdentifierInvalid(TokenKind::ParenLeft),
                },
                ErrFlow::Break(Recovery::Fail),
            )) if Rc::ptr_eq(&line, &txt)
        ));
        assert_eq!(s, "start\n");
    }
}

mod sequence {
    use super::*;
    use crate::number::Real;

    #[test]
    fn constant() {
        let mut seq = Vec::new();
        let token = Token {
            kind: TokenKind::Constant(Constant::Boolean(true)),
            span: 0..3,
        };
        let txt = Rc::new(make_textline());

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
    fn imaginary() {
        let mut seq = Vec::new();
        let token = Token {
            kind: TokenKind::Imaginary(Real::Float(1.2)),
            span: 0..3,
        };
        let txt = Rc::new(make_textline());

        let f = parse_sequence(&mut seq, token, &txt);

        assert!(matches!(f, ParseFlow::Continue(())));
        assert_eq!(seq.len(), 1);
        assert!(matches!(
            &seq[0],
            Expression {
                ctx: ExprCtx { span: Range { start: 0, end: 3 }, txt: line },
                kind: ExpressionKind::Literal(Value::Constant(Constant::Number(n))),
            } if n.as_datum().to_string() == "+1.2i" && Rc::ptr_eq(&txt, &line)
        ));
    }

    #[test]
    fn start_string() {
        let mut seq = Vec::new();
        let token = Token {
            kind: TokenKind::StringBegin {
                s: "start".to_owned(),
                line_cont: false,
            },
            span: 3..8,
        };
        let txt = Rc::new(make_textline());

        let f = parse_sequence(&mut seq, token, &txt);

        assert!(matches!(
            f,
            ParseFlow::Break(ParseBreak::New(
                ParseNew {
                    mode: ParseMode::StringLiteral(s),
                    start: 3
                }
            )) if s == "start\n"
        ));
        assert!(seq.is_empty());
    }

    #[test]
    fn start_string_line_cont() {
        let mut seq = Vec::new();
        let token = Token {
            kind: TokenKind::StringBegin {
                s: "start".to_owned(),
                line_cont: true,
            },
            span: 3..8,
        };
        let txt = Rc::new(make_textline());

        let f = parse_sequence(&mut seq, token, &txt);

        assert!(matches!(
            f,
            ParseFlow::Break(ParseBreak::New(
                ParseNew {
                    mode: ParseMode::StringLiteral(s),
                    start: 3
                }
            )) if s == "start"
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
        let txt = Rc::new(make_textline());

        let f = parse_sequence(&mut seq, token, &txt);

        assert!(matches!(
            f,
            ParseFlow::Break(ParseBreak::Err(
                ExpressionError {
                    ctx: ExprCtx { span: Range { start: 0, end: 3 }, txt: line },
                    kind: ExpressionErrorKind::SeqInvalid(TokenKind::StringEnd(_)),
                },
                ErrFlow::Break(Recovery::Fail),
            )) if Rc::ptr_eq(&line, &txt)
        ));
        assert!(seq.is_empty());
    }

    #[test]
    fn comment() {
        let mut seq = Vec::new();
        let token = Token {
            kind: TokenKind::Comment,
            span: 0..3,
        };
        let txt = Rc::new(make_textline());

        let f = parse_sequence(&mut seq, token, &txt);

        assert!(matches!(f, ParseFlow::Continue(())));
        assert!(seq.is_empty());
    }

    #[test]
    fn comment_begin() {
        let mut seq = Vec::new();
        let token = Token {
            kind: TokenKind::CommentBlockBegin { depth: 0 },
            span: 3..6,
        };
        let txt = Rc::new(make_textline());

        let f = parse_sequence(&mut seq, token, &txt);

        assert!(matches!(
            f,
            ParseFlow::Break(ParseBreak::New(ParseNew {
                mode: ParseMode::CommentBlock,
                start: 3
            }))
        ));
        assert!(seq.is_empty());
    }

    #[test]
    fn start_bytevector() {
        let mut seq = Vec::new();
        let token = Token {
            kind: TokenKind::ByteVector,
            span: 3..6,
        };
        let txt = Rc::new(make_textline());

        let f = parse_sequence(&mut seq, token, &txt);

        assert!(matches!(
            f,
            ParseFlow::Break(ParseBreak::New(ParseNew {
                mode: ParseMode::ByteVector(vec),
                start: 3
            })) if vec.is_empty()
        ));
        assert!(seq.is_empty());
    }

    #[test]
    fn identifier() {
        let mut seq = Vec::new();
        let token = Token {
            kind: TokenKind::Identifier("myproc".to_owned()),
            span: 0..6,
        };
        let txt = Rc::new(make_textline());

        let f = parse_sequence(&mut seq, token, &txt);

        assert!(matches!(f, ParseFlow::Continue(())));
        assert_eq!(seq.len(), 1);
        assert!(matches!(
            &seq[0],
            Expression {
                ctx: ExprCtx { span: Range { start: 0, end: 6 }, txt: line },
                kind: ExpressionKind::Identifier(s),
            } if &**s == "myproc" && Rc::ptr_eq(&txt, &line)
        ));
    }

    #[test]
    fn empty_identifier() {
        let mut seq = Vec::new();
        let token = Token {
            kind: TokenKind::Identifier("".to_owned()),
            span: 0..0,
        };
        let txt = Rc::new(make_textline());

        let f = parse_sequence(&mut seq, token, &txt);

        assert!(matches!(f, ParseFlow::Continue(())));
        assert_eq!(seq.len(), 1);
        assert!(matches!(
            &seq[0],
            Expression {
                ctx: ExprCtx { span: Range { start: 0, end: 0 }, txt: line },
                kind: ExpressionKind::Identifier(s),
            } if &**s == "" && Rc::ptr_eq(&txt, &line)
        ));
    }

    #[test]
    fn start_identifier() {
        let mut seq = Vec::new();
        let token = Token {
            kind: TokenKind::IdentifierBegin("start".to_owned()),
            span: 3..8,
        };
        let txt = Rc::new(make_textline());

        let f = parse_sequence(&mut seq, token, &txt);

        assert!(matches!(
            f,
            ParseFlow::Break(ParseBreak::New(
                ParseNew {
                    mode: ParseMode::Identifier(s),
                    start: 3
                }
            )) if s == "start\n"
        ));
        assert!(seq.is_empty());
    }

    #[test]
    fn start_list() {
        let mut seq = Vec::new();
        let token = Token {
            kind: TokenKind::ParenLeft,
            span: 1..2,
        };
        let txt = Rc::new(make_textline());

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

        assert!(matches!(f, ParseFlow::Break(ParseBreak::Complete)));
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
        let txt = Rc::new(make_textline());

        let f = parse_list(&mut seq, token, &txt);

        assert!(matches!(f, ParseFlow::Break(ParseBreak::Complete)));
        assert!(seq.is_empty());
    }

    #[test]
    fn non_list_expression() {
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
            ParseFlow::Break(ParseBreak::Err(
                ExpressionError {
                    ctx: ExprCtx { span: Range { start: 6, end: 7 }, txt: line },
                    kind: ExpressionErrorKind::SeqInvalid(TokenKind::StringDiscard),
                },
                ErrFlow::Break(Recovery::Fail),
            )) if Rc::ptr_eq(&line, &txt)
        ));
        assert_eq!(seq.len(), 3);
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
        let txt = Rc::new(make_textline());

        let f = parse_str(&mut s, token, &txt);

        assert!(matches!(f, ParseFlow::Break(ParseBreak::Complete)));
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
        let txt = Rc::new(make_textline());

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
        let txt = Rc::new(make_textline());

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
        let txt = Rc::new(make_textline());

        let f = parse_str(&mut s, token, &txt);

        assert!(matches!(
            f,
            ParseFlow::Break(ParseBreak::Err(
                ExpressionError {
                    ctx: ExprCtx { span: Range { start: 4, end: 5 }, txt: line },
                    kind: ExpressionErrorKind::StrInvalid(TokenKind::ParenLeft),
                },
                ErrFlow::Break(Recovery::Fail),
            )) if Rc::ptr_eq(&line, &txt)
        ));
        assert_eq!(s, "start\n");
    }
}

mod comment {
    use super::*;

    #[test]
    fn end() {
        let token = Token {
            kind: TokenKind::CommentBlockEnd,
            span: 0..4,
        };
        let txt = Rc::new(make_textline());

        let f = parse_comment_block(token, &txt);

        assert!(matches!(f, ParseFlow::Break(ParseBreak::Complete)));
    }

    #[test]
    fn fragment() {
        let token = Token {
            kind: TokenKind::CommentBlockFragment { depth: 0 },
            span: 0..4,
        };
        let txt = Rc::new(make_textline());

        let f = parse_comment_block(token, &txt);

        assert!(matches!(f, ParseFlow::Continue(())));
    }

    #[test]
    fn unexpected_new_block() {
        let token = Token {
            kind: TokenKind::CommentBlockBegin { depth: 1 },
            span: 0..4,
        };
        let txt = Rc::new(make_textline());

        let f = parse_comment_block(token, &txt);

        assert!(matches!(
            f,
            ParseFlow::Break(ParseBreak::Err(
                ExpressionError {
                    ctx: ExprCtx { span: Range { start: 0, end: 4 }, txt: line },
                    kind: ExpressionErrorKind::CommentBlockInvalid(
                        TokenKind::CommentBlockBegin { .. }
                    ),
                },
                ErrFlow::Break(Recovery::Fail),
            )) if Rc::ptr_eq(&line, &txt)
        ));
    }

    #[test]
    fn invalid() {
        let token = Token {
            kind: TokenKind::ParenLeft,
            span: 0..1,
        };
        let txt = Rc::new(make_textline());

        let f = parse_comment_block(token, &txt);

        assert!(matches!(
            f,
            ParseFlow::Break(ParseBreak::Err(
                ExpressionError {
                    ctx: ExprCtx { span: Range { start: 0, end: 1 }, txt: line },
                    kind: ExpressionErrorKind::CommentBlockInvalid(TokenKind::ParenLeft),
                },
                ErrFlow::Break(Recovery::Fail),
            )) if Rc::ptr_eq(&line, &txt)
        ));
    }
}
