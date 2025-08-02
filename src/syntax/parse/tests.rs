mod form;

use super::*;
use crate::{
    testutil::{TestEnv, err_or_fail, extract_or_fail, make_textline, ok_or_fail, some_or_fail},
    txt::TxtSpan,
};

mod expr {
    use super::*;
    use crate::number::Real;

    #[test]
    fn boolean() {
        let token = Token {
            kind: TokenKind::Boolean(true),
            span: 0..3,
        };
        let txt = make_textline().into();
        let env = TestEnv::default();
        let ns = env.new_namespace();

        let f = parse_expr(token, &txt, false, &ns);

        assert!(matches!(
            f,
            ExprFlow::Continue(Some(
                Expression {
                ctx: ExprCtx { span: TxtSpan { start: 0, end: 3 }, txt: line },
                kind: ExpressionKind::Literal(Value::Boolean(true)),
            })) if Rc::ptr_eq(&txt, &line)
        ));
    }

    #[test]
    fn character() {
        let token = Token {
            kind: TokenKind::Character('a'),
            span: 0..3,
        };
        let txt = make_textline().into();
        let env = TestEnv::default();
        let ns = env.new_namespace();

        let f = parse_expr(token, &txt, false, &ns);

        assert!(matches!(
            f,
            ExprFlow::Continue(Some(
                Expression {
                ctx: ExprCtx { span: TxtSpan { start: 0, end: 3 }, txt: line },
                kind: ExpressionKind::Literal(Value::Character('a')),
            })) if Rc::ptr_eq(&txt, &line)
        ));
    }

    #[test]
    fn number() {
        let token = Token {
            kind: TokenKind::Number(Number::real(45)),
            span: 0..3,
        };
        let txt = make_textline().into();
        let env = TestEnv::default();
        let ns = env.new_namespace();

        let f = parse_expr(token, &txt, false, &ns);

        assert!(matches!(
            f,
            ExprFlow::Continue(Some(
                Expression {
                ctx: ExprCtx { span: TxtSpan { start: 0, end: 3 }, txt: line },
                kind: ExpressionKind::Literal(Value::Number(n)),
            })) if Rc::ptr_eq(&txt, &line) && n.to_string() == "45"
        ));
    }

    #[test]
    fn imaginary() {
        let token = Token {
            kind: TokenKind::Imaginary(Real::Float(1.2)),
            span: 0..3,
        };
        let txt = make_textline().into();
        let env = TestEnv::default();
        let ns = env.new_namespace();

        let f = parse_expr(token, &txt, false, &ns);

        assert!(matches!(
            f,
            ExprFlow::Continue(Some(
                Expression {
                ctx: ExprCtx { span: TxtSpan { start: 0, end: 3 }, txt: line },
                kind: ExpressionKind::Literal(Value::Number(n)),
            })) if n.to_string() == "+1.2i" && Rc::ptr_eq(&txt, &line)
        ));
    }

    #[test]
    fn string() {
        let token = Token {
            kind: TokenKind::String("foo".to_owned()),
            span: 0..3,
        };
        let txt = make_textline().into();
        let env = TestEnv::default();
        let ns = env.new_namespace();

        let f = parse_expr(token, &txt, false, &ns);

        assert!(matches!(
            f,
            ExprFlow::Continue(Some(
                Expression {
                ctx: ExprCtx { span: TxtSpan { start: 0, end: 3 }, txt: line },
                kind: ExpressionKind::Literal(Value::String(s)),
            })) if Rc::ptr_eq(&txt, &line) && s.as_ref() == "foo"
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
        let env = TestEnv::default();
        let ns = env.new_namespace();

        let f = parse_expr(token, &txt, false, &ns);

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
        let env = TestEnv::default();
        let ns = env.new_namespace();

        let f = parse_expr(token, &txt, false, &ns);

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
        let env = TestEnv::default();
        let ns = env.new_namespace();

        let f = parse_expr(token, &txt, false, &ns);

        assert!(matches!(
            f,
            ExprFlow::Break(ParseBreak::Err {
                err: ExpressionError {
                    ctx: ExprCtx { span: TxtSpan { start: 0, end: 3 }, txt: line },
                    kind: ExpressionErrorKind::SeqInvalid(TokenKind::StringEnd(_)),
                },
                flow: ParseErrFlow::Break(ParseErrBreak::InvalidTokenStream),
            }) if Rc::ptr_eq(&line, &txt)
        ));
    }

    #[test]
    fn invalid_pair_joiner() {
        let token = Token {
            kind: TokenKind::PairJoiner,
            span: 2..3,
        };
        let txt = make_textline().into();
        let env = TestEnv::default();
        let ns = env.new_namespace();

        let f = parse_expr(token, &txt, false, &ns);

        assert!(matches!(
            f,
            ExprFlow::Break(ParseBreak::Err {
                err: ExpressionError {
                    ctx: ExprCtx { span: TxtSpan { start: 2, end: 3 }, txt: line },
                    kind: ExpressionErrorKind::PairUnexpected,
                },
                flow: ParseErrFlow::Continue(()),
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
        let env = TestEnv::default();
        let ns = env.new_namespace();

        let f = parse_expr(token, &txt, false, &ns);

        assert!(matches!(f, ExprFlow::Continue(None)));
    }

    #[test]
    fn comment_begin() {
        let token = Token {
            kind: TokenKind::CommentBlockBegin { depth: 0 },
            span: 3..6,
        };
        let txt = make_textline().into();
        let env = TestEnv::default();
        let ns = env.new_namespace();

        let f = parse_expr(token, &txt, false, &ns);

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
        let env = TestEnv::default();
        let ns = env.new_namespace();

        let f = parse_expr(token, &txt, false, &ns);

        assert!(matches!(
            f,
            ExprFlow::Break(ParseBreak::New(ParseNew {
                mode: ParseMode::ByteVector(vec),
                start: 3
            })) if vec.is_empty()
        ));
    }

    #[test]
    fn variable() {
        let token = Token {
            kind: TokenKind::Identifier("myproc".to_owned()),
            span: 0..6,
        };
        let txt = make_textline().into();
        let env = TestEnv::default();
        let ns = env.new_namespace();

        let f = parse_expr(token, &txt, false, &ns);

        assert!(matches!(
            f,
            ExprFlow::Continue(Some(
                Expression {
                ctx: ExprCtx { span: TxtSpan { start: 0, end: 6 }, txt: line },
                kind: ExpressionKind::Variable(s),
            })) if s.as_ref() == "myproc" && Rc::ptr_eq(&txt, &line)
        ));
    }

    #[test]
    fn empty_variable() {
        let token = Token {
            kind: TokenKind::Identifier("".to_owned()),
            span: 0..0,
        };
        let txt = make_textline().into();
        let env = TestEnv::default();
        let ns = env.new_namespace();

        let f = parse_expr(token, &txt, false, &ns);

        assert!(matches!(
            f,
            ExprFlow::Continue(Some(
                Expression {
                ctx: ExprCtx { span: TxtSpan { start: 0, end: 0 }, txt: line },
                kind: ExpressionKind::Variable(s),
            })) if s.as_ref() == "" && Rc::ptr_eq(&txt, &line)
        ));
    }

    #[test]
    fn symbol() {
        let token = Token {
            kind: TokenKind::Identifier("foo".to_owned()),
            span: 0..6,
        };
        let txt = make_textline().into();
        let env = TestEnv::default();
        let ns = env.new_namespace();

        let f = parse_expr(token, &txt, true, &ns);

        assert!(matches!(
            f,
            ExprFlow::Continue(Some(
                Expression {
                ctx: ExprCtx { span: TxtSpan { start: 0, end: 6 }, txt: line },
                kind: ExpressionKind::Literal(Value::Symbol(s)),
            })) if s.as_ref() == "foo" && Rc::ptr_eq(&txt, &line)
        ));
    }

    #[test]
    fn empty_symbol() {
        let token = Token {
            kind: TokenKind::Identifier("".to_owned()),
            span: 0..0,
        };
        let txt = make_textline().into();
        let env = TestEnv::default();
        let ns = env.new_namespace();

        let f = parse_expr(token, &txt, true, &ns);

        assert!(matches!(
            f,
            ExprFlow::Continue(Some(
                Expression {
                ctx: ExprCtx { span: TxtSpan { start: 0, end: 0 }, txt: line },
                kind: ExpressionKind::Literal(Value::Symbol(s)),
            })) if s.as_ref() == "" && Rc::ptr_eq(&txt, &line)
        ));
    }

    #[test]
    fn start_identifier() {
        let token = Token {
            kind: TokenKind::IdentifierBegin("start".to_owned()),
            span: 3..8,
        };
        let txt = make_textline().into();
        let env = TestEnv::default();
        let ns = env.new_namespace();

        let f = parse_expr(token, &txt, false, &ns);

        assert!(matches!(
            f,
            ExprFlow::Break(ParseBreak::New(
                ParseNew {
                    mode: ParseMode::Identifier { name, quoted: false },
                    start: 3
                }
            )) if name == "start\n"
        ));
    }

    #[test]
    fn start_datum_identifier() {
        let token = Token {
            kind: TokenKind::IdentifierBegin("start".to_owned()),
            span: 3..8,
        };
        let txt = make_textline().into();
        let env = TestEnv::default();
        let ns = env.new_namespace();

        let f = parse_expr(token, &txt, true, &ns);

        assert!(matches!(
            f,
            ExprFlow::Break(ParseBreak::New(
                ParseNew {
                    mode: ParseMode::Identifier { name, quoted: true },
                    start: 3
                }
            )) if name == "start\n"
        ));
    }

    #[test]
    fn start_form() {
        let token = Token {
            kind: TokenKind::ParenLeft,
            span: 1..2,
        };
        let txt = make_textline().into();
        let env = TestEnv::default();
        let ns = env.new_namespace();

        let f = parse_expr(token, &txt, false, &ns);

        assert!(matches!(
            f,
            ExprFlow::Break(ParseBreak::New(
                ParseNew {
                    mode: ParseMode::List { form: SyntacticForm::Call, seq },
                    start: 1
                }
            )) if seq.is_empty()
        ));
    }

    #[test]
    fn start_list() {
        let token = Token {
            kind: TokenKind::ParenLeft,
            span: 1..2,
        };
        let txt = make_textline().into();
        let env = TestEnv::default();
        let ns = env.new_namespace();

        let f = parse_expr(token, &txt, true, &ns);

        assert!(matches!(
            f,
            ExprFlow::Break(ParseBreak::New(
                ParseNew {
                    mode: ParseMode::List { form: SyntacticForm::Datum, seq },
                    start: 1
                }
            )) if seq.is_empty()
        ));
    }

    #[test]
    fn start_comment_datum() {
        let token = Token {
            kind: TokenKind::CommentDatum,
            span: 1..2,
        };
        let txt = make_textline().into();
        let env = TestEnv::default();
        let ns = env.new_namespace();

        let f = parse_expr(token, &txt, false, &ns);

        assert!(matches!(
            f,
            ExprFlow::Break(ParseBreak::New(ParseNew {
                mode: ParseMode::CommentDatum(None),
                start: 1
            }))
        ));
    }

    #[test]
    fn start_quote() {
        let token = Token {
            kind: TokenKind::Quote,
            span: 1..2,
        };
        let txt = make_textline().into();
        let env = TestEnv::default();
        let ns = env.new_namespace();

        let f = parse_expr(token, &txt, false, &ns);

        assert!(matches!(
            f,
            ExprFlow::Break(ParseBreak::New(ParseNew {
                mode: ParseMode::Quote {
                    inner: None,
                    quoted: false,
                },
                start: 1
            }))
        ));
    }

    #[test]
    fn start_quoted_quote() {
        let token = Token {
            kind: TokenKind::Quote,
            span: 1..2,
        };
        let txt = make_textline().into();
        let env = TestEnv::default();
        let ns = env.new_namespace();

        let f = parse_expr(token, &txt, true, &ns);

        assert!(matches!(
            f,
            ExprFlow::Break(ParseBreak::New(ParseNew {
                mode: ParseMode::Quote {
                    inner: None,
                    quoted: true,
                },
                start: 1
            }))
        ));
    }

    #[test]
    fn start_vector() {
        let token = Token {
            kind: TokenKind::Vector,
            span: 1..2,
        };
        let txt = make_textline().into();
        let env = TestEnv::default();
        let ns = env.new_namespace();

        let f = parse_expr(token, &txt, false, &ns);

        assert!(matches!(
            f,
            ExprFlow::Break(ParseBreak::New(ParseNew {
                mode: ParseMode::Vector(v),
                start: 1
            })) if v.is_empty()
        ));
    }
}

mod datum {
    use super::*;

    #[test]
    fn simple() {
        let token = Token {
            kind: TokenKind::Boolean(true),
            span: 1..3,
        };
        let txt = make_textline().into();
        let mut inner = None;
        let ctx = ExprCtx {
            span: 0..1,
            txt: Rc::clone(&txt),
        };
        let env = TestEnv::default();
        let ns = env.new_namespace();

        let f = parse_datum(&mut inner, token, &txt, &ctx, &ns);

        assert!(matches!(
            f,
            ParseFlow::Break(ParseBreak::Complete(ExprEnd { lineno: 1, pos: 3 })),
        ));
        let expr = some_or_fail!(inner);
        assert!(matches!(
            expr,
            Expression {
                ctx: ExprCtx { span: TxtSpan { start: 1, end: 3 }, txt: line },
                kind: ExpressionKind::Literal(Value::Boolean(true))
            } if Rc::ptr_eq(&txt, &line)
        ));
    }

    #[test]
    fn compound() {
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
        let env = TestEnv::default();
        let ns = env.new_namespace();

        let f = parse_datum(&mut inner, token, &txt, &ctx, &ns);

        assert!(matches!(
            f,
            ParseFlow::Break(ParseBreak::New(ParseNew {
                mode: ParseMode::List { form: SyntacticForm::Datum, seq },
                start: 1
            })) if seq.is_empty(),
        ));
        assert!(inner.is_none());
    }

    #[test]
    fn no_expression() {
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
        let env = TestEnv::default();
        let ns = env.new_namespace();

        let f = parse_datum(&mut inner, token, &txt, &ctx, &ns);

        assert!(matches!(f, ParseFlow::Continue(())));
        assert!(inner.is_none());
    }

    #[test]
    fn invalid() {
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
        let env = TestEnv::default();
        let ns = env.new_namespace();

        let f = parse_datum(&mut inner, token, &txt, &ctx, &ns);

        assert!(matches!(
            f,
            ParseFlow::Break(ParseBreak::Err{
                err: ExpressionError {
                    ctx: ExprCtx { span: TxtSpan { start: 1, end: 2 }, txt: line },
                    kind: ExpressionErrorKind::SeqInvalid(TokenKind::StringDiscard),
                },
                flow: ParseErrFlow::Break(ParseErrBreak::InvalidTokenStream),
            }) if Rc::ptr_eq(&txt, &line),
        ));
        assert!(inner.is_none());
    }

    #[test]
    fn end_of_list() {
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
        let env = TestEnv::default();
        let ns = env.new_namespace();

        let f = parse_datum(&mut inner, token, &txt, &ctx, &ns);

        assert!(matches!(
            f,
            ParseFlow::Break(ParseBreak::Err{
                err: ExpressionError {
                    ctx: ExprCtx { span: TxtSpan { start: 0, end: 2 }, txt: line },
                    kind: ExpressionErrorKind::DatumExpected,
                },
                flow: ParseErrFlow::Break(ParseErrBreak::FailedNode),
            }) if Rc::ptr_eq(&txt, &line),
        ));
        assert!(inner.is_none());
    }
}

mod bytevector {
    use super::*;
    use crate::number::NumericError;

    #[test]
    fn byte() {
        let txt = make_textline().into();
        let seq = vec![
            ExprCtx {
                span: 0..3,
                txt: Rc::clone(&txt),
            }
            .into_expr(ExpressionKind::Literal(Value::real(24))),
        ];
        let node = ExprNode {
            ctx: ExprCtx {
                span: 0..3,
                txt: Rc::clone(&txt),
            },
            mode: ParseMode::ByteVector(seq),
        };
        let env = TestEnv::default();
        let ns = env.new_namespace();

        let r = node.try_into_expr(&ns);

        let expr = some_or_fail!(ok_or_fail!(r));
        assert!(matches!(
            expr,
            Expression {
                ctx: ExprCtx { span: TxtSpan { start: 0, end: 3 }, txt: line },
                kind: ExpressionKind::Literal(Value::ByteVector(_)),
            } if Rc::ptr_eq(&txt, &line)
        ));
        let bv = extract_or_fail!(
            extract_or_fail!(expr.kind, ExpressionKind::Literal),
            Value::ByteVector
        );
        assert_eq!(*bv, [24]);
    }

    #[test]
    fn multiple_bytes() {
        let txt = make_textline().into();
        let seq = vec![
            ExprCtx {
                span: 0..3,
                txt: Rc::clone(&txt),
            }
            .into_expr(ExpressionKind::Literal(Value::real(24))),
            ExprCtx {
                span: 3..6,
                txt: Rc::clone(&txt),
            }
            .into_expr(ExpressionKind::Literal(Value::real(25))),
            ExprCtx {
                span: 6..9,
                txt: Rc::clone(&txt),
            }
            .into_expr(ExpressionKind::Literal(Value::real(26))),
        ];
        let node = ExprNode {
            ctx: ExprCtx {
                span: 0..9,
                txt: Rc::clone(&txt),
            },
            mode: ParseMode::ByteVector(seq),
        };
        let env = TestEnv::default();
        let ns = env.new_namespace();

        let r = node.try_into_expr(&ns);

        let expr = some_or_fail!(ok_or_fail!(r));
        assert!(matches!(
            expr,
            Expression {
                ctx: ExprCtx { span: TxtSpan { start: 0, end: 9 }, txt: line },
                kind: ExpressionKind::Literal(Value::ByteVector(_)),
            } if Rc::ptr_eq(&txt, &line)
        ));
        let bv = extract_or_fail!(
            extract_or_fail!(expr.kind, ExpressionKind::Literal),
            Value::ByteVector
        );
        assert_eq!(*bv, [24, 25, 26]);
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
        let env = TestEnv::default();
        let ns = env.new_namespace();

        let r = node.try_into_expr(&ns);

        let expr = some_or_fail!(ok_or_fail!(r));
        assert!(matches!(
            expr,
            Expression {
                ctx: ExprCtx { span: TxtSpan { start: 0, end: 3 }, txt: line },
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
        let env = TestEnv::default();
        let seq = vec![
            ExprCtx {
                span: 0..3,
                txt: Rc::clone(&txt),
            }
            .into_expr(ExpressionKind::Literal(Value::real(24))),
            Expression::symbol(
                env.symbols.get("foo"),
                ExprCtx {
                    span: 3..6,
                    txt: Rc::clone(&txt),
                },
            ),
            ExprCtx {
                span: 6..9,
                txt: Rc::clone(&txt),
            }
            .into_expr(ExpressionKind::Literal(Value::real(26))),
        ];
        let node = ExprNode {
            ctx: ExprCtx {
                span: 0..9,
                txt: Rc::clone(&txt),
            },
            mode: ParseMode::ByteVector(seq),
        };
        let ns = env.new_namespace();

        let r = node.try_into_expr(&ns);

        let errs = err_or_fail!(r);
        assert_eq!(errs.len(), 1);
        assert!(matches!(
            &errs[0],
            ExpressionError {
                ctx: ExprCtx { span: TxtSpan { start: 3, end: 6 }, txt: line },
                kind: ExpressionErrorKind::ByteVectorInvalidItem(ExpressionKind::Literal(Value::Symbol(s))),
            } if s.as_ref() == "foo" && Rc::ptr_eq(&txt, &line)
        ));
    }

    #[test]
    fn invalid_items() {
        let env = TestEnv::default();
        let txt = make_textline().into();
        let seq = vec![
            ExprCtx {
                span: 0..3,
                txt: Rc::clone(&txt),
            }
            .into_expr(ExpressionKind::Literal(Value::real(24))),
            Expression::variable(
                env.symbols.get("foo"),
                ExprCtx {
                    span: 3..6,
                    txt: Rc::clone(&txt),
                },
            ),
            ExprCtx {
                span: 6..9,
                txt: Rc::clone(&txt),
            }
            .into_expr(ExpressionKind::Literal(Value::real(26))),
            ExprCtx {
                span: 9..12,
                txt: Rc::clone(&txt),
            }
            .into_expr(ExpressionKind::Literal(Value::real(1.2))),
        ];
        let node = ExprNode {
            ctx: ExprCtx {
                span: 0..12,
                txt: Rc::clone(&txt),
            },
            mode: ParseMode::ByteVector(seq),
        };
        let ns = env.new_namespace();

        let r = node.try_into_expr(&ns);

        let errs = err_or_fail!(r);
        assert_eq!(errs.len(), 2);
        assert!(matches!(
            &errs[0],
            ExpressionError {
                ctx: ExprCtx { span: TxtSpan { start: 3, end: 6 }, txt: line },
                kind: ExpressionErrorKind::ByteVectorInvalidItem(ExpressionKind::Variable(s)),
            } if s.as_ref() == "foo" && Rc::ptr_eq(&txt, &line)
        ));
        assert!(matches!(
            &errs[1],
            ExpressionError {
                ctx: ExprCtx { span: TxtSpan { start: 9, end: 12 }, txt: line },
                kind: ExpressionErrorKind::ByteVectorInvalidNumber(NumericError::IntConversionInvalidType(s)),
            } if s == "floating-point" && Rc::ptr_eq(&txt, &line)
        ));
    }
}

mod vector {
    use super::*;

    #[test]
    fn simple_item() {
        let txt = make_textline().into();
        let seq = vec![Expression::string(
            "foo",
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
            mode: ParseMode::Vector(seq),
        };
        let env = TestEnv::default();
        let ns = env.new_namespace();

        let r = node.try_into_expr(&ns);

        let expr = some_or_fail!(ok_or_fail!(r));
        assert!(matches!(
            expr,
            Expression {
                ctx: ExprCtx { span: TxtSpan { start: 0, end: 3 }, txt: line },
                kind: ExpressionKind::Literal(Value::Vector(_)),
            } if Rc::ptr_eq(&txt, &line)
        ));
        let v = extract_or_fail!(
            extract_or_fail!(expr.kind, ExpressionKind::Literal),
            Value::Vector
        );
        assert_eq!(v.len(), 1);
        assert!(matches!(&v[0], Value::String(s) if s.as_ref() == "foo"));
    }

    #[test]
    fn multiple_items() {
        let txt = make_textline().into();
        let env = TestEnv::default();
        let seq = vec![
            Expression::symbol(
                env.symbols.get("a"),
                ExprCtx {
                    span: 0..1,
                    txt: Rc::clone(&txt),
                },
            ),
            ExprCtx {
                span: 3..6,
                txt: Rc::clone(&txt),
            }
            .into_expr(ExpressionKind::Literal(Value::null())),
            ExprCtx {
                span: 6..9,
                txt: Rc::clone(&txt),
            }
            .into_expr(ExpressionKind::Literal(Value::real(26))),
        ];
        let node = ExprNode {
            ctx: ExprCtx {
                span: 0..9,
                txt: Rc::clone(&txt),
            },
            mode: ParseMode::Vector(seq),
        };
        let ns = env.new_namespace();

        let r = node.try_into_expr(&ns);

        let expr = some_or_fail!(ok_or_fail!(r));
        assert!(matches!(
            expr,
            Expression {
                ctx: ExprCtx { span: TxtSpan { start: 0, end: 9 }, txt: line },
                kind: ExpressionKind::Literal(Value::Vector(_)),
            } if Rc::ptr_eq(&txt, &line)
        ));
        let v = extract_or_fail!(
            extract_or_fail!(expr.kind, ExpressionKind::Literal),
            Value::Vector
        );
        assert_eq!(v.len(), 3);
        assert!(matches!(&v[0], Value::Symbol(s) if s.as_ref() == "a"));
        assert!(matches!(&v[1], Value::Pair(None)));
        assert!(matches!(
            &v[2],
            Value::Number(n) if n.to_string() == "26"
        ));
    }

    #[test]
    fn empty() {
        let txt = make_textline().into();
        let node = ExprNode {
            ctx: ExprCtx {
                span: 0..3,
                txt: Rc::clone(&txt),
            },
            mode: ParseMode::Vector(Vec::new()),
        };
        let env = TestEnv::default();
        let ns = env.new_namespace();

        let r = node.try_into_expr(&ns);

        let expr = some_or_fail!(ok_or_fail!(r));
        assert!(matches!(
            expr,
            Expression {
                ctx: ExprCtx { span: TxtSpan { start: 0, end: 3 }, txt: line },
                kind: ExpressionKind::Literal(Value::Vector(_)),
            } if Rc::ptr_eq(&txt, &line)
        ));
        let v = extract_or_fail!(
            extract_or_fail!(expr.kind, ExpressionKind::Literal),
            Value::Vector
        );
        assert!(v.is_empty());
    }

    #[test]
    fn invalid_item() {
        let txt = make_textline().into();
        let env = TestEnv::default();
        let seq = vec![
            Expression::symbol(
                env.symbols.get("a"),
                ExprCtx {
                    span: 0..1,
                    txt: Rc::clone(&txt),
                },
            ),
            Expression::variable(
                env.symbols.get("foo"),
                ExprCtx {
                    span: 3..6,
                    txt: Rc::clone(&txt),
                },
            ),
            ExprCtx {
                span: 6..9,
                txt: Rc::clone(&txt),
            }
            .into_expr(ExpressionKind::Literal(Value::real(26))),
        ];
        let node = ExprNode {
            ctx: ExprCtx {
                span: 0..9,
                txt: Rc::clone(&txt),
            },
            mode: ParseMode::Vector(seq),
        };
        let ns = env.new_namespace();

        let r = node.try_into_expr(&ns);

        let errs = err_or_fail!(r);
        assert_eq!(errs.len(), 1);
        assert!(matches!(
            &errs[0],
            ExpressionError {
                ctx: ExprCtx { span: TxtSpan { start: 3, end: 6 }, txt: line },
                kind: ExpressionErrorKind::VectorInvalidItem(ExpressionKind::Variable(s)),
            } if s.as_ref() == "foo" && Rc::ptr_eq(&txt, &line)
        ));
    }

    #[test]
    fn invalid_items() {
        let txt = make_textline().into();
        let env = TestEnv::default();
        let seq = vec![
            ExprCtx {
                span: 0..3,
                txt: Rc::clone(&txt),
            }
            .into_expr(ExpressionKind::Literal(Value::real(24))),
            Expression::variable(
                env.symbols.get("foo"),
                ExprCtx {
                    span: 3..6,
                    txt: Rc::clone(&txt),
                },
            ),
            ExprCtx {
                span: 6..9,
                txt: Rc::clone(&txt),
            }
            .into_expr(ExpressionKind::Literal(Value::real(26))),
            ExprCtx {
                span: 9..12,
                txt: Rc::clone(&txt),
            }
            .into_expr(ExpressionKind::Call {
                proc: Expression::variable(
                    env.symbols.get("bar"),
                    ExprCtx {
                        span: 9..12,
                        txt: Rc::clone(&txt),
                    },
                )
                .into(),
                args: [].into(),
            }),
        ];
        let node = ExprNode {
            ctx: ExprCtx {
                span: 0..12,
                txt: Rc::clone(&txt),
            },
            mode: ParseMode::Vector(seq),
        };
        let ns = env.new_namespace();

        let r = node.try_into_expr(&ns);

        let errs = err_or_fail!(r);
        assert_eq!(errs.len(), 2);
        assert!(matches!(
            &errs[0],
            ExpressionError {
                ctx: ExprCtx { span: TxtSpan { start: 3, end: 6 }, txt: line },
                kind: ExpressionErrorKind::VectorInvalidItem(ExpressionKind::Variable(s)),
            } if s.as_ref() == "foo" && Rc::ptr_eq(&txt, &line)
        ));
        assert!(matches!(
            &errs[1],
            ExpressionError {
                ctx: ExprCtx { span: TxtSpan { start: 9, end: 12 }, txt: line },
                kind: ExpressionErrorKind::VectorInvalidItem(ExpressionKind::Call{ .. }),
            } if Rc::ptr_eq(&txt, &line)
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
                    ctx: ExprCtx { span: TxtSpan { start: 4, end: 5 }, txt: line },
                    kind: ExpressionErrorKind::IdentifierInvalid(TokenKind::ParenLeft),
                },
                flow: ParseErrFlow::Break(ParseErrBreak::InvalidTokenStream),
            }) if Rc::ptr_eq(&line, &txt)
        ));
        assert_eq!(s, "start\n");
    }

    #[test]
    fn into_variable() {
        let txt = make_textline().into();
        let p = ExprNode {
            ctx: ExprCtx {
                span: 0..3,
                txt: Rc::clone(&txt),
            },
            mode: ParseMode::Identifier {
                name: "foo".to_owned(),
                quoted: false,
            },
        };
        let env = TestEnv::default();
        let ns = env.new_namespace();

        let r = p.try_into_expr(&ns);

        let expr = some_or_fail!(ok_or_fail!(r));
        assert!(matches!(
            expr,
            Expression {
                ctx: ExprCtx { span: TxtSpan { start: 0, end: 3 }, txt: line },
                kind: ExpressionKind::Variable(s),
            } if s.as_ref() == "foo" && Rc::ptr_eq(&txt, &line)
        ));
    }

    #[test]
    fn into_symbol() {
        let txt = make_textline().into();
        let p = ExprNode {
            ctx: ExprCtx {
                span: 0..3,
                txt: Rc::clone(&txt),
            },
            mode: ParseMode::Identifier {
                name: "foo".to_owned(),
                quoted: true,
            },
        };
        let env = TestEnv::default();
        let ns = env.new_namespace();

        let r = p.try_into_expr(&ns);

        let expr = some_or_fail!(ok_or_fail!(r));
        assert!(matches!(
            expr,
            Expression {
                ctx: ExprCtx { span: TxtSpan { start: 0, end: 3 }, txt: line },
                kind: ExpressionKind::Literal(Value::Symbol(s)),
            } if s.as_ref() == "foo" && Rc::ptr_eq(&txt, &line)
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
                    ctx: ExprCtx { span: TxtSpan { start: 4, end: 5 }, txt: line },
                    kind: ExpressionErrorKind::StrInvalid(TokenKind::ParenLeft),
                },
                flow: ParseErrFlow::Break(ParseErrBreak::InvalidTokenStream),
            }) if Rc::ptr_eq(&line, &txt)
        ));
        assert_eq!(s, "start\n");
    }

    #[test]
    fn into_expr() {
        let txt = make_textline().into();
        let p = ExprNode {
            ctx: ExprCtx {
                span: 0..3,
                txt: Rc::clone(&txt),
            },
            mode: ParseMode::StringLiteral("foo".to_owned()),
        };
        let env = TestEnv::default();
        let ns = env.new_namespace();

        let r = p.try_into_expr(&ns);

        let expr = some_or_fail!(ok_or_fail!(r));
        assert!(matches!(
            expr,
            Expression {
                ctx: ExprCtx { span: TxtSpan { start: 0, end: 3 }, txt: line },
                kind: ExpressionKind::Literal(Value::String(s)),
            } if s.as_ref() == "foo" && Rc::ptr_eq(&txt, &line)
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
                    ctx: ExprCtx { span: TxtSpan { start: 0, end: 4 }, txt: line },
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
                    ctx: ExprCtx { span: TxtSpan { start: 0, end: 1 }, txt: line },
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
        let env = TestEnv::default();
        let ns = env.new_namespace();

        let r = p.try_into_expr(&ns);

        let o = ok_or_fail!(r);
        assert!(o.is_none());
    }

    #[test]
    fn datum_into_expr() {
        let txt = make_textline().into();
        let env = TestEnv::default();
        let p = ExprNode {
            ctx: ExprCtx {
                span: 0..2,
                txt: Rc::clone(&txt),
            },
            mode: ParseMode::CommentDatum(Some(Expression::symbol(
                env.symbols.get("foo"),
                ExprCtx {
                    span: 3..5,
                    txt: Rc::clone(&txt),
                },
            ))),
        };
        let ns = env.new_namespace();

        let r = p.try_into_expr(&ns);

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
        let env = TestEnv::default();
        let ns = env.new_namespace();

        let r = p.try_into_expr(&ns);

        let errs = err_or_fail!(r);
        assert_eq!(errs.len(), 1);
        assert!(matches!(
            &errs[0],
            ExpressionError {
                ctx: ExprCtx { span: TxtSpan { start: 0, end: 2 }, txt },
                kind: ExpressionErrorKind::DatumExpected,
            } if txt.lineno == 1
        ));
    }
}

mod quote {
    use super::*;

    #[test]
    fn literal_into_expr() {
        let txt = make_textline().into();
        let p = ExprNode {
            ctx: ExprCtx {
                span: 0..1,
                txt: Rc::clone(&txt),
            },
            mode: ParseMode::Quote {
                inner: Some(
                    ExprCtx {
                        span: 2..4,
                        txt: Rc::clone(&txt),
                    }
                    .into_expr(ExpressionKind::Literal(Value::Boolean(true))),
                ),
                quoted: false,
            },
        };
        let env = TestEnv::default();
        let ns = env.new_namespace();

        let r = p.try_into_expr(&ns);

        let expr = some_or_fail!(ok_or_fail!(r));
        assert!(matches!(
            expr,
            Expression {
                ctx: ExprCtx {
                    span: TxtSpan { start: 2, end: 4 },
                    txt: line,
                },
                kind: ExpressionKind::Literal(Value::Boolean(true)),
            } if Rc::ptr_eq(&txt, &line)
        ));
    }

    #[test]
    fn symbol_into_expr() {
        let txt = make_textline().into();
        let env = TestEnv::default();
        let p = ExprNode {
            ctx: ExprCtx {
                span: 0..1,
                txt: Rc::clone(&txt),
            },
            mode: ParseMode::Quote {
                inner: Some(Expression::symbol(
                    env.symbols.get("foo"),
                    ExprCtx {
                        span: 2..5,
                        txt: Rc::clone(&txt),
                    },
                )),
                quoted: false,
            },
        };
        let ns = env.new_namespace();

        let r = p.try_into_expr(&ns);

        let expr = some_or_fail!(ok_or_fail!(r));
        assert!(matches!(
            expr,
            Expression {
                ctx: ExprCtx {
                    span: TxtSpan { start: 2, end: 5 },
                    txt: line,
                },
                kind: ExpressionKind::Literal(Value::Symbol(s)),
            } if Rc::ptr_eq(&txt, &line) && s.as_ref() == "foo"
        ));
    }

    #[test]
    fn quoted_literal_into_expr() {
        let txt = make_textline().into();
        let p = ExprNode {
            ctx: ExprCtx {
                span: 1..2,
                txt: Rc::clone(&txt),
            },
            mode: ParseMode::Quote {
                inner: Some(
                    ExprCtx {
                        span: 2..4,
                        txt: Rc::clone(&txt),
                    }
                    .into_expr(ExpressionKind::Literal(Value::Boolean(true))),
                ),
                quoted: true,
            },
        };
        let env = TestEnv::default();
        let ns = env.new_namespace();

        let r = p.try_into_expr(&ns);

        let expr = some_or_fail!(ok_or_fail!(r));
        assert!(matches!(
            expr,
            Expression {
                ctx: ExprCtx {
                    span: TxtSpan { start: 1, end: 2 },
                    txt: line,
                },
                kind: ExpressionKind::Literal(Value::Pair(Some(_))),
            } if Rc::ptr_eq(&txt, &line)
        ));
        let value = extract_or_fail!(expr.kind, ExpressionKind::Literal);
        assert_eq!(value.to_string(), "(quote #t)");
    }

    #[test]
    fn invalid_into_expr() {
        let txt = make_textline().into();
        let env = TestEnv::default();
        let p = ExprNode {
            ctx: ExprCtx {
                span: 0..2,
                txt: Rc::clone(&txt),
            },
            mode: ParseMode::Quote {
                inner: Some(
                    ExprCtx {
                        span: 3..10,
                        txt: Rc::clone(&txt),
                    }
                    .into_expr(ExpressionKind::Call {
                        proc: Expression::variable(
                            env.symbols.get("foo"),
                            ExprCtx {
                                span: 4..7,
                                txt: Rc::clone(&txt),
                            },
                        )
                        .into(),
                        args: [ExprCtx {
                            span: 7..9,
                            txt: Rc::clone(&txt),
                        }
                        .into_expr(ExpressionKind::Literal(Value::Boolean(true)))]
                        .into(),
                    }),
                ),
                quoted: false,
            },
        };
        let ns = env.new_namespace();

        let r = p.try_into_expr(&ns);

        let errs = err_or_fail!(r);
        assert_eq!(errs.len(), 1);
        assert!(matches!(
            &errs[0],
            ExpressionError {
                ctx: ExprCtx { span: TxtSpan { start: 0, end: 10 }, txt },
                kind: ExpressionErrorKind::DatumInvalid(ExpressionKind::Call { .. }),
            } if txt.lineno == 1
        ));
    }

    #[test]
    fn variable_into_expr() {
        let txt = make_textline().into();
        let env = TestEnv::default();
        let p = ExprNode {
            ctx: ExprCtx {
                span: 0..1,
                txt: Rc::clone(&txt),
            },
            mode: ParseMode::Quote {
                inner: Some(Expression::variable(
                    env.symbols.get("foo"),
                    ExprCtx {
                        span: 2..5,
                        txt: Rc::clone(&txt),
                    },
                )),
                quoted: false,
            },
        };
        let ns = env.new_namespace();

        let r = p.try_into_expr(&ns);

        let errs = err_or_fail!(r);
        assert_eq!(errs.len(), 1);
        assert!(matches!(
            &errs[0],
            ExpressionError {
                ctx: ExprCtx { span: TxtSpan { start: 0, end: 5 }, txt },
                kind: ExpressionErrorKind::DatumInvalid(ExpressionKind::Variable(s)),
            } if txt.lineno == 1 && s.as_ref() == "foo"
        ));
    }

    #[test]
    fn missing_into_expr() {
        let txt = make_textline().into();
        let p = ExprNode {
            ctx: ExprCtx {
                span: 0..2,
                txt: Rc::clone(&txt),
            },
            mode: ParseMode::Quote {
                inner: None,
                quoted: false,
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
                ctx: ExprCtx { span: TxtSpan { start: 0, end: 2 }, txt },
                kind: ExpressionErrorKind::DatumExpected,
            } if txt.lineno == 1
        ));
    }
}

mod program {
    use super::*;

    #[test]
    fn empty() {
        let mut seq = Vec::new();
        let token = Token {
            kind: TokenKind::Boolean(true),
            span: 0..3,
        };
        let txt = make_textline().into();
        let env = TestEnv::default();
        let ns = env.new_namespace();

        let f = parse_prg(&mut seq, token, &txt, &ns);

        assert!(matches!(f, ParseFlow::Continue(())));
        assert_eq!(seq.len(), 1);
        assert!(matches!(
            &seq[0],
            Expression {
                ctx: ExprCtx { span: TxtSpan { start: 0, end: 3 }, txt: line },
                kind: ExpressionKind::Literal(Value::Boolean(true)),
            } if Rc::ptr_eq(&txt, &line)
        ));
    }

    #[test]
    fn non_empty() {
        let txt = make_textline().into();
        let mut seq = vec![
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
            kind: TokenKind::Boolean(true),
            span: 6..9,
        };
        let txt = make_textline().into();
        let env = TestEnv::default();
        let ns = env.new_namespace();

        let f = parse_prg(&mut seq, token, &txt, &ns);

        assert!(matches!(f, ParseFlow::Continue(())));
        assert_eq!(seq.len(), 3);
        assert!(matches!(
            &seq[2],
            Expression {
                ctx: ExprCtx { span: TxtSpan { start: 6, end: 9 }, txt: line },
                kind: ExpressionKind::Literal(Value::Boolean(true)),
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
        let env = TestEnv::default();
        let ns = env.new_namespace();

        let f = parse_prg(&mut seq, token, &txt, &ns);

        assert!(matches!(
            f,
            ParseFlow::Break(ParseBreak::New(
                ParseNew {
                    mode: ParseMode::List { form: SyntacticForm::Call, seq },
                    start: 1
                }
            )) if seq.is_empty()
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
        let env = TestEnv::default();
        let ns = env.new_namespace();

        let f = parse_prg(&mut seq, token, &txt, &ns);

        assert!(matches!(
            f,
            ParseFlow::Break(ParseBreak::Err {
                err: ExpressionError {
                    ctx: ExprCtx { span: TxtSpan { start: 0, end: 3 }, txt: line },
                    kind: ExpressionErrorKind::SeqInvalid(TokenKind::StringEnd(_)),
                },
                flow: ParseErrFlow::Break(ParseErrBreak::InvalidTokenStream),
            }) if Rc::ptr_eq(&line, &txt)
        ));
        assert!(seq.is_empty());
    }

    #[test]
    fn node_to_program() {
        let txt = make_textline().into();
        let p = ParseNode::Prg(vec![
            ExprCtx {
                span: 0..3,
                txt: Rc::clone(&txt),
            }
            .into_expr(ExpressionKind::Literal(Value::real(24))),
        ]);

        let r = p.try_into();

        let prg: Sequence = ok_or_fail!(r);
        let seq = prg.iter().collect::<Vec<_>>();
        assert_eq!(seq.len(), 1);
        assert!(matches!(
            &seq[0],
            Expression {
                ctx: ExprCtx { span: TxtSpan { start: 0, end: 3 }, txt: line },
                kind: ExpressionKind::Literal(Value::Number(n)),
            } if n.to_string() == "24" && Rc::ptr_eq(&txt, &line)
        ));
    }

    #[test]
    fn parse_failure_node_to_program_error() {
        let p = ParseNode::InvalidParseTree(InvalidParseError::InvalidExprSource);

        let r: Result<Sequence, InvalidParseError> = p.try_into();

        let err = err_or_fail!(r);
        assert!(matches!(err, InvalidParseError::InvalidExprSource));
    }

    #[test]
    fn invalid_node_to_program_error() {
        let p = ParseNode::new(ParseMode::CommentBlock, 0, make_textline());

        let r: Result<Sequence, InvalidParseError> = p.try_into();

        let err = err_or_fail!(r);
        assert!(matches!(err, InvalidParseError::EndOfParse));
    }
}

mod merge {
    use super::*;

    #[test]
    fn prg_merge() {
        let txt = make_textline().into();
        let mut p = ParseNode::Prg(vec![
            ExprCtx {
                span: 0..1,
                txt: Rc::clone(&txt),
            }
            .into_expr(ExpressionKind::Literal(Value::Boolean(true))),
        ]);
        let other = ExprNode {
            ctx: ExprCtx {
                span: 0..3,
                txt: Rc::clone(&txt),
            },
            mode: ParseMode::Identifier {
                name: "foo".to_owned(),
                quoted: false,
            },
        };
        let env = TestEnv::default();
        let ns = env.new_namespace();

        let r = p.merge(other, &ns);

        assert!(matches!(r, Ok(MergeFlow::Continue(()))));
        let seq = extract_or_fail!(p, ParseNode::Prg);
        assert_eq!(seq.len(), 2);
        assert!(matches!(
            &seq[1],
            Expression {
                ctx: ExprCtx { span: TxtSpan { start: 0, end: 3 }, txt: line },
                kind: ExpressionKind::Variable(s),
            } if s.as_ref() == "foo" && Rc::ptr_eq(&txt, &line)
        ));
    }

    #[test]
    fn prg_merge_fail() {
        let txt = make_textline().into();
        let env = TestEnv::default();
        let mut p = ParseNode::prg();
        let other = ExprNode {
            ctx: ExprCtx {
                span: 0..3,
                txt: Rc::clone(&txt),
            },
            mode: ParseMode::ByteVector(vec![Expression::variable(
                env.symbols.get("foo"),
                ExprCtx {
                    span: 0..3,
                    txt: Rc::clone(&txt),
                },
            )]),
        };
        let ns = env.new_namespace();

        let r = p.merge(other, &ns);

        let errs = extract_or_fail!(err_or_fail!(r), ParserError::Syntax).0;
        assert_eq!(errs.len(), 1);
        assert!(matches!(
            &errs[0],
            ExpressionError {
                ctx: ExprCtx { span: TxtSpan { start: 0, end: 3 }, txt: line },
                kind: ExpressionErrorKind::ByteVectorInvalidItem(ExpressionKind::Variable(s)),
            } if s.as_ref() == "foo" && Rc::ptr_eq(&txt, &line)
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
            mode: ParseMode::Identifier {
                name: "foo".to_owned(),
                quoted: false,
            },
        };
        let env = TestEnv::default();
        let ns = env.new_namespace();

        let r = p.merge(other, &ns);

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
            mode: ParseMode::Identifier {
                name: "foo".to_owned(),
                quoted: false,
            },
        };
        let env = TestEnv::default();
        let ns = env.new_namespace();

        let r = p.merge(other, &ns);

        assert!(matches!(
            r,
            Err(ParserError::Invalid(InvalidParseError::InvalidExprTarget))
        ));
    }

    #[test]
    fn comment_datum_simple_merge() {
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
                span: 3..6,
                txt: Rc::clone(&txt),
            },
            mode: ParseMode::Identifier {
                name: "foo".to_owned(),
                quoted: true,
            },
        };
        let env = TestEnv::default();
        let ns = env.new_namespace();

        let r = p.merge(other, &ns);

        assert!(matches!(r, Ok(MergeFlow::Break(()))));
        let inner = some_or_fail!(extract_or_fail!(p.mode, ParseMode::CommentDatum));
        assert!(matches!(
            inner,
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
    fn comment_datum_compound_merge() {
        let txt = make_textline().into();
        let env = TestEnv::default();
        let mut p = ExprNode {
            ctx: ExprCtx {
                span: 0..3,
                txt: Rc::clone(&txt),
            },
            mode: ParseMode::CommentDatum(None),
        };
        let other = ExprNode {
            ctx: ExprCtx {
                span: 3..8,
                txt: Rc::clone(&txt),
            },
            mode: ParseMode::List {
                form: SyntacticForm::Datum,
                seq: vec![Expression::symbol(
                    env.symbols.get("foo"),
                    ExprCtx {
                        span: 4..7,
                        txt: Rc::clone(&txt),
                    },
                )],
            },
        };
        let ns = env.new_namespace();

        let r = p.merge(other, &ns);

        assert!(matches!(r, Ok(MergeFlow::Break(()))));
        let inner = some_or_fail!(extract_or_fail!(p.mode, ParseMode::CommentDatum));
        assert!(matches!(
            inner,
            Expression {
                ctx: ExprCtx {
                    span: TxtSpan { start: 3, end: 8 },
                    txt: line
                },
                kind: ExpressionKind::Literal(Value::Pair(Some(_))),
            } if Rc::ptr_eq(&txt, &line)
        ));
        let value = extract_or_fail!(inner.kind, ExpressionKind::Literal);
        assert_eq!(value.to_string(), "(foo)");
    }

    #[test]
    fn comment_datum_empty_compound_merge() {
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
                span: 3..5,
                txt: Rc::clone(&txt),
            },
            mode: ParseMode::List {
                form: SyntacticForm::Datum,
                seq: Vec::new(),
            },
        };
        let env = TestEnv::default();
        let ns = env.new_namespace();

        let r = p.merge(other, &ns);

        assert!(matches!(r, Ok(MergeFlow::Break(()))));
        let inner = some_or_fail!(extract_or_fail!(p.mode, ParseMode::CommentDatum));
        assert!(matches!(
            inner,
            Expression {
                ctx: ExprCtx {
                    span: TxtSpan { start: 3, end: 5 },
                    txt: line
                },
                kind: ExpressionKind::Literal(Value::Pair(None)),
            } if Rc::ptr_eq(&txt, &line)
        ));
        let value = extract_or_fail!(inner.kind, ExpressionKind::Literal);
        assert_eq!(value.to_string(), "()");
    }

    #[test]
    fn quote_simple_merge() {
        let txt = make_textline().into();
        let mut p = ExprNode {
            ctx: ExprCtx {
                span: 0..3,
                txt: Rc::clone(&txt),
            },
            mode: ParseMode::Quote {
                inner: None,
                quoted: false,
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
        let env = TestEnv::default();
        let ns = env.new_namespace();

        let r = p.merge(other, &ns);

        assert!(matches!(r, Ok(MergeFlow::Break(()))));
        let ParseMode::Quote {
            inner: Some(inner), ..
        } = p.mode
        else {
            unreachable!();
        };
        assert!(matches!(
            inner,
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
    fn quote_compound_merge() {
        let txt = make_textline().into();
        let env = TestEnv::default();
        let mut p = ExprNode {
            ctx: ExprCtx {
                span: 0..3,
                txt: Rc::clone(&txt),
            },
            mode: ParseMode::Quote {
                inner: None,
                quoted: false,
            },
        };
        let other = ExprNode {
            ctx: ExprCtx {
                span: 3..8,
                txt: Rc::clone(&txt),
            },
            mode: ParseMode::List {
                form: SyntacticForm::Datum,
                seq: vec![Expression::symbol(
                    env.symbols.get("foo"),
                    ExprCtx {
                        span: 4..7,
                        txt: Rc::clone(&txt),
                    },
                )],
            },
        };
        let ns = env.new_namespace();

        let r = p.merge(other, &ns);

        assert!(matches!(r, Ok(MergeFlow::Break(()))));
        let ParseMode::Quote {
            inner: Some(inner), ..
        } = p.mode
        else {
            unreachable!();
        };
        assert!(matches!(
            inner,
            Expression {
                ctx: ExprCtx {
                    span: TxtSpan { start: 3, end: 8 },
                    txt: line
                },
                kind: ExpressionKind::Literal(Value::Pair(Some(_))),
            } if Rc::ptr_eq(&txt, &line)
        ));
        let value = extract_or_fail!(inner.kind, ExpressionKind::Literal);
        assert_eq!(value.to_string(), "(foo)");
    }

    #[test]
    fn quote_empty_compound_merge() {
        let txt = make_textline().into();
        let mut p = ExprNode {
            ctx: ExprCtx {
                span: 0..3,
                txt: Rc::clone(&txt),
            },
            mode: ParseMode::Quote {
                inner: None,
                quoted: false,
            },
        };
        let other = ExprNode {
            ctx: ExprCtx {
                span: 3..5,
                txt: Rc::clone(&txt),
            },
            mode: ParseMode::List {
                form: SyntacticForm::Datum,
                seq: Vec::new(),
            },
        };
        let env = TestEnv::default();
        let ns = env.new_namespace();

        let r = p.merge(other, &ns);

        assert!(matches!(r, Ok(MergeFlow::Break(()))));
        let ParseMode::Quote {
            inner: Some(inner), ..
        } = p.mode
        else {
            unreachable!();
        };
        assert!(matches!(
            inner,
            Expression {
                ctx: ExprCtx {
                    span: TxtSpan { start: 3, end: 5 },
                    txt: line
                },
                kind: ExpressionKind::Literal(Value::Pair(None)),
            } if Rc::ptr_eq(&txt, &line)
        ));
        let value = extract_or_fail!(inner.kind, ExpressionKind::Literal);
        assert_eq!(value.to_string(), "()");
    }

    #[test]
    fn identifier_merge() {
        let txt = make_textline().into();
        let mut p = ExprNode {
            ctx: ExprCtx {
                span: 0..3,
                txt: Rc::clone(&txt),
            },
            mode: ParseMode::Identifier {
                name: "bar".to_owned(),
                quoted: false,
            },
        };
        let other = ExprNode {
            ctx: ExprCtx {
                span: 0..3,
                txt: Rc::clone(&txt),
            },
            mode: ParseMode::Identifier {
                name: "foo".to_owned(),
                quoted: false,
            },
        };
        let env = TestEnv::default();
        let ns = env.new_namespace();

        let r = p.merge(other, &ns);

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
            mode: ParseMode::Identifier {
                name: "foo".to_owned(),
                quoted: false,
            },
        };
        let env = TestEnv::default();
        let ns = env.new_namespace();

        let r = p.merge(other, &ns);

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
            mode: ParseMode::ByteVector(vec![
                ExprCtx {
                    span: 0..3,
                    txt: Rc::clone(&txt),
                }
                .into_expr(ExpressionKind::Literal(Value::real(24))),
            ]),
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
        let env = TestEnv::default();
        let ns = env.new_namespace();

        let r = p.merge(other, &ns);

        assert!(matches!(r, Ok(MergeFlow::Continue(()))));
        let seq = extract_or_fail!(p.mode, ParseMode::ByteVector);
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
    fn vector_merge() {
        let txt = make_textline().into();
        let mut p = ExprNode {
            ctx: ExprCtx {
                span: 0..3,
                txt: Rc::clone(&txt),
            },
            mode: ParseMode::Vector(vec![Expression::string(
                "bar",
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
            mode: ParseMode::Identifier {
                name: "foo".to_owned(),
                quoted: true,
            },
        };
        let env = TestEnv::default();
        let ns = env.new_namespace();

        let r = p.merge(other, &ns);

        assert!(matches!(r, Ok(MergeFlow::Continue(()))));
        let seq = extract_or_fail!(p.mode, ParseMode::Vector);
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
    fn bytevector_continuation() {
        let p = ParseNode::new(ParseMode::ByteVector(Vec::new()), 3, make_textline());

        let o = p.into_continuation_unsupported();

        let err = some_or_fail!(o);
        assert!(matches!(
            &err,
            ExpressionError {
                ctx: ExprCtx { span: TxtSpan { start: 3, end: 19 }, txt },
                kind: ExpressionErrorKind::ByteVectorUnterminated,
            } if txt.lineno == 1
        ));
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
                ctx: ExprCtx { span: TxtSpan { start: 3, end: 19 }, txt },
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
                ctx: ExprCtx { span: TxtSpan { start: 3, end: 19 }, txt },
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
                ctx: ExprCtx { span: TxtSpan { start: 3, end: 19 }, txt },
                kind: ExpressionErrorKind::DatumExpected,
            } if txt.lineno == 1
        ));
    }

    #[test]
    fn identifier_continuation() {
        let p = ParseNode::new(
            ParseMode::Identifier {
                name: "myproc".to_owned(),
                quoted: false,
            },
            3,
            make_textline(),
        );

        let o = p.into_continuation_unsupported();

        let err = some_or_fail!(o);
        assert!(matches!(
            &err,
            ExpressionError {
                ctx: ExprCtx { span: TxtSpan { start: 3, end: 19 }, txt },
                kind: ExpressionErrorKind::IdentifierUnterminated,
            } if txt.lineno == 1
        ));
    }

    #[test]
    fn list_continuation() {
        let txt = make_textline().into();
        let env = TestEnv::default();
        let p = ParseNode::new(
            ParseMode::List {
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
            3,
            Rc::clone(&txt),
        );

        let o = p.into_continuation_unsupported();

        let err = some_or_fail!(o);
        assert!(matches!(
            &err,
            ExpressionError {
                ctx: ExprCtx { span: TxtSpan { start: 3, end: 19 }, txt },
                kind: ExpressionErrorKind::ListUnterminated,
            } if txt.lineno == 1
        ));
    }

    #[test]
    fn quote_continuation() {
        let p = ParseNode::new(
            ParseMode::Quote {
                inner: None,
                quoted: false,
            },
            3,
            make_textline(),
        );

        let o = p.into_continuation_unsupported();

        let err = some_or_fail!(o);
        assert!(matches!(
            &err,
            ExpressionError {
                ctx: ExprCtx { span: TxtSpan { start: 3, end: 19 }, txt },
                kind: ExpressionErrorKind::DatumExpected,
            } if txt.lineno == 1
        ));
    }

    #[test]
    fn vector_continuation() {
        let p = ParseNode::new(ParseMode::Vector(Vec::new()), 3, make_textline());

        let o = p.into_continuation_unsupported();

        let err = some_or_fail!(o);
        assert!(matches!(
            &err,
            ExpressionError {
                ctx: ExprCtx { span: TxtSpan { start: 3, end: 19 }, txt },
                kind: ExpressionErrorKind::VectorUnterminated,
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
                    span: TxtSpan { start: 3, end: 8 },
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
                    span: TxtSpan { start: 3, end: 19 },
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
    fn invalid_node() {
        let p = ParseNode::InvalidTokenStream;

        assert!(p.is_invalid_parse());
    }

    #[test]
    fn not_invalid_node() {
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
        let env = TestEnv::default();
        let ns = env.new_namespace();

        let r = p.parse(token, &txt, &ns);

        assert!(r.is_continue());
    }
}
