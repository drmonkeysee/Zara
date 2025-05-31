use super::{expr::ExpressionErrorKind, *};
use crate::{
    lex::{Token, TokenKind},
    testutil::{err_or_fail, extract_or_fail, make_textline_no, some_or_fail},
    txt::LineNumber,
};

fn make_tokenline(kinds: impl IntoIterator<Item = TokenKind>) -> TokenLine {
    make_tokenline_no(kinds, 1)
}

fn make_tokenline_no(kinds: impl IntoIterator<Item = TokenKind>, lineno: LineNumber) -> TokenLine {
    let tokens = kinds.into_iter().enumerate().map(|(i, kind)| Token {
        kind,
        span: i..i + 1,
    });
    TokenLine(tokens.collect(), make_textline_no(lineno))
}

mod parsing {
    use super::*;
    use crate::{number::Number, testutil::ok_or_fail};

    #[test]
    fn no_tokens() {
        let mut et = ExpressionTree::default();
        let tokens = [].into();

        let r = et.parse(tokens);

        let prg = extract_or_fail!(ok_or_fail!(r), ParserOutput::Complete);
        let seq = prg.unwrap();
        assert!(seq.is_empty());
        assert!(et.parsers.is_empty());
    }

    #[test]
    fn single_literal_sequence() {
        let mut et = ExpressionTree::default();
        let tokens = [make_tokenline([TokenKind::Boolean(true)])];

        let r = et.parse(tokens.into());

        let prg = extract_or_fail!(ok_or_fail!(r), ParserOutput::Complete);
        let seq = prg.unwrap();
        assert_eq!(seq.len(), 1);
        assert!(matches!(
            &seq[0],
            Expression {
                ctx: ExprCtx { span: TxtSpan { start: 0, end: 1 }, txt },
                kind: ExpressionKind::Literal(Value::Boolean(true)),
            } if txt.lineno == 1
        ));
        assert!(et.parsers.is_empty());
    }

    #[test]
    fn multiple_literals_sequence() {
        let mut et = ExpressionTree::default();
        let tokens = [make_tokenline([
            TokenKind::Boolean(true),
            TokenKind::Character('a'),
            TokenKind::String("foo".into()),
        ])];

        let r = et.parse(tokens.into());

        let prg = extract_or_fail!(ok_or_fail!(r), ParserOutput::Complete);
        let seq = prg.unwrap();
        assert_eq!(seq.len(), 3);
        assert!(matches!(
            &seq[0],
            Expression {
                ctx: ExprCtx { span: TxtSpan { start: 0, end: 1 }, txt },
                kind: ExpressionKind::Literal(Value::Boolean(true)),
            } if txt.lineno == 1
        ));
        assert!(matches!(
            &seq[1],
            Expression {
                ctx: ExprCtx { span: TxtSpan { start: 1, end: 2 }, txt },
                kind: ExpressionKind::Literal(Value::Character('a')),
            } if txt.lineno == 1
        ));
        assert!(matches!(
            &seq[2],
            Expression {
                ctx: ExprCtx { span: TxtSpan { start: 2, end: 3 }, txt },
                kind: ExpressionKind::Literal(Value::String(s)),
            } if txt.lineno == 1 && &**s == "foo"
        ));
        assert!(et.parsers.is_empty());
    }

    #[test]
    fn sequence_multiple_lines() {
        let mut et = ExpressionTree::default();
        let tokens = [
            make_tokenline_no(
                [
                    TokenKind::Boolean(true),
                    TokenKind::Character('a'),
                    TokenKind::String("foo".into()),
                ],
                1,
            ),
            make_tokenline_no([TokenKind::Boolean(false), TokenKind::Character('b')], 2),
        ];

        let r = et.parse(tokens.into());

        let prg = extract_or_fail!(ok_or_fail!(r), ParserOutput::Complete);
        let seq = prg.unwrap();
        assert_eq!(seq.len(), 5);
        assert!(matches!(
            &seq[0],
            Expression {
                ctx: ExprCtx { span: TxtSpan { start: 0, end: 1 }, txt },
                kind: ExpressionKind::Literal(Value::Boolean(true)),
            } if txt.lineno == 1
        ));
        assert!(matches!(
            &seq[1],
            Expression {
                ctx: ExprCtx { span: TxtSpan { start: 1, end: 2 }, txt },
                kind: ExpressionKind::Literal(Value::Character('a')),
            } if txt.lineno == 1
        ));
        assert!(matches!(
            &seq[2],
            Expression {
                ctx: ExprCtx { span: TxtSpan { start: 2, end: 3 }, txt },
                kind: ExpressionKind::Literal(Value::String(s)),
            } if txt.lineno == 1 && &**s == "foo"
        ));
        assert!(matches!(
            &seq[3],
            Expression {
                ctx: ExprCtx { span: TxtSpan { start: 0, end: 1 }, txt },
                kind: ExpressionKind::Literal(Value::Boolean(false)),
            } if txt.lineno == 2
        ));
        assert!(matches!(
            &seq[4],
            Expression {
                ctx: ExprCtx { span: TxtSpan { start: 1, end: 2 }, txt },
                kind: ExpressionKind::Literal(Value::Character('b')),
            } if txt.lineno == 2
        ));

        assert!(et.parsers.is_empty());
    }

    #[test]
    fn datum_comments_stack() {
        let mut et = ExpressionTree::default();
        // NOTE: #u8(10 #; #; 11 12 13) -> #u8(10 13)
        let tokens = [make_tokenline([
            TokenKind::ByteVector,
            TokenKind::Number(Number::real(10)),
            TokenKind::CommentDatum,
            TokenKind::CommentDatum,
            TokenKind::Number(Number::real(11)),
            TokenKind::Number(Number::real(12)),
            TokenKind::Number(Number::real(13)),
            TokenKind::ParenRight,
        ])];

        let r = et.parse(tokens.into());

        let prg = extract_or_fail!(ok_or_fail!(r), ParserOutput::Complete);
        let seq = prg.unwrap();
        assert_eq!(seq.len(), 1);
        assert!(matches!(
            &seq[0],
            Expression {
                ctx: ExprCtx { span: TxtSpan { start: 0, end: 8 }, txt },
                kind: ExpressionKind::Literal(Value::ByteVector(bv)),
            } if txt.lineno == 1 && format!("{bv:?}") == "[10, 13]"
        ));
        assert!(et.parsers.is_empty());
        assert!(et.errs.is_empty());
    }

    #[test]
    fn datum_comment_allows_otherwise_invalid_syntax() {
        let mut et = ExpressionTree::default();
        // NOTE: (+ #; (if 5) 2 3) -> (+ 2 3)
        let tokens = [make_tokenline([
            TokenKind::ParenLeft,
            TokenKind::Identifier("+".to_owned()),
            TokenKind::CommentDatum,
            TokenKind::ParenLeft,
            TokenKind::Identifier("if".to_owned()),
            TokenKind::Number(Number::real(5)),
            TokenKind::ParenRight,
            TokenKind::Number(Number::real(2)),
            TokenKind::Number(Number::real(3)),
            TokenKind::ParenRight,
        ])];

        let r = et.parse(tokens.into());

        let prg = extract_or_fail!(ok_or_fail!(r), ParserOutput::Complete);
        let seq = prg.unwrap();
        assert_eq!(seq.len(), 1);
        let expr = &seq[0];
        assert!(matches!(
            expr,
            Expression {
                ctx: ExprCtx {
                    span: TxtSpan { start: 0, end: 10 },
                    ..
                },
                kind: ExpressionKind::Call { .. },
            }
        ));
        let ExpressionKind::Call { args, proc } = &expr.kind else {
            unreachable!();
        };
        assert!(matches!(
            &**proc,
            Expression {
                ctx: ExprCtx { span: TxtSpan { start: 1, end: 2 }, .. },
                kind: ExpressionKind::Variable(s),
            } if &**s == "+"
        ));
        assert_eq!(args.len(), 2);
        assert!(matches!(
            &args[0],
            Expression {
                ctx: ExprCtx { span: TxtSpan { start: 7, end: 8 }, .. },
                kind: ExpressionKind::Literal(Value::Number(n)),
            } if n.to_string() == "2"
        ));
        assert!(matches!(
            &args[1],
            Expression {
                ctx: ExprCtx { span: TxtSpan { start: 8, end: 9 }, .. },
                kind: ExpressionKind::Literal(Value::Number(n)),
            } if n.to_string() == "3"
        ));
        assert!(et.parsers.is_empty());
        assert!(et.errs.is_empty());
    }

    #[test]
    fn quoting_quote() {
        let mut et = ExpressionTree::default();
        // NOTE: ''a -> (quote a)
        let tokens = [make_tokenline([
            TokenKind::Quote,
            TokenKind::Quote,
            TokenKind::Identifier("a".to_owned()),
        ])];

        let r = et.parse(tokens.into());

        let prg = extract_or_fail!(ok_or_fail!(r), ParserOutput::Complete);
        let seq = prg.unwrap();
        assert_eq!(seq.len(), 1);
        assert!(matches!(
            &seq[0],
            Expression {
                ctx: ExprCtx { span: TxtSpan { start: 1, end: 3 }, txt },
                kind: ExpressionKind::Literal(Value::Pair(Some(_))),
            } if txt.lineno == 1
        ));
        let value = extract_or_fail!(&seq[0].kind, ExpressionKind::Literal);
        assert_eq!(value.to_string(), "(quote a)");

        assert!(et.parsers.is_empty());
        assert!(et.errs.is_empty());
    }

    #[test]
    #[ignore = "quasiquote quoting shortforms not implemented yet"]
    fn quoting_all_the_shortforms() {
        let mut et = ExpressionTree::default();
        // NOTE: '`(a ,(+ 1 2) ,@(map abs '(-1 -2 -3)))
        //  -> (quasiquote (a (unquote (+ 1 2)) (unquote-splicing (map abs (quote (-1 -2 -3))))))
        let tokens = [make_tokenline([
            TokenKind::Quote,
            TokenKind::Quasiquote,
            TokenKind::ParenLeft,
            TokenKind::Identifier("a".to_owned()),
            TokenKind::Unquote,
            TokenKind::ParenLeft,
            TokenKind::Identifier("+".to_owned()),
            TokenKind::Number(Number::real(1)),
            TokenKind::Number(Number::real(2)),
            TokenKind::ParenRight,
            TokenKind::UnquoteSplice,
            TokenKind::ParenLeft,
            TokenKind::Identifier("map".to_owned()),
            TokenKind::Identifier("abs".to_owned()),
            TokenKind::Quote,
            TokenKind::ParenLeft,
            TokenKind::Number(Number::real(-1)),
            TokenKind::Number(Number::real(-2)),
            TokenKind::Number(Number::real(-3)),
            TokenKind::ParenRight,
            TokenKind::ParenRight,
            TokenKind::ParenRight,
        ])];

        let r = et.parse(tokens.into());

        let prg = extract_or_fail!(ok_or_fail!(r), ParserOutput::Complete);
        let seq = prg.unwrap();
        assert!(matches!(
            &seq[0],
            Expression {
                ctx: ExprCtx { span: TxtSpan { start: 1, end: 3 }, txt },
                kind: ExpressionKind::Literal(Value::Pair(Some(_))),
            } if txt.lineno == 1
        ));
        let value = extract_or_fail!(&seq[0].kind, ExpressionKind::Literal);
        assert_eq!(
            value.to_string(),
            "(quasiquote (a (unquote (+ 1 2)) (unquote-splicing (map abs (quote (-1 -2 -3))))))"
        );

        assert!(et.parsers.is_empty());
        assert!(et.errs.is_empty());
    }

    #[test]
    fn longform_quote_with_otherwise_invalid_syntax() {
        let mut et = ExpressionTree::default();
        // NOTE: (quote (if 5)) -> (if 5)
        let tokens = [make_tokenline([
            TokenKind::ParenLeft,
            TokenKind::Identifier("quote".to_owned()),
            TokenKind::ParenLeft,
            TokenKind::Identifier("if".to_owned()),
            TokenKind::Number(Number::real(5)),
            TokenKind::ParenRight,
            TokenKind::ParenRight,
        ])];

        let r = et.parse(tokens.into());

        let prg = extract_or_fail!(ok_or_fail!(r), ParserOutput::Complete);
        let seq = prg.unwrap();
        assert_eq!(seq.len(), 1);
        assert!(matches!(
            &seq[0],
            Expression {
                ctx: ExprCtx { span: TxtSpan { start: 2, end: 6 }, txt },
                kind: ExpressionKind::Literal(Value::Pair(Some(_))),
            } if txt.lineno == 1
        ));
        let value = extract_or_fail!(&seq[0].kind, ExpressionKind::Literal);
        assert_eq!(value.to_string(), "(if 5)");

        assert!(et.parsers.is_empty());
        assert!(et.errs.is_empty());
    }

    #[test]
    fn vector_with_otherwise_invalid_syntax() {
        let mut et = ExpressionTree::default();
        // NOTE: #(if 5) -> #(if 5)
        let tokens = [make_tokenline([
            TokenKind::Vector,
            TokenKind::Identifier("if".to_owned()),
            TokenKind::Number(Number::real(5)),
            TokenKind::ParenRight,
        ])];

        let r = et.parse(tokens.into());

        let prg = extract_or_fail!(ok_or_fail!(r), ParserOutput::Complete);
        let seq = prg.unwrap();
        assert_eq!(seq.len(), 1);
        assert!(matches!(
            &seq[0],
            Expression {
                ctx: ExprCtx { span: TxtSpan { start: 0, end: 4 }, txt },
                kind: ExpressionKind::Literal(Value::Vector(_)),
            } if txt.lineno == 1
        ));
        let value = extract_or_fail!(&seq[0].kind, ExpressionKind::Literal);
        assert_eq!(value.to_string(), "#(if 5)");

        assert!(et.parsers.is_empty());
        assert!(et.errs.is_empty());
    }

    #[test]
    fn standard_list() {
        let mut et = ExpressionTree::default();
        let tokens = [make_tokenline([
            TokenKind::Quote,
            TokenKind::ParenLeft,
            TokenKind::Identifier("a".to_owned()),
            TokenKind::Identifier("b".to_owned()),
            TokenKind::Identifier("c".to_owned()),
            TokenKind::ParenRight,
        ])];

        let r = et.parse(tokens.into());

        let prg = extract_or_fail!(ok_or_fail!(r), ParserOutput::Complete);
        let seq = prg.unwrap();
        assert_eq!(seq.len(), 1);
        assert!(matches!(
            &seq[0],
            Expression {
                ctx: ExprCtx { span: TxtSpan { start: 1, end: 6 }, txt },
                kind: ExpressionKind::Literal(Value::Pair(Some(_))),
            } if txt.lineno == 1
        ));
        let value = extract_or_fail!(&seq[0].kind, ExpressionKind::Literal);
        assert_eq!(value.to_string(), "(a b c)");

        assert!(et.parsers.is_empty());
        assert!(et.errs.is_empty());
    }

    #[test]
    fn empty_list() {
        let mut et = ExpressionTree::default();
        let tokens = [make_tokenline([
            TokenKind::Quote,
            TokenKind::ParenLeft,
            TokenKind::ParenRight,
        ])];

        let r = et.parse(tokens.into());

        let prg = extract_or_fail!(ok_or_fail!(r), ParserOutput::Complete);
        let seq = prg.unwrap();
        assert_eq!(seq.len(), 1);
        assert!(matches!(
            &seq[0],
            Expression {
                ctx: ExprCtx { span: TxtSpan { start: 1, end: 3 }, txt },
                kind: ExpressionKind::Literal(Value::Pair(None)),
            } if txt.lineno == 1
        ));
        let value = extract_or_fail!(&seq[0].kind, ExpressionKind::Literal);
        assert_eq!(value.to_string(), "()");

        assert!(et.parsers.is_empty());
        assert!(et.errs.is_empty());
    }

    #[test]
    fn standard_pair() {
        let mut et = ExpressionTree::default();
        let tokens = [make_tokenline([
            TokenKind::Quote,
            TokenKind::ParenLeft,
            TokenKind::Identifier("a".to_owned()),
            TokenKind::PairJoiner,
            TokenKind::Identifier("b".to_owned()),
            TokenKind::ParenRight,
        ])];

        let r = et.parse(tokens.into());

        let prg = extract_or_fail!(ok_or_fail!(r), ParserOutput::Complete);
        let seq = prg.unwrap();
        assert_eq!(seq.len(), 1);
        assert!(matches!(
            &seq[0],
            Expression {
                ctx: ExprCtx { span: TxtSpan { start: 1, end: 6 }, txt },
                kind: ExpressionKind::Literal(Value::Pair(Some(_))),
            } if txt.lineno == 1
        ));
        let value = extract_or_fail!(&seq[0].kind, ExpressionKind::Literal);
        assert_eq!(value.to_string(), "(a . b)");

        assert!(et.parsers.is_empty());
        assert!(et.errs.is_empty());
    }

    #[test]
    fn improper_list() {
        let mut et = ExpressionTree::default();
        let tokens = [make_tokenline([
            TokenKind::Quote,
            TokenKind::ParenLeft,
            TokenKind::Identifier("a".to_owned()),
            TokenKind::Identifier("b".to_owned()),
            TokenKind::PairJoiner,
            TokenKind::Identifier("c".to_owned()),
            TokenKind::ParenRight,
        ])];

        let r = et.parse(tokens.into());

        let prg = extract_or_fail!(ok_or_fail!(r), ParserOutput::Complete);
        let seq = prg.unwrap();
        assert_eq!(seq.len(), 1);
        assert!(matches!(
            &seq[0],
            Expression {
                ctx: ExprCtx { span: TxtSpan { start: 1, end: 7 }, txt },
                kind: ExpressionKind::Literal(Value::Pair(Some(_))),
            } if txt.lineno == 1
        ));
        let value = extract_or_fail!(&seq[0].kind, ExpressionKind::Literal);
        assert_eq!(value.to_string(), "(a b . c)");

        assert!(et.parsers.is_empty());
        assert!(et.errs.is_empty());
    }

    #[test]
    fn pair_with_list_cdr() {
        let mut et = ExpressionTree::default();
        // NOTE: '(a . (b c)) -> (a b c)
        let tokens = [make_tokenline([
            TokenKind::Quote,
            TokenKind::ParenLeft,
            TokenKind::Identifier("a".to_owned()),
            TokenKind::PairJoiner,
            TokenKind::ParenLeft,
            TokenKind::Identifier("b".to_owned()),
            TokenKind::Identifier("c".to_owned()),
            TokenKind::ParenRight,
            TokenKind::ParenRight,
        ])];

        let r = et.parse(tokens.into());

        let prg = extract_or_fail!(ok_or_fail!(r), ParserOutput::Complete);
        let seq = prg.unwrap();
        assert_eq!(seq.len(), 1);
        assert!(matches!(
            &seq[0],
            Expression {
                ctx: ExprCtx { span: TxtSpan { start: 1, end: 9 }, txt },
                kind: ExpressionKind::Literal(Value::Pair(Some(_))),
            } if txt.lineno == 1
        ));
        let value = extract_or_fail!(&seq[0].kind, ExpressionKind::Literal);
        assert_eq!(value.to_string(), "(a b c)");

        assert!(et.parsers.is_empty());
        assert!(et.errs.is_empty());
    }

    #[test]
    fn pair_notation_list() {
        let mut et = ExpressionTree::default();
        // NOTE: '(a . (b . (c . ()))) -> (a b c)
        let tokens = [make_tokenline([
            TokenKind::Quote,
            TokenKind::ParenLeft,
            TokenKind::Identifier("a".to_owned()),
            TokenKind::PairJoiner,
            TokenKind::ParenLeft,
            TokenKind::Identifier("b".to_owned()),
            TokenKind::PairJoiner,
            TokenKind::ParenLeft,
            TokenKind::Identifier("c".to_owned()),
            TokenKind::PairJoiner,
            TokenKind::ParenLeft,
            TokenKind::ParenRight,
            TokenKind::ParenRight,
            TokenKind::ParenRight,
            TokenKind::ParenRight,
        ])];

        let r = et.parse(tokens.into());

        let prg = extract_or_fail!(ok_or_fail!(r), ParserOutput::Complete);
        let seq = prg.unwrap();
        assert_eq!(seq.len(), 1);
        assert!(matches!(
            &seq[0],
            Expression {
                ctx: ExprCtx { span: TxtSpan { start: 1, end: 15 }, txt },
                kind: ExpressionKind::Literal(Value::Pair(Some(_))),
            } if txt.lineno == 1
        ));
        let value = extract_or_fail!(&seq[0].kind, ExpressionKind::Literal);
        assert_eq!(value.to_string(), "(a b c)");

        assert!(et.parsers.is_empty());
        assert!(et.errs.is_empty());
    }

    #[test]
    fn list_of_pair() {
        let mut et = ExpressionTree::default();
        let tokens = [make_tokenline([
            TokenKind::Quote,
            TokenKind::ParenLeft,
            TokenKind::ParenLeft,
            TokenKind::Identifier("a".to_owned()),
            TokenKind::PairJoiner,
            TokenKind::Identifier("b".to_owned()),
            TokenKind::ParenRight,
            TokenKind::ParenRight,
        ])];

        let r = et.parse(tokens.into());

        let prg = extract_or_fail!(ok_or_fail!(r), ParserOutput::Complete);
        let seq = prg.unwrap();
        assert_eq!(seq.len(), 1);
        assert!(matches!(
            &seq[0],
            Expression {
                ctx: ExprCtx { span: TxtSpan { start: 1, end: 8 }, txt },
                kind: ExpressionKind::Literal(Value::Pair(Some(_))),
            } if txt.lineno == 1
        ));
        let value = extract_or_fail!(&seq[0].kind, ExpressionKind::Literal);
        assert_eq!(value.to_string(), "((a . b))");

        assert!(et.parsers.is_empty());
        assert!(et.errs.is_empty());
    }

    #[test]
    fn pair_ignores_datum_comment() {
        let mut et = ExpressionTree::default();
        // NOTE: '(a . #; b c) -> (a . c)
        let tokens = [make_tokenline([
            TokenKind::Quote,
            TokenKind::ParenLeft,
            TokenKind::Identifier("a".to_owned()),
            TokenKind::PairJoiner,
            TokenKind::CommentDatum,
            TokenKind::Identifier("b".to_owned()),
            TokenKind::Identifier("c".to_owned()),
            TokenKind::ParenRight,
        ])];

        let r = et.parse(tokens.into());

        let prg = extract_or_fail!(ok_or_fail!(r), ParserOutput::Complete);
        let seq = prg.unwrap();
        assert_eq!(seq.len(), 1);
        assert!(matches!(
            &seq[0],
            Expression {
                ctx: ExprCtx { span: TxtSpan { start: 1, end: 8 }, txt },
                kind: ExpressionKind::Literal(Value::Pair(Some(_))),
            } if txt.lineno == 1
        ));
        let value = extract_or_fail!(&seq[0].kind, ExpressionKind::Literal);
        assert_eq!(value.to_string(), "(a . c)");

        assert!(et.parsers.is_empty());
        assert!(et.errs.is_empty());
    }

    #[test]
    fn pair_ignores_compound_datum_comment() {
        let mut et = ExpressionTree::default();
        // NOTE: '(a . #; (b c) d) -> (a . d)
        let tokens = [make_tokenline([
            TokenKind::Quote,
            TokenKind::ParenLeft,
            TokenKind::Identifier("a".to_owned()),
            TokenKind::PairJoiner,
            TokenKind::CommentDatum,
            TokenKind::ParenLeft,
            TokenKind::Identifier("b".to_owned()),
            TokenKind::Identifier("c".to_owned()),
            TokenKind::ParenRight,
            TokenKind::Identifier("d".to_owned()),
            TokenKind::ParenRight,
        ])];

        let r = et.parse(tokens.into());

        let prg = extract_or_fail!(ok_or_fail!(r), ParserOutput::Complete);
        let seq = prg.unwrap();
        assert_eq!(seq.len(), 1);
        assert!(matches!(
            &seq[0],
            Expression {
                ctx: ExprCtx { span: TxtSpan { start: 1, end: 11 }, txt },
                kind: ExpressionKind::Literal(Value::Pair(Some(_))),
            } if txt.lineno == 1
        ));
        let value = extract_or_fail!(&seq[0].kind, ExpressionKind::Literal);
        assert_eq!(value.to_string(), "(a . d)");

        assert!(et.parsers.is_empty());
        assert!(et.errs.is_empty());
    }

    #[test]
    fn pair_ignores_comment_block() {
        let mut et = ExpressionTree::default();
        // NOTE: '(a . #| foo |# b) -> (a . b)
        let tokens = [make_tokenline([
            TokenKind::Quote,
            TokenKind::ParenLeft,
            TokenKind::Identifier("a".to_owned()),
            TokenKind::PairJoiner,
            TokenKind::Comment,
            TokenKind::Identifier("b".to_owned()),
            TokenKind::ParenRight,
        ])];

        let r = et.parse(tokens.into());

        let prg = extract_or_fail!(ok_or_fail!(r), ParserOutput::Complete);
        let seq = prg.unwrap();
        assert_eq!(seq.len(), 1);
        assert!(matches!(
            &seq[0],
            Expression {
                ctx: ExprCtx { span: TxtSpan { start: 1, end: 7 }, txt },
                kind: ExpressionKind::Literal(Value::Pair(Some(_))),
            } if txt.lineno == 1
        ));
        let value = extract_or_fail!(&seq[0].kind, ExpressionKind::Literal);
        assert_eq!(value.to_string(), "(a . b)");

        assert!(et.parsers.is_empty());
        assert!(et.errs.is_empty());
    }

    #[test]
    fn pair_ignores_trailing_comment_block() {
        let mut et = ExpressionTree::default();
        // NOTE: '(a . b #| foo |# ) -> (a . b)
        let tokens = [make_tokenline([
            TokenKind::Quote,
            TokenKind::ParenLeft,
            TokenKind::Identifier("a".to_owned()),
            TokenKind::PairJoiner,
            TokenKind::Identifier("b".to_owned()),
            TokenKind::Comment,
            TokenKind::ParenRight,
        ])];

        let r = et.parse(tokens.into());

        let prg = extract_or_fail!(ok_or_fail!(r), ParserOutput::Complete);
        let seq = prg.unwrap();
        assert_eq!(seq.len(), 1);
        assert!(matches!(
            &seq[0],
            Expression {
                ctx: ExprCtx { span: TxtSpan { start: 1, end: 7 }, txt },
                kind: ExpressionKind::Literal(Value::Pair(Some(_))),
            } if txt.lineno == 1
        ));
        let value = extract_or_fail!(&seq[0].kind, ExpressionKind::Literal);
        assert_eq!(value.to_string(), "(a . b)");

        assert!(et.parsers.is_empty());
        assert!(et.errs.is_empty());
    }

    #[test]
    fn sequence_line_with_errors() {
        let mut et = ExpressionTree::default();
        let tokens = [make_tokenline([
            TokenKind::Boolean(true),
            TokenKind::DirectiveCase(true),
            TokenKind::Character('a'),
            TokenKind::DirectiveCase(false),
            TokenKind::String("foo".into()),
        ])];

        let r = et.parse(tokens.into());

        let errs = extract_or_fail!(err_or_fail!(r), ParserError::Syntax).0;
        assert_eq!(errs.len(), 2);
        assert!(matches!(
            &errs[0],
            ExpressionError {
                ctx: ExprCtx { span: TxtSpan { start: 1, end: 2 }, txt },
                kind: ExpressionErrorKind::Unimplemented(TokenKind::DirectiveCase(true)),
            } if txt.lineno == 1
        ));
        assert!(matches!(
            &errs[1],
            ExpressionError {
                ctx: ExprCtx { span: TxtSpan { start: 3, end: 4 }, txt },
                kind: ExpressionErrorKind::Unimplemented(TokenKind::DirectiveCase(false)),
            }  if txt.lineno == 1
        ));
        assert!(et.parsers.is_empty());
        assert!(et.errs.is_empty());
    }

    #[test]
    fn multiple_sequence_lines_with_errors() {
        let mut et = ExpressionTree::default();
        let tokens = [
            make_tokenline_no(
                [
                    TokenKind::Boolean(true),
                    TokenKind::DirectiveCase(true),
                    TokenKind::Character('a'),
                    TokenKind::DirectiveCase(false),
                    TokenKind::String("foo".into()),
                ],
                1,
            ),
            make_tokenline_no(
                [
                    TokenKind::StringDiscard,
                    TokenKind::Boolean(false),
                    TokenKind::Character('b'),
                ],
                2,
            ),
        ];

        let r = et.parse(tokens.into());

        let errs = extract_or_fail!(err_or_fail!(r), ParserError::Syntax).0;
        assert_eq!(errs.len(), 3);
        assert!(matches!(
            &errs[0],
            ExpressionError {
                ctx: ExprCtx { span: TxtSpan { start: 1, end: 2 }, txt },
                kind: ExpressionErrorKind::Unimplemented(TokenKind::DirectiveCase(true)),
            } if txt.lineno == 1
        ));
        assert!(matches!(
            &errs[1],
            ExpressionError {
                ctx: ExprCtx { span: TxtSpan { start: 3, end: 4 }, txt },
                kind: ExpressionErrorKind::Unimplemented(TokenKind::DirectiveCase(false)),
            }  if txt.lineno == 1
        ));
        assert!(matches!(
            &errs[2],
            ExpressionError {
                ctx: ExprCtx { span: TxtSpan { start: 0, end: 1 }, txt },
                kind: ExpressionErrorKind::SeqInvalid(TokenKind::StringDiscard),
            }  if txt.lineno == 2
        ));
        assert!(et.parsers.is_empty());
        assert!(et.errs.is_empty());
    }

    #[test]
    fn parse_fail_skips_rest_of_tokens() {
        let mut et = ExpressionTree::default();
        let tokens = [
            make_tokenline_no(
                [
                    TokenKind::Boolean(true),
                    TokenKind::DirectiveCase(true),
                    TokenKind::Character('a'),
                    TokenKind::DirectiveCase(false),
                    TokenKind::String("foo".into()),
                ],
                1,
            ),
            make_tokenline_no(
                [
                    TokenKind::Character('c'),
                    TokenKind::IdentifierDiscard,
                    TokenKind::Character('d'),
                    TokenKind::CommentDatum,
                ],
                2,
            ),
            make_tokenline_no(
                [
                    TokenKind::CommentDatum,
                    TokenKind::Boolean(false),
                    TokenKind::Character('b'),
                ],
                3,
            ),
        ];

        let r = et.parse(tokens.into());

        let errs = extract_or_fail!(err_or_fail!(r), ParserError::Syntax).0;
        assert_eq!(errs.len(), 3);
        assert!(matches!(
            &errs[0],
            ExpressionError {
                ctx: ExprCtx { span: TxtSpan { start: 1, end: 2 }, txt },
                kind: ExpressionErrorKind::Unimplemented(TokenKind::DirectiveCase(true)),
            } if txt.lineno == 1
        ));
        assert!(matches!(
            &errs[1],
            ExpressionError {
                ctx: ExprCtx { span: TxtSpan { start: 3, end: 4 }, txt },
                kind: ExpressionErrorKind::Unimplemented(TokenKind::DirectiveCase(false)),
            }  if txt.lineno == 1
        ));
        assert!(matches!(
            &errs[2],
            ExpressionError {
                ctx: ExprCtx { span: TxtSpan { start: 1, end: 2 }, txt },
                kind: ExpressionErrorKind::SeqInvalid(TokenKind::IdentifierDiscard),
            }  if txt.lineno == 2
        ));
        assert!(et.parsers.is_empty());
        assert!(et.errs.is_empty());
    }

    #[test]
    fn invalid_parse_skips_rest_of_tokens() {
        let mut et = ExpressionTree::default();
        et.parsers.push(ParseNode::InvalidParseTree(
            InvalidParseError::InvalidExprSource,
        ));
        let tokens = [make_tokenline_no(
            [
                TokenKind::Boolean(true),
                TokenKind::DirectiveCase(true),
                TokenKind::Character('a'),
                TokenKind::DirectiveCase(false),
                TokenKind::String("foo".into()),
            ],
            1,
        )];

        let r = et.parse(tokens.into());

        let err = extract_or_fail!(err_or_fail!(r), ParserError::Invalid);
        assert!(matches!(err, InvalidParseError::InvalidExprSource));
        assert!(et.parsers.is_empty());
        assert!(et.errs.is_empty());
    }

    #[test]
    fn unterminated_comment_datum_causes_other_errors() {
        let mut et = ExpressionTree::default();
        // NOTE: (foo #u8(10 #;) #t) -> unterminated datum comment, invalid bytevector item
        let tokens = [make_tokenline([
            TokenKind::ParenLeft,
            TokenKind::Identifier("foo".to_owned()),
            TokenKind::ByteVector,
            TokenKind::Number(Number::real(10)),
            TokenKind::CommentDatum,
            TokenKind::ParenRight,
            TokenKind::Boolean(true),
            TokenKind::ParenRight,
        ])];

        let r = et.parse(tokens.into());

        let errs = extract_or_fail!(err_or_fail!(r), ParserError::Syntax).0;
        assert_eq!(errs.len(), 2);
        assert!(matches!(
            &errs[0],
            ExpressionError {
                ctx: ExprCtx { span: TxtSpan { start: 4, end: 6 }, txt },
                kind: ExpressionErrorKind::DatumExpected,
            } if txt.lineno == 1
        ));
        assert!(matches!(
            &errs[1],
            ExpressionError {
                ctx: ExprCtx { span: TxtSpan { start: 6, end: 7 }, txt },
                kind: ExpressionErrorKind::ByteVectorInvalidItem(ExpressionKind::Literal(
                    Value::Boolean(true))),
            }  if txt.lineno == 1
        ));
        assert!(et.parsers.is_empty());
        assert!(et.errs.is_empty());
    }

    #[test]
    fn failed_node_into_unexpected_end_of_parse_discards_rest_of_input() {
        let mut et = ExpressionTree::default();
        let tokens = [make_tokenline_no([TokenKind::CommentDatum], 1)];

        let r = et.parse(tokens.into());

        assert!(matches!(r, Ok(ParserOutput::Continuation)));
        assert_eq!(et.parsers.len(), 2);

        let datum_node = et.parsers.pop().unwrap();
        et.parsers.clear();
        et.parsers.push(datum_node);

        let tokens = [make_tokenline_no(
            [
                TokenKind::ParenRight,
                TokenKind::StringDiscard,
                TokenKind::IdentifierDiscard,
            ],
            2,
        )];

        let r = et.parse(tokens.into());

        let errs = extract_or_fail!(err_or_fail!(r), ParserError::Syntax).0;
        assert_eq!(errs.len(), 1);
        assert!(matches!(
            &errs[0],
            ExpressionError {
                ctx: ExprCtx { span: TxtSpan { start: 0, end: 19 }, txt },
                kind: ExpressionErrorKind::DatumExpected,
            } if txt.lineno == 1
        ));
        assert!(et.parsers.is_empty());
        assert!(et.errs.is_empty());
    }

    #[test]
    fn too_many_pair_elements() {
        let mut et = ExpressionTree::default();
        // NOTE: '(a . b . c) -> err
        let tokens = [make_tokenline([
            TokenKind::Quote,
            TokenKind::ParenLeft,
            TokenKind::Identifier("a".to_owned()),
            TokenKind::PairJoiner,
            TokenKind::Identifier("b".to_owned()),
            TokenKind::PairJoiner,
            TokenKind::Identifier("c".to_owned()),
            TokenKind::ParenRight,
        ])];

        let r = et.parse(tokens.into());

        let errs = extract_or_fail!(err_or_fail!(r), ParserError::Syntax).0;
        assert_eq!(errs.len(), 1);
        assert!(matches!(
            &errs[0],
            ExpressionError {
                ctx: ExprCtx { span: TxtSpan { start: 5, end: 6 }, txt },
                kind: ExpressionErrorKind::PairUnterminated,
            } if txt.lineno == 1
        ));

        assert!(et.parsers.is_empty());
        assert!(et.errs.is_empty());
    }

    #[test]
    fn invalid_pair_trailing_datum() {
        let mut et = ExpressionTree::default();
        // NOTE: '(a . b  c) -> err
        let tokens = [make_tokenline([
            TokenKind::Quote,
            TokenKind::ParenLeft,
            TokenKind::Identifier("a".to_owned()),
            TokenKind::PairJoiner,
            TokenKind::Identifier("b".to_owned()),
            TokenKind::Identifier("c".to_owned()),
            TokenKind::ParenRight,
        ])];

        let r = et.parse(tokens.into());

        let errs = extract_or_fail!(err_or_fail!(r), ParserError::Syntax).0;
        assert_eq!(errs.len(), 1);
        assert!(matches!(
            &errs[0],
            ExpressionError {
                ctx: ExprCtx { span: TxtSpan { start: 5, end: 6 }, txt },
                kind: ExpressionErrorKind::PairUnterminated,
            } if txt.lineno == 1
        ));

        assert!(et.parsers.is_empty());
        assert!(et.errs.is_empty());
    }

    #[test]
    fn not_enough_pair_elements() {
        let mut et = ExpressionTree::default();
        // NOTE: '( . b . c) -> err
        let tokens = [make_tokenline([
            TokenKind::Quote,
            TokenKind::ParenLeft,
            TokenKind::PairJoiner,
            TokenKind::Identifier("b".to_owned()),
            TokenKind::PairJoiner,
            TokenKind::Identifier("c".to_owned()),
            TokenKind::ParenRight,
        ])];

        let r = et.parse(tokens.into());

        let errs = extract_or_fail!(err_or_fail!(r), ParserError::Syntax).0;
        assert_eq!(errs.len(), 1);
        assert!(matches!(
            &errs[0],
            ExpressionError {
                ctx: ExprCtx { span: TxtSpan { start: 2, end: 3 }, txt },
                kind: ExpressionErrorKind::PairIncomplete,
            } if txt.lineno == 1
        ));

        assert!(et.parsers.is_empty());
        assert!(et.errs.is_empty());
    }

    #[test]
    fn double_dotted_pair() {
        let mut et = ExpressionTree::default();
        // NOTE: '(a . . b) -> err
        let tokens = [make_tokenline([
            TokenKind::Quote,
            TokenKind::ParenLeft,
            TokenKind::Identifier("a".to_owned()),
            TokenKind::PairJoiner,
            TokenKind::PairJoiner,
            TokenKind::Identifier("b".to_owned()),
            TokenKind::ParenRight,
        ])];

        let r = et.parse(tokens.into());

        let errs = extract_or_fail!(err_or_fail!(r), ParserError::Syntax).0;
        assert_eq!(errs.len(), 1);
        assert!(matches!(
            &errs[0],
            ExpressionError {
                ctx: ExprCtx { span: TxtSpan { start: 4, end: 5 }, txt },
                kind: ExpressionErrorKind::PairUnterminated,
            } if txt.lineno == 1
        ));

        assert!(et.parsers.is_empty());
        assert!(et.errs.is_empty());
    }

    #[test]
    fn pair_comment_datum_leaves_pair_open() {
        let mut et = ExpressionTree::default();
        // NOTE: '(a . #; b) -> err
        let tokens = [make_tokenline([
            TokenKind::Quote,
            TokenKind::ParenLeft,
            TokenKind::Identifier("a".to_owned()),
            TokenKind::PairJoiner,
            TokenKind::CommentDatum,
            TokenKind::Identifier("b".to_owned()),
            TokenKind::ParenRight,
        ])];

        let r = et.parse(tokens.into());

        let errs = extract_or_fail!(err_or_fail!(r), ParserError::Syntax).0;
        assert_eq!(errs.len(), 1);
        assert!(matches!(
            &errs[0],
            ExpressionError {
                ctx: ExprCtx { span: TxtSpan { start: 6, end: 7 }, txt },
                kind: ExpressionErrorKind::PairUnterminated,
            } if txt.lineno == 1
        ));

        assert!(et.parsers.is_empty());
        assert!(et.errs.is_empty());
    }

    #[test]
    fn invalid_syntax_pair_member() {
        let mut et = ExpressionTree::default();
        // NOTE: '(a . #u8(1 b 2)) -> err
        let tokens = [make_tokenline([
            TokenKind::Quote,
            TokenKind::ParenLeft,
            TokenKind::Identifier("a".to_owned()),
            TokenKind::PairJoiner,
            TokenKind::ByteVector,
            TokenKind::Number(Number::real(1)),
            TokenKind::Identifier("b".to_owned()),
            TokenKind::Number(Number::real(2)),
            TokenKind::ParenRight,
            TokenKind::ParenRight,
        ])];

        let r = et.parse(tokens.into());

        let errs = extract_or_fail!(err_or_fail!(r), ParserError::Syntax).0;
        assert_eq!(errs.len(), 2);
        assert!(matches!(
            &errs[0],
            ExpressionError {
                ctx: ExprCtx { span: TxtSpan { start: 6, end: 7 }, txt },
                kind: ExpressionErrorKind::ByteVectorInvalidItem(ExpressionKind::Literal(Value::Symbol(s))),
            } if txt.lineno == 1 && &**s == "b"
        ));
        assert!(matches!(
            &errs[1],
            ExpressionError {
                ctx: ExprCtx { span: TxtSpan { start: 9, end: 10 }, txt },
                kind: ExpressionErrorKind::PairUnterminated,
            } if txt.lineno == 1
        ));

        assert!(et.parsers.is_empty());
        assert!(et.errs.is_empty());
    }

    #[test]
    fn unquoted_pair() {
        let mut et = ExpressionTree::default();
        // NOTE: (a . b) -> err (no quote)
        let tokens = [make_tokenline([
            TokenKind::ParenLeft,
            TokenKind::Identifier("a".to_owned()),
            TokenKind::PairJoiner,
            TokenKind::Identifier("b".to_owned()),
            TokenKind::ParenRight,
        ])];

        let r = et.parse(tokens.into());

        let errs = extract_or_fail!(err_or_fail!(r), ParserError::Syntax).0;
        assert_eq!(errs.len(), 1);
        assert!(matches!(
            &errs[0],
            ExpressionError {
                ctx: ExprCtx { span: TxtSpan { start: 2, end: 3 }, txt },
                kind: ExpressionErrorKind::PairUnexpected,
            } if txt.lineno == 1
        ));

        assert!(et.parsers.is_empty());
        assert!(et.errs.is_empty());
    }

    #[test]
    fn unquoted_empty_pair() {
        let mut et = ExpressionTree::default();
        // NOTE: ( . ) -> err
        let tokens = [make_tokenline([
            TokenKind::ParenLeft,
            TokenKind::PairJoiner,
            TokenKind::ParenRight,
        ])];

        let r = et.parse(tokens.into());

        let errs = extract_or_fail!(err_or_fail!(r), ParserError::Syntax).0;
        assert_eq!(errs.len(), 2);
        assert!(matches!(
            &errs[0],
            ExpressionError {
                ctx: ExprCtx { span: TxtSpan { start: 1, end: 2 }, txt },
                kind: ExpressionErrorKind::PairUnexpected,
            } if txt.lineno == 1
        ));
        assert!(matches!(
            &errs[1],
            ExpressionError {
                ctx: ExprCtx { span: TxtSpan { start: 0, end: 3 }, txt },
                kind: ExpressionErrorKind::ProcedureEmpty,
            } if txt.lineno == 1
        ));

        assert!(et.parsers.is_empty());
        assert!(et.errs.is_empty());
    }

    #[test]
    fn bytevector_with_pair_joiner() {
        let mut et = ExpressionTree::default();
        // NOTE: #(a . b) -> err (pair not allowed)
        let tokens = [make_tokenline([
            TokenKind::ByteVector,
            TokenKind::Number(Number::real(11)),
            TokenKind::PairJoiner,
            TokenKind::Number(Number::real(13)),
            TokenKind::ParenRight,
        ])];

        let r = et.parse(tokens.into());

        let errs = extract_or_fail!(err_or_fail!(r), ParserError::Syntax).0;
        assert_eq!(errs.len(), 1);
        assert!(matches!(
            &errs[0],
            ExpressionError {
                ctx: ExprCtx { span: TxtSpan { start: 2, end: 3 }, txt },
                kind: ExpressionErrorKind::PairUnexpected,
            } if txt.lineno == 1
        ));

        assert!(et.parsers.is_empty());
        assert!(et.errs.is_empty());
    }

    #[test]
    fn vector_with_pair_joiner() {
        let mut et = ExpressionTree::default();
        // NOTE: #(a . b) -> err (pair not allowed)
        let tokens = [make_tokenline([
            TokenKind::Vector,
            TokenKind::Identifier("a".to_owned()),
            TokenKind::PairJoiner,
            TokenKind::Identifier("b".to_owned()),
            TokenKind::ParenRight,
        ])];

        let r = et.parse(tokens.into());

        let errs = extract_or_fail!(err_or_fail!(r), ParserError::Syntax).0;
        assert_eq!(errs.len(), 1);
        assert!(matches!(
            &errs[0],
            ExpressionError {
                ctx: ExprCtx { span: TxtSpan { start: 2, end: 3 }, txt },
                kind: ExpressionErrorKind::PairUnexpected,
            } if txt.lineno == 1
        ));

        assert!(et.parsers.is_empty());
        assert!(et.errs.is_empty());
    }
}

mod continuation {
    use super::*;

    #[test]
    fn no_continuation() {
        let mut et = ExpressionTree::default();

        let o = et.unsupported_continuation();

        assert!(o.is_none());
    }

    #[test]
    fn continuation_to_error() {
        let mut et = ExpressionTree::default();
        let tokens = [make_tokenline([
            TokenKind::Boolean(true),
            TokenKind::StringBegin {
                s: "foo".to_owned(),
                line_cont: false,
            },
        ])];

        let r = et.parse(tokens.into());

        assert!(matches!(r, Ok(ParserOutput::Continuation)));
        assert_eq!(et.parsers.len(), 2);
        assert!(et.errs.is_empty());

        let o = et.unsupported_continuation();

        let errs = extract_or_fail!(some_or_fail!(o), ParserError::Syntax).0;
        assert_eq!(errs.len(), 1);
        assert!(matches!(
            &errs[0],
            ExpressionError {
                ctx: ExprCtx { span: TxtSpan { start: 1, end: 19 }, txt },
                kind: ExpressionErrorKind::StrUnterminated,
            } if txt.lineno == 1
        ));

        assert!(et.parsers.is_empty());
        assert!(et.errs.is_empty());
    }

    #[test]
    fn continuation_tied_to_expression_first_line() {
        let mut et = ExpressionTree::default();
        let tokens = [
            make_tokenline_no(
                [TokenKind::StringBegin {
                    s: "foo".to_owned(),
                    line_cont: false,
                }],
                1,
            ),
            make_tokenline_no(
                [TokenKind::StringFragment {
                    s: "bar".to_owned(),
                    line_cont: false,
                }],
                2,
            ),
        ];

        let r = et.parse(tokens.into());

        assert!(matches!(r, Ok(ParserOutput::Continuation)));
        assert_eq!(et.parsers.len(), 2);
        assert!(et.errs.is_empty());

        let o = et.unsupported_continuation();

        let errs = extract_or_fail!(some_or_fail!(o), ParserError::Syntax).0;
        assert_eq!(errs.len(), 1);
        assert!(matches!(
            &errs[0],
            ExpressionError {
                ctx: ExprCtx { span: TxtSpan { start: 0, end: 19 }, txt },
                kind: ExpressionErrorKind::StrUnterminated,
            } if txt.lineno == 1
        ));

        assert!(et.parsers.is_empty());
        assert!(et.errs.is_empty());
    }

    #[test]
    fn continuation_ignored_if_existing_errors() {
        let mut et = ExpressionTree::default();
        let tokens = [make_tokenline([
            TokenKind::Boolean(true),
            TokenKind::DirectiveCase(true),
            TokenKind::StringBegin {
                s: "foo".to_owned(),
                line_cont: false,
            },
        ])];

        let r = et.parse(tokens.into());

        let errs = extract_or_fail!(err_or_fail!(r), ParserError::Syntax).0;
        assert_eq!(errs.len(), 1);
        assert!(matches!(
            &errs[0],
            ExpressionError {
                ctx: ExprCtx { span: TxtSpan { start: 1, end: 2 }, txt },
                kind: ExpressionErrorKind::Unimplemented(TokenKind::DirectiveCase(true)),
            } if txt.lineno == 1
        ));
        assert!(et.parsers.is_empty());
        assert!(et.errs.is_empty());
    }
}

mod errors {
    use super::*;
    use crate::testutil::make_textline;

    #[test]
    fn display_syntax_error() {
        let err = ParserError::Syntax(SyntaxError(Vec::new()));

        assert_eq!(err.to_string(), "fatal error: invalid syntax");
    }

    #[test]
    fn display_invalid_error() {
        let err = ParserError::Invalid(InvalidParseError::EndOfParse);

        assert_eq!(err.to_string(), "fatal error: invalid parser state reached");
    }

    #[test]
    fn display_syntax_message() {
        let err = ParserError::Syntax(SyntaxError(vec![
            ExprCtx {
                span: 0..3,
                txt: make_textline().into(),
            }
            .into_error(ExpressionErrorKind::ListUnterminated),
        ]));

        assert_eq!(
            err.display_message().to_string(),
            "Syntax Error\nmylib:1 (lib/mylib.scm)\n\tline of source code\n\t^^^\n1: unterminated list expression\n"
        );
    }

    #[test]
    fn display_overlap_suffix_syntax_messages() {
        let txt = make_textline().into();
        let err = ParserError::Syntax(SyntaxError(vec![
            ExprCtx {
                span: 2..4,
                txt: Rc::clone(&txt),
            }
            .into_error(ExpressionErrorKind::ListUnterminated),
            ExprCtx {
                span: 3..6,
                txt: Rc::clone(&txt),
            }
            .into_error(ExpressionErrorKind::ListUnterminated),
        ]));

        assert_eq!(
            err.display_message().to_string(),
            "Syntax Error\nmylib:1 (lib/mylib.scm)\n\tline of source code\n\t  ^^\n\t   ^^^\n3: unterminated list expression\n4: unterminated list expression\n"
        );
    }

    #[test]
    fn display_overlap_prefix_syntax_messages() {
        let txt = make_textline().into();
        let err = ParserError::Syntax(SyntaxError(vec![
            ExprCtx {
                span: 2..4,
                txt: Rc::clone(&txt),
            }
            .into_error(ExpressionErrorKind::ListUnterminated),
            ExprCtx {
                span: 0..3,
                txt: Rc::clone(&txt),
            }
            .into_error(ExpressionErrorKind::ListUnterminated),
        ]));

        assert_eq!(
            err.display_message().to_string(),
            "Syntax Error\nmylib:1 (lib/mylib.scm)\n\tline of source code\n\t  ^^\n\t^^^\n3: unterminated list expression\n1: unterminated list expression\n"
        );
    }

    #[test]
    fn display_encompassed_syntax_messages() {
        let txt = make_textline().into();
        let err = ParserError::Syntax(SyntaxError(vec![
            ExprCtx {
                span: 2..4,
                txt: Rc::clone(&txt),
            }
            .into_error(ExpressionErrorKind::ListUnterminated),
            ExprCtx {
                span: 0..5,
                txt: Rc::clone(&txt),
            }
            .into_error(ExpressionErrorKind::ListUnterminated),
        ]));

        assert_eq!(
            err.display_message().to_string(),
            "Syntax Error\nmylib:1 (lib/mylib.scm)\n\tline of source code\n\t  ^^\n\t^^^^^\n3: unterminated list expression\n1: unterminated list expression\n"
        );
    }

    #[test]
    fn display_invalid_message() {
        let err = ParserError::Invalid(InvalidParseError::EndOfParse);

        assert_eq!(
            err.display_message().to_string(),
            "unexpected end-of-parse\n"
        );

        let err = ParserError::Invalid(InvalidParseError::InvalidExprSource);

        assert_eq!(
            err.display_message().to_string(),
            "unexpected merge source not an expression\n"
        );

        let err = ParserError::Invalid(InvalidParseError::InvalidExprTarget);

        assert_eq!(
            err.display_message().to_string(),
            "invalid merge target expression\n"
        );

        let err = ParserError::Invalid(InvalidParseError::MissingExprTarget);

        assert_eq!(
            err.display_message().to_string(),
            "unexpected end-of-parse for merge target\n"
        );
    }

    #[test]
    fn syntax_source() {
        let err = ParserError::Syntax(SyntaxError(Vec::new()));

        let inner = some_or_fail!(err.source());

        assert!(matches!(
            inner.downcast_ref::<SyntaxError>().unwrap(),
            SyntaxError(_)
        ));
    }

    #[test]
    fn invalid_source() {
        let err = ParserError::Invalid(InvalidParseError::EndOfParse);

        let inner = some_or_fail!(err.source());

        assert!(matches!(
            inner.downcast_ref::<InvalidParseError>().unwrap(),
            InvalidParseError::EndOfParse
        ));
    }
}

mod partition {
    use super::*;

    #[test]
    fn empty() {
        let spans = [];
        let p = PartitionByOverlap::new(spans);

        let groups = p.into_iter().collect::<Vec<_>>();

        assert!(groups.is_empty());
    }

    #[test]
    fn one_span() {
        let spans = [0..5];
        let p = PartitionByOverlap::new(spans.iter());

        let groups = p.into_iter().collect::<Vec<_>>();

        assert_eq!(groups.len(), 1);
        let g = &groups[0];
        assert_eq!(g.len(), 1);
        assert!(matches!(&g[0], TxtSpan { start: 0, end: 5 }));
    }

    #[test]
    fn no_overlap() {
        let spans = [0..5, 6..9, 9..13];
        let p = PartitionByOverlap::new(spans.iter());

        let groups = p.into_iter().collect::<Vec<_>>();

        assert_eq!(groups.len(), 1);
        let g = &groups[0];
        assert_eq!(g.len(), 3);
        assert!(matches!(&g[0], TxtSpan { start: 0, end: 5 }));
        assert!(matches!(&g[1], TxtSpan { start: 6, end: 9 }));
        assert!(matches!(&g[2], TxtSpan { start: 9, end: 13 }));
    }

    #[test]
    fn contiguous() {
        let spans = [0..5, 5..8, 8..12];
        let p = PartitionByOverlap::new(spans.iter());

        let groups = p.into_iter().collect::<Vec<_>>();

        assert_eq!(groups.len(), 1);
        let g = &groups[0];
        assert_eq!(g.len(), 3);
        assert!(matches!(&g[0], TxtSpan { start: 0, end: 5 }));
        assert!(matches!(&g[1], TxtSpan { start: 5, end: 8 }));
        assert!(matches!(&g[2], TxtSpan { start: 8, end: 12 }));
    }

    #[test]
    fn stacked() {
        let spans = [0..5, 3..6, 4..8];
        let p = PartitionByOverlap::new(spans.iter());

        let groups = p.into_iter().collect::<Vec<_>>();

        assert_eq!(groups.len(), 3);
        let g = &groups[0];
        assert_eq!(g.len(), 1);
        assert!(matches!(&g[0], TxtSpan { start: 0, end: 5 }));
        let g = &groups[1];
        assert_eq!(g.len(), 1);
        assert!(matches!(&g[0], TxtSpan { start: 3, end: 6 }));
        let g = &groups[2];
        assert_eq!(g.len(), 1);
        assert!(matches!(&g[0], TxtSpan { start: 4, end: 8 }));
    }

    #[test]
    fn middle_overlap() {
        let spans = [0..5, 3..6, 5..8];
        let p = PartitionByOverlap::new(spans.iter());

        let groups = p.into_iter().collect::<Vec<_>>();

        assert_eq!(groups.len(), 2);
        let g = &groups[0];
        assert_eq!(g.len(), 2);
        assert!(matches!(&g[0], TxtSpan { start: 0, end: 5 }));
        assert!(matches!(&g[1], TxtSpan { start: 5, end: 8 }));
        let g = &groups[1];
        assert_eq!(g.len(), 1);
        assert!(matches!(&g[0], TxtSpan { start: 3, end: 6 }));
    }

    #[test]
    fn zig_zag() {
        let spans = [0..5, 3..6, 5..8, 7..10];
        let p = PartitionByOverlap::new(spans.iter());

        let groups = p.into_iter().collect::<Vec<_>>();

        assert_eq!(groups.len(), 2);
        let g = &groups[0];
        assert_eq!(g.len(), 2);
        assert!(matches!(&g[0], TxtSpan { start: 0, end: 5 }));
        assert!(matches!(&g[1], TxtSpan { start: 5, end: 8 }));
        let g = &groups[1];
        assert_eq!(g.len(), 2);
        assert!(matches!(&g[0], TxtSpan { start: 3, end: 6 }));
        assert!(matches!(&g[1], TxtSpan { start: 7, end: 10 }));
    }

    #[test]
    fn contained() {
        let spans = [0..10, 2..5, 5..8];
        let p = PartitionByOverlap::new(spans.iter());

        let groups = p.into_iter().collect::<Vec<_>>();

        assert_eq!(groups.len(), 2);
        let g = &groups[0];
        assert_eq!(g.len(), 1);
        assert!(matches!(&g[0], TxtSpan { start: 0, end: 10 }));
        let g = &groups[1];
        assert_eq!(g.len(), 2);
        assert!(matches!(&g[0], TxtSpan { start: 2, end: 5 }));
        assert!(matches!(&g[1], TxtSpan { start: 5, end: 8 }));
    }
}
