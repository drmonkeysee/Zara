use super::*;
use crate::{literal::Literal, testutil::extract_or_fail, txt::TextContext};
use std::path::Path;

fn make_textline() -> TextLine {
    TextLine {
        ctx: TextContext {
            name: "mylib".to_owned(),
            path: Some(Path::new("lib/mylib.scm").to_path_buf()),
        }
        .into(),
        line: "line of source code".to_owned(),
        lineno: 1,
    }
}

mod lexer {
    use self::token::{TokenErrorKind, TokenType};
    use super::*;
    use crate::{
        testutil::{err_or_fail, ok_or_fail},
        txt::{LineNumber, TextResult},
    };
    use std::{ops::Range, rc::Rc, str::Lines};

    struct MockTxtSource<'a> {
        can_continue: bool,
        ctx: Rc<TextContext>,
        lines: Lines<'a>,
        lineno: LineNumber,
    }

    impl<'a> MockTxtSource<'a> {
        fn new(src: &'a str, can_continue: bool) -> Self {
            Self {
                can_continue,
                ctx: TextContext::named("<mock>").into(),
                lines: src.lines(),
                lineno: 0,
            }
        }
    }

    impl Iterator for MockTxtSource<'_> {
        type Item = TextResult;

        fn next(&mut self) -> Option<Self::Item> {
            self.lineno += 1;
            let line = self.lines.next()?;
            if line == "BAD BYTES" {
                Some(Err(TextError::new(
                    self.context(),
                    self.lineno(),
                    "BAD BYTES FOUND",
                )))
            } else {
                Some(Ok(TextLine {
                    ctx: self.context(),
                    line: line.to_owned(),
                    lineno: self.lineno(),
                }))
            }
        }
    }

    impl TextSource for MockTxtSource<'_> {
        fn can_continue(&self) -> bool {
            self.can_continue
        }

        fn context(&self) -> Rc<TextContext> {
            self.ctx.clone()
        }

        fn lineno(&self) -> LineNumber {
            self.lineno
        }
    }

    #[test]
    fn empty_line() {
        let mut src = MockTxtSource::new("", false);
        let mut target = Lexer::new();

        let r = target.tokenize(&mut src);

        let o = ok_or_fail!(r);
        let lines = extract_or_fail!(o, LexerOutput::Complete);
        assert!(lines.is_empty());
        assert!(target.cont.is_none());
    }

    #[test]
    fn single_token() {
        let mut src = MockTxtSource::new("#t", false);
        let mut target = Lexer::new();

        let r = target.tokenize(&mut src);

        let o = ok_or_fail!(r);
        let lines = extract_or_fail!(o, LexerOutput::Complete);
        assert_eq!(lines.len(), 1);
        let line = &lines[0];
        assert_eq!(line.0.len(), 1);
        assert!(matches!(
            line.0[0],
            TokenType {
                kind: TokenKind::Literal(Literal::Boolean(true)),
                span: Range { start: 0, end: 2 }
            }
        ));
        assert!(matches!(
            &line.1,
            TextLine {
                ctx,
                line,
                lineno: 1,
            } if Rc::ptr_eq(&ctx, &src.ctx) && line == "#t"
        ));
        assert!(target.cont.is_none());
    }

    #[test]
    fn multi_tokens() {
        let mut src = MockTxtSource::new("#t #f #\\a", false);
        let mut target = Lexer::new();

        let r = target.tokenize(&mut src);

        let o = ok_or_fail!(r);
        let lines = extract_or_fail!(o, LexerOutput::Complete);
        assert_eq!(lines.len(), 1);
        let line = &lines[0];
        assert_eq!(line.0.len(), 3);
        assert!(matches!(
            line.0[0],
            TokenType {
                kind: TokenKind::Literal(Literal::Boolean(true)),
                span: Range { start: 0, end: 2 }
            }
        ));
        assert!(matches!(
            line.0[1],
            TokenType {
                kind: TokenKind::Literal(Literal::Boolean(false)),
                span: Range { start: 3, end: 5 }
            }
        ));
        assert!(matches!(
            line.0[2],
            TokenType {
                kind: TokenKind::Literal(Literal::Character('a')),
                span: Range { start: 6, end: 9 }
            }
        ));
        assert!(matches!(
            &line.1,
            TextLine {
                ctx,
                line,
                lineno: 1,
            } if Rc::ptr_eq(&ctx, &src.ctx) && line == "#t #f #\\a"
        ));
        assert!(target.cont.is_none());
    }

    #[test]
    fn multi_lines() {
        let mut src = MockTxtSource::new("#t\n  #f #\\a\n#f #f\n", false);
        let mut target = Lexer::new();

        let r = target.tokenize(&mut src);

        let o = ok_or_fail!(r);
        let lines = extract_or_fail!(o, LexerOutput::Complete);
        assert_eq!(lines.len(), 3);
        let line = &lines[0];
        assert_eq!(line.0.len(), 1);
        assert!(matches!(
            line.0[0],
            TokenType {
                kind: TokenKind::Literal(Literal::Boolean(true)),
                span: Range { start: 0, end: 2 }
            }
        ));
        assert!(matches!(
            &line.1,
            TextLine {
                ctx,
                line,
                lineno: 1,
            } if Rc::ptr_eq(&ctx, &src.ctx) && line == "#t"
        ));
        let line = &lines[1];
        assert_eq!(line.0.len(), 2);
        assert!(matches!(
            line.0[0],
            TokenType {
                kind: TokenKind::Literal(Literal::Boolean(false)),
                span: Range { start: 2, end: 4 }
            }
        ));
        assert!(matches!(
            line.0[1],
            TokenType {
                kind: TokenKind::Literal(Literal::Character('a')),
                span: Range { start: 5, end: 8 }
            }
        ));
        assert!(matches!(
            &line.1,
            TextLine {
                ctx,
                line,
                lineno: 2,
            } if Rc::ptr_eq(&ctx, &src.ctx) && line == "  #f #\\a"
        ));
        assert!(target.cont.is_none());
        let line = &lines[2];
        assert_eq!(line.0.len(), 2);
        assert!(matches!(
            line.0[0],
            TokenType {
                kind: TokenKind::Literal(Literal::Boolean(false)),
                span: Range { start: 0, end: 2 }
            }
        ));
        assert!(matches!(
            line.0[1],
            TokenType {
                kind: TokenKind::Literal(Literal::Boolean(false)),
                span: Range { start: 3, end: 5 }
            }
        ));
        assert!(matches!(
            &line.1,
            TextLine {
                ctx,
                line,
                lineno: 3,
            } if Rc::ptr_eq(&ctx, &src.ctx) && line == "#f #f"
        ));
        assert!(target.cont.is_none());
    }

    #[test]
    fn multi_lines_with_errors() {
        let mut src = MockTxtSource::new("#t\n #z #f #z #\\a\n#f #z #f", false);
        let mut target = Lexer::new();

        let r = target.tokenize(&mut src);

        let err = err_or_fail!(r);
        let err_lines = err.0;
        assert_eq!(err_lines.len(), 2);
        let TokenErrorLine(errs, line) = extract_or_fail!(&err_lines[0], LineFailure::Tokenize);
        assert_eq!(errs.len(), 2);
        assert!(matches!(
            errs[0],
            TokenType {
                kind: TokenErrorKind::HashInvalid,
                span: Range { start: 1, end: 3 }
            }
        ));
        assert!(matches!(
            errs[1],
            TokenType {
                kind: TokenErrorKind::HashInvalid,
                span: Range { start: 7, end: 9 }
            }
        ));
        assert!(matches!(
            line,
            TextLine {
                ctx,
                line,
                lineno: 2,
            } if Rc::ptr_eq(&ctx, &src.ctx) && line == " #z #f #z #\\a"
        ));
        let TokenErrorLine(errs, line) = extract_or_fail!(&err_lines[1], LineFailure::Tokenize);
        assert_eq!(errs.len(), 1);
        assert!(matches!(
            errs[0],
            TokenType {
                kind: TokenErrorKind::HashInvalid,
                span: Range { start: 3, end: 5 }
            }
        ));
        assert!(matches!(
            line,
            TextLine {
                ctx,
                line,
                lineno: 3,
            } if Rc::ptr_eq(&ctx, &src.ctx) && line == "#f #z #f"
        ));
        assert!(target.cont.is_none());
    }

    #[test]
    fn multi_lines_with_read_error() {
        let mut src = MockTxtSource::new("#t\nBAD BYTES\n#f #f\n", false);
        let mut target = Lexer::new();

        let r = target.tokenize(&mut src);

        let err = err_or_fail!(r);
        let err_lines = err.0;
        assert_eq!(err_lines.len(), 1);
        let inner = extract_or_fail!(&err_lines[0], LineFailure::Read);
        assert!(Rc::ptr_eq(&inner.ctx, &src.ctx));
        assert_eq!(inner.lineno, 2);
        assert!(inner.source().is_some());
        assert!(target.cont.is_none());
    }

    #[test]
    fn mixed_errors() {
        let mut src = MockTxtSource::new("#t\n #z #f #z #\\a\nBAD BYTES", false);
        let mut target = Lexer::new();

        let r = target.tokenize(&mut src);

        let err = err_or_fail!(r);
        let err_lines = err.0;
        assert_eq!(err_lines.len(), 2);
        let TokenErrorLine(errs, line) = extract_or_fail!(&err_lines[0], LineFailure::Tokenize);
        assert_eq!(errs.len(), 2);
        assert!(matches!(
            errs[0],
            TokenType {
                kind: TokenErrorKind::HashInvalid,
                span: Range { start: 1, end: 3 }
            }
        ));
        assert!(matches!(
            errs[1],
            TokenType {
                kind: TokenErrorKind::HashInvalid,
                span: Range { start: 7, end: 9 }
            }
        ));
        assert!(matches!(
            line,
            TextLine {
                ctx,
                line,
                lineno: 2,
            } if Rc::ptr_eq(&ctx, &src.ctx) && line == " #z #f #z #\\a"
        ));
        let inner = extract_or_fail!(&err_lines[1], LineFailure::Read);
        assert!(Rc::ptr_eq(&inner.ctx, &src.ctx));
        assert_eq!(inner.lineno, 3);
        assert!(inner.source().is_some());
        assert!(target.cont.is_none());
    }

    #[test]
    fn continuation_token_persists_on_lexer() {
        let mut src = MockTxtSource::new("#t #|trailing...", true);
        let mut target = Lexer::new();

        let r = target.tokenize(&mut src);

        let o = ok_or_fail!(r);
        assert!(matches!(o, LexerOutput::Continuation));
        let (lines, cont) = extract_or_fail!(target.cont, Some);
        assert_eq!(lines.len(), 1);
        let line = &lines[0];
        assert_eq!(line.0.len(), 2);
        assert!(matches!(
            line.0[0],
            TokenType {
                kind: TokenKind::Literal(Literal::Boolean(true)),
                span: Range { start: 0, end: 2 }
            }
        ));
        assert!(matches!(
            line.0[1],
            TokenType {
                kind: TokenKind::CommentBlockBegin { depth: 0 },
                span: Range { start: 3, end: 16 }
            }
        ));
        assert!(matches!(
            &line.1,
            TextLine {
                ctx,
                line,
                lineno: 1,
            } if Rc::ptr_eq(&ctx, &src.ctx) && line == "#t #|trailing..."
        ));
        assert!(matches!(cont, TokenContinuation::BlockComment { depth: 0 }));
    }

    #[test]
    fn continuation_token_converted_to_error_if_src_does_not_support() {
        let mut src = MockTxtSource::new("#t #|trailing...", false);
        let mut target = Lexer::new();

        let r = target.tokenize(&mut src);

        let err = err_or_fail!(r);
        let err_lines = err.0;
        assert_eq!(err_lines.len(), 1);
        let TokenErrorLine(errs, line) = extract_or_fail!(&err_lines[0], LineFailure::Tokenize);
        assert_eq!(errs.len(), 1);
        assert!(matches!(
            errs[0],
            TokenType {
                kind: TokenErrorKind::BlockCommentUnterminated,
                span: Range { start: 3, end: 16 }
            }
        ));
        assert!(matches!(
            line,
            TextLine {
                ctx,
                line,
                lineno: 1,
            } if Rc::ptr_eq(&ctx, &src.ctx) && line == "#t #|trailing..."
        ));
        assert!(target.cont.is_none());
    }

    #[test]
    fn continuation_cleared_on_completion_followed_by_error() {
        let mut src = MockTxtSource::new("#t #|trailing...", true);
        let mut target = Lexer::new();

        let r = target.tokenize(&mut src);

        assert!(r.is_ok());
        assert!(target.cont.is_some());

        let mut src = MockTxtSource::new("...finish|# [invalid_token]", true);

        let r = target.tokenize(&mut src);

        assert!(r.is_err());
        assert!(target.cont.is_none());
    }

    #[test]
    fn continuation_cleared_on_error_and_exhausted_source() {
        let mut src = MockTxtSource::new("#t #z #|trailing...", true);
        let mut target = Lexer::new();

        let r = target.tokenize(&mut src);

        assert!(r.is_err());
        assert!(target.cont.is_none());
    }

    #[test]
    fn double_line_comment() {
        let mut src = MockTxtSource::new("#| double line\ncomment |#", false);
        let mut target = Lexer::new();

        let r = target.tokenize(&mut src);

        let o = ok_or_fail!(r);
        let lines = extract_or_fail!(o, LexerOutput::Complete);
        assert_eq!(lines.len(), 2);
        let line = &lines[0];
        assert_eq!(line.0.len(), 1);
        assert!(matches!(
            line.0[0],
            TokenType {
                kind: TokenKind::CommentBlockBegin { depth: 0 },
                span: Range { start: 0, end: 14 }
            }
        ));
        assert!(matches!(
            &line.1,
            TextLine {
                ctx,
                line,
                lineno: 1,
            } if Rc::ptr_eq(&ctx, &src.ctx) && line == "#| double line"
        ));
        let line = &lines[1];
        assert!(matches!(
            line.0[0],
            TokenType {
                kind: TokenKind::CommentBlockEnd,
                span: Range { start: 0, end: 10 }
            }
        ));
        assert!(matches!(
            &line.1,
            TextLine {
                ctx,
                line,
                lineno: 2,
            } if Rc::ptr_eq(&ctx, &src.ctx) && line == "comment |#"
        ));
        assert!(target.cont.is_none());
    }

    #[test]
    fn multi_line_comment() {
        let mut src = MockTxtSource::new("#| multi\nline\ncomment |#", false);
        let mut target = Lexer::new();

        let r = target.tokenize(&mut src);

        let o = ok_or_fail!(r);
        let lines = extract_or_fail!(o, LexerOutput::Complete);
        assert_eq!(lines.len(), 3);
        let line = &lines[0];
        assert_eq!(line.0.len(), 1);
        assert!(matches!(
            line.0[0],
            TokenType {
                kind: TokenKind::CommentBlockBegin { depth: 0 },
                span: Range { start: 0, end: 8 }
            }
        ));
        assert!(matches!(
            &line.1,
            TextLine {
                ctx,
                line,
                lineno: 1,
            } if Rc::ptr_eq(&ctx, &src.ctx) && line == "#| multi"
        ));
        let line = &lines[1];
        assert!(matches!(
            line.0[0],
            TokenType {
                kind: TokenKind::CommentBlockFragment { depth: 0 },
                span: Range { start: 0, end: 4 }
            }
        ));
        assert!(matches!(
            &line.1,
            TextLine {
                ctx,
                line,
                lineno: 2,
            } if Rc::ptr_eq(&ctx, &src.ctx) && line == "line"
        ));
        let line = &lines[2];
        assert!(matches!(
            line.0[0],
            TokenType {
                kind: TokenKind::CommentBlockEnd,
                span: Range { start: 0, end: 10 }
            }
        ));
        assert!(matches!(
            &line.1,
            TextLine {
                ctx,
                line,
                lineno: 3,
            } if Rc::ptr_eq(&ctx, &src.ctx) && line == "comment |#"
        ));
        assert!(target.cont.is_none());
    }

    #[test]
    fn double_line_comment_when_prefixed_by_error_and_followed_by_error() {
        let mut src = MockTxtSource::new("#t #z #| double line\n#z comment |# #z", false);
        let mut target = Lexer::new();

        let r = target.tokenize(&mut src);

        let err = err_or_fail!(r);
        let err_lines = err.0;
        assert_eq!(err_lines.len(), 2);
        let TokenErrorLine(errs, line) = extract_or_fail!(&err_lines[0], LineFailure::Tokenize);
        assert_eq!(errs.len(), 1);
        assert!(matches!(
            errs[0],
            TokenType {
                kind: TokenErrorKind::HashInvalid,
                span: Range { start: 3, end: 5 }
            }
        ));
        assert!(matches!(
            line,
            TextLine {
                ctx,
                line,
                lineno: 1,
            } if Rc::ptr_eq(&ctx, &src.ctx) && line == "#t #z #| double line"
        ));
        let TokenErrorLine(errs, line) = extract_or_fail!(&err_lines[1], LineFailure::Tokenize);
        assert_eq!(errs.len(), 1);
        assert!(matches!(
            errs[0],
            TokenType {
                kind: TokenErrorKind::HashInvalid,
                span: Range { start: 14, end: 16 }
            }
        ));
        assert!(matches!(
            line,
            TextLine {
                ctx,
                line,
                lineno: 2,
            } if Rc::ptr_eq(&ctx, &src.ctx) && line == "#z comment |# #z"
        ));
        assert!(target.cont.is_none());
    }

    #[test]
    fn double_line_comment_when_prefixed_by_error() {
        let mut src = MockTxtSource::new("#t #z #| double line\n#z comment |#", false);
        let mut target = Lexer::new();

        let r = target.tokenize(&mut src);

        let err = err_or_fail!(r);
        let err_lines = err.0;
        assert_eq!(err_lines.len(), 1);
        let TokenErrorLine(errs, line) = extract_or_fail!(&err_lines[0], LineFailure::Tokenize);
        assert_eq!(errs.len(), 1);
        assert!(matches!(
            errs[0],
            TokenType {
                kind: TokenErrorKind::HashInvalid,
                span: Range { start: 3, end: 5 }
            }
        ));
        assert!(matches!(
            line,
            TextLine {
                ctx,
                line,
                lineno: 1,
            } if Rc::ptr_eq(&ctx, &src.ctx) && line == "#t #z #| double line"
        ));
        assert!(target.cont.is_none());
    }

    #[test]
    fn double_line_string() {
        let mut src = MockTxtSource::new("\" double line\nstring \"", false);
        let mut target = Lexer::new();

        let r = target.tokenize(&mut src);

        let o = ok_or_fail!(r);
        let lines = extract_or_fail!(o, LexerOutput::Complete);
        assert_eq!(lines.len(), 2);
        let line = &lines[0];
        assert_eq!(line.0.len(), 1);
        assert!(matches!(
            &line.0[0],
            TokenType {
                kind: TokenKind::StringBegin { s, line_cont: false },
                span: Range { start: 0, end: 13 }
            } if s == " double line"
        ));
        assert!(matches!(
            &line.1,
            TextLine {
                ctx,
                line,
                lineno: 1,
            } if Rc::ptr_eq(&ctx, &src.ctx) && line == "\" double line"
        ));
        let line = &lines[1];
        assert!(matches!(
            &line.0[0],
            TokenType {
                kind: TokenKind::StringEnd(s),
                span: Range { start: 0, end: 8 }
            } if s == "string "
        ));
        assert!(matches!(
            &line.1,
            TextLine {
                ctx,
                line,
                lineno: 2,
            } if Rc::ptr_eq(&ctx, &src.ctx) && line == "string \""
        ));
        assert!(target.cont.is_none());
    }

    #[test]
    fn multi_line_string() {
        let mut src = MockTxtSource::new("\" multi\nline\nstring \"", false);
        let mut target = Lexer::new();

        let r = target.tokenize(&mut src);

        let o = ok_or_fail!(r);
        let lines = extract_or_fail!(o, LexerOutput::Complete);
        assert_eq!(lines.len(), 3);
        let line = &lines[0];
        assert_eq!(line.0.len(), 1);
        assert!(matches!(
            &line.0[0],
            TokenType {
                kind: TokenKind::StringBegin { s, line_cont: false },
                span: Range { start: 0, end: 7 }
            } if s == " multi"
        ));
        assert!(matches!(
            &line.1,
            TextLine {
                ctx,
                line,
                lineno: 1,
            } if Rc::ptr_eq(&ctx, &src.ctx) && line == "\" multi"
        ));
        let line = &lines[1];
        assert!(matches!(
            &line.0[0],
            TokenType {
                kind: TokenKind::StringFragment { s, line_cont: false },
                span: Range { start: 0, end: 4 }
            } if s == "line"
        ));
        assert!(matches!(
            &line.1,
            TextLine {
                ctx,
                line,
                lineno: 2,
            } if Rc::ptr_eq(&ctx, &src.ctx) && line == "line"
        ));
        let line = &lines[2];
        assert!(matches!(
            &line.0[0],
            TokenType {
                kind: TokenKind::StringEnd(s),
                span: Range { start: 0, end: 8 }
            } if s == "string "
        ));
        assert!(matches!(
            &line.1,
            TextLine {
                ctx,
                line,
                lineno: 3,
            } if Rc::ptr_eq(&ctx, &src.ctx) && line == "string \""
        ));
        assert!(target.cont.is_none());
    }

    #[test]
    fn double_line_string_with_errors() {
        let mut src = MockTxtSource::new("\" double \\xZZ; line\n#z string\"", false);
        let mut target = Lexer::new();

        let r = target.tokenize(&mut src);

        let err = err_or_fail!(r);
        let err_lines = err.0;
        assert_eq!(err_lines.len(), 1);
        let TokenErrorLine(errs, line) = extract_or_fail!(&err_lines[0], LineFailure::Tokenize);
        assert_eq!(errs.len(), 1);
        assert!(matches!(
            errs[0],
            TokenType {
                kind: TokenErrorKind::StringExpectedHex { at: 9 },
                span: Range { start: 9, end: 14 }
            }
        ));
        assert!(matches!(
            line,
            TextLine {
                ctx,
                line,
                lineno: 1,
            } if Rc::ptr_eq(&ctx, &src.ctx) && line == "\" double \\xZZ; line"
        ));
        assert!(target.cont.is_none());
    }

    #[test]
    fn double_line_string_with_errors_followed_by_other_error() {
        let mut src = MockTxtSource::new("\" double \\xZZ; line\n#z string\" #z", false);
        let mut target = Lexer::new();

        let r = target.tokenize(&mut src);

        let err = err_or_fail!(r);
        let err_lines = err.0;
        assert_eq!(err_lines.len(), 2);
        let TokenErrorLine(errs, line) = extract_or_fail!(&err_lines[0], LineFailure::Tokenize);
        assert_eq!(errs.len(), 1);
        assert!(matches!(
            errs[0],
            TokenType {
                kind: TokenErrorKind::StringExpectedHex { at: 9 },
                span: Range { start: 9, end: 14 }
            }
        ));
        assert!(matches!(
            line,
            TextLine {
                ctx,
                line,
                lineno: 1,
            } if Rc::ptr_eq(&ctx, &src.ctx) && line == "\" double \\xZZ; line"
        ));
        let TokenErrorLine(errs, line) = extract_or_fail!(&err_lines[1], LineFailure::Tokenize);
        assert_eq!(errs.len(), 1);
        assert!(matches!(
            errs[0],
            TokenType {
                kind: TokenErrorKind::HashInvalid,
                span: Range { start: 11, end: 13 }
            }
        ));
        assert!(matches!(
            line,
            TextLine {
                ctx,
                line,
                lineno: 2,
            } if Rc::ptr_eq(&ctx, &src.ctx) && line == "#z string\" #z"
        ));
        assert!(target.cont.is_none());
    }

    #[test]
    fn double_line_string_with_errors_and_line_continuation() {
        let mut src = MockTxtSource::new("\" double \\xZZ; line \\\n   #z string\"", false);
        let mut target = Lexer::new();

        let r = target.tokenize(&mut src);

        let err = err_or_fail!(r);
        let err_lines = err.0;
        assert_eq!(err_lines.len(), 1);
        let TokenErrorLine(errs, line) = extract_or_fail!(&err_lines[0], LineFailure::Tokenize);
        assert_eq!(errs.len(), 1);
        assert!(matches!(
            errs[0],
            TokenType {
                kind: TokenErrorKind::StringExpectedHex { at: 9 },
                span: Range { start: 9, end: 14 }
            }
        ));
        assert!(matches!(
            line,
            TextLine {
                ctx,
                line,
                lineno: 1,
            } if Rc::ptr_eq(&ctx, &src.ctx) && line == "\" double \\xZZ; line \\"
        ));
        assert!(target.cont.is_none());
    }

    #[test]
    fn double_line_input_with_unterminated_hex_string_error_on_one_line() {
        let mut src = MockTxtSource::new(
            "\"single \\x42 line string\" #t #z\"double\n#z line string\"",
            false,
        );
        let mut target = Lexer::new();

        let r = target.tokenize(&mut src);

        let err = err_or_fail!(r);
        let err_lines = err.0;
        assert_eq!(err_lines.len(), 1);
        let TokenErrorLine(errs, line) = extract_or_fail!(&err_lines[0], LineFailure::Tokenize);
        assert_eq!(errs.len(), 2);
        assert!(matches!(
            errs[0],
            TokenType {
                kind: TokenErrorKind::StringUnterminatedHex { at: 8 },
                span: Range { start: 8, end: 12 }
            }
        ));
        assert!(matches!(
            errs[1],
            TokenType {
                kind: TokenErrorKind::HashInvalid,
                span: Range { start: 29, end: 31 }
            }
        ));
        assert!(matches!(
            line,
            TextLine {
                ctx,
                line,
                lineno: 1,
            } if Rc::ptr_eq(&ctx, &src.ctx) && line == "\"single \\x42 line string\" #t #z\"double"
        ));
        assert!(target.cont.is_none());
    }

    #[test]
    fn double_line_string_prefixed_with_error() {
        let mut src = MockTxtSource::new("#t #z \" double line\n#z string\"", false);
        let mut target = Lexer::new();

        let r = target.tokenize(&mut src);

        let err = err_or_fail!(r);
        let err_lines = err.0;
        assert_eq!(err_lines.len(), 1);
        let TokenErrorLine(errs, line) = extract_or_fail!(&err_lines[0], LineFailure::Tokenize);
        assert_eq!(errs.len(), 1);
        assert!(matches!(
            errs[0],
            TokenType {
                kind: TokenErrorKind::HashInvalid,
                span: Range { start: 3, end: 5 }
            }
        ));
        assert!(matches!(
            line,
            TextLine {
                ctx,
                line,
                lineno: 1,
            } if Rc::ptr_eq(&ctx, &src.ctx) && line == "#t #z \" double line"
        ));
        assert!(target.cont.is_none());
    }

    #[test]
    fn double_line_identifier() {
        let mut src = MockTxtSource::new("| double line\nverbatim |", false);
        let mut target = Lexer::new();

        let r = target.tokenize(&mut src);

        let o = ok_or_fail!(r);
        let lines = extract_or_fail!(o, LexerOutput::Complete);
        assert_eq!(lines.len(), 2);
        let line = &lines[0];
        assert_eq!(line.0.len(), 1);
        assert!(matches!(
            &line.0[0],
            TokenType {
                kind: TokenKind::IdentifierBegin(s),
                span: Range { start: 0, end: 13 }
            } if s == " double line"
        ));
        assert!(matches!(
            &line.1,
            TextLine {
                ctx,
                line,
                lineno: 1,
            } if Rc::ptr_eq(&ctx, &src.ctx) && line == "| double line"
        ));
        let line = &lines[1];
        assert!(matches!(
            &line.0[0],
            TokenType {
                kind: TokenKind::IdentifierEnd(s),
                span: Range { start: 0, end: 10 }
            } if s == "verbatim "
        ));
        assert!(matches!(
            &line.1,
            TextLine {
                ctx,
                line,
                lineno: 2,
            } if Rc::ptr_eq(&ctx, &src.ctx) && line == "verbatim |"
        ));
        assert!(target.cont.is_none());
    }

    #[test]
    fn multi_line_identifier() {
        let mut src = MockTxtSource::new("| multi\nline\nverbatim |", false);
        let mut target = Lexer::new();

        let r = target.tokenize(&mut src);

        let o = ok_or_fail!(r);
        let lines = extract_or_fail!(o, LexerOutput::Complete);
        assert_eq!(lines.len(), 3);
        let line = &lines[0];
        assert_eq!(line.0.len(), 1);
        assert!(matches!(
            &line.0[0],
            TokenType {
                kind: TokenKind::IdentifierBegin(s),
                span: Range { start: 0, end: 7 }
            } if s == " multi"
        ));
        assert!(matches!(
            &line.1,
            TextLine {
                ctx,
                line,
                lineno: 1,
            } if Rc::ptr_eq(&ctx, &src.ctx) && line == "| multi"
        ));
        let line = &lines[1];
        assert!(matches!(
            &line.0[0],
            TokenType {
                kind: TokenKind::IdentifierFragment(s),
                span: Range { start: 0, end: 4 }
            } if s == "line"
        ));
        assert!(matches!(
            &line.1,
            TextLine {
                ctx,
                line,
                lineno: 2,
            } if Rc::ptr_eq(&ctx, &src.ctx) && line == "line"
        ));
        let line = &lines[2];
        assert!(matches!(
            &line.0[0],
            TokenType {
                kind: TokenKind::IdentifierEnd(s),
                span: Range { start: 0, end: 10 }
            } if s == "verbatim "
        ));
        assert!(matches!(
            &line.1,
            TextLine {
                ctx,
                line,
                lineno: 3,
            } if Rc::ptr_eq(&ctx, &src.ctx) && line == "verbatim |"
        ));
        assert!(target.cont.is_none());
    }

    #[test]
    fn double_line_identifier_with_errors() {
        let mut src = MockTxtSource::new("| double \\xZZ; line\n#z verbatim|", false);
        let mut target = Lexer::new();

        let r = target.tokenize(&mut src);

        let err = err_or_fail!(r);
        let err_lines = err.0;
        assert_eq!(err_lines.len(), 1);
        let TokenErrorLine(errs, line) = extract_or_fail!(&err_lines[0], LineFailure::Tokenize);
        assert_eq!(errs.len(), 1);
        assert!(matches!(
            errs[0],
            TokenType {
                kind: TokenErrorKind::IdentifierExpectedHex { at: 9 },
                span: Range { start: 9, end: 14 }
            }
        ));
        assert!(matches!(
            line,
            TextLine {
                ctx,
                line,
                lineno: 1,
            } if Rc::ptr_eq(&ctx, &src.ctx) && line == "| double \\xZZ; line"
        ));
        assert!(target.cont.is_none());
    }

    #[test]
    fn double_line_identifier_with_errors_followed_by_other_error() {
        let mut src = MockTxtSource::new("| double \\xZZ; line\n#z verbatim| #z", false);
        let mut target = Lexer::new();

        let r = target.tokenize(&mut src);

        let err = err_or_fail!(r);
        let err_lines = err.0;
        assert_eq!(err_lines.len(), 2);
        let TokenErrorLine(errs, line) = extract_or_fail!(&err_lines[0], LineFailure::Tokenize);
        assert_eq!(errs.len(), 1);
        assert!(matches!(
            errs[0],
            TokenType {
                kind: TokenErrorKind::IdentifierExpectedHex { at: 9 },
                span: Range { start: 9, end: 14 }
            }
        ));
        assert!(matches!(
            line,
            TextLine {
                ctx,
                line,
                lineno: 1,
            } if Rc::ptr_eq(&ctx, &src.ctx) && line == "| double \\xZZ; line"
        ));
        let TokenErrorLine(errs, line) = extract_or_fail!(&err_lines[1], LineFailure::Tokenize);
        assert_eq!(errs.len(), 1);
        assert!(matches!(
            errs[0],
            TokenType {
                kind: TokenErrorKind::HashInvalid,
                span: Range { start: 13, end: 15 }
            }
        ));
        assert!(matches!(
            line,
            TextLine {
                ctx,
                line,
                lineno: 2,
            } if Rc::ptr_eq(&ctx, &src.ctx) && line == "#z verbatim| #z"
        ));
        assert!(target.cont.is_none());
    }

    #[test]
    fn double_line_input_with_unterminated_hex_identifier_error_on_one_line() {
        let mut src = MockTxtSource::new(
            "|single \\x42 line verbatim| #t #z|double\n#z line verbatim|",
            false,
        );
        let mut target = Lexer::new();

        let r = target.tokenize(&mut src);

        let err = err_or_fail!(r);
        let err_lines = err.0;
        assert_eq!(err_lines.len(), 1);
        let TokenErrorLine(errs, line) = extract_or_fail!(&err_lines[0], LineFailure::Tokenize);
        assert_eq!(errs.len(), 2);
        assert!(matches!(
            errs[0],
            TokenType {
                kind: TokenErrorKind::IdentifierUnterminatedHex { at: 8 },
                span: Range { start: 8, end: 12 }
            }
        ));
        assert!(matches!(
            errs[1],
            TokenType {
                kind: TokenErrorKind::HashInvalid,
                span: Range { start: 31, end: 33 }
            }
        ));
        assert!(matches!(
            line,
            TextLine {
                ctx,
                line,
                lineno: 1,
            } if Rc::ptr_eq(&ctx, &src.ctx) && line == "|single \\x42 line verbatim| #t #z|double"
        ));
        assert!(target.cont.is_none());
    }

    #[test]
    fn double_line_identifier_prefixed_with_error() {
        let mut src = MockTxtSource::new("#t #z | double line\n#z verbatim|", false);
        let mut target = Lexer::new();

        let r = target.tokenize(&mut src);

        let err = err_or_fail!(r);
        let err_lines = err.0;
        assert_eq!(err_lines.len(), 1);
        let TokenErrorLine(errs, line) = extract_or_fail!(&err_lines[0], LineFailure::Tokenize);
        assert_eq!(errs.len(), 1);
        assert!(matches!(
            errs[0],
            TokenType {
                kind: TokenErrorKind::HashInvalid,
                span: Range { start: 3, end: 5 }
            }
        ));
        assert!(matches!(
            line,
            TextLine {
                ctx,
                line,
                lineno: 1,
            } if Rc::ptr_eq(&ctx, &src.ctx) && line == "#t #z | double line"
        ));
        assert!(target.cont.is_none());
    }
}

mod result {
    use super::*;

    #[test]
    fn display_empty_line() {
        let line = TokenLine(Vec::new(), make_textline());

        assert_eq!(line.to_string(), "1:");
    }

    #[test]
    fn display_single_token() {
        let line = TokenLine(
            vec![Token {
                kind: TokenKind::ParenLeft,
                span: 8..14,
            }],
            make_textline(),
        );

        assert_eq!(line.to_string(), "1:LEFTPAREN[8..14]('source')");
    }

    #[test]
    fn display_multiple_tokens() {
        let line = TokenLine(
            vec![
                Token {
                    kind: TokenKind::ParenLeft,
                    span: 0..4,
                },
                Token {
                    kind: TokenKind::ParenRight,
                    span: 5..7,
                },
                Token {
                    kind: TokenKind::ParenLeft,
                    span: 8..14,
                },
            ],
            make_textline(),
        );

        assert_eq!(
            line.to_string(),
            "1:LEFTPAREN[0..4]('line'), \
                RIGHTPAREN[5..7]('of'), \
                LEFTPAREN[8..14]('source')"
        );
    }

    #[test]
    fn display_invalid_span() {
        let line = TokenLine(
            vec![Token {
                kind: TokenKind::ParenLeft,
                span: 8..4,
            }],
            make_textline(),
        );

        assert_eq!(
            line.to_string(),
            "1:LEFTPAREN[8..4]('#<token-invalid-range>')"
        );
    }

    #[test]
    fn display_span_out_of_range() {
        let line = TokenLine(
            vec![Token {
                kind: TokenKind::ParenLeft,
                span: 8..50,
            }],
            make_textline(),
        );

        assert_eq!(
            line.to_string(),
            "1:LEFTPAREN[8..50]('#<token-invalid-range>')"
        );
    }
}

mod error {
    use self::token::TokenErrorKind;
    use super::*;
    use crate::number::NumericError;

    #[test]
    fn display_empty_error() {
        let err = LexerError(Vec::new());

        assert_eq!(err.display_message().to_string(), "");
    }

    #[test]
    fn display_read_error() {
        let inner = TextError::new(TextContext::named("foo"), 3, "OH NO!");
        let err = LexerError(vec![LineFailure::Read(inner)]);

        assert_eq!(
            err.display_message().to_string(),
            "foo:3\n\treadline failure: OH NO!\n"
        );
    }

    #[test]
    fn display_empty_tokenize_errors() {
        let err = LexerError(vec![LineFailure::Tokenize(TokenErrorLine(
            Vec::new(),
            make_textline(),
        ))]);

        assert_eq!(
            err.display_message().to_string(),
            "mylib:1 (lib/mylib.scm)\n\
                \tline of source code\n"
        );
    }

    #[test]
    fn display_single_error() {
        let err = LexerError(vec![LineFailure::Tokenize(TokenErrorLine(
            vec![TokenError {
                kind: TokenErrorKind::NumericError(NumericError::Unimplemented("myerr".to_owned())),
                span: 5..7,
            }],
            make_textline(),
        ))]);

        assert_eq!(
            err.display_message().to_string(),
            "mylib:1 (lib/mylib.scm)\n\
                \tline of source code\n\
                \t     ^^\n\
                6: numeric error: unimplemented number parse: 'myerr'\n"
        );
    }

    #[test]
    fn display_single_error_at_beginning_of_line() {
        let err = LexerError(vec![LineFailure::Tokenize(TokenErrorLine(
            vec![TokenError {
                kind: TokenErrorKind::NumericError(NumericError::Unimplemented("myerr".to_owned())),
                span: 0..4,
            }],
            make_textline(),
        ))]);

        assert_eq!(
            err.display_message().to_string(),
            "mylib:1 (lib/mylib.scm)\n\
                \tline of source code\n\
                \t^^^^\n\
                1: numeric error: unimplemented number parse: 'myerr'\n"
        );
    }

    #[test]
    fn display_multiple_errors() {
        let err = LexerError(vec![LineFailure::Tokenize(TokenErrorLine(
            vec![
                TokenError {
                    kind: TokenErrorKind::NumericError(NumericError::Unimplemented(
                        "myerr".to_owned(),
                    )),
                    span: 5..7,
                },
                TokenError {
                    kind: TokenErrorKind::CharacterExpected,
                    span: 15..19,
                },
            ],
            make_textline(),
        ))]);

        assert_eq!(
            err.display_message().to_string(),
            "mylib:1 (lib/mylib.scm)\n\
                \tline of source code\n\
                \t     ^^        ^^^^\n\
                6: numeric error: unimplemented number parse: 'myerr'\n\
                16: expected character literal\n"
        );
    }

    #[test]
    fn display_single_error_no_filename() {
        let err = LexerError(vec![LineFailure::Tokenize(TokenErrorLine(
            vec![TokenError {
                kind: TokenErrorKind::NumericError(NumericError::Unimplemented("myerr".to_owned())),
                span: 5..7,
            }],
            TextLine {
                ctx: TextContext {
                    name: "mylib".to_owned(),
                    path: None,
                }
                .into(),
                line: "line of source code".to_owned(),
                lineno: 1,
            },
        ))]);

        assert_eq!(
            err.display_message().to_string(),
            "mylib:1\n\
                \tline of source code\n\
                \t     ^^\n\
                6: numeric error: unimplemented number parse: 'myerr'\n"
        );
    }

    #[test]
    fn display_single_error_invalid_span() {
        let err = LexerError(vec![LineFailure::Tokenize(TokenErrorLine(
            vec![TokenError {
                kind: TokenErrorKind::NumericError(NumericError::Unimplemented("myerr".to_owned())),
                span: 5..2,
            }],
            make_textline(),
        ))]);

        assert_eq!(
            err.display_message().to_string(),
            "mylib:1 (lib/mylib.scm)\n\
                \tline of source code\n\
                \t\n\
                6: numeric error: unimplemented number parse: 'myerr'\n"
        );
    }

    #[test]
    fn display_single_error_span_out_of_range() {
        let err = LexerError(vec![LineFailure::Tokenize(TokenErrorLine(
            vec![TokenError {
                kind: TokenErrorKind::NumericError(NumericError::Unimplemented("myerr".to_owned())),
                span: 15..25,
            }],
            make_textline(),
        ))]);

        assert_eq!(
            err.display_message().to_string(),
            "mylib:1 (lib/mylib.scm)\n\
                \tline of source code\n\
                \t               ^^^^^^^^^^\n\
                16: numeric error: unimplemented number parse: 'myerr'\n"
        );
    }

    #[test]
    #[should_panic(expected = "assertion failed: !self.0.is_empty()")]
    fn convert_invalid_line_into_continuation_failure() {
        let line = TokenLine(Vec::new(), make_textline());

        line.into_continuation_unsupported();
    }

    #[test]
    fn token_failure_to_lexer_error() {
        let target = TokenErrorLine(
            vec![TokenError {
                kind: TokenErrorKind::HashInvalid,
                span: 0..1,
            }],
            make_textline(),
        );

        let line_err = LineFailure::from(target);
        let err: LexerError = line_err.into();

        let err_lines = err.0;
        assert_eq!(err_lines.len(), 1);
    }
}

mod display {
    use super::*;

    #[test]
    fn empty_token_stream() {
        let lines = Vec::new();

        let target = DisplayTokenLines(&lines);

        assert_eq!(target.to_string(), "[]");
    }

    #[test]
    fn token_stream() {
        let lines = vec![TokenLine(
            vec![
                Token {
                    kind: TokenKind::ParenLeft,
                    span: 0..1,
                },
                Token {
                    kind: TokenKind::Literal(Literal::Boolean(false)),
                    span: 1..3,
                },
                Token {
                    kind: TokenKind::ParenRight,
                    span: 3..4,
                },
            ],
            TextLine {
                ctx: TextContext {
                    name: "mylib".to_owned(),
                    path: None,
                }
                .into(),
                line: "(#f)".to_owned(),
                lineno: 1,
            },
        )];

        let target = DisplayTokenLines(&lines);

        assert_eq!(
            target.to_string(),
            "[1:LEFTPAREN[0..1]('('), LITERAL<BOOL>[1..3]('#f'), RIGHTPAREN[3..4](')')]"
        );
    }

    #[test]
    fn multiline_token_stream() {
        let lines = vec![
            TokenLine(
                vec![
                    Token {
                        kind: TokenKind::ParenLeft,
                        span: 0..1,
                    },
                    Token {
                        kind: TokenKind::Literal(Literal::Boolean(false)),
                        span: 1..3,
                    },
                    Token {
                        kind: TokenKind::ParenRight,
                        span: 3..4,
                    },
                ],
                TextLine {
                    ctx: TextContext {
                        name: "mylib".to_owned(),
                        path: None,
                    }
                    .into(),
                    line: "(#f)".to_owned(),
                    lineno: 1,
                },
            ),
            TokenLine(
                vec![
                    Token {
                        kind: TokenKind::ParenLeft,
                        span: 0..1,
                    },
                    Token {
                        kind: TokenKind::Literal(Literal::Boolean(true)),
                        span: 2..4,
                    },
                    Token {
                        kind: TokenKind::ParenRight,
                        span: 5..6,
                    },
                ],
                TextLine {
                    ctx: TextContext {
                        name: "mylib".to_owned(),
                        path: None,
                    }
                    .into(),
                    line: "( #t )".to_owned(),
                    lineno: 2,
                },
            ),
            TokenLine(
                vec![
                    Token {
                        kind: TokenKind::ParenLeft,
                        span: 0..1,
                    },
                    Token {
                        kind: TokenKind::Literal(Literal::Character('a')),
                        span: 1..4,
                    },
                    Token {
                        kind: TokenKind::ParenRight,
                        span: 4..5,
                    },
                ],
                TextLine {
                    ctx: TextContext {
                        name: "mylib".to_owned(),
                        path: None,
                    }
                    .into(),
                    line: "(#\\a)".to_owned(),
                    lineno: 3,
                },
            ),
        ];

        let target = DisplayTokenLines(&lines);

        assert_eq!(
            target.to_string(),
            "[\n\
                \t1:LEFTPAREN[0..1]('('), LITERAL<BOOL>[1..3]('#f'), RIGHTPAREN[3..4](')'),\n\
                \t2:LEFTPAREN[0..1]('('), LITERAL<BOOL>[2..4]('#t'), RIGHTPAREN[5..6](')'),\n\
                \t3:LEFTPAREN[0..1]('('), LITERAL<CHAR>[1..4]('#\\a'), RIGHTPAREN[4..5](')'),\n\
                ]"
        );
    }
}
