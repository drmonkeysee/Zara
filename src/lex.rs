mod token;
mod tokenize;

pub(crate) use self::token::{Token, TokenKind};
use self::{
    token::{TokenContinuation, TokenError},
    tokenize::TokenStream,
};
use crate::txt::{TextError, TextLine, TextSource};
use std::{
    error::Error,
    fmt::{self, Display, Formatter, Write},
};

#[derive(Debug)]
pub struct LexLine(Vec<Token>, TextLine);

impl LexLine {
    fn continuation(&self) -> Option<TokenContinuation> {
        self.0.last().and_then(|t| t.kind.to_continuation())
    }

    fn into_continuation_unsupported(mut self) -> LexerError {
        self.0.pop().map_or(LexerError::InvalidOperation, |t| {
            LexerError::Tokenize(TokenizeError(
                vec![t.into_continuation_unsupported()],
                self.1,
            ))
        })
    }
}

impl Display for LexLine {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let LexLine(tokens, txt) = self;
        let token_txt = tokens
            .iter()
            .map(|t| TokenWithSource(t, txt).to_string())
            .collect::<Vec<_>>()
            .join(", ");
        write!(f, "{}:{}", txt.lineno, token_txt)
    }
}

// TODO: used by .flatten() in syntax.rs, removal can de-publicize tokens
impl IntoIterator for LexLine {
    type Item = Token;
    type IntoIter = <Vec<Token> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

#[derive(Debug)]
pub enum LexerError {
    InvalidOperation,
    Read(TextError),
    Tokenize(TokenizeError),
}

#[derive(Debug)]
pub enum LexerError2 {
    InvalidOperation,
    Read(TextError),
    Tokenize(TokenError),
}

impl LexerError {
    pub(crate) fn display_message(&self) -> LexerErrorMessage<'_> {
        LexerErrorMessage(self)
    }
}

impl Display for LexerError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str("fatal error: ")?;
        match self {
            Self::InvalidOperation => f.write_str("invalid lexer operation"),
            Self::Read(err) => err.fmt(f),
            Self::Tokenize(err) => err.fmt(f),
        }
    }
}

impl Error for LexerError {}

impl From<TextError> for LexerError {
    fn from(value: TextError) -> Self {
        Self::Read(value)
    }
}

#[derive(Debug)]
pub struct TokenizeError(Vec<TokenError>, TextLine);

impl Display for TokenizeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str("tokenization failure")
    }
}

impl Error for TokenizeError {}

pub(crate) type LexerResult = Result<LexerOutput, LexerError>;
pub(crate) type LexerResult2 = Result<LexerOutput2, LexerError2>;

#[derive(Debug)]
pub(crate) enum LexerOutput {
    Complete(Vec<LexLine>),
    Continuation,
}

#[derive(Debug)]
pub(crate) enum LexerOutput2 {
    Complete(Token), // TODO: better names for these
    Continuation,
}

pub(crate) struct Lexer {
    cont: Option<(Vec<LexLine>, TokenContinuation)>,
}

impl Lexer {
    pub(crate) fn new() -> Self {
        Self { cont: None }
    }

    pub(crate) fn tokenize(&mut self, src: &mut impl TextSource) -> LexerResult {
        let (mut d, prev) = if let Some((prev_lines, token_cont)) = self.cont.take() {
            (LexerDriver::cont(token_cont), Some(prev_lines))
        } else {
            (LexerDriver::new(), None)
        };
        let mut new_lines = d.tokenize(src)?;
        let lines = prev
            .and_then(|mut p| {
                p.append(&mut new_lines);
                Some(p)
            })
            .unwrap_or(new_lines);
        if let Some(token_cont) = lines.last().and_then(LexLine::continuation) {
            return self.continuation_result(token_cont, src.can_continue(), lines);
        }
        Ok(LexerOutput::Complete(lines))
    }

    fn continuation_result(
        &mut self,
        token_cont: TokenContinuation,
        can_continue: bool,
        mut lines: Vec<LexLine>,
    ) -> LexerResult {
        if can_continue {
            self.cont = Some((lines, token_cont));
            Ok(LexerOutput::Continuation)
        } else {
            Err(lines.pop().map_or(
                LexerError::InvalidOperation,
                LexLine::into_continuation_unsupported,
            ))
        }
    }
}

pub(crate) struct Lexer2<T> {
    src: T,
}

impl<T: TextSource> Lexer2<T> {
    pub(crate) fn new(src: T) -> Self {
        Self { src }
    }
}

impl<T> Iterator for Lexer2<T> {
    type Item = LexerResult2;

    fn next(&mut self) -> Option<Self::Item> {
        todo!()
    }
}

pub(crate) struct LexerErrorMessage<'a>(&'a LexerError);

impl Display for LexerErrorMessage<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.0 {
            LexerError::InvalidOperation => f.write_str(
                "invalid operation attempted on lexer output; this is likely a library logic error!\n",
            ),
            LexerError::Read(err) => err.display_header().fmt(f),
            LexerError::Tokenize(err) => TokenizeErrorMessage(err).fmt(f),
        }
    }
}

pub(crate) struct DisplayLexLines<'a>(pub(crate) &'a [LexLine]);

impl DisplayLexLines<'_> {
    fn flatten_to_string(&self, cvt: impl FnMut(&LexLine) -> String) -> String {
        self.0.iter().map(cvt).collect()
    }
}

impl Display for DisplayLexLines<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if self.0.len() < 2 {
            write!(f, "[{}]", self.flatten_to_string(LexLine::to_string))
        } else {
            write!(
                f,
                "[\n{}]",
                self.flatten_to_string(|ln| format!("\t{ln},\n"))
            )
        }
    }
}

pub(crate) struct LexLinesMessage<'a>(pub(crate) &'a [LexLine]);

impl Display for LexLinesMessage<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for line in self.0 {
            LexLineMessage(line).fmt(f)?;
        }
        Ok(())
    }
}

type LexerDriverResult = Result<Vec<LexLine>, LexerError>;

struct LexerDriver {
    cont: Option<TokenContinuation>,
}

impl LexerDriver {
    fn new() -> Self {
        Self { cont: None }
    }

    fn cont(cont: TokenContinuation) -> Self {
        Self { cont: Some(cont) }
    }

    fn tokenize(&mut self, src: &mut impl TextSource) -> LexerDriverResult {
        src.map(|tr| self.tokenize_line(tr?)).collect()
    }

    fn tokenize_line(&mut self, text: TextLine) -> Result<LexLine, LexerError> {
        let mut errors = Vec::new();
        let tokens: Vec<_> = TokenStream::new(&text.line, self.cont.take())
            .filter_map(|tr| tr.map_err(|err| errors.push(err)).ok())
            .collect();
        if errors.is_empty() {
            let lines = LexLine(tokens, text);
            self.cont = lines.continuation();
            Ok(lines)
        } else {
            Err(LexerError::Tokenize(TokenizeError(errors, text)))
        }
    }
}

struct LexLineMessage<'a>(&'a LexLine);

impl Display for LexLineMessage<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let LexLine(tokens, txt) = self.0;
        for token in tokens {
            TokenWithSourceMessage(token, txt).fmt(f)?
        }
        Ok(())
    }
}

struct TokenWithSource<'a>(&'a Token, &'a TextLine);

impl Display for TokenWithSource<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let Self(t, txt) = *self;
        write!(
            f,
            "{t}('{}')",
            txt.line
                .get(t.span.clone())
                .unwrap_or("#<token-invalid-range>")
        )
    }
}

struct TokenWithSourceMessage<'a>(&'a Token, &'a TextLine);

impl Display for TokenWithSourceMessage<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let Self(t, txt) = *self;
        writeln!(
            f,
            "{:20}{:30}'{}'",
            format!("{}:{:?}", txt.lineno, t.span),
            t.kind.to_string(),
            txt.line.get(t.span.clone()).unwrap_or("INVALID RANGE")
        )
    }
}

struct TokenizeErrorMessage<'a>(&'a TokenizeError);

impl Display for TokenizeErrorMessage<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let TokenizeError(errs, txtline) = self.0;
        txtline.display_header().fmt(f)?;

        if errs.is_empty() {
            return Ok(());
        }

        let mut cursor = 0;
        f.write_char('\t')?;
        for span in errs
            .iter()
            .filter_map(|err| (!err.span.is_empty()).then_some(&err.span))
        {
            write!(
                f,
                "{0:>1$}{2:^<3$}",
                "^",
                span.start + 1 - cursor,
                "",
                span.len() - 1
            )?;
            cursor = span.end;
        }
        f.write_char('\n')?;
        for err in errs {
            writeln!(f, "{}: {err}", err.span.start + 1)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{literal::Literal, txt::TextContext};
    use std::path::Path;

    mod lexer {
        use self::token::{TokenErrorKind, TokenType};
        use super::*;
        use crate::txt::{LineNumber, TextResult};
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

            assert!(r.is_ok());
            let o = r.unwrap();
            assert!(matches!(o, LexerOutput::Complete(_)));
            let lines = if let LexerOutput::Complete(toks) = o {
                toks
            } else {
                unreachable!();
            };
            assert!(lines.is_empty());
            assert!(target.cont.is_none());
        }

        #[test]
        fn single_token() {
            let mut src = MockTxtSource::new("#t", false);
            let mut target = Lexer::new();

            let r = target.tokenize(&mut src);

            assert!(r.is_ok());
            let o = r.unwrap();
            assert!(matches!(o, LexerOutput::Complete(_)));
            let lines = if let LexerOutput::Complete(toks) = o {
                toks
            } else {
                unreachable!();
            };
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

            assert!(r.is_ok());
            let o = r.unwrap();
            assert!(matches!(o, LexerOutput::Complete(_)));
            let lines = if let LexerOutput::Complete(toks) = o {
                toks
            } else {
                unreachable!();
            };
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

            assert!(r.is_ok());
            let o = r.unwrap();
            assert!(matches!(o, LexerOutput::Complete(_)));
            let lines = if let LexerOutput::Complete(toks) = o {
                toks
            } else {
                unreachable!();
            };
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

            assert!(r.is_err());
            let err = r.unwrap_err();
            assert!(matches!(err, LexerError::Tokenize(_)));
            let (errs, line) = if let LexerError::Tokenize(TokenizeError(es, ln)) = err {
                (es, ln)
            } else {
                unreachable!()
            };
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
            assert!(target.cont.is_none());
        }

        #[test]
        fn multi_lines_with_read_error() {
            let mut src = MockTxtSource::new("#t\nBAD BYTES\n#f #f\n", false);
            let mut target = Lexer::new();

            let r = target.tokenize(&mut src);

            assert!(r.is_err());
            let err = r.unwrap_err();
            assert!(matches!(err, LexerError::Read(_)));
            let inner = if let LexerError::Read(inn) = err {
                inn
            } else {
                unreachable!()
            };
            assert!(Rc::ptr_eq(&inner.ctx, &src.ctx));
            assert_eq!(inner.lineno, 2);
            assert!(inner.source().is_some());
            assert!(target.cont.is_none());
        }

        #[test]
        fn continuation_token_persists_on_lexer() {
            let mut src = MockTxtSource::new("#t #|trailing...", true);
            let mut target = Lexer::new();

            let r = target.tokenize(&mut src);

            assert!(r.is_ok());
            let o = r.unwrap();
            assert!(matches!(o, LexerOutput::Continuation));
            assert!(target.cont.is_some());
            let (lines, cont) = if let Some((ln, c)) = target.cont {
                (ln, c)
            } else {
                unreachable!();
            };
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
                    kind: TokenKind::CommentBlockBegin(0),
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
            assert!(matches!(cont, TokenContinuation::BlockComment(0)));
        }

        #[test]
        fn continuation_token_converted_to_error_if_src_does_not_support() {
            let mut src = MockTxtSource::new("#t #|trailing...", false);
            let mut target = Lexer::new();

            let r = target.tokenize(&mut src);

            assert!(r.is_err());
            let err = r.unwrap_err();
            assert!(matches!(err, LexerError::Tokenize(_)));
            let (errs, line) = if let LexerError::Tokenize(TokenizeError(es, ln)) = err {
                (es, ln)
            } else {
                unreachable!()
            };
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
        fn continuation_cleared_on_error() {
            let mut src = MockTxtSource::new("#t #|trailing...", true);
            let mut target = Lexer::new();

            let r = target.tokenize(&mut src);

            assert!(r.is_ok());
            assert!(target.cont.is_some());

            let mut src = MockTxtSource::new("...finish|# invalid_token", true);

            let r = target.tokenize(&mut src);

            assert!(r.is_err());
            assert!(target.cont.is_none());
        }

        #[test]
        fn double_line_comment() {
            let mut src = MockTxtSource::new("#| double line\ncomment |#", false);
            let mut target = Lexer::new();

            let r = target.tokenize(&mut src);

            assert!(r.is_ok());
            let o = r.unwrap();
            assert!(matches!(o, LexerOutput::Complete(_)));
            let lines = if let LexerOutput::Complete(toks) = o {
                toks
            } else {
                unreachable!();
            };
            assert_eq!(lines.len(), 2);
            let line = &lines[0];
            assert_eq!(line.0.len(), 1);
            assert!(matches!(
                line.0[0],
                TokenType {
                    kind: TokenKind::CommentBlockBegin(0),
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

            assert!(r.is_ok());
            let o = r.unwrap();
            assert!(matches!(o, LexerOutput::Complete(_)));
            let lines = if let LexerOutput::Complete(toks) = o {
                toks
            } else {
                unreachable!();
            };
            assert_eq!(lines.len(), 3);
            let line = &lines[0];
            assert_eq!(line.0.len(), 1);
            assert!(matches!(
                line.0[0],
                TokenType {
                    kind: TokenKind::CommentBlockBegin(0),
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
                    kind: TokenKind::CommentBlockFragment(0),
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
        fn double_line_string() {
            let mut src = MockTxtSource::new("\" double line\nstring \"", false);
            let mut target = Lexer::new();

            let r = target.tokenize(&mut src);

            assert!(r.is_ok());
            let o = r.unwrap();
            assert!(matches!(o, LexerOutput::Complete(_)));
            let lines = if let LexerOutput::Complete(toks) = o {
                toks
            } else {
                unreachable!();
            };
            assert_eq!(lines.len(), 2);
            let line = &lines[0];
            assert_eq!(line.0.len(), 1);
            assert!(matches!(
                &line.0[0],
                TokenType {
                    kind: TokenKind::StringBegin(s, false),
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

            assert!(r.is_ok());
            let o = r.unwrap();
            assert!(matches!(o, LexerOutput::Complete(_)));
            let lines = if let LexerOutput::Complete(toks) = o {
                toks
            } else {
                unreachable!();
            };
            assert_eq!(lines.len(), 3);
            let line = &lines[0];
            assert_eq!(line.0.len(), 1);
            assert!(matches!(
                &line.0[0],
                TokenType {
                    kind: TokenKind::StringBegin(s, false),
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
                    kind: TokenKind::StringFragment(s, false),
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
    }

    mod result {
        use super::*;

        #[test]
        fn display_empty_line() {
            let line = LexLine(Vec::new(), make_textline());

            assert_eq!(line.to_string(), "1:");
        }

        #[test]
        fn display_single_token() {
            let line = LexLine(
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
            let line = LexLine(
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
            let line = LexLine(
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
            let line = LexLine(
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

        #[test]
        fn display_read_error() {
            let inner = TextError::new(TextContext::named("foo"), 3, "OH NO!");
            let err = LexerError::Read(inner);

            assert_eq!(
                err.display_message().to_string(),
                "foo:3\n\tunable to read text line\n"
            );
        }

        #[test]
        fn display_empty_errors() {
            let err = LexerError::Tokenize(TokenizeError(Vec::new(), make_textline()));

            assert_eq!(
                err.display_message().to_string(),
                "mylib:1 (lib/mylib.scm)\n\
                \tline of source code\n"
            );
        }

        #[test]
        fn display_single_error() {
            let err = LexerError::Tokenize(TokenizeError(
                vec![TokenError {
                    kind: TokenErrorKind::Unimplemented("myerr".to_owned()),
                    span: 5..7,
                }],
                make_textline(),
            ));

            assert_eq!(
                err.display_message().to_string(),
                "mylib:1 (lib/mylib.scm)\n\
                \tline of source code\n\
                \t     ^^\n\
                6: unimplemented tokenization: 'myerr'\n"
            );
        }

        #[test]
        fn display_single_error_at_beginning_of_line() {
            let err = LexerError::Tokenize(TokenizeError(
                vec![TokenError {
                    kind: TokenErrorKind::Unimplemented("myerr".to_owned()),
                    span: 0..4,
                }],
                make_textline(),
            ));

            assert_eq!(
                err.display_message().to_string(),
                "mylib:1 (lib/mylib.scm)\n\
                \tline of source code\n\
                \t^^^^\n\
                1: unimplemented tokenization: 'myerr'\n"
            );
        }

        #[test]
        fn display_multiple_errors() {
            let err = LexerError::Tokenize(TokenizeError(
                vec![
                    TokenError {
                        kind: TokenErrorKind::Unimplemented("myerr".to_owned()),
                        span: 5..7,
                    },
                    TokenError {
                        kind: TokenErrorKind::CharacterExpected,
                        span: 15..19,
                    },
                ],
                make_textline(),
            ));

            assert_eq!(
                err.display_message().to_string(),
                "mylib:1 (lib/mylib.scm)\n\
                \tline of source code\n\
                \t     ^^        ^^^^\n\
                6: unimplemented tokenization: 'myerr'\n\
                16: expected character literal\n"
            );
        }

        #[test]
        fn display_single_error_no_filename() {
            let err = LexerError::Tokenize(TokenizeError(
                vec![TokenError {
                    kind: TokenErrorKind::Unimplemented("myerr".to_owned()),
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
            ));

            assert_eq!(
                err.display_message().to_string(),
                "mylib:1\n\
                \tline of source code\n\
                \t     ^^\n\
                6: unimplemented tokenization: 'myerr'\n"
            );
        }

        #[test]
        fn display_single_error_invalid_span() {
            let err = LexerError::Tokenize(TokenizeError(
                vec![TokenError {
                    kind: TokenErrorKind::Unimplemented("myerr".to_owned()),
                    span: 5..2,
                }],
                make_textline(),
            ));

            assert_eq!(
                err.display_message().to_string(),
                "mylib:1 (lib/mylib.scm)\n\
                \tline of source code\n\
                \t\n\
                6: unimplemented tokenization: 'myerr'\n"
            );
        }

        #[test]
        fn display_single_error_span_out_of_range() {
            let err = LexerError::Tokenize(TokenizeError(
                vec![TokenError {
                    kind: TokenErrorKind::Unimplemented("myerr".to_owned()),
                    span: 15..25,
                }],
                make_textline(),
            ));

            assert_eq!(
                err.display_message().to_string(),
                "mylib:1 (lib/mylib.scm)\n\
                \tline of source code\n\
                \t               ^^^^^^^^^^\n\
                16: unimplemented tokenization: 'myerr'\n"
            );
        }

        #[test]
        fn display_invalid_op() {
            let err = LexerError::InvalidOperation;

            assert_eq!(
                err.display_message().to_string(),
                "invalid operation attempted on lexer output; this is likely a library logic error!\n"
            );
        }

        #[test]
        fn convert_invalid_line_into_continuation_failure() {
            let line = LexLine(Vec::new(), make_textline());

            let err = line.into_continuation_unsupported();

            assert!(matches!(err, LexerError::InvalidOperation));
        }
    }

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

    mod display {
        use super::*;

        #[test]
        fn display_empty_token_stream() {
            let lines = Vec::new();

            let target = DisplayLexLines(&lines);

            assert_eq!(target.to_string(), "[]");
        }

        #[test]
        fn display_token_stream() {
            let lines = vec![LexLine(
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

            let target = DisplayLexLines(&lines);

            assert_eq!(
                target.to_string(),
                "[1:LEFTPAREN[0..1]('('), LITERAL<BOOL>[1..3]('#f'), RIGHTPAREN[3..4](')')]"
            );
        }

        #[test]
        fn display_multiline_token_stream() {
            let lines = vec![
                LexLine(
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
                LexLine(
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
                LexLine(
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

            let target = DisplayLexLines(&lines);

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
}
