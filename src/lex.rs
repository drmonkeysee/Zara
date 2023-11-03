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
    ops::ControlFlow,
};

#[derive(Debug)]
pub struct TokenLine(Vec<Token>, TextLine);

impl TokenLine {
    fn continuation(&self) -> Option<TokenContinuation> {
        self.0.last().and_then(|t| t.kind.to_continuation())
    }

    fn into_continuation_unsupported(mut self) -> LexerError {
        self.0.pop().map_or(LexerError::InvalidOperation, |t| {
            LexerError::Lines(vec![LineFailure::Tokenize(TokenErrorLine(
                vec![t.into_continuation_unsupported()],
                self.1,
            ))])
        })
    }
}

// TODO: this should probably be a datum new-type representation
impl Display for TokenLine {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let TokenLine(tokens, txt) = self;
        let token_txt = tokens
            .iter()
            .map(|t| TokenWithSource(t, txt).to_string())
            .collect::<Vec<_>>()
            .join(", ");
        write!(f, "{}:{}", txt.lineno, token_txt)
    }
}

// TODO: used by .flatten() in syntax.rs, removal can de-publicize tokens
impl IntoIterator for TokenLine {
    type Item = Token;
    type IntoIter = <Vec<Token> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

#[derive(Debug)]
pub enum LineFailure {
    Read(TextError),
    Tokenize(TokenErrorLine),
}

impl LineFailure {
    fn accumulate(&self, acc: LineFailureAcc) -> ControlFlow<(), LineFailureAcc> {
        match self {
            Self::Read(_) => {
                if matches!(acc, LineFailureAcc::Empty | LineFailureAcc::Read) {
                    ControlFlow::Continue(LineFailureAcc::Read)
                } else {
                    ControlFlow::Break(())
                }
            }
            Self::Tokenize(_) => {
                if matches!(acc, LineFailureAcc::Empty | LineFailureAcc::Tokenize) {
                    ControlFlow::Continue(LineFailureAcc::Tokenize)
                } else {
                    ControlFlow::Break(())
                }
            }
        }
    }
}

impl From<TextError> for LineFailure {
    fn from(value: TextError) -> Self {
        Self::Read(value)
    }
}

impl From<TokenErrorLine> for LineFailure {
    fn from(value: TokenErrorLine) -> Self {
        Self::Tokenize(value)
    }
}

#[derive(Debug)]
pub enum LexerError {
    InvalidOperation,
    Lines(Vec<LineFailure>),
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
            LexerError::Lines(lines) => DisplayLineFailures(lines).fmt(f),
        }
    }
}

impl Error for LexerError {}

#[derive(Debug)]
pub struct TokenErrorLine(Vec<TokenError>, TextLine);

pub(crate) type LexerResult = Result<LexerOutput, LexerError>;

#[derive(Debug)]
pub(crate) enum LexerOutput {
    Complete(Vec<TokenLine>),
    Continuation,
}

pub(crate) struct Lexer {
    cont: Option<(Vec<TokenLine>, TokenContinuation)>,
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
        if let Some(token_cont) = lines.last().and_then(TokenLine::continuation) {
            return self.continuation_result(token_cont, src.can_continue(), lines);
        }
        Ok(LexerOutput::Complete(lines))
    }

    fn continuation_result(
        &mut self,
        token_cont: TokenContinuation,
        can_continue: bool,
        mut lines: Vec<TokenLine>,
    ) -> LexerResult {
        if can_continue {
            self.cont = Some((lines, token_cont));
            Ok(LexerOutput::Continuation)
        } else {
            Err(lines.pop().map_or(
                LexerError::InvalidOperation,
                TokenLine::into_continuation_unsupported,
            ))
        }
    }
}

pub(crate) struct LexerErrorMessage<'a>(&'a LexerError);

impl Display for LexerErrorMessage<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.0 {
            LexerError::InvalidOperation => f.write_str(
                "invalid operation attempted on lexer output; this is likely a library logic error!\n",
            ),
            LexerError::Lines(lines) => LineFailuresMessage(lines).fmt(f),
        }
    }
}

pub(crate) struct DisplayTokenLines<'a>(pub(crate) &'a [TokenLine]);

impl DisplayTokenLines<'_> {
    fn flatten_to_string(&self, cvt: impl FnMut(&TokenLine) -> String) -> String {
        self.0.iter().map(cvt).collect()
    }
}

impl Display for DisplayTokenLines<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if self.0.len() < 2 {
            write!(f, "[{}]", self.flatten_to_string(TokenLine::to_string))
        } else {
            write!(
                f,
                "[\n{}]",
                self.flatten_to_string(|ln| format!("\t{ln},\n"))
            )
        }
    }
}

pub(crate) struct TokenLinesMessage<'a>(pub(crate) &'a [TokenLine]);

impl Display for TokenLinesMessage<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for line in self.0 {
            TokenLineMessage(line).fmt(f)?;
        }
        Ok(())
    }
}

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

    fn tokenize(&mut self, src: &mut impl TextSource) -> Result<Vec<TokenLine>, LexerError> {
        let (token_lines, err_lines): (Vec<_>, Vec<_>) = src
            .map(|tr| self.tokenize_line(tr?))
            .partition(|r| r.is_ok());
        if err_lines.is_empty() {
            Ok(token_lines.into_iter().flatten().collect())
        } else {
            Err(LexerError::Lines(
                err_lines.into_iter().flat_map(|r| r.err()).collect(),
            ))
        }
    }

    fn tokenize_line(&mut self, text: TextLine) -> Result<TokenLine, LineFailure> {
        let (tokens, errs): (Vec<_>, Vec<_>) =
            TokenStream::new(&text.line, self.cont.take()).partition(|r| r.is_ok());
        if errs.is_empty() {
            let line = TokenLine(tokens.into_iter().flatten().collect(), text);
            self.cont = line.continuation();
            Ok(line)
        } else {
            Err(TokenErrorLine(
                errs.into_iter().flat_map(|r| r.err()).collect(),
                text,
            ))?
        }
    }
}

struct TokenLineMessage<'a>(&'a TokenLine);

impl Display for TokenLineMessage<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let TokenLine(tokens, txt) = self.0;
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

enum LineFailureAcc {
    Empty,
    Read,
    Tokenize,
}

struct DisplayLineFailures<'a>(&'a [LineFailure]);

impl Display for DisplayLineFailures<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let ctrl = self
            .0
            .into_iter()
            .try_fold(LineFailureAcc::Empty, |acc, f| f.accumulate(acc));
        match ctrl {
            ControlFlow::Break(_) => f.write_str("multiple lexer failures"),
            ControlFlow::Continue(acc) => match acc {
                LineFailureAcc::Empty => Ok(()),
                LineFailureAcc::Read => f.write_str("read failure"),
                LineFailureAcc::Tokenize => f.write_str("tokenization failure"),
            },
        }
    }
}

struct LineFailuresMessage<'a>(&'a [LineFailure]);

impl Display for LineFailuresMessage<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for line in self.0 {
            LineFailureMessage(line).fmt(f)?;
        }
        Ok(())
    }
}

struct LineFailureMessage<'a>(&'a LineFailure);

impl Display for LineFailureMessage<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.0 {
            LineFailure::Read(err) => err.display_header().fmt(f),
            LineFailure::Tokenize(err) => TokenErrorLineMessage(err).fmt(f),
        }
    }
}

struct TokenErrorLineMessage<'a>(&'a TokenErrorLine);

impl Display for TokenErrorLineMessage<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let TokenErrorLine(errs, txtline) = self.0;
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
            assert!(matches!(err, LexerError::Lines(_)));
            let err_lines = if let LexerError::Lines(errs) = err {
                errs
            } else {
                unreachable!()
            };
            assert_eq!(err_lines.len(), 2);
            assert!(matches!(err_lines[0], LineFailure::Tokenize(_)));
            let (errs, line) = if let LineFailure::Tokenize(TokenErrorLine(es, ln)) = &err_lines[0]
            {
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
            assert!(matches!(err_lines[1], LineFailure::Tokenize(_)));
            let (errs, line) = if let LineFailure::Tokenize(TokenErrorLine(es, ln)) = &err_lines[1]
            {
                (es, ln)
            } else {
                unreachable!()
            };
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

            assert!(r.is_err());
            let err = r.unwrap_err();
            assert!(matches!(err, LexerError::Lines(_)));
            let err_lines = if let LexerError::Lines(lines) = err {
                lines
            } else {
                unreachable!()
            };
            assert_eq!(err_lines.len(), 1);
            assert!(matches!(err_lines[0], LineFailure::Read(_)));
            let inner = if let LineFailure::Read(inn) = &err_lines[0] {
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
        fn mixed_errors() {
            let mut src = MockTxtSource::new("#t\n #z #f #z #\\a\nBAD BYTES", false);
            let mut target = Lexer::new();

            let r = target.tokenize(&mut src);

            assert!(r.is_err());
            let err = r.unwrap_err();
            assert!(matches!(err, LexerError::Lines(_)));
            let err_lines = if let LexerError::Lines(errs) = err {
                errs
            } else {
                unreachable!()
            };
            assert_eq!(err_lines.len(), 2);
            assert!(matches!(err_lines[0], LineFailure::Tokenize(_)));
            let (errs, line) = if let LineFailure::Tokenize(TokenErrorLine(es, ln)) = &err_lines[0]
            {
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
            assert!(matches!(err_lines[1], LineFailure::Read(_)));
            let inner = if let LineFailure::Read(inn) = &err_lines[1] {
                inn
            } else {
                unreachable!()
            };
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
            assert!(matches!(err, LexerError::Lines(_)));
            let err_lines = if let LexerError::Lines(errs) = err {
                errs
            } else {
                unreachable!()
            };
            assert_eq!(err_lines.len(), 1);
            assert!(matches!(err_lines[0], LineFailure::Tokenize(_)));
            let (errs, line) = if let LineFailure::Tokenize(TokenErrorLine(es, ln)) = &err_lines[0]
            {
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

        #[test]
        fn display_empty_error() {
            let err = LexerError::Lines(Vec::new());

            assert_eq!(err.display_message().to_string(), "");
        }

        #[test]
        fn display_read_error() {
            let inner = TextError::new(TextContext::named("foo"), 3, "OH NO!");
            let err = LexerError::Lines(vec![LineFailure::Read(inner)]);

            assert_eq!(
                err.display_message().to_string(),
                "foo:3\n\treadline failure: OH NO!\n"
            );
        }

        #[test]
        fn display_empty_tokenize_errors() {
            let err = LexerError::Lines(vec![LineFailure::Tokenize(TokenErrorLine(
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
            let err = LexerError::Lines(vec![LineFailure::Tokenize(TokenErrorLine(
                vec![TokenError {
                    kind: TokenErrorKind::Unimplemented("myerr".to_owned()),
                    span: 5..7,
                }],
                make_textline(),
            ))]);

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
            let err = LexerError::Lines(vec![LineFailure::Tokenize(TokenErrorLine(
                vec![TokenError {
                    kind: TokenErrorKind::Unimplemented("myerr".to_owned()),
                    span: 0..4,
                }],
                make_textline(),
            ))]);

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
            let err = LexerError::Lines(vec![LineFailure::Tokenize(TokenErrorLine(
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
            ))]);

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
            let err = LexerError::Lines(vec![LineFailure::Tokenize(TokenErrorLine(
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
            ))]);

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
            let err = LexerError::Lines(vec![LineFailure::Tokenize(TokenErrorLine(
                vec![TokenError {
                    kind: TokenErrorKind::Unimplemented("myerr".to_owned()),
                    span: 5..2,
                }],
                make_textline(),
            ))]);

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
            let err = LexerError::Lines(vec![LineFailure::Tokenize(TokenErrorLine(
                vec![TokenError {
                    kind: TokenErrorKind::Unimplemented("myerr".to_owned()),
                    span: 15..25,
                }],
                make_textline(),
            ))]);

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
            let line = TokenLine(Vec::new(), make_textline());

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

            let target = DisplayTokenLines(&lines);

            assert_eq!(target.to_string(), "[]");
        }

        #[test]
        fn display_token_stream() {
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
        fn display_multiline_token_stream() {
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
}
