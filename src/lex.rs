#[cfg(test)]
mod tests;
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

pub(crate) type LexerResult = Result<LexerOutput, LexerError>;

#[derive(Debug)]
pub(crate) enum LexerOutput {
    Complete(Vec<TokenLine>),
    Continuation,
}

#[derive(Debug)]
pub(crate) struct LexerError(Vec<LineFailure>);

impl LexerError {
    pub(crate) fn display_message(&self) -> LexerErrorMessage {
        LexerErrorMessage(&self.0)
    }
}

impl Display for LexerError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let ctrl = self
            .0
            .iter()
            .try_fold(LineFailureAcc::Empty, |acc, ln| ln.accumulate(acc));
        format!(
            "fatal error: {}",
            match ctrl {
                ControlFlow::Break(()) => "multiple lexer failures",
                ControlFlow::Continue(LineFailureAcc::Empty) => "unknown failure",
                ControlFlow::Continue(LineFailureAcc::Read) => "read failure",
                ControlFlow::Continue(LineFailureAcc::Tokenize) => "tokenization failure",
            }
        )
        .fmt(f)
    }
}

impl Error for LexerError {}

impl From<LineFailure> for LexerError {
    fn from(value: LineFailure) -> Self {
        Self(vec![value])
    }
}

pub(crate) struct Lexer {
    cont: Option<(Vec<TokenLine>, TokenContinuation)>,
}

impl Lexer {
    pub(crate) fn new() -> Self {
        Self { cont: None }
    }

    pub(crate) fn tokenize(&mut self, src: &mut impl TextSource) -> LexerResult {
        let (mut d, prev) = self.cont.take().map_or_else(
            || (LexerDriver::new(), None),
            |(prev_lines, token_cont)| (LexerDriver::cont(token_cont), Some(prev_lines)),
        );
        let mut new_lines = d.tokenize(src)?;
        let lines = prev
            .map(|mut p| {
                p.append(&mut new_lines);
                p
            })
            .unwrap_or(new_lines);
        match d.cont.take() {
            Some(token_cont) => self.continuation_result(token_cont, src.can_continue(), lines),
            None => Ok(LexerOutput::Complete(lines)),
        }
    }

    fn continuation_result(
        &mut self,
        cont: TokenContinuation,
        can_continue: bool,
        mut lines: Vec<TokenLine>,
    ) -> LexerResult {
        if can_continue {
            self.cont = Some((lines, cont));
            Ok(LexerOutput::Continuation)
        } else {
            debug_assert!(!lines.is_empty());
            Err(lines.pop().unwrap().into_continuation_unsupported(cont))
        }
    }
}

#[derive(Debug)]
pub(crate) enum LineFailure {
    Read(TextError),
    Tokenize(TokenErrorLine),
}

impl LineFailure {
    fn accumulate(&self, acc: LineFailureAcc) -> ControlFlow<(), LineFailureAcc> {
        match self {
            Self::Read(_) => match acc {
                LineFailureAcc::Empty | LineFailureAcc::Read => {
                    ControlFlow::Continue(LineFailureAcc::Read)
                }
                LineFailureAcc::Tokenize => ControlFlow::Break(()),
            },
            Self::Tokenize(_) => match acc {
                LineFailureAcc::Empty | LineFailureAcc::Tokenize => {
                    ControlFlow::Continue(LineFailureAcc::Tokenize)
                }
                LineFailureAcc::Read => ControlFlow::Break(()),
            },
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
pub(crate) struct TokenLine(Vec<Token>, TextLine);

impl TokenLine {
    fn into_continuation_unsupported(self, cont: TokenContinuation) -> LexerError {
        let Self(mut tokens, txtline) = self;
        debug_assert!(!tokens.is_empty());
        LineFailure::from(TokenErrorLine(
            vec![TokenError {
                kind: cont.into(),
                span: tokens.pop().unwrap().span,
            }],
            txtline,
        ))
        .into()
    }
}

impl Display for TokenLine {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let Self(tokens, txt) = self;
        let token_txt = tokens
            .iter()
            .map(|t| TokenWithSource(t, txt).to_string())
            .collect::<Vec<_>>()
            .join(", ");
        format!("{}:{}", txt.lineno, token_txt).fmt(f)
    }
}

// NOTE: used by .flatten() in syntax.rs
impl IntoIterator for TokenLine {
    type Item = Token;
    type IntoIter = <Vec<Token> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

#[derive(Debug)]
pub(crate) struct TokenErrorLine(Vec<TokenError>, TextLine);

pub(crate) struct LexerErrorMessage<'a>(&'a [LineFailure]);

impl Display for LexerErrorMessage<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        for line in self.0 {
            LineFailureMessage(line).fmt(f)?;
        }
        Ok(())
    }
}

pub(crate) struct DisplayTokenLines<'a>(pub(crate) &'a [TokenLine]);

impl DisplayTokenLines<'_> {
    fn flatten_to_string(&self, cvt: impl FnMut(&TokenLine) -> String) -> String {
        self.0.iter().map(cvt).collect()
    }
}

impl Display for DisplayTokenLines<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
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
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        for line in self.0 {
            TokenLineMessage(line).fmt(f)?;
        }
        Ok(())
    }
}

struct TokenLineMessage<'a>(&'a TokenLine);

impl Display for TokenLineMessage<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let TokenLine(tokens, txt) = self.0;
        for token in tokens {
            TokenWithSourceMessage(token, txt).fmt(f)?;
        }
        Ok(())
    }
}

struct TokenWithSource<'a>(&'a Token, &'a TextLine);

impl Display for TokenWithSource<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let Self(t, txt) = *self;
        format!(
            "{t}('{}')",
            txt.line
                .get(t.span.clone())
                .unwrap_or("#<token-invalid-range>")
        )
        .fmt(f)
    }
}

struct TokenWithSourceMessage<'a>(&'a Token, &'a TextLine);

impl Display for TokenWithSourceMessage<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let Self(t, txt) = *self;
        writeln!(
            f,
            "{:20}{:30}'{}'",
            format!("{}:{:?}", txt.lineno, t.span),
            t.kind,
            txt.line.get(t.span.clone()).unwrap_or("INVALID RANGE")
        )
    }
}

#[derive(Clone, Copy)]
enum LineFailureAcc {
    Empty,
    Read,
    Tokenize,
}

struct LineFailureMessage<'a>(&'a LineFailure);

impl Display for LineFailureMessage<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self.0 {
            LineFailure::Read(err) => err.display_header().fmt(f),
            LineFailure::Tokenize(err) => TokenErrorLineMessage(err).fmt(f),
        }
    }
}

struct TokenErrorLineMessage<'a>(&'a TokenErrorLine);

impl Display for TokenErrorLineMessage<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
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
            .partition(Result::is_ok);
        if err_lines.is_empty() {
            Ok(token_lines.into_iter().flatten().collect())
        } else {
            Err(LexerError(
                err_lines.into_iter().filter_map(Result::err).collect(),
            ))
        }
    }

    fn tokenize_line(&mut self, text: TextLine) -> Result<TokenLine, LineFailure> {
        let (tokens, errs): (Vec<_>, Vec<_>) =
            TokenStream::new(&text.line, self.cont.take()).partition(Result::is_ok);
        // NOTE: even with errors the last token in this line may be a continuation
        self.cont = tokens
            .last()
            .and_then(|r| r.as_ref().ok().and_then(|t| t.kind.to_continuation()));
        if errs.is_empty() {
            Ok(TokenLine(tokens.into_iter().flatten().collect(), text))
        } else {
            Err(TokenErrorLine(
                errs.into_iter().filter_map(Result::err).collect(),
                text,
            ))?
        }
    }
}
