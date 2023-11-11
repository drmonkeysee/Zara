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

#[derive(Debug)]
pub struct TokenLine(Vec<Token>, TextLine);

impl TokenLine {
    fn into_continuation_unsupported(mut self) -> LexerError {
        self.0.pop().map_or(LexerError::InvalidOperation, |t| {
            let err: LineFailure =
                TokenErrorLine(vec![t.into_continuation_unsupported()], self.1).into();
            err.into()
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
            Self::Read(_) => match acc {
                LineFailureAcc::Empty | LineFailureAcc::Read => {
                    ControlFlow::Continue(LineFailureAcc::Read)
                }
                _ => ControlFlow::Break(()),
            },
            Self::Tokenize(_) => match acc {
                LineFailureAcc::Empty | LineFailureAcc::Tokenize => {
                    ControlFlow::Continue(LineFailureAcc::Tokenize)
                }
                _ => ControlFlow::Break(()),
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

impl From<LineFailure> for LexerError {
    fn from(value: LineFailure) -> Self {
        Self::Lines(vec![value])
    }
}

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
        if let Some(token_cont) = d.cont.take() {
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
                "invalid operation attempted on lexer output; this is likely an interpreter bug!\n",
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
            Err(LexerError::Lines(
                err_lines.into_iter().flat_map(Result::err).collect(),
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
                errs.into_iter().flat_map(Result::err).collect(),
                text,
            ))?
        }
    }
}
