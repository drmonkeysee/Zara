mod eval;
mod lex;
mod literal;
mod syn;
mod txt;

use self::{eval::EvalError, lex::LexerError, syn::ParserError, txt::TextContext};
pub use self::{eval::Evaluation, syn::Expression};
use std::{
    fmt,
    fmt::{Display, Formatter},
    result,
};

pub type Result = result::Result<Evaluation, Error>;

pub struct Interpreter {
    ctx: Option<TextContext>,
}

impl Interpreter {
    pub fn new() -> Self {
        Self { ctx: None }
    }

    pub fn runline(&mut self, textline: String) -> Result {
        let ctx = match self.ctx.take() {
            Some(c) => c.nextline(textline),
            None => TextContext::for_repl(textline),
        };
        let tokens = lex::tokenize(&ctx)?;
        let ast = syn::parse(tokens.into_iter())?;
        let evaluation = eval::evaluate(ast)?;
        // NOTE: save text context for next line
        self.ctx = if matches!(evaluation, Evaluation::Continuation) {
            Some(ctx)
        } else {
            None
        };
        Ok(evaluation)
    }
}

#[derive(Debug)]
pub enum Error {
    Lex(LexerError),
    Parse(ParserError),
    Eval(EvalError),
}

impl Error {
    pub fn verbose_display(&self) -> VerboseError<'_> {
        VerboseError(self)
    }
}

impl From<LexerError> for Error {
    fn from(value: LexerError) -> Self {
        Self::Lex(value)
    }
}

impl From<ParserError> for Error {
    fn from(value: ParserError) -> Self {
        Self::Parse(value)
    }
}

impl From<EvalError> for Error {
    fn from(value: EvalError) -> Self {
        Self::Eval(value)
    }
}

pub struct VerboseError<'a>(&'a Error);

impl Display for VerboseError<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let err = self.0;
        match err {
            Error::Lex(lex_err) => write!(f, "{}", lex_err.verbose_display()),
            _ => write!(f, "#<intr_err_undef({:?})>", err),
        }
    }
}
