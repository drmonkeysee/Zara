mod eval;
mod lex;
mod literal;
mod syn;
pub mod txt;

use self::{eval::EvalError, lex::LexerError, syn::ParserError, txt::TextSource};
pub use self::{eval::Evaluation, syn::Expression};
use std::{
    fmt,
    fmt::{Display, Formatter},
    result,
};

pub type Result = result::Result<Evaluation, Error>;

pub struct Interpreter;

impl Interpreter {
    pub fn new() -> Self {
        Self {}
    }

    pub fn run(&mut self, src: &mut impl TextSource) -> Result {
        let token_lines = lex::tokenize(src)?;
        let ast = syn::parse(token_lines.into_iter())?;
        let evaluation = eval::evaluate(ast)?;
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
            _ => write!(f, "#<error-display-undefined({:?})>\n", err),
        }
    }
}
