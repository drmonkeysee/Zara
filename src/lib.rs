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
    io,
    path::PathBuf,
    result,
};

pub type Result = result::Result<Evaluation, Error>;

#[derive(Default)]
pub struct Interpreter {
    ast_output: bool,
    token_output: bool,
}

impl Interpreter {
    // TODO: should these be type parameters
    pub fn new(token_output: bool, ast_output: bool) -> Self {
        Self {
            ast_output,
            token_output,
        }
    }

    pub fn run(&self, src: &mut impl TextSource) -> Result {
        let token_lines = lex::tokenize(src)?;
        let ast = if self.token_output {
            syn::tokens(token_lines)
        } else {
            syn::parse(token_lines.into_iter())
        }?;
        let evaluation = if self.ast_output {
            eval::ast(ast)
        } else {
            eval::evaluate(ast)
        }?;
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
    pub fn extended_display(&self) -> ExtendedError<'_> {
        ExtendedError(self)
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

pub struct ExtendedError<'a>(&'a Error);

impl Display for ExtendedError<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let err = self.0;
        match err {
            Error::Lex(lex_err) => lex_err.extended_display().fmt(f),
            _ => writeln!(f, "#<error-display-undefined({err:?})>"),
        }
    }
}

pub fn resolve_library(name: &str) -> io::Result<PathBuf> {
    todo!();
}
