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

#[derive(Default)]
pub struct Interpreter {
    ast_output: bool,
    token_output: bool,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            ..Default::default()
        }
    }

    pub fn run(&self, src: &mut impl TextSource) -> Result {
        let token_lines = lex::tokenize(src)?;
        // TODO: should these be templatized
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
            _ => writeln!(f, "#<error-display-undefined({:?})>", err),
        }
    }
}
