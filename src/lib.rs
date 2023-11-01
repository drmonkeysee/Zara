pub mod eval;
pub mod lex;
pub mod literal;
pub mod src;
pub mod syntax;
pub mod txt;

use self::{
    eval::EvalError,
    lex::{Lexer, Lexer2, LexerError, LexerOutput},
    syntax::ParserError,
    txt::TextSource,
};
pub use self::{eval::Evaluation, syntax::Expression};
use std::{
    error,
    fmt::{self, Display, Formatter},
    result,
};

pub type Result = result::Result<Evaluation, Error>;

pub struct Interpreter {
    ast_output: bool,
    lexer: Lexer,
    token_output: bool,
}

impl Interpreter {
    // TODO: should these be type parameters
    pub fn new(token_output: bool, ast_output: bool) -> Self {
        Self {
            ast_output,
            lexer: Lexer::new(),
            token_output,
        }
    }

    pub fn run(&mut self, src: &mut impl TextSource) -> Result {
        let lex_output = self.lexer.tokenize(src)?;
        let token_lines = match lex_output {
            LexerOutput::Complete(lines) => lines,
            LexerOutput::Continuation => return Ok(Evaluation::Continuation),
        };
        let ast = if self.token_output {
            syntax::tokens(token_lines)
        } else {
            syntax::parse(token_lines)
        }?;
        let evaluation = if self.ast_output {
            eval::ast(ast)
        } else {
            eval::evaluate(ast)
        }?;
        Ok(evaluation)
    }

    pub fn run2(self, src: impl TextSource) -> Result {
        let ast = syntax::tokens2(Lexer2::new(src))?;
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
    pub fn display_message(&self) -> ErrorMessage<'_> {
        ErrorMessage(self)
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Lex(lx) => lx.fmt(f),
            Self::Parse(ps) => ps.fmt(f),
            Self::Eval(ev) => ev.fmt(f),
        }
    }
}

impl error::Error for Error {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        Some(match self {
            Self::Lex(lx) => lx,
            Self::Parse(ps) => ps,
            Self::Eval(ev) => ev,
        })
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

pub struct ErrorMessage<'a>(&'a Error);

impl Display for ErrorMessage<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let err = self.0;
        match err {
            Error::Lex(lex_err) => lex_err.display_message().fmt(f),
            _ => writeln!(f, "#<error-extended-undef({err:?})>"),
        }
    }
}
