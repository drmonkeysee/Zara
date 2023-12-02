pub mod eval;
pub mod lex;
pub mod literal;
pub mod number;
pub mod src;
mod syntax;
#[cfg(test)]
mod testutil;
pub mod txt;

pub use self::eval::{Evaluation, Expr};
use self::{
    eval::{Ast, Environment, EvalError, Evaluator},
    lex::{Lexer, LexerError, LexerOutput, TokenLine},
    syntax::{ExpressionTree, Parser, ParserError, TokenList},
    txt::TextSource,
};
use std::{
    error,
    fmt::{self, Display, Formatter},
    result,
};

pub type Result = result::Result<Evaluation, Error>;

pub enum RunMode {
    Evaluate,
    SyntaxTree,
    Tokenize,
    TokenTree,
}

pub struct Interpreter {
    lexer: Lexer,
    runner: Box<dyn Executor>,
}

impl Interpreter {
    #[must_use]
    pub fn new(mode: RunMode) -> Self {
        Self {
            lexer: Lexer::new(),
            runner: resolve_executor(mode),
        }
    }

    pub fn run(&mut self, src: &mut impl TextSource) -> Result {
        let lex_output = self.lex(src)?;
        let token_lines = match lex_output {
            LexerOutput::Complete(lines) => lines,
            LexerOutput::Continuation => return Ok(Evaluation::Continuation),
        };
        Ok(self.runner.exec(token_lines)?)
    }

    fn lex(&mut self, src: &mut impl TextSource) -> result::Result<LexerOutput, ExecError> {
        Ok(self.lexer.tokenize(src)?)
    }
}

#[derive(Debug)]
pub struct Error(ExecError);

impl Error {
    #[must_use]
    pub fn display_message(&self) -> ErrorMessage<'_> {
        ErrorMessage(&self.0)
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl error::Error for Error {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        self.0.source()
    }
}

impl From<ExecError> for Error {
    fn from(value: ExecError) -> Self {
        Self(value)
    }
}

pub struct ErrorMessage<'a>(&'a ExecError);

impl Display for ErrorMessage<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let err = self.0;
        match err {
            ExecError::Lex(lex_err) => lex_err.display_message().fmt(f),
            _ => writeln!(f, "#<error-extended-undef({err:?})>"),
        }
    }
}

trait Executor {
    fn exec(&self, token_lines: Vec<TokenLine>) -> result::Result<Evaluation, ExecError>;
}

#[derive(Debug)]
enum ExecError {
    Lex(LexerError),
    Parse(ParserError),
    Eval(EvalError),
}

impl Display for ExecError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Lex(lx) => lx.fmt(f),
            Self::Parse(ps) => ps.fmt(f),
            Self::Eval(ev) => ev.fmt(f),
        }
    }
}

impl error::Error for ExecError {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        Some(match self {
            Self::Lex(lx) => lx,
            Self::Parse(ps) => ps,
            Self::Eval(ev) => ev,
        })
    }
}

impl From<LexerError> for ExecError {
    fn from(value: LexerError) -> Self {
        Self::Lex(value)
    }
}

impl From<ParserError> for ExecError {
    fn from(value: ParserError) -> Self {
        Self::Parse(value)
    }
}

impl From<EvalError> for ExecError {
    fn from(value: EvalError) -> Self {
        Self::Eval(value)
    }
}

struct Engine<P, E> {
    evaluator: E,
    parser: P,
}

impl<P: Parser, E: Evaluator> Executor for Engine<P, E> {
    fn exec(&self, token_lines: Vec<TokenLine>) -> result::Result<Evaluation, ExecError> {
        Ok(self.evaluator.evaluate(self.parser.parse(token_lines)?)?)
    }
}

fn resolve_executor(mode: RunMode) -> Box<dyn Executor> {
    match mode {
        RunMode::Evaluate => Box::new(Engine {
            evaluator: Environment,
            parser: ExpressionTree,
        }),
        RunMode::SyntaxTree => Box::new(Engine {
            evaluator: Ast,
            parser: ExpressionTree,
        }),
        RunMode::Tokenize | RunMode::TokenTree => {
            let parser = TokenList;
            if matches!(mode, RunMode::TokenTree) {
                Box::new(Engine {
                    evaluator: Ast,
                    parser,
                })
            } else {
                Box::new(Engine {
                    evaluator: Environment,
                    parser,
                })
            }
        }
    }
}
