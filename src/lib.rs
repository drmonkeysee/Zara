mod eval;
mod lex;
mod literal;
mod syn;
mod txt;

use self::{eval::EvalError, lex::LexerError, syn::ParserError, txt::TextContext};
pub use self::{eval::Evaluation, syn::Expression};
use std::fmt::{Display, Formatter};

pub type InterpreterResult = Result<Evaluation, InterpreterError>;

pub struct Interpreter {
    ctx: Option<TextContext>,
}

impl Interpreter {
    pub fn new() -> Self {
        Self { ctx: None }
    }

    pub fn runline(&mut self, textline: String) -> InterpreterResult {
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
pub enum InterpreterError {
    Lexer(LexerError),
    Parser(ParserError),
    Eval(EvalError),
}

impl InterpreterError {
    pub fn verbose_display(&self) -> VerboseInterpreterError<'_> {
        VerboseInterpreterError(self)
    }
}

impl From<LexerError> for InterpreterError {
    fn from(value: LexerError) -> Self {
        Self::Lexer(value)
    }
}

impl From<ParserError> for InterpreterError {
    fn from(value: ParserError) -> Self {
        Self::Parser(value)
    }
}

impl From<EvalError> for InterpreterError {
    fn from(value: EvalError) -> Self {
        Self::Eval(value)
    }
}

pub struct VerboseInterpreterError<'a>(&'a InterpreterError);

impl Display for VerboseInterpreterError<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}:{} ({})\n\
            \tthis is an invalid src line with multiple errors\n\
            \t           ^^^^^^^               ^^^^^^^^\n\
            {}:{:?}\n\
            {}:{:?}",
            "library",
            "line",
            "/full/path/to/lib/file.scm",
            "position1",
            self.0,
            "position2",
            self.0
        )
    }
}
