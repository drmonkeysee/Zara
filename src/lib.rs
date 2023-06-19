mod eval;
mod lex;
mod literal;
mod syntax;

use self::{
    eval::EvalError,
    lex::LexerError,
    syntax::{Expression, ParserError},
};

type InterpreterResult = Result<Expression, InterpreterError>;

pub fn run_line(textline: String) -> InterpreterResult {
    Ok(eval::evaluate(syntax::parse(
        lex::tokenize(&textline)?.into_iter(),
    )?)?)
}

#[derive(Debug)]
pub enum InterpreterError {
    Lexer(LexerError),
    Parser(ParserError),
    Eval(EvalError),
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
