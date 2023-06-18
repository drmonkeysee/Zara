mod eval;
mod lex;
mod literal;
mod syntax;

use self::{
    eval::EvalError,
    lex::LexerFailure,
    syntax::{Expression, ParserFailure},
};

pub fn run_line(textline: String) -> InterpreterResult {
    Ok(eval::evaluate(
        syntax::parse(lex::tokenize(&textline)?.into_iter())?.into_iter(),
    )?)
}

#[derive(Debug)]
pub enum InterpreterError {
    Lexer(LexerFailure),
    Parser(ParserFailure),
    Eval(EvalError),
}

type InterpreterResult = Result<Expression, InterpreterError>;

impl From<LexerFailure> for InterpreterError {
    fn from(value: LexerFailure) -> Self {
        Self::Lexer(value)
    }
}

impl From<ParserFailure> for InterpreterError {
    fn from(value: ParserFailure) -> Self {
        Self::Parser(value)
    }
}

impl From<EvalError> for InterpreterError {
    fn from(value: EvalError) -> Self {
        Self::Eval(value)
    }
}
