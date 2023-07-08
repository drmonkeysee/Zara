mod eval;
mod lex;
mod literal;
mod syn;

use self::{
    eval::EvalError,
    lex::LexerError,
    syn::{Expression, ParserError},
};

type InterpreterResult = Result<Expression, InterpreterError>;

pub fn runline(textline: String) -> InterpreterResult {
    let tokens = lex::tokenize(textline)?;
    let ast = syn::parse(tokens.into_iter())?;
    Ok(eval::evaluate(ast)?)
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
