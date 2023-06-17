mod lex;
mod literal;
mod syntax;

use self::{
    lex::LexerFailure,
    syntax::{Expression, ParserFailure},
};

pub fn eval(textline: String) -> EvalResult {
    evaluate(syntax::parse(lex::tokenize(&textline)?.into_iter())?.into_iter())
}

#[derive(Debug)]
pub enum EvalError {
    Lexer(LexerFailure),
    Parser(ParserFailure),
}

type EvalResult = Result<Expression, EvalError>;

fn evaluate(expressions: impl Iterator<Item = Expression>) -> EvalResult {
    Ok(expressions.last().unwrap())
}

impl From<LexerFailure> for EvalError {
    fn from(value: LexerFailure) -> Self {
        Self::Lexer(value)
    }
}

impl From<ParserFailure> for EvalError {
    fn from(value: ParserFailure) -> Self {
        Self::Parser(value)
    }
}
