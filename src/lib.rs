mod lex;
mod literal;
mod syntax;

use self::{
    lex::LexerFailure,
    syntax::{Expression, ParserFailure},
};

pub fn eval(textline: String) -> EvalResult {
    let token_stream = lex::tokenize(&textline);
    let expressions = syntax::parse(token_stream?.into_iter());
    Ok(expressions?.pop().unwrap())
}

#[derive(Debug)]
pub enum EvalError {
    Lexer(LexerFailure),
    Parser(ParserFailure),
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

type EvalResult = Result<Expression, EvalError>;
