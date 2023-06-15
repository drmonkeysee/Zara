mod lex;
mod literal;
mod syntax;

use self::{
    lex::{LexerFailure, LexerResult},
    syntax::{Expression, ParserFailure, ParserResult},
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

#[derive(Debug)]
pub struct Evaluation {
    textline: String,
    output: OutputKind,
}

#[derive(Debug)]
enum OutputKind {
    Lexer(LexerResult),
    Parser(ParserResult),
}
