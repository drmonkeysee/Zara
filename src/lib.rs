mod lex;
mod literal;
mod syntax;

use self::{
    lex::LexerFailure,
    syntax::{Expression, ParserFailure},
};

pub fn eval(textline: String) -> InterpreterResult {
    Ok(evaluate(
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

#[derive(Debug)]
pub struct EvalError;

type EvalResult = Result<Expression, EvalError>;

fn evaluate(mut expressions: impl Iterator<Item = Expression>) -> EvalResult {
    expressions.try_fold(Expression::Empty, |_, expr| eval_expr(expr))
}

fn eval_expr(expr: Expression) -> EvalResult {
    Ok(expr)
}

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
