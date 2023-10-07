use crate::syntax::Expression;
use std::{
    error::Error,
    fmt::{self, Display, Formatter},
};

#[derive(Debug)]
pub enum Evaluation {
    Expression(Expression),
    Continuation,
}

impl Evaluation {
    pub fn display_message(&self) -> EvaluationMessage {
        EvaluationMessage(self)
    }
}

pub struct EvaluationMessage<'a>(&'a Evaluation);

impl Display for EvaluationMessage<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.0 {
            Evaluation::Expression(expr) => expr.display_message().fmt(f),
            Evaluation::Continuation => writeln!(f, "#<cont-extended-undef({:?})>", self.0),
        }
    }
}

#[derive(Debug)]
pub struct EvalError;

impl Display for EvalError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str("fatal error: evaluation failure")
    }
}

impl Error for EvalError {}

pub(crate) type EvalResult = Result<Evaluation, EvalError>;

pub(crate) fn evaluate(expression: Expression) -> EvalResult {
    match expression {
        Expression::Begin(exprs) => eval_begin(exprs),
        Expression::TokenList(_) => Ok(expression),
        _ => Err(EvalError),
    }
    .map(Evaluation::Expression)
}

pub(crate) fn ast(expression: Expression) -> EvalResult {
    Ok(Evaluation::Expression(Expression::Ast(expression.into())))
}

type ExprResult = Result<Expression, EvalError>;

fn eval_expr(expr: Expression) -> ExprResult {
    Ok(expr)
}

fn eval_begin(exprs: impl IntoIterator<Item = Expression>) -> ExprResult {
    exprs
        .into_iter()
        .try_fold(Expression::Empty, |_, expr| eval_expr(expr))
}
