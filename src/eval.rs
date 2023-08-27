use crate::syn::Expression;
use std::{
    error::Error,
    fmt,
    fmt::{Display, Formatter},
};

pub(crate) type EvalResult = Result<Evaluation, EvalError>;
type ExprResult = Result<Expression, EvalError>;

#[derive(Debug)]
pub enum Evaluation {
    Expression(Expression),
    Continuation,
}

impl Evaluation {
    pub fn extended_display(&self) -> ExtendedEvaluation {
        ExtendedEvaluation(self)
    }
}

pub struct ExtendedEvaluation<'a>(&'a Evaluation);

impl Display for ExtendedEvaluation<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        writeln!(f, "#<eval-display-undefined({:?})>", self.0)
    }
}

#[derive(Debug)]
pub struct EvalError;

impl Display for EvalError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str("Fatal error: evaluation failure")
    }
}

impl Error for EvalError {}

pub(crate) fn evaluate(expression: Expression) -> EvalResult {
    match expression {
        Expression::Begin(exprs) => eval_begin(exprs.into_iter()),
        Expression::TokenStream(_) => Ok(expression),
        _ => Err(EvalError),
    }
    .map(Evaluation::Expression)
}

pub(crate) fn ast(expression: Expression) -> EvalResult {
    Ok(Evaluation::Expression(Expression::Ast(expression.into())))
}

fn eval_expr(expr: Expression) -> ExprResult {
    Ok(expr)
}

fn eval_begin(mut exprs: impl Iterator<Item = Expression>) -> ExprResult {
    exprs.try_fold(Expression::Empty, |_, expr| eval_expr(expr))
}
