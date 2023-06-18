use crate::syntax::Expression;

type EvalResult = Result<Expression, EvalError>;

#[derive(Debug)]
pub struct EvalError;

pub(super) fn evaluate(mut expressions: impl Iterator<Item = Expression>) -> EvalResult {
    expressions.try_fold(Expression::Empty, |_, expr| eval_expr(expr))
}

fn eval_expr(expr: Expression) -> EvalResult {
    Ok(expr)
}
