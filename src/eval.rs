use crate::syntax::Expression;

type EvalResult = Result<Expression, EvalError>;

#[derive(Debug)]
pub struct EvalError;

pub(super) fn evaluate(expression: Expression) -> EvalResult {
    match expression {
        Expression::Begin(exprs) => exprs
            .into_iter()
            .try_fold(Expression::Empty, |_, expr| eval_expr(expr)),
        _ => Err(EvalError),
    }
}

fn eval_expr(expr: Expression) -> EvalResult {
    Ok(expr)
}
