use crate::syn::Expression;

type EvalResult = Result<Expression, EvalError>;

#[derive(Debug)]
pub struct EvalError;

pub(super) fn evaluate(expression: Expression) -> EvalResult {
    // TODO: support CLI flag for outputing Abstract Syntax Tree expression
    //return Ok(Expression::Ast(Box::new(expression)));
    match expression {
        Expression::Begin(exprs) => eval_begin(exprs.into_iter()),
        _ => Err(EvalError),
    }
}

fn eval_expr(expr: Expression) -> EvalResult {
    Ok(expr)
}

fn eval_begin(mut exprs: impl Iterator<Item = Expression>) -> EvalResult {
    exprs.try_fold(Expression::Empty, |_, expr| eval_expr(expr))
}
