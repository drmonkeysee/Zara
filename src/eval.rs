use crate::syn::Expression;

pub(crate) type EvalResult = Result<Evaluation, EvalError>;
type ExprResult = Result<Expression, EvalError>;

pub enum Evaluation {
    Expression(Expression),
    Continuation,
}

#[derive(Debug)]
pub struct EvalError;

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
