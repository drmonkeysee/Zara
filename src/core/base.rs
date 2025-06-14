// (scheme base)
use crate::{
    eval::{Binding, EvalResult, Frame, MAX_ARITY},
    number::Number,
    value::Value,
};

pub(super) fn load(scope: &mut Binding) {
    // boolean
    super::bind_intrinsic(scope, "boolean?", 1..1, is_boolean);
    super::bind_intrinsic(scope, "boolean=?", 0..MAX_ARITY, all_boolean);
    super::bind_intrinsic(scope, "not", 1..1, not);

    // number
    // NOTE: complex and number predicates are identical sets
    super::bind_intrinsic(scope, "complex?", 1..1, is_number);
    super::bind_intrinsic(scope, "exact?", 1..1, is_exact);
    super::bind_intrinsic(scope, "inexact?", 1..1, is_inexact);
    super::bind_intrinsic(scope, "integer?", 1..1, is_integer);
    super::bind_intrinsic(scope, "number?", 1..1, is_number);
    super::bind_intrinsic(scope, "rational?", 1..1, is_rational);
    super::bind_intrinsic(scope, "real?", 1..1, is_real);
}

//
// Boolean
//

fn is_boolean(args: &[Value], _env: &mut Frame) -> EvalResult {
    let arg = args.first().unwrap();
    Ok(Value::Boolean(matches!(arg, Value::Boolean(_))))
}

fn all_boolean(args: &[Value], _env: &mut Frame) -> EvalResult {
    Ok(Value::Boolean(
        args.iter().all(|v| matches!(v, Value::Boolean(_))),
    ))
}

fn not(args: &[Value], _env: &mut Frame) -> EvalResult {
    let arg = args.first().unwrap();
    Ok(Value::Boolean(matches!(arg, Value::Boolean(false))))
}

//
// Number
//

fn is_exact(args: &[Value], _env: &mut Frame) -> EvalResult {
    let arg = args.first().unwrap();
    Ok(Value::Boolean(matches!(
        arg,
        Value::Number(n) if !n.is_inexact()
    )))
}

fn is_inexact(args: &[Value], _env: &mut Frame) -> EvalResult {
    let arg = args.first().unwrap();
    Ok(Value::Boolean(matches!(
        arg,
        Value::Number(n) if n.is_inexact()
    )))
}

fn is_integer(args: &[Value], _env: &mut Frame) -> EvalResult {
    let arg = args.first().unwrap();
    Ok(Value::Boolean(matches!(
        arg,
        Value::Number(Number::Real(r)) if r.is_integer()
    )))
}

fn is_number(args: &[Value], _env: &mut Frame) -> EvalResult {
    let arg = args.first().unwrap();
    Ok(Value::Boolean(matches!(arg, Value::Number(_))))
}

fn is_rational(args: &[Value], _env: &mut Frame) -> EvalResult {
    let arg = args.first().unwrap();
    Ok(Value::Boolean(matches!(
        arg,
        Value::Number(Number::Real(r)) if r.is_rational()
    )))
}

fn is_real(args: &[Value], _env: &mut Frame) -> EvalResult {
    let arg = args.first().unwrap();
    Ok(Value::Boolean(matches!(
        arg,
        Value::Number(Number::Real(_))
    )))
}
