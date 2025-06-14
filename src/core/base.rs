// (scheme base)
use crate::{
    eval::{Binding, EvalResult, Frame, MAX_ARITY},
    value::Value,
};

pub(super) fn load(scope: &mut Binding) {
    super::bind_intrinsic(scope, "boolean?", 1..1, is_boolean);
    super::bind_intrinsic(scope, "boolean=?", 0..MAX_ARITY, all_boolean);
    super::bind_intrinsic(scope, "not", 1..1, not);
}

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
