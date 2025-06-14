// (scheme base)
macro_rules! predicate {
    ($name:ident, $pred:pat $(if $guard:expr)? $(,)?) => {
        fn $name(args: &[Value], _env: &mut Frame) -> EvalResult {
            let arg = args.first().unwrap();
            Ok(Value::Boolean(matches!(arg, $pred $(if $guard)?)))
        }
    };
}

use crate::{
    eval::{Binding, EvalResult, Frame, MAX_ARITY},
    number::{Number, Real},
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
    super::bind_intrinsic(scope, "exact-integer?", 1..1, is_exact_integer);
    super::bind_intrinsic(scope, "finite?", 1..1, is_finite);
    super::bind_intrinsic(scope, "inexact?", 1..1, is_inexact);
    super::bind_intrinsic(scope, "infinite?", 1..1, is_infinite);
    super::bind_intrinsic(scope, "integer?", 1..1, is_integer);
    super::bind_intrinsic(scope, "nan?", 1..1, is_nan);
    super::bind_intrinsic(scope, "number?", 1..1, is_number);
    super::bind_intrinsic(scope, "rational?", 1..1, is_rational);
    super::bind_intrinsic(scope, "real?", 1..1, is_real);
}

//
// Boolean
//

predicate!(is_boolean, Value::Boolean(_));
predicate!(not, Value::Boolean(false));

fn all_boolean(args: &[Value], _env: &mut Frame) -> EvalResult {
    Ok(Value::Boolean(
        args.iter().all(|v| matches!(v, Value::Boolean(_))),
    ))
}

//
// Number
//

predicate!(is_exact, Value::Number(n) if !n.is_inexact());
predicate!(
    is_exact_integer,
    Value::Number(Number::Real(Real::Integer(_))),
);
predicate!(is_finite, Value::Number(n) if !n.is_infinite() && !n.is_nan());
predicate!(is_inexact, Value::Number(n) if n.is_inexact());
predicate!(is_infinite, Value::Number(n) if n.is_infinite());
predicate!(is_integer, Value::Number(Number::Real(r)) if r.is_integer());
predicate!(is_nan, Value::Number(n) if n.is_nan());
predicate!(is_number, Value::Number(_));
predicate!(is_rational, Value::Number(Number::Real(r)) if r.is_rational());
predicate!(is_real, Value::Number(Number::Real(_)));
