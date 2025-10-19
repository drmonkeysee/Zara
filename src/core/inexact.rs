// (scheme inexact)
use super::{first, invalid_target};
use crate::{
    eval::{EvalResult, Frame},
    number::Number,
    value::{TypeName, Value},
};

pub(super) fn load(env: &Frame) {
    super::bind_intrinsic(env, "finite?", 1..1, is_finite);
    super::bind_intrinsic(env, "infinite?", 1..1, is_infinite);
    super::bind_intrinsic(env, "nan?", 1..1, is_nan);

    super::bind_intrinsic(env, "exp", 1..1, exponential);
    super::bind_intrinsic(env, "log", 1..2, logarithm);

    super::bind_intrinsic(env, "sin", 1..1, sine);
    super::bind_intrinsic(env, "cos", 1..1, cosine);
    super::bind_intrinsic(env, "tan", 1..1, tangent);

    super::bind_intrinsic(env, "asin", 1..1, arc_sine);
    super::bind_intrinsic(env, "acos", 1..1, arc_cosine);
    super::bind_intrinsic(env, "atan", 1..2, arc_tangent);

    super::bind_intrinsic(env, "sqrt", 1..1, square_root);
}

try_predicate!(is_finite, Value::Number, TypeName::NUMBER, |n: &Number| {
    !n.is_infinite() && !n.is_nan()
});
try_predicate!(
    is_infinite,
    Value::Number,
    TypeName::NUMBER,
    |n: &Number| n.is_infinite()
);
try_predicate!(is_nan, Value::Number, TypeName::NUMBER, |n: &Number| n
    .is_nan());

fn exponential(_args: &[Value], _env: &Frame) -> EvalResult {
    todo!();
}

fn logarithm(_args: &[Value], _env: &Frame) -> EvalResult {
    todo!();
}

fn sine(_args: &[Value], _env: &Frame) -> EvalResult {
    todo!();
}

fn cosine(_args: &[Value], _env: &Frame) -> EvalResult {
    todo!();
}

fn tangent(_args: &[Value], _env: &Frame) -> EvalResult {
    todo!();
}

fn arc_sine(_args: &[Value], _env: &Frame) -> EvalResult {
    todo!();
}

fn arc_cosine(_args: &[Value], _env: &Frame) -> EvalResult {
    todo!();
}

fn arc_tangent(_args: &[Value], _env: &Frame) -> EvalResult {
    todo!();
}

fn square_root(_args: &[Value], _env: &Frame) -> EvalResult {
    todo!();
}
