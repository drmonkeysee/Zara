// (scheme inexact)
use super::{FIRST_ARG_LABEL, first};
use crate::{
    eval::{EvalResult, Frame},
    number::Number,
    value::{Condition, TypeName, Value},
};

pub(super) fn load(env: &mut Frame) {
    super::bind_intrinsic(env, "finite?", 1..1, is_finite);
    super::bind_intrinsic(env, "infinite?", 1..1, is_infinite);
    super::bind_intrinsic(env, "nan?", 1..1, is_nan);
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
