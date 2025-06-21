// (scheme inexact)
use super::FIRST_ARG_LABEL;
use crate::{
    eval::{Binding, EvalResult, Frame},
    number::Number,
    value::{Condition, TypeName, Value},
};

pub(super) fn load(scope: &mut Binding) {
    super::bind_intrinsic(scope, "finite?", 1..1, is_finite);
    super::bind_intrinsic(scope, "infinite?", 1..1, is_infinite);
    super::bind_intrinsic(scope, "nan?", 1..1, is_nan);
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
