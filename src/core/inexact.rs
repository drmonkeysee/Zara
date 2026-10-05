// (scheme inexact)
use super::{FIRST_ARG_LABEL, SECOND_ARG_LABEL, first, invalid_target};
use crate::{
    eval::{EvalResult, Frame},
    number::{Number, NumericTypeName},
    value::{Condition, TypeName, Value},
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

fn exponential(args: &[Value], _env: &Frame) -> EvalResult {
    let arg = first(args);
    super::num_op(arg, Number::exp, super::num_to_valresult)
}

fn logarithm(args: &[Value], _env: &Frame) -> EvalResult {
    let arg = first(args);
    match args.get(1) {
        None => super::num_op(arg, Number::ln, super::numresult_to_valresult),
        Some(base) => {
            if let Value::Number(x) = base {
                super::num_op(
                    arg,
                    |y| y.log(x),
                    |res, first| {
                        // pick which argument threw the zero error
                        super::numresult_to_valresult(
                            res,
                            if let Value::Number(Number::Real(r)) = first
                                && !r.is_inexact()
                                && r.is_zero()
                            {
                                first
                            } else {
                                base
                            },
                        )
                    },
                )
            } else {
                Err(invalid_target(TypeName::NUMBER, base))
            }
        }
    }
}

fn sine(args: &[Value], _env: &Frame) -> EvalResult {
    let arg = first(args);
    super::num_op(arg, Number::sin, super::num_to_valresult)
}

fn cosine(args: &[Value], _env: &Frame) -> EvalResult {
    let arg = first(args);
    super::num_op(arg, Number::cos, super::num_to_valresult)
}

fn tangent(args: &[Value], _env: &Frame) -> EvalResult {
    let arg = first(args);
    super::num_op(arg, Number::tan, super::num_to_valresult)
}

fn arc_sine(args: &[Value], _env: &Frame) -> EvalResult {
    let arg = first(args);
    super::num_op(arg, Number::asin, super::num_to_valresult)
}

fn arc_cosine(args: &[Value], _env: &Frame) -> EvalResult {
    let arg = first(args);
    super::num_op(arg, Number::acos, super::num_to_valresult)
}

fn arc_tangent(args: &[Value], _env: &Frame) -> EvalResult {
    let arg = first(args);
    match args.get(1) {
        None => super::num_op(arg, Number::atan, super::num_to_valresult),
        Some(arg2) => {
            let y = super::arg_to_real(arg, FIRST_ARG_LABEL, NumericTypeName::REAL)?;
            let x = super::arg_to_real(arg2, SECOND_ARG_LABEL, NumericTypeName::REAL)?;
            Number::complex(x.clone(), y.clone()).angle().map_or_else(
                |err| Err(Condition::bi_value_error(err, arg, arg2).into()),
                |r| Ok(Value::real(r)),
            )
        }
    }
}

fn square_root(args: &[Value], _env: &Frame) -> EvalResult {
    let arg = first(args);
    if let Value::Number(x) = arg {
        Ok(Value::Number(x.sqrt()))
    } else {
        Err(invalid_target(TypeName::NUMBER, arg))
    }
}
