#[cfg(test)]
mod tests;

use super::{FIRST_ARG_LABEL, MAX_ARITY, first, invalid_target};
use crate::{
    Exception,
    eval::{EvalResult, Frame},
    number::{Integer, NumResult, Number, NumericError, NumericTypeName, Real},
    value::{Condition, TypeName, Value},
};
use std::{
    fmt::Display,
    ops::{Add, Div, Mul},
};

pub(super) fn load(env: &Frame) {
    // complex and number predicates are identical sets
    super::bind_intrinsic(env, "number?", 1..1, is_number);
    super::bind_intrinsic(env, "complex?", 1..1, is_number);
    super::bind_intrinsic(env, "real?", 1..1, is_real);
    super::bind_intrinsic(env, "rational?", 1..1, is_rational);
    super::bind_intrinsic(env, "integer?", 1..1, is_integer);

    super::bind_intrinsic(env, "exact?", 1..1, is_exact);
    super::bind_intrinsic(env, "inexact?", 1..1, is_inexact);
    super::bind_intrinsic(env, "exact-integer?", 1..1, is_exact_integer);

    super::bind_intrinsic(env, "=", 0..MAX_ARITY, nums_equal);
    super::bind_intrinsic(env, "<", 0..MAX_ARITY, nums_lt);
    super::bind_intrinsic(env, ">", 0..MAX_ARITY, nums_gt);
    super::bind_intrinsic(env, "<=", 0..MAX_ARITY, nums_lte);
    super::bind_intrinsic(env, ">=", 0..MAX_ARITY, nums_gte);

    super::bind_intrinsic(env, "zero?", 1..1, is_zero);
    super::bind_intrinsic(env, "positive?", 1..1, is_positive);
    super::bind_intrinsic(env, "negative?", 1..1, is_negative);
    super::bind_intrinsic(env, "odd?", 1..1, is_odd);
    super::bind_intrinsic(env, "even?", 1..1, is_even);

    super::bind_intrinsic(env, "max", 1..MAX_ARITY, nums_max);
    super::bind_intrinsic(env, "min", 1..MAX_ARITY, nums_min);

    super::bind_intrinsic(env, "+", 0..MAX_ARITY, nums_add);
    super::bind_intrinsic(env, "*", 0..MAX_ARITY, nums_mult);

    super::bind_intrinsic(env, "-", 1..MAX_ARITY, nums_sub);
    super::bind_intrinsic(env, "/", 1..MAX_ARITY, nums_div);

    super::bind_intrinsic(env, "abs", 1..1, abs);

    super::bind_intrinsic(env, "floor/", 2..2, floor_qr);
    super::bind_intrinsic(env, "floor-quotient", 2..2, floor_quotient);
    super::bind_intrinsic(env, "floor-remainder", 2..2, floor_remainder);
    super::bind_intrinsic(env, "truncate/", 2..2, truncate_qr);
    super::bind_intrinsic(env, "truncate-quotient", 2..2, truncate_quotient);
    super::bind_intrinsic(env, "truncate-remainder", 2..2, truncate_remainder);

    super::bind_intrinsic(env, "quotient", 2..2, truncate_quotient);
    super::bind_intrinsic(env, "remainder", 2..2, truncate_remainder);
    super::bind_intrinsic(env, "modulo", 2..2, floor_remainder);

    super::bind_intrinsic(env, "gcd", 0..MAX_ARITY, nums_gcd);
    super::bind_intrinsic(env, "lcm", 0..MAX_ARITY, nums_lcm);

    super::bind_intrinsic(env, "numerator", 1..1, get_numerator);
    super::bind_intrinsic(env, "denominator", 1..1, get_denominator);

    super::bind_intrinsic(env, "floor", 1..1, floor);
    super::bind_intrinsic(env, "ceiling", 1..1, ceiling);
    super::bind_intrinsic(env, "truncate", 1..1, truncate);
    super::bind_intrinsic(env, "round", 1..1, round);

    super::bind_intrinsic(env, "square", 1..1, square);

    super::bind_intrinsic(env, "inexact", 1..1, into_inexact);
    super::bind_intrinsic(env, "exact", 1..1, into_exact);
}

predicate!(is_number, Value::Number(_));
predicate!(is_real, Value::Number(Number::Real(_)));
predicate!(is_rational, Value::Number(Number::Real(r)) if r.is_rational());
predicate!(is_integer, Value::Number(Number::Real(r)) if r.is_integer());
seq_predicate!(nums_equal, Value::Number, TypeName::NUMBER, Number::eq);
seq_predicate!(
    nums_lt,
    Value::Number(Number::Real(r)),
    r,
    NumericTypeName::REAL,
    Real::lt,
    seq_error
);
seq_predicate!(
    nums_gt,
    Value::Number(Number::Real(r)),
    r,
    NumericTypeName::REAL,
    Real::gt,
    seq_error
);
seq_predicate!(
    nums_lte,
    Value::Number(Number::Real(r)),
    r,
    NumericTypeName::REAL,
    Real::le,
    seq_error
);
seq_predicate!(
    nums_gte,
    Value::Number(Number::Real(r)),
    r,
    NumericTypeName::REAL,
    Real::ge,
    seq_error
);
try_predicate!(is_exact, Value::Number, TypeName::NUMBER, |n: &Number| {
    !n.is_inexact()
});
try_predicate!(is_inexact, Value::Number, TypeName::NUMBER, |n: &Number| {
    n.is_inexact()
});
try_predicate!(
    is_exact_integer,
    Value::Number,
    TypeName::NUMBER,
    |n: &Number| matches!(n, Number::Real(Real::Integer(_)))
);
try_predicate!(is_zero, Value::Number, TypeName::NUMBER, |n: &Number| n
    .is_zero());

fn is_positive(args: &[Value], _env: &Frame) -> EvalResult {
    real_op(first(args), |r| Ok(Value::Boolean(r.is_positive())))
}

fn is_negative(args: &[Value], _env: &Frame) -> EvalResult {
    real_op(first(args), |r| Ok(Value::Boolean(r.is_negative())))
}

fn is_odd(args: &[Value], _env: &Frame) -> EvalResult {
    exact_int_predicate(first(args), |n| !n.is_even())
}

fn is_even(args: &[Value], _env: &Frame) -> EvalResult {
    exact_int_predicate(first(args), Integer::is_even)
}

fn nums_max(args: &[Value], _env: &Frame) -> EvalResult {
    real_acc_cmp(first(args), args.iter().skip(1), Real::strict_lt)
}

fn nums_min(args: &[Value], _env: &Frame) -> EvalResult {
    real_acc_cmp(first(args), args.iter().skip(1), Real::strict_gt)
}

fn nums_add(args: &[Value], _env: &Frame) -> EvalResult {
    commutative_arithmetic(args, Number::zero(), Number::add)
}

fn nums_mult(args: &[Value], _env: &Frame) -> EvalResult {
    commutative_arithmetic(args, Number::one(), Number::mul)
}

fn nums_sub(args: &[Value], _env: &Frame) -> EvalResult {
    inverse_arithmetic(args, |x| Ok(-x), |a, b| Ok(a - b))
}

fn nums_div(args: &[Value], _env: &Frame) -> EvalResult {
    inverse_arithmetic(args, Number::try_into_reciprocal, Number::div)
}

fn abs(args: &[Value], _env: &Frame) -> EvalResult {
    real_op(first(args), |r| Ok(Value::real(r.clone().into_abs())))
}

fn floor_qr(args: &[Value], _env: &Frame) -> EvalResult {
    exact_division(
        first(args),
        super::second(args),
        Integer::into_floor_quotrem,
        into_quotrem,
    )
}

fn floor_quotient(args: &[Value], _env: &Frame) -> EvalResult {
    exact_division(
        first(args),
        super::second(args),
        Integer::into_floor_quotient,
        into_quotient,
    )
}

fn floor_remainder(args: &[Value], _env: &Frame) -> EvalResult {
    exact_division(
        first(args),
        super::second(args),
        Integer::into_floor_rem,
        into_remainder,
    )
}

fn truncate_qr(args: &[Value], _env: &Frame) -> EvalResult {
    exact_division(
        first(args),
        super::second(args),
        Integer::into_truncate_quotrem,
        into_quotrem,
    )
}

fn truncate_quotient(args: &[Value], _env: &Frame) -> EvalResult {
    exact_division(
        first(args),
        super::second(args),
        Integer::into_truncate_quotient,
        into_quotient,
    )
}

fn truncate_remainder(args: &[Value], _env: &Frame) -> EvalResult {
    exact_division(
        first(args),
        super::second(args),
        Integer::into_truncate_rem,
        into_remainder,
    )
}

fn nums_gcd(args: &[Value], _env: &Frame) -> EvalResult {
    exact_factor_op(args, Integer::zero(), Integer::gcd)
}

fn nums_lcm(args: &[Value], _env: &Frame) -> EvalResult {
    exact_factor_op(args, Integer::one(), Integer::lcm)
}

fn get_numerator(args: &[Value], _env: &Frame) -> EvalResult {
    let arg = first(args);
    rational_op(arg, |r| {
        r.clone().try_into_numerator().map_or_else(
            |err| Err(Condition::value_error(err, arg).into()),
            |r| Ok(Value::real(r)),
        )
    })
}

fn get_denominator(args: &[Value], _env: &Frame) -> EvalResult {
    let arg = first(args);
    rational_op(arg, |r| {
        r.clone().try_into_denominator().map_or_else(
            |err| Err(Condition::value_error(err, arg).into()),
            |r| Ok(Value::real(r)),
        )
    })
}

fn floor(args: &[Value], _env: &Frame) -> EvalResult {
    real_op(first(args), |r| Ok(Value::real(r.clone().into_floor())))
}

fn ceiling(args: &[Value], _env: &Frame) -> EvalResult {
    real_op(first(args), |r| Ok(Value::real(r.clone().into_ceiling())))
}

fn truncate(args: &[Value], _env: &Frame) -> EvalResult {
    real_op(first(args), |r| Ok(Value::real(r.clone().into_truncate())))
}

fn round(args: &[Value], _env: &Frame) -> EvalResult {
    real_op(first(args), |r| Ok(Value::real(r.clone().into_round())))
}

fn square(args: &[Value], env: &Frame) -> EvalResult {
    let arg = first(args);
    nums_mult(&[arg.clone(), arg.clone()], env)
}

fn into_inexact(args: &[Value], _env: &Frame) -> EvalResult {
    let arg = first(args);
    if let Value::Number(x) = arg {
        Ok(Value::Number(x.clone().into_inexact()))
    } else {
        Err(invalid_target(TypeName::NUMBER, arg))
    }
}

fn into_exact(args: &[Value], _env: &Frame) -> EvalResult {
    let arg = first(args);
    if let Value::Number(x) = arg {
        x.clone().try_into_exact().map_or_else(
            |err| Err(Condition::value_error(err, arg).into()),
            |n| Ok(Value::Number(n)),
        )
    } else {
        Err(invalid_target(TypeName::NUMBER, arg))
    }
}

//
// Helpers
//

fn real_op(arg: &Value, op: impl FnOnce(&Real) -> EvalResult) -> EvalResult {
    guarded_real_op(arg, NumericTypeName::REAL, op)
}

fn rational_op(arg: &Value, op: impl FnOnce(&Real) -> EvalResult) -> EvalResult {
    guarded_real_op(arg, NumericTypeName::RATIONAL, op)
}

fn exact_int_predicate(arg: &Value, pred: impl FnOnce(&Integer) -> bool) -> EvalResult {
    guarded_real_op(arg, NumericTypeName::INTEGER, |r| {
        r.clone().try_into_exact_integer().map_or_else(
            |err| Err(Condition::value_error(err, arg).into()),
            |n| Ok(Value::Boolean(pred(&n))),
        )
    })
}

fn guarded_real_op(
    arg: &Value,
    expected_type: impl Display,
    op: impl FnOnce(&Real) -> EvalResult,
) -> EvalResult {
    let r = arg_to_real(arg, FIRST_ARG_LABEL, expected_type)?;
    op(r)
}

fn real_acc_cmp<'a>(
    first: &Value,
    rest: impl IntoIterator<Item = &'a Value>,
    cmp: impl Fn(&Real, &Real) -> bool,
) -> EvalResult {
    let r = arg_to_real(first, FIRST_ARG_LABEL, NumericTypeName::REAL)?;
    let mut float_taint = r.is_inexact();
    rest.into_iter()
        .enumerate()
        .try_fold(r.clone(), |mut acc, (idx, v)| {
            let r = arg_to_real(v, idx + 1, NumericTypeName::REAL)?;
            float_taint = float_taint || r.is_inexact();
            if cmp(&acc, r) {
                acc = r.clone();
            }
            Ok(acc)
        })
        .map(|r| Value::real(if float_taint { r.into_inexact() } else { r }))
}

fn commutative_arithmetic(
    args: &[Value],
    identity: Number,
    op: impl Fn(Number, Number) -> Number,
) -> EvalResult {
    args.iter()
        .enumerate()
        .try_fold(identity, |acc, (idx, v)| {
            if let Value::Number(x) = v {
                Ok(op(acc, x.clone()))
            } else {
                Err(Condition::arg_error(idx, TypeName::NUMBER, v).into())
            }
        })
        .map(Value::Number)
}

fn inverse_arithmetic(
    args: &[Value],
    inverse: impl FnOnce(Number) -> NumResult,
    op: impl Fn(Number, Number) -> NumResult,
) -> EvalResult {
    let arg = first(args);
    let Value::Number(x) = arg else {
        return Err(invalid_target(TypeName::NUMBER, arg));
    };
    if args.len() == 1 {
        inverse(x.clone()).map_or_else(
            |err| Err(Condition::value_error(err, arg).into()),
            |x| Ok(Value::Number(x)),
        )
    } else {
        args.iter()
            .skip(1)
            .enumerate()
            .try_fold(x.clone(), |sum, (idx, v)| {
                if let Value::Number(x) = v {
                    Ok(op(sum, x.clone())
                        .map_err(|err| Exception::signal(Condition::value_error(err, v)))?)
                } else {
                    Err(Condition::arg_error(idx + 1, TypeName::NUMBER, v).into())
                }
            })
            .map(Value::Number)
    }
}

fn exact_factor_op(
    args: &[Value],
    identity: Integer,
    op: impl Fn(&Integer, &Integer) -> Integer,
) -> EvalResult {
    let mut float_taint = false;
    args.iter()
        .enumerate()
        .try_fold(identity, |acc, (idx, v)| {
            let r = arg_to_real(v, idx, NumericTypeName::INTEGER)?;
            float_taint = float_taint || r.is_inexact();
            let n = r
                .clone()
                .try_into_exact_integer()
                .map_err(|err| Exception::signal(Condition::value_error(err, v)))?;
            Ok(op(&acc, &n))
        })
        .map(|n| {
            if float_taint {
                Value::real(n.into_inexact())
            } else {
                Value::real(n)
            }
        })
}

fn exact_division<R>(
    a: &Value,
    b: &Value,
    div: impl FnOnce(Integer, Integer) -> Result<R, NumericError>,
    map: impl FnOnce(bool, bool, R) -> EvalResult,
) -> EvalResult {
    let ra = arg_to_real(a, FIRST_ARG_LABEL, NumericTypeName::INTEGER)?;
    let rb = arg_to_real(b, super::SECOND_ARG_LABEL, NumericTypeName::INTEGER)?;
    let float_taint = ra.is_inexact() || rb.is_inexact();
    let negdiv = (ra.signum() < 0.0) ^ (rb.signum() < 0.0);
    let n = ra
        .clone()
        .try_into_exact_integer()
        .map_err(|err| Exception::signal(Condition::value_error(err, a)))?;
    let d = rb
        .clone()
        .try_into_exact_integer()
        .map_err(|err| Exception::signal(Condition::value_error(err, b)))?;
    div(n, d).map_or_else(
        |err| Err(Condition::value_error(err, b).into()),
        |r| map(float_taint, negdiv, r),
    )
}

fn into_quotrem(float_taint: bool, negdiv: bool, (q, r): (Integer, Integer)) -> EvalResult {
    Ok(Value::reals(
        if float_taint {
            let qr = q.into_inexact();
            if negdiv && qr.is_zero() { -qr } else { qr }
        } else {
            q.into()
        },
        if float_taint {
            r.into_inexact()
        } else {
            r.into()
        },
    ))
}

fn into_quotient(float_taint: bool, negdiv: bool, q: Integer) -> EvalResult {
    Ok(if float_taint {
        let qr = q.into_inexact();
        Value::real(if negdiv && qr.is_zero() { -qr } else { qr })
    } else {
        Value::real(q)
    })
}

fn into_remainder(float_taint: bool, _negdiv: bool, r: Integer) -> EvalResult {
    Ok(if float_taint {
        Value::real(r.into_inexact())
    } else {
        Value::real(r)
    })
}

fn arg_to_real(
    arg: &Value,
    arg_name: impl Display,
    expected_type: impl Display,
) -> Result<&Real, Exception> {
    let Value::Number(x) = arg else {
        return Err(Condition::arg_error(arg_name, expected_type, arg).into());
    };
    let Number::Real(r) = x else {
        return Err(
            Condition::arg_type_error(arg_name, expected_type, x.as_typename(), arg).into(),
        );
    };
    Ok(r)
}

fn seq_error(name: impl Display, expected_type: impl Display, arg: &Value) -> Condition {
    if let Value::Number(x) = arg {
        Condition::arg_type_error(name, expected_type, x.as_typename(), arg)
    } else {
        Condition::arg_error(name, expected_type, arg)
    }
}
