use super::{FIRST_ARG_LABEL, MAX_ARITY, first, invalid_target};
use crate::{
    Exception,
    eval::{EvalResult, Frame},
    number::{Integer, NumResult, Number, NumericTypeName, Real},
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

    super::bind_intrinsic(env, "gcd", 0..MAX_ARITY, nums_gcd);
    super::bind_intrinsic(env, "lcm", 0..MAX_ARITY, nums_lcm);

    super::bind_intrinsic(env, "numerator", 1..1, get_numerator);
    super::bind_intrinsic(env, "denominator", 1..1, get_denominator);

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
    inverse_arithmetic(args, |x| Ok(-x), |a, b| Ok(a + (-b)))
}

fn nums_div(args: &[Value], _env: &Frame) -> EvalResult {
    inverse_arithmetic(args, Number::try_into_reciprocal, Number::div)
}

fn abs(args: &[Value], _env: &Frame) -> EvalResult {
    real_op(first(args), |r| Ok(Value::real(r.clone().into_abs())))
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::testutil::{TestEnv, err_or_fail, extract_or_fail, ok_or_fail};
    use std::assert_matches;

    #[test]
    fn is_even_integer() {
        let args = [Value::real(4)];
        let env = TestEnv::default();

        let r = is_even(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_matches!(v, Value::Boolean(true));

        let r = is_odd(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_matches!(v, Value::Boolean(false));
    }

    #[test]
    fn is_even_float_with_no_frac() {
        let args = [Value::real(4.0)];
        let env = TestEnv::default();

        let r = is_even(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_matches!(v, Value::Boolean(true));

        let r = is_odd(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_matches!(v, Value::Boolean(false));
    }

    #[test]
    fn is_even_float_with_frac() {
        let args = [Value::real(4.2)];
        let env = TestEnv::default();

        let r = is_even(&args, &env.new_frame());

        let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
        assert_eq!(
            err.to_string(),
            "#<value-error \"expected exact integer, got: 4.2\" (4.2)>"
        );

        let r = is_odd(&args, &env.new_frame());

        let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
        assert_eq!(
            err.to_string(),
            "#<value-error \"expected exact integer, got: 4.2\" (4.2)>"
        );
    }

    #[test]
    fn is_odd_integer() {
        let args = [Value::real(3)];
        let env = TestEnv::default();

        let r = is_even(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_matches!(v, Value::Boolean(false));

        let r = is_odd(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_matches!(v, Value::Boolean(true));
    }

    #[test]
    fn is_odd_float_with_no_frac() {
        let args = [Value::real(3.0)];
        let env = TestEnv::default();

        let r = is_even(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_matches!(v, Value::Boolean(false));

        let r = is_odd(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_matches!(v, Value::Boolean(true));
    }

    #[test]
    fn is_odd_float_with_frac() {
        let args = [Value::real(3.2)];
        let env = TestEnv::default();

        let r = is_even(&args, &env.new_frame());

        let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
        assert_eq!(
            err.to_string(),
            "#<value-error \"expected exact integer, got: 3.2\" (3.2)>"
        );

        let r = is_odd(&args, &env.new_frame());

        let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
        assert_eq!(
            err.to_string(),
            "#<value-error \"expected exact integer, got: 3.2\" (3.2)>"
        );
    }

    #[test]
    fn max_single_arg() {
        let args = [Value::real(4)];
        let env = TestEnv::default();

        let r = nums_max(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_matches!(v, Value::Number(Number::Real(Real::Integer(_))));
        assert_eq!(v.as_datum().to_string(), "4");
    }

    #[test]
    fn max_of_integers() {
        let args = [Value::real(3), Value::real(7), Value::real(5)];
        let env = TestEnv::default();

        let r = nums_max(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_matches!(v, Value::Number(Number::Real(Real::Integer(_))));
        assert_eq!(v.as_datum().to_string(), "7");
    }

    #[test]
    fn max_first_arg_is_largest() {
        let args = [Value::real(9), Value::real(2), Value::real(3)];
        let env = TestEnv::default();

        let r = nums_max(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_matches!(v, Value::Number(Number::Real(Real::Integer(_))));
        assert_eq!(v.as_datum().to_string(), "9");
    }

    #[test]
    fn max_negative_integers() {
        let args = [Value::real(-5), Value::real(-2), Value::real(-9)];
        let env = TestEnv::default();

        let r = nums_max(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_matches!(v, Value::Number(Number::Real(Real::Integer(_))));
        assert_eq!(v.as_datum().to_string(), "-2");
    }

    #[test]
    fn max_equal_args() {
        let args = [Value::real(4), Value::real(4), Value::real(4)];
        let env = TestEnv::default();

        let r = nums_max(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_matches!(v, Value::Number(Number::Real(Real::Integer(_))));
        assert_eq!(v.as_datum().to_string(), "4");
    }

    #[test]
    #[ignore = "Rational ordering not yet implemented"]
    fn max_rationals() {
        let args = [
            Value::real(ok_or_fail!(Real::reduce(1, 2))),
            Value::real(ok_or_fail!(Real::reduce(2, 3))),
        ];
        let env = TestEnv::default();

        let r = nums_max(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_matches!(v, Value::Number(Number::Real(Real::Rational(_))));
        assert_eq!(v.as_datum().to_string(), "2/3");
    }

    #[test]
    fn max_inexact_arg_wins() {
        let args = [Value::real(3), Value::real(4.0)];
        let env = TestEnv::default();

        let r = nums_max(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_matches!(v, Value::Number(Number::Real(Real::Float(_))));
        assert_eq!(v.as_datum().to_string(), "4.0");
    }

    #[test]
    fn max_inexact_first_arg() {
        let args = [Value::real(1.5), Value::real(3)];
        let env = TestEnv::default();

        let r = nums_max(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_matches!(v, Value::Number(Number::Real(Real::Float(_))));
        assert_eq!(v.as_datum().to_string(), "3.0");
    }

    #[test]
    fn max_exact_arg_beats_inexact() {
        let args = [Value::real(5), Value::real(2.0)];
        let env = TestEnv::default();

        let r = nums_max(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_matches!(v, Value::Number(Number::Real(Real::Float(_))));
        assert_eq!(v.as_datum().to_string(), "5.0");
    }

    #[test]
    fn max_invalid_first_arg() {
        let args = [Value::string("foo"), Value::real(1)];
        let env = TestEnv::default();

        let r = nums_max(&args, &env.new_frame());

        let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
        assert_eq!(
            err.to_string(),
            "#<value-error \"invalid type for arg `0` - expected: real, got: string\" (\"foo\")>"
        );
    }

    #[test]
    fn max_complex_first_arg() {
        let args = [Value::Number(Number::complex(3, 4)), Value::real(1)];
        let env = TestEnv::default();

        let r = nums_max(&args, &env.new_frame());

        let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
        assert_eq!(
            err.to_string(),
            "#<value-error \"invalid type for arg `0` - expected: real, got: complex\" (3+4i)>"
        );
    }

    #[test]
    fn max_invalid_later_arg() {
        let args = [Value::real(1), Value::string("foo")];
        let env = TestEnv::default();

        let r = nums_max(&args, &env.new_frame());

        let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
        assert_eq!(
            err.to_string(),
            "#<value-error \"invalid type for arg `1` - expected: real, got: string\" (\"foo\")>"
        );
    }

    // NaN must propagate through max/min (R6RS explicitly requires it, and
    // every IEEE-754/major-Scheme convention agrees): the max/min of a set
    // containing an undefined element is undefined, i.e. NaN.
    #[test]
    fn max_with_nan_later_arg_is_nan() {
        let args = [Value::real(1.0), Value::real(f64::NAN)];
        let env = TestEnv::default();

        let r = nums_max(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_eq!(v.as_datum().to_string(), "+nan.0");
    }

    #[test]
    fn max_with_nan_first_arg_is_nan() {
        let args = [Value::real(f64::NAN), Value::real(1.0)];
        let env = TestEnv::default();

        let r = nums_max(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_eq!(v.as_datum().to_string(), "+nan.0");
    }

    #[test]
    fn max_with_nan_among_many_args_is_nan() {
        let args = [
            Value::real(1),
            Value::real(5.0),
            Value::real(f64::NAN),
            Value::real(2),
        ];
        let env = TestEnv::default();

        let r = nums_max(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_eq!(v.as_datum().to_string(), "+nan.0");
    }

    // R7RS-small is silent on max/min tie-breaking between numerically-equal
    // signed zeros (-0.0 = 0.0 under `=`, so either is a "legal" maximum).
    // IEEE 754-2019 SS9.6 maximum/minimum settle it by ordering -0 < +0, and
    // that convention is what these tests assert.
    #[test]
    fn max_negative_zero_then_positive_zero() {
        let args = [Value::real(-0.0), Value::real(0.0)];
        let env = TestEnv::default();

        let r = nums_max(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_matches!(v, Value::Number(Number::Real(Real::Float(_))));
        assert_eq!(v.as_datum().to_string(), "0.0");
    }

    #[test]
    fn max_positive_zero_then_negative_zero() {
        let args = [Value::real(0.0), Value::real(-0.0)];
        let env = TestEnv::default();

        let r = nums_max(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_matches!(v, Value::Number(Number::Real(Real::Float(_))));
        assert_eq!(v.as_datum().to_string(), "0.0");
    }

    #[test]
    fn max_exact_zero_then_negative_zero() {
        let args = [Value::real(0), Value::real(-0.0)];
        let env = TestEnv::default();

        let r = nums_max(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_matches!(v, Value::Number(Number::Real(Real::Float(_))));
        assert_eq!(v.as_datum().to_string(), "0.0");
    }

    #[test]
    fn max_negative_zero_then_exact_zero() {
        let args = [Value::real(-0.0), Value::real(0)];
        let env = TestEnv::default();

        let r = nums_max(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_matches!(v, Value::Number(Number::Real(Real::Float(_))));
        assert_eq!(v.as_datum().to_string(), "0.0");
    }

    #[test]
    fn max_all_negative_zeros() {
        let args = [Value::real(-0.0), Value::real(-0.0)];
        let env = TestEnv::default();

        let r = nums_max(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_matches!(v, Value::Number(Number::Real(Real::Float(_))));
        assert_eq!(v.as_datum().to_string(), "-0.0");
    }

    #[test]
    fn max_signed_zeros_among_many_args() {
        let args = [Value::real(-0.0), Value::real(0.0), Value::real(-0.0)];
        let env = TestEnv::default();

        let r = nums_max(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_matches!(v, Value::Number(Number::Real(Real::Float(_))));
        assert_eq!(v.as_datum().to_string(), "0.0");
    }

    #[test]
    fn max_single_negative_zero_preserves_sign() {
        let args = [Value::real(-0.0)];
        let env = TestEnv::default();

        let r = nums_max(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_matches!(v, Value::Number(Number::Real(Real::Float(_))));
        assert_eq!(v.as_datum().to_string(), "-0.0");
    }

    #[test]
    fn max_negative_zero_loses_to_positive() {
        let args = [Value::real(-0.0), Value::real(1)];
        let env = TestEnv::default();

        let r = nums_max(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_matches!(v, Value::Number(Number::Real(Real::Float(_))));
        assert_eq!(v.as_datum().to_string(), "1.0");
    }

    #[test]
    fn max_negative_zero_beats_negative() {
        let args = [Value::real(-1), Value::real(-0.0)];
        let env = TestEnv::default();

        let r = nums_max(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_matches!(v, Value::Number(Number::Real(Real::Float(_))));
        assert_eq!(v.as_datum().to_string(), "-0.0");
    }

    #[test]
    fn max_negative_zero_with_nan_is_nan() {
        let args = [Value::real(-0.0), Value::real(f64::NAN)];
        let env = TestEnv::default();

        let r = nums_max(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_eq!(v.as_datum().to_string(), "+nan.0");
    }

    #[test]
    fn min_of_integers() {
        let args = [Value::real(3), Value::real(7), Value::real(5)];
        let env = TestEnv::default();

        let r = nums_min(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_matches!(v, Value::Number(Number::Real(Real::Integer(_))));
        assert_eq!(v.as_datum().to_string(), "3");
    }

    #[test]
    fn min_inexact_arg_wins() {
        let args = [Value::real(3), Value::real(1.0)];
        let env = TestEnv::default();

        let r = nums_min(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_matches!(v, Value::Number(Number::Real(Real::Float(_))));
        assert_eq!(v.as_datum().to_string(), "1.0");
    }

    // See the max_with_nan_* comment above -- min must propagate NaN too.
    #[test]
    fn min_with_nan_later_arg_is_nan() {
        let args = [Value::real(1.0), Value::real(f64::NAN)];
        let env = TestEnv::default();

        let r = nums_min(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_eq!(v.as_datum().to_string(), "+nan.0");
    }

    #[test]
    fn min_with_nan_first_arg_is_nan() {
        let args = [Value::real(f64::NAN), Value::real(1.0)];
        let env = TestEnv::default();

        let r = nums_min(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_eq!(v.as_datum().to_string(), "+nan.0");
    }

    // See the max_negative_zero_* comment above -- min ties on signed zeros
    // are decided the same IEEE 754-2019 way (-0 < +0), so -0.0 must win.
    #[test]
    fn min_positive_zero_then_negative_zero() {
        let args = [Value::real(0.0), Value::real(-0.0)];
        let env = TestEnv::default();

        let r = nums_min(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_matches!(v, Value::Number(Number::Real(Real::Float(_))));
        assert_eq!(v.as_datum().to_string(), "-0.0");
    }

    #[test]
    fn min_negative_zero_then_positive_zero() {
        let args = [Value::real(-0.0), Value::real(0.0)];
        let env = TestEnv::default();

        let r = nums_min(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_matches!(v, Value::Number(Number::Real(Real::Float(_))));
        assert_eq!(v.as_datum().to_string(), "-0.0");
    }

    #[test]
    fn min_exact_zero_then_negative_zero() {
        let args = [Value::real(0), Value::real(-0.0)];
        let env = TestEnv::default();

        let r = nums_min(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_matches!(v, Value::Number(Number::Real(Real::Float(_))));
        assert_eq!(v.as_datum().to_string(), "-0.0");
    }

    #[test]
    fn min_negative_zero_then_exact_zero() {
        let args = [Value::real(-0.0), Value::real(0)];
        let env = TestEnv::default();

        let r = nums_min(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_matches!(v, Value::Number(Number::Real(Real::Float(_))));
        assert_eq!(v.as_datum().to_string(), "-0.0");
    }

    #[test]
    fn min_all_positive_zeros() {
        let args = [Value::real(0.0), Value::real(0.0)];
        let env = TestEnv::default();

        let r = nums_min(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_matches!(v, Value::Number(Number::Real(Real::Float(_))));
        assert_eq!(v.as_datum().to_string(), "0.0");
    }

    #[test]
    fn min_signed_zeros_among_many_args() {
        let args = [Value::real(1), Value::real(0.0), Value::real(-0.0)];
        let env = TestEnv::default();

        let r = nums_min(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_matches!(v, Value::Number(Number::Real(Real::Float(_))));
        assert_eq!(v.as_datum().to_string(), "-0.0");
    }

    #[test]
    fn min_single_negative_zero_preserves_sign() {
        let args = [Value::real(-0.0)];
        let env = TestEnv::default();

        let r = nums_min(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_matches!(v, Value::Number(Number::Real(Real::Float(_))));
        assert_eq!(v.as_datum().to_string(), "-0.0");
    }

    #[test]
    fn min_negative_zero_loses_to_negative() {
        let args = [Value::real(0.0), Value::real(-1)];
        let env = TestEnv::default();

        let r = nums_min(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_matches!(v, Value::Number(Number::Real(Real::Float(_))));
        assert_eq!(v.as_datum().to_string(), "-1.0");
    }

    #[test]
    fn min_negative_zero_with_nan_is_nan() {
        let args = [Value::real(-0.0), Value::real(f64::NAN)];
        let env = TestEnv::default();

        let r = nums_min(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_eq!(v.as_datum().to_string(), "+nan.0");
    }

    #[test]
    fn min_invalid_first_arg() {
        let args = [Value::string("foo"), Value::real(1)];
        let env = TestEnv::default();

        let r = nums_min(&args, &env.new_frame());

        let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
        assert_eq!(
            err.to_string(),
            "#<value-error \"invalid type for arg `0` - expected: real, got: string\" (\"foo\")>"
        );
    }

    #[test]
    fn gcd_basic() {
        let args = [Value::real(32), Value::real(-36)];
        let env = TestEnv::default();

        let r = nums_gcd(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_matches!(v, Value::Number(Number::Real(Real::Integer(_))));
        assert_eq!(v.as_datum().to_string(), "4");
    }

    #[test]
    fn gcd_empty_sequence() {
        let args = [];
        let env = TestEnv::default();

        let r = nums_gcd(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_matches!(v, Value::Number(Number::Real(Real::Integer(_))));
        assert_eq!(v.as_datum().to_string(), "0");
    }

    #[test]
    fn gcd_single_arg() {
        let args = [Value::real(-9)];
        let env = TestEnv::default();

        let r = nums_gcd(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_matches!(v, Value::Number(Number::Real(Real::Integer(_))));
        assert_eq!(v.as_datum().to_string(), "9");
    }

    #[test]
    fn gcd_float_taint_later_arg() {
        let args = [Value::real(4), Value::real(6.0)];
        let env = TestEnv::default();

        let r = nums_gcd(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_matches!(v, Value::Number(Number::Real(Real::Float(_))));
        assert_eq!(v.as_datum().to_string(), "2.0");
    }

    #[test]
    fn gcd_float_taint_first_arg() {
        let args = [Value::real(4.0), Value::real(6)];
        let env = TestEnv::default();

        let r = nums_gcd(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_matches!(v, Value::Number(Number::Real(Real::Float(_))));
        assert_eq!(v.as_datum().to_string(), "2.0");
    }

    #[test]
    fn gcd_non_integral_float() {
        let args = [Value::real(4.5)];
        let env = TestEnv::default();

        let r = nums_gcd(&args, &env.new_frame());

        let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
        assert_eq!(
            err.to_string(),
            "#<value-error \"expected exact integer, got: 4.5\" (4.5)>"
        );
    }

    #[test]
    fn gcd_invalid_later_arg() {
        let args = [Value::real(4), Value::string("foo")];
        let env = TestEnv::default();

        let r = nums_gcd(&args, &env.new_frame());

        let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
        assert_eq!(
            err.to_string(),
            "#<value-error \"invalid type for arg `1` - expected: integer, got: string\" (\"foo\")>"
        );
    }

    #[test]
    fn gcd_complex_arg() {
        let args = [Value::Number(Number::complex(3, 4))];
        let env = TestEnv::default();

        let r = nums_gcd(&args, &env.new_frame());

        let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
        assert_eq!(
            err.to_string(),
            "#<value-error \"invalid type for arg `0` - expected: integer, got: complex\" (3+4i)>"
        );
    }

    #[test]
    fn sub_single_positive_arg_negates() {
        let args = [Value::real(4)];
        let env = TestEnv::default();

        let r = nums_sub(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_eq!(v.as_datum().to_string(), "-4");
    }

    #[test]
    fn sub_single_negative_arg_negates() {
        let args = [Value::real(-4)];
        let env = TestEnv::default();

        let r = nums_sub(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_eq!(v.as_datum().to_string(), "4");
    }

    #[test]
    fn sub_single_zero_arg() {
        let args = [Value::real(0)];
        let env = TestEnv::default();

        let r = nums_sub(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_eq!(v.as_datum().to_string(), "0");
    }

    #[test]
    fn sub_single_float_arg_negates() {
        let args = [Value::real(1.5)];
        let env = TestEnv::default();

        let r = nums_sub(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_eq!(v.as_datum().to_string(), "-1.5");
    }

    #[test]
    fn sub_single_complex_arg_negates() {
        let args = [Value::Number(Number::complex(3, 4))];
        let env = TestEnv::default();

        let r = nums_sub(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_eq!(v.as_datum().to_string(), "-3-4i");
    }

    #[test]
    fn sub_two_args() {
        let args = [Value::real(10), Value::real(3)];
        let env = TestEnv::default();

        let r = nums_sub(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_eq!(v.as_datum().to_string(), "7");
    }

    #[test]
    fn sub_two_args_result_negative() {
        let args = [Value::real(3), Value::real(10)];
        let env = TestEnv::default();

        let r = nums_sub(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_eq!(v.as_datum().to_string(), "-7");
    }

    #[test]
    fn sub_two_equal_args_is_zero() {
        let args = [Value::real(5), Value::real(5)];
        let env = TestEnv::default();

        let r = nums_sub(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_eq!(v.as_datum().to_string(), "0");
    }

    #[test]
    fn sub_is_left_associative() {
        let args = [Value::real(10), Value::real(3), Value::real(2)];
        let env = TestEnv::default();

        let r = nums_sub(&args, &env.new_frame());

        // (10 - 3) - 2, not 10 - (3 - 2)
        let v = ok_or_fail!(r);
        assert_eq!(v.as_datum().to_string(), "5");
    }

    #[test]
    fn sub_exact_args_stay_exact() {
        let args = [Value::real(10), Value::real(3)];
        let env = TestEnv::default();

        let r = nums_sub(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_matches!(v, Value::Number(Number::Real(Real::Integer(_))));
    }

    #[test]
    fn sub_inexact_later_arg_taints_result() {
        let args = [Value::real(5), Value::real(2.0)];
        let env = TestEnv::default();

        let r = nums_sub(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_matches!(v, Value::Number(Number::Real(Real::Float(_))));
        assert_eq!(v.as_datum().to_string(), "3.0");
    }

    #[test]
    fn sub_inexact_first_arg_taints_result() {
        let args = [Value::real(5.0), Value::real(2)];
        let env = TestEnv::default();

        let r = nums_sub(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_matches!(v, Value::Number(Number::Real(Real::Float(_))));
        assert_eq!(v.as_datum().to_string(), "3.0");
    }

    #[test]
    fn sub_invalid_first_arg() {
        let args = [Value::string("foo"), Value::real(1)];
        let env = TestEnv::default();

        let r = nums_sub(&args, &env.new_frame());

        let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
        assert_eq!(
            err.to_string(),
            "#<value-error \"invalid type for arg `0` - expected: number, got: string\" (\"foo\")>"
        );
    }

    #[test]
    fn sub_invalid_later_arg() {
        let args = [Value::real(1), Value::string("foo")];
        let env = TestEnv::default();

        let r = nums_sub(&args, &env.new_frame());

        let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
        assert_eq!(
            err.to_string(),
            "#<value-error \"invalid type for arg `1` - expected: number, got: string\" (\"foo\")>"
        );
    }

    #[test]
    #[ignore = "rational addition not yet implemented"]
    fn sub_rationals() {
        let args = [
            Value::real(ok_or_fail!(Real::reduce(3, 4))),
            Value::real(ok_or_fail!(Real::reduce(1, 4))),
        ];
        let env = TestEnv::default();

        let r = nums_sub(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_eq!(v.as_datum().to_string(), "1/2");
    }

    #[test]
    fn add_empty_sequence() {
        let args = [];
        let env = TestEnv::default();

        let r = nums_add(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_matches!(v, Value::Number(Number::Real(Real::Integer(_))));
        assert_eq!(v.as_datum().to_string(), "0");
    }

    #[test]
    fn add_single_arg() {
        let args = [Value::real(7)];
        let env = TestEnv::default();

        let r = nums_add(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_eq!(v.as_datum().to_string(), "7");
    }

    #[test]
    fn add_two_args() {
        let args = [Value::real(3), Value::real(4)];
        let env = TestEnv::default();

        let r = nums_add(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_eq!(v.as_datum().to_string(), "7");
    }

    #[test]
    fn add_many_args() {
        let args = [Value::real(1), Value::real(2), Value::real(3)];
        let env = TestEnv::default();

        let r = nums_add(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_eq!(v.as_datum().to_string(), "6");
    }

    // Exact zero is *not* additive identity for floats, specifically it
    // can mess with zero-sign if we blindly convert 0 => 0.0; next few
    // tests cover this case.
    #[test]
    fn add_single_negative_zero_arg_preserves_sign() {
        let args = [Value::real(-0.0)];
        let env = TestEnv::default();

        let r = nums_add(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_eq!(v.as_datum().to_string(), "-0.0");
    }

    #[test]
    fn add_two_negative_zero_args_preserves_sign() {
        let args = [Value::real(-0.0), Value::real(-0.0)];
        let env = TestEnv::default();

        let r = nums_add(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_eq!(v.as_datum().to_string(), "-0.0");
    }

    #[test]
    fn add_negative_zero_then_exact_zero_preserves_sign() {
        let args = [Value::real(-0.0), Value::real(0)];
        let env = TestEnv::default();

        let r = nums_add(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_eq!(v.as_datum().to_string(), "-0.0");
    }

    #[test]
    fn add_exact_args_stay_exact() {
        let args = [Value::real(3), Value::real(4)];
        let env = TestEnv::default();

        let r = nums_add(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_matches!(v, Value::Number(Number::Real(Real::Integer(_))));
    }

    #[test]
    fn add_inexact_later_arg_taints_result() {
        let args = [Value::real(5), Value::real(2.0)];
        let env = TestEnv::default();

        let r = nums_add(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_matches!(v, Value::Number(Number::Real(Real::Float(_))));
        assert_eq!(v.as_datum().to_string(), "7.0");
    }

    #[test]
    fn add_inexact_first_arg_taints_result() {
        let args = [Value::real(5.0), Value::real(2)];
        let env = TestEnv::default();

        let r = nums_add(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_matches!(v, Value::Number(Number::Real(Real::Float(_))));
        assert_eq!(v.as_datum().to_string(), "7.0");
    }

    #[test]
    fn add_complex_arg() {
        let args = [Value::real(2), Value::Number(Number::complex(3, 4))];
        let env = TestEnv::default();

        let r = nums_add(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_eq!(v.as_datum().to_string(), "5+4i");
    }

    #[test]
    fn add_invalid_first_arg() {
        let args = [Value::string("foo"), Value::real(1)];
        let env = TestEnv::default();

        let r = nums_add(&args, &env.new_frame());

        let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
        assert_eq!(
            err.to_string(),
            "#<value-error \"invalid type for arg `0` - expected: number, got: string\" (\"foo\")>"
        );
    }

    #[test]
    fn add_invalid_later_arg() {
        let args = [Value::real(1), Value::string("foo")];
        let env = TestEnv::default();

        let r = nums_add(&args, &env.new_frame());

        let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
        assert_eq!(
            err.to_string(),
            "#<value-error \"invalid type for arg `1` - expected: number, got: string\" (\"foo\")>"
        );
    }

    #[test]
    fn mult_empty_sequence() {
        let args = [];
        let env = TestEnv::default();

        let r = nums_mult(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_matches!(v, Value::Number(Number::Real(Real::Integer(_))));
        assert_eq!(v.as_datum().to_string(), "1");
    }

    #[test]
    fn mult_single_arg() {
        let args = [Value::real(7)];
        let env = TestEnv::default();

        let r = nums_mult(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_eq!(v.as_datum().to_string(), "7");
    }

    #[test]
    fn mult_single_negative_arg() {
        let args = [Value::real(-4)];
        let env = TestEnv::default();

        let r = nums_mult(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_eq!(v.as_datum().to_string(), "-4");
    }

    #[test]
    fn mult_two_args() {
        let args = [Value::real(2), Value::real(3)];
        let env = TestEnv::default();

        let r = nums_mult(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_eq!(v.as_datum().to_string(), "6");
    }

    #[test]
    fn mult_many_args() {
        let args = [Value::real(2), Value::real(3), Value::real(4)];
        let env = TestEnv::default();

        let r = nums_mult(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_eq!(v.as_datum().to_string(), "24");
    }

    #[test]
    fn mult_exact_args_stay_exact() {
        let args = [Value::real(2), Value::real(3)];
        let env = TestEnv::default();

        let r = nums_mult(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_matches!(v, Value::Number(Number::Real(Real::Integer(_))));
    }

    #[test]
    fn mult_inexact_later_arg_taints_result() {
        let args = [Value::real(5), Value::real(2.0)];
        let env = TestEnv::default();

        let r = nums_mult(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_matches!(v, Value::Number(Number::Real(Real::Float(_))));
        assert_eq!(v.as_datum().to_string(), "10.0");
    }

    #[test]
    fn mult_inexact_first_arg_taints_result() {
        let args = [Value::real(5.0), Value::real(2)];
        let env = TestEnv::default();

        let r = nums_mult(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_matches!(v, Value::Number(Number::Real(Real::Float(_))));
        assert_eq!(v.as_datum().to_string(), "10.0");
    }

    #[test]
    fn mult_exact_zero_arg() {
        let args = [Value::real(0), Value::real(1.5)];
        let env = TestEnv::default();

        let r = nums_mult(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_eq!(v.as_datum().to_string(), "0");
    }

    // R7RS SS6.2.3 permits (but does not require) exact 0 times anything to
    // be exact 0, and Zara takes that option -- exact zero short-circuits
    // inexact contagion even when the other argument is NaN or infinite.
    // This is a deliberate divergence from IEEE-754 float semantics (where
    // 0 * NaN = NaN and 0 * inf = NaN).
    #[test]
    fn mult_exact_zero_with_nan_arg_is_exact_zero() {
        let args = [Value::real(0), Value::real(f64::NAN)];
        let env = TestEnv::default();

        let r = nums_mult(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_matches!(v, Value::Number(Number::Real(Real::Integer(_))));
        assert_eq!(v.as_datum().to_string(), "0");
    }

    #[test]
    fn mult_exact_zero_with_infinity_arg_is_exact_zero() {
        let args = [Value::real(0), Value::real(f64::INFINITY)];
        let env = TestEnv::default();

        let r = nums_mult(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_matches!(v, Value::Number(Number::Real(Real::Integer(_))));
        assert_eq!(v.as_datum().to_string(), "0");
    }

    #[test]
    fn mult_complex_arg() {
        let args = [Value::real(2), Value::Number(Number::complex(3, 4))];
        let env = TestEnv::default();

        let r = nums_mult(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_eq!(v.as_datum().to_string(), "6+8i");
    }

    #[test]
    #[ignore = "rational multiplication not yet implemented"]
    fn mult_rational_arg() {
        let args = [Value::real(2), Value::real(ok_or_fail!(Real::reduce(1, 2)))];
        let env = TestEnv::default();

        let r = nums_mult(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_eq!(v.as_datum().to_string(), "1");
    }

    #[test]
    fn mult_invalid_first_arg() {
        let args = [Value::string("foo"), Value::real(1)];
        let env = TestEnv::default();

        let r = nums_mult(&args, &env.new_frame());

        let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
        assert_eq!(
            err.to_string(),
            "#<value-error \"invalid type for arg `0` - expected: number, got: string\" (\"foo\")>"
        );
    }

    #[test]
    fn mult_invalid_later_arg() {
        let args = [Value::real(1), Value::string("foo")];
        let env = TestEnv::default();

        let r = nums_mult(&args, &env.new_frame());

        let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
        assert_eq!(
            err.to_string(),
            "#<value-error \"invalid type for arg `1` - expected: number, got: string\" (\"foo\")>"
        );
    }

    #[test]
    fn div_single_positive_arg_reciprocates() {
        let args = [Value::real(4)];
        let env = TestEnv::default();

        let r = nums_div(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_eq!(v.as_datum().to_string(), "1/4");
    }

    #[test]
    fn div_single_zero_arg_is_an_error() {
        let args = [Value::real(0)];
        let env = TestEnv::default();

        let r = nums_div(&args, &env.new_frame());

        let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
        assert_eq!(err.to_string(), "#<value-error \"divide by zero\" (0)>");
    }

    #[test]
    fn div_two_args() {
        let args = [Value::real(10), Value::real(2)];
        let env = TestEnv::default();

        let r = nums_div(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_eq!(v.as_datum().to_string(), "5");
    }

    #[test]
    fn div_exact_zero_first_arg_is_exact_zero() {
        let args = [Value::real(0), Value::real(5)];
        let env = TestEnv::default();

        let r = nums_div(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_matches!(v, Value::Number(Number::Real(Real::Integer(_))));
        assert_eq!(v.as_datum().to_string(), "0");
    }

    // Unlike `*` above, exact zero as the dividend still short-circuits
    // inexact contagion when the divisor is an inexact zero or infinity --
    // but NaN wins over the exact-zero shortcut here, so `(/ 0 +nan.0)` is
    // `+nan.0`, not `0`.
    #[test]
    fn div_exact_zero_by_inexact_zero_is_exact_zero() {
        let args = [Value::real(0), Value::real(0.0)];
        let env = TestEnv::default();

        let r = nums_div(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_matches!(v, Value::Number(Number::Real(Real::Integer(_))));
        assert_eq!(v.as_datum().to_string(), "0");
    }

    #[test]
    fn div_exact_zero_by_nan_is_nan() {
        let args = [Value::real(0), Value::real(f64::NAN)];
        let env = TestEnv::default();

        let r = nums_div(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_matches!(v, Value::Number(Number::Real(Real::Float(_))));
        assert_eq!(v.as_datum().to_string(), "+nan.0");
    }

    #[test]
    fn div_by_zero_later_arg_is_an_error() {
        let args = [Value::real(5), Value::real(0)];
        let env = TestEnv::default();

        let r = nums_div(&args, &env.new_frame());

        let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
        assert_eq!(err.to_string(), "#<value-error \"divide by zero\" (0)>");
    }

    #[test]
    fn div_is_left_associative() {
        let args = [Value::real(100), Value::real(5), Value::real(2)];
        let env = TestEnv::default();

        let r = nums_div(&args, &env.new_frame());

        // (100 / 5) / 2, not 100 / (5 / 2)
        let v = ok_or_fail!(r);
        assert_eq!(v.as_datum().to_string(), "10");
    }

    #[test]
    fn div_exact_args_stay_exact() {
        let args = [Value::real(10), Value::real(2)];
        let env = TestEnv::default();

        let r = nums_div(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_matches!(v, Value::Number(Number::Real(Real::Integer(_))));
    }

    #[test]
    fn div_inexact_later_arg_taints_result() {
        let args = [Value::real(10), Value::real(2.0)];
        let env = TestEnv::default();

        let r = nums_div(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_matches!(v, Value::Number(Number::Real(Real::Float(_))));
        assert_eq!(v.as_datum().to_string(), "5.0");
    }

    #[test]
    fn div_single_complex_arg_reciprocates() {
        let args = [Value::Number(Number::complex(3, 4))];
        let env = TestEnv::default();

        let r = nums_div(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_eq!(v.as_datum().to_string(), "3/25-4/25i");
    }

    #[test]
    fn div_complex_by_real_arg() {
        let args = [Value::Number(Number::complex(3, 2)), Value::real(2)];
        let env = TestEnv::default();

        let r = nums_div(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_eq!(v.as_datum().to_string(), "3/2+i");
    }

    #[test]
    fn div_real_by_complex_arg() {
        let args = [Value::real(5), Value::Number(Number::complex(1, 2))];
        let env = TestEnv::default();

        let r = nums_div(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        assert_eq!(v.as_datum().to_string(), "1-2i");
    }

    #[test]
    fn div_single_complex_arg_with_large_magnitude_does_not_overflow() {
        let args = [Value::Number(Number::complex(1e200, 1e200))];
        let env = TestEnv::default();

        let r = nums_div(&args, &env.new_frame());

        let v = ok_or_fail!(r);
        let n = extract_or_fail!(v, Value::Number);
        assert!(!n.is_zero());
    }

    #[test]
    fn div_invalid_first_arg() {
        let args = [Value::string("foo"), Value::real(1)];
        let env = TestEnv::default();

        let r = nums_div(&args, &env.new_frame());

        let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
        assert_eq!(
            err.to_string(),
            "#<value-error \"invalid type for arg `0` - expected: number, got: string\" (\"foo\")>"
        );
    }

    #[test]
    fn div_invalid_later_arg() {
        let args = [Value::real(1), Value::string("foo")];
        let env = TestEnv::default();

        let r = nums_div(&args, &env.new_frame());

        let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
        assert_eq!(
            err.to_string(),
            "#<value-error \"invalid type for arg `1` - expected: number, got: string\" (\"foo\")>"
        );
    }
}
