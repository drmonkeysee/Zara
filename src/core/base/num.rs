use super::{FIRST_ARG_LABEL, MAX_ARITY, first, invalid_target};
use crate::{
    eval::{EvalResult, Frame},
    number::{Integer, Number, NumericTypeName, Real},
    value::{Condition, TypeName, Value},
};
use std::fmt::Display;

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

    super::bind_intrinsic(env, "abs", 1..1, abs);

    super::bind_intrinsic(env, "numerator", 1..1, get_numerator);
    super::bind_intrinsic(env, "denominator", 1..1, get_denominator);

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
    real_acc_op(first(args), args.iter().skip(1), Real::lt)
}

fn nums_min(args: &[Value], _env: &Frame) -> EvalResult {
    real_acc_op(first(args), args.iter().skip(1), Real::gt)
}

fn nums_add(args: &[Value], _env: &Frame) -> EvalResult {
    args.iter()
        .enumerate()
        .try_fold(Number::zero(), |sum, (k, v)| {
            if let Value::Number(n) = v {
                Ok(sum + n.clone())
            } else {
                Err(Condition::arg_error(k, TypeName::NUMBER, v).into())
            }
        })
        .map(Value::Number)
}

fn abs(args: &[Value], _env: &Frame) -> EvalResult {
    real_op(first(args), |r| Ok(Value::real(r.clone().into_abs())))
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

fn into_inexact(args: &[Value], _env: &Frame) -> EvalResult {
    let arg = first(args);
    if let Value::Number(n) = arg {
        Ok(Value::Number(n.clone().into_inexact()))
    } else {
        Err(invalid_target(TypeName::NUMBER, arg))
    }
}

fn into_exact(args: &[Value], _env: &Frame) -> EvalResult {
    let arg = first(args);
    if let Value::Number(n) = arg {
        n.clone().try_into_exact().map_or_else(
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
    let Value::Number(n) = arg else {
        return Err(invalid_target(expected_type, arg));
    };
    if let Number::Real(r) = n {
        op(r)
    } else {
        Err(Condition::arg_type_error(FIRST_ARG_LABEL, expected_type, n.as_typename(), arg).into())
    }
}

fn real_acc_op<'a>(
    first: &Value,
    rest: impl IntoIterator<Item = &'a Value>,
    op: impl Fn(&Real, &Real) -> bool,
) -> EvalResult {
    let Value::Number(x) = first else {
        return Err(invalid_target(NumericTypeName::REAL, first));
    };
    let Number::Real(r) = x else {
        return Err(Condition::arg_type_error(
            FIRST_ARG_LABEL,
            NumericTypeName::REAL,
            x.as_typename(),
            first,
        )
        .into());
    };
    rest.into_iter()
        .enumerate()
        .try_fold(r.clone(), |mut acc, (k, v)| {
            let Value::Number(x) = v else {
                return Err(Condition::arg_error(k + 1, NumericTypeName::REAL, v).into());
            };
            let Number::Real(r) = x else {
                return Err(Condition::arg_type_error(
                    k + 1,
                    NumericTypeName::REAL,
                    x.as_typename(),
                    v,
                )
                .into());
            };
            let float_taint = acc.is_inexact() || r.is_inexact();
            if op(&acc, r) {
                acc = r.clone();
            }
            Ok(if float_taint { acc.into_inexact() } else { acc })
        })
        .map(|r| Value::Number(Number::real(r.clone())))
}

fn seq_error(name: impl Display, expected_type: impl Display, arg: &Value) -> Condition {
    if let Value::Number(n) = arg {
        Condition::arg_type_error(name, expected_type, n.as_typename(), arg)
    } else {
        Condition::arg_error(name, expected_type, arg)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        Exception,
        testutil::{TestEnv, err_or_fail, extract_or_fail, ok_or_fail},
    };
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
}
