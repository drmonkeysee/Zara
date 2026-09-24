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

// R7RS-small 6.2.6 examples: (floor/ 5 2) => 2 1,
// (floor/ -5 2) => -3 1, (floor/ 5 -2) => -3 -1, (floor/ -5 -2) => 2 -1;
// (truncate/ 5 2) => 2 1, (truncate/ -5 2) => -2 -1,
// (truncate/ 5 -2) => -2 1, (truncate/ -5 -2) => 2 -1,
// (truncate/ -5.0 -2) => 2.0 -1.0

#[test]
fn floor_qr_basic() {
    let args = [Value::real(7), Value::real(2)];
    let env = TestEnv::default();

    let r = floor_qr(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_matches!(v, Value::Reals(_));
    assert_eq!(v.as_datum().to_string(), "[3, 1]");
}

#[test]
fn truncate_qr_basic() {
    let args = [Value::real(7), Value::real(2)];
    let env = TestEnv::default();

    let r = truncate_qr(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_matches!(v, Value::Reals(_));
    assert_eq!(v.as_datum().to_string(), "[3, 1]");
}

#[test]
fn floor_quotient_basic() {
    let args = [Value::real(7), Value::real(2)];
    let env = TestEnv::default();

    let r = floor_quotient(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_matches!(v, Value::Number(Number::Real(Real::Integer(_))));
    assert_eq!(v.as_datum().to_string(), "3");
}

#[test]
fn truncate_quotient_basic() {
    let args = [Value::real(7), Value::real(2)];
    let env = TestEnv::default();

    let r = truncate_quotient(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_matches!(v, Value::Number(Number::Real(Real::Integer(_))));
    assert_eq!(v.as_datum().to_string(), "3");
}

#[test]
fn floor_remainder_basic() {
    let args = [Value::real(7), Value::real(2)];
    let env = TestEnv::default();

    let r = floor_remainder(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_matches!(v, Value::Number(Number::Real(Real::Integer(_))));
    assert_eq!(v.as_datum().to_string(), "1");
}

#[test]
fn truncate_remainder_basic() {
    let args = [Value::real(7), Value::real(2)];
    let env = TestEnv::default();

    let r = truncate_remainder(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_matches!(v, Value::Number(Number::Real(Real::Integer(_))));
    assert_eq!(v.as_datum().to_string(), "1");
}

#[test]
fn floor_qr_negative_dividend() {
    let args = [Value::real(-5), Value::real(2)];
    let env = TestEnv::default();

    let r = floor_qr(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v.as_datum().to_string(), "[-3, 1]");
}

#[test]
fn truncate_qr_negative_dividend() {
    let args = [Value::real(-5), Value::real(2)];
    let env = TestEnv::default();

    let r = truncate_qr(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v.as_datum().to_string(), "[-2, -1]");
}

#[test]
fn floor_qr_negative_divisor() {
    let args = [Value::real(5), Value::real(-2)];
    let env = TestEnv::default();

    let r = floor_qr(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v.as_datum().to_string(), "[-3, -1]");
}

#[test]
fn truncate_qr_negative_divisor() {
    let args = [Value::real(5), Value::real(-2)];
    let env = TestEnv::default();

    let r = truncate_qr(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v.as_datum().to_string(), "[-2, 1]");
}

#[test]
fn floor_qr_both_negative() {
    let args = [Value::real(-5), Value::real(-2)];
    let env = TestEnv::default();

    let r = floor_qr(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v.as_datum().to_string(), "[2, -1]");
}

#[test]
fn truncate_qr_both_negative() {
    let args = [Value::real(-5), Value::real(-2)];
    let env = TestEnv::default();

    let r = truncate_qr(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v.as_datum().to_string(), "[2, -1]");
}

#[test]
fn floor_quotient_negative_dividend() {
    let args = [Value::real(-5), Value::real(2)];
    let env = TestEnv::default();

    let r = floor_quotient(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v.as_datum().to_string(), "-3");
}

#[test]
fn truncate_quotient_negative_dividend() {
    let args = [Value::real(-5), Value::real(2)];
    let env = TestEnv::default();

    let r = truncate_quotient(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v.as_datum().to_string(), "-2");
}

// the classic modulo/remainder distinction (`modulo` binds to floor_remainder,
// `remainder` to truncate_remainder; see load())
#[test]
fn floor_remainder_negative_dividend() {
    let args = [Value::real(-5), Value::real(2)];
    let env = TestEnv::default();

    let r = floor_remainder(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v.as_datum().to_string(), "1");
}

#[test]
fn truncate_remainder_negative_dividend() {
    let args = [Value::real(-5), Value::real(2)];
    let env = TestEnv::default();

    let r = truncate_remainder(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v.as_datum().to_string(), "-1");
}

#[test]
fn truncate_qr_float_taint_dividend() {
    let args = [Value::real(-5.0), Value::real(-2)];
    let env = TestEnv::default();

    let r = truncate_qr(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v.as_datum().to_string(), "[2.0, -1.0]");
}

#[test]
fn floor_qr_float_taint_divisor() {
    let args = [Value::real(-5), Value::real(-2.0)];
    let env = TestEnv::default();

    let r = floor_qr(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v.as_datum().to_string(), "[2.0, -1.0]");
}

#[test]
fn floor_quotient_float_taint_dividend() {
    let args = [Value::real(7.0), Value::real(2)];
    let env = TestEnv::default();

    let r = floor_quotient(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_matches!(v, Value::Number(Number::Real(Real::Float(_))));
    assert_eq!(v.as_datum().to_string(), "3.0");
}

#[test]
fn truncate_remainder_float_taint_divisor() {
    let args = [Value::real(7), Value::real(2.0)];
    let env = TestEnv::default();

    let r = truncate_remainder(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_matches!(v, Value::Number(Number::Real(Real::Float(_))));
    assert_eq!(v.as_datum().to_string(), "1.0");
}

#[test]
fn floor_remainder_float_taint_both_args() {
    let args = [Value::real(7.0), Value::real(2.0)];
    let env = TestEnv::default();

    let r = floor_remainder(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_matches!(v, Value::Number(Number::Real(Real::Float(_))));
    assert_eq!(v.as_datum().to_string(), "1.0");
}

// Zero-dividend cases, checked against Chez Scheme 10 and Guile 3.0.
// Both give the quotient the IEEE sign of the division (sign(n) xor sign(d))
// while the remainder of a zero dividend is always +0.0 (not raw fmod's -0.0).

#[test]
fn floor_qr_exact_zero_dividend() {
    let args = [Value::real(0), Value::real(5)];
    let env = TestEnv::default();

    let r = floor_qr(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v.as_datum().to_string(), "[0, 0]");
}

#[test]
fn truncate_qr_exact_zero_dividend() {
    let args = [Value::real(0), Value::real(5)];
    let env = TestEnv::default();

    let r = truncate_qr(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v.as_datum().to_string(), "[0, 0]");
}

#[test]
fn floor_qr_exact_zero_dividend_negative_divisor() {
    let args = [Value::real(0), Value::real(-5)];
    let env = TestEnv::default();

    let r = floor_qr(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v.as_datum().to_string(), "[0, 0]");
}

#[test]
fn truncate_qr_exact_zero_dividend_negative_divisor() {
    let args = [Value::real(0), Value::real(-5)];
    let env = TestEnv::default();

    let r = truncate_qr(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v.as_datum().to_string(), "[0, 0]");
}

#[test]
fn floor_qr_positive_zero_dividend() {
    let args = [Value::real(0.0), Value::real(5)];
    let env = TestEnv::default();

    let r = floor_qr(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v.as_datum().to_string(), "[0.0, 0.0]");
}

#[test]
fn truncate_qr_positive_zero_dividend() {
    let args = [Value::real(0.0), Value::real(5)];
    let env = TestEnv::default();

    let r = truncate_qr(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v.as_datum().to_string(), "[0.0, 0.0]");
}

// Chez: (quotient -0.0 5) => -0.0; Guile: (quotient -0.0 5) => -0.0.
// Zara currently loses the dividend's zero sign in try_into_exact_integer
// and returns [0.0, 0.0].
#[test]
fn floor_qr_negative_zero_dividend() {
    let args = [Value::real(-0.0), Value::real(5)];
    let env = TestEnv::default();

    let r = floor_qr(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v.as_datum().to_string(), "[-0.0, 0.0]");
}

// Chez: (quotient -0.0 5) => -0.0; Guile: (quotient -0.0 5) => -0.0.
#[test]
fn truncate_qr_negative_zero_dividend() {
    let args = [Value::real(-0.0), Value::real(5)];
    let env = TestEnv::default();

    let r = truncate_qr(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v.as_datum().to_string(), "[-0.0, 0.0]");
}

// Chez: (quotient 0.0 -5) => -0.0; Guile: (quotient 0.0 -5) => -0.0.
#[test]
fn floor_qr_positive_zero_dividend_negative_divisor() {
    let args = [Value::real(0.0), Value::real(-5)];
    let env = TestEnv::default();

    let r = floor_qr(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v.as_datum().to_string(), "[-0.0, 0.0]");
}

// Chez: (quotient 0.0 -5) => -0.0; Guile: (quotient 0.0 -5) => -0.0.
#[test]
fn truncate_qr_positive_zero_dividend_negative_divisor() {
    let args = [Value::real(0.0), Value::real(-5)];
    let env = TestEnv::default();

    let r = truncate_qr(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v.as_datum().to_string(), "[-0.0, 0.0]");
}

// signs cancel here, so this already passes today; pins down that a fix
// for the negative-zero-dividend cases above must not over-correct this one.
#[test]
fn floor_qr_negative_zero_dividend_negative_divisor() {
    let args = [Value::real(-0.0), Value::real(-5)];
    let env = TestEnv::default();

    let r = floor_qr(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v.as_datum().to_string(), "[0.0, 0.0]");
}

#[test]
fn truncate_qr_negative_zero_dividend_negative_divisor() {
    let args = [Value::real(-0.0), Value::real(-5)];
    let env = TestEnv::default();

    let r = truncate_qr(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v.as_datum().to_string(), "[0.0, 0.0]");
}

// Chez: (quotient -0.0 5) => -0.0; Guile: (quotient -0.0 5) => -0.0.
#[test]
fn floor_quotient_negative_zero_dividend() {
    let args = [Value::real(-0.0), Value::real(5)];
    let env = TestEnv::default();

    let r = floor_quotient(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v.as_datum().to_string(), "-0.0");
}

// Chez: (quotient -0.0 5) => -0.0; Guile: (quotient -0.0 5) => -0.0.
#[test]
fn truncate_quotient_negative_zero_dividend() {
    let args = [Value::real(-0.0), Value::real(5)];
    let env = TestEnv::default();

    let r = truncate_quotient(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v.as_datum().to_string(), "-0.0");
}

// Chez and Guile both give (remainder -0.0 5) => 0.0 (not fmod's -0.0).
#[test]
fn floor_remainder_negative_zero_dividend() {
    let args = [Value::real(-0.0), Value::real(5)];
    let env = TestEnv::default();

    let r = floor_remainder(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v.as_datum().to_string(), "0.0");
}

#[test]
fn truncate_remainder_negative_zero_dividend() {
    let args = [Value::real(-0.0), Value::real(5)];
    let env = TestEnv::default();

    let r = truncate_remainder(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v.as_datum().to_string(), "0.0");
}

// Chez and Guile disagree here: Chez keeps an exact 0 result ((quotient 0 -5.0) => 0),
// Guile taints and signs it (-0.0). We follow Guile since Zara's float_taint
// convention already always taints on an inexact operand.
#[test]
fn floor_qr_exact_zero_dividend_inexact_divisor() {
    let args = [Value::real(0), Value::real(-5.0)];
    let env = TestEnv::default();

    let r = floor_qr(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v.as_datum().to_string(), "[-0.0, 0.0]");
}

#[test]
fn truncate_quotient_exact_zero_dividend_inexact_divisor() {
    let args = [Value::real(0), Value::real(5.0)];
    let env = TestEnv::default();

    let r = truncate_quotient(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v.as_datum().to_string(), "0.0");
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
