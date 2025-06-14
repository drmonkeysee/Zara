use super::*;
use crate::testutil::{err_or_fail, extract_or_fail, ok_or_fail};

macro_rules! rational_parts {
    ($real:expr) => {{
        let rat = extract_or_fail!($real, Real::Rational);
        *rat.0
    }};
}

mod sign {
    use super::*;

    #[test]
    fn values() {
        assert_eq!(Sign::Negative as i32, -1);
        assert_eq!(Sign::Zero as i32, 0);
        assert_eq!(Sign::Positive as i32, 1);
    }

    #[test]
    fn comparisons() {
        assert!(Sign::Negative == Sign::Negative);
        assert!(Sign::Negative != Sign::Zero);
        assert!(Sign::Negative != Sign::Positive);

        assert!(Sign::Zero == Sign::Zero);
        assert!(Sign::Zero != Sign::Positive);

        assert!(Sign::Positive == Sign::Positive);

        assert!(Sign::Negative < Sign::Zero);
        assert!(Sign::Negative <= Sign::Zero);
        assert!(Sign::Negative < Sign::Positive);
        assert!(Sign::Negative <= Sign::Positive);

        assert!(Sign::Zero < Sign::Positive);
        assert!(Sign::Zero <= Sign::Positive);
        assert!(Sign::Zero > Sign::Negative);
        assert!(Sign::Zero >= Sign::Negative);

        assert!(Sign::Positive > Sign::Negative);
        assert!(Sign::Positive >= Sign::Negative);
        assert!(Sign::Positive > Sign::Zero);
        assert!(Sign::Positive >= Sign::Zero);
    }

    #[test]
    fn from() {
        let cases = [(-10, Sign::Negative), (0, Sign::Zero), (10, Sign::Positive)];
        for (case, exp) in cases {
            let s = Sign::from(case);
            assert_eq!(s, exp);
        }
    }
}

mod token {
    use super::*;

    #[test]
    fn integer() {
        let n = Number::real(42);

        assert_eq!(n.as_token_descriptor().to_string(), "INT");
    }

    #[test]
    fn float() {
        let n = Number::real(4.2);

        assert_eq!(n.as_token_descriptor().to_string(), "FLT");
    }

    #[test]
    fn rational() {
        let n = Number::Real(ok_or_fail!(Real::reduce(4, 5)));

        assert_eq!(n.as_token_descriptor().to_string(), "RAT");
    }

    #[test]
    fn complex() {
        let n = Number::complex(3, 5);

        assert_eq!(n.as_token_descriptor().to_string(), "CPX");
    }
}

mod display {
    use super::*;

    #[test]
    fn zero() {
        let n = Number::real(0);

        assert_eq!(n.to_string(), "0");
    }

    #[test]
    fn negative_int_zero() {
        let n = Number::real(-0);

        assert_eq!(n.to_string(), "0");
    }

    #[test]
    fn positive_int() {
        let n = Number::real(23);

        assert_eq!(n.to_string(), "23");
    }

    #[test]
    fn negative_int() {
        let n = Number::real(-32);

        assert_eq!(n.to_string(), "-32");
    }

    #[test]
    fn int_max() {
        let n = Number::real(i64::MAX);

        assert_eq!(n.to_string(), "9223372036854775807");
    }

    #[test]
    fn int_min() {
        let n = Number::real(i64::MIN);

        assert_eq!(n.to_string(), "-9223372036854775808");
    }

    #[test]
    fn positive_zero() {
        let n = Number::real(0.0);

        assert_eq!(n.to_string(), "0.0");
    }

    #[test]
    fn negative_zero() {
        let n = Number::real(-0.0);

        assert_eq!(n.to_string(), "-0.0");
    }

    #[test]
    fn whole_float() {
        let n = Number::real(1.0);

        assert_eq!(n.to_string(), "1.0");
    }

    #[test]
    fn positive_float() {
        let n = Number::real(234.23);

        assert_eq!(n.to_string(), "234.23");
    }

    #[test]
    fn negative_float() {
        let n = Number::real(-789.34);

        assert_eq!(n.to_string(), "-789.34");
    }

    #[test]
    fn fractional_float() {
        let n = Number::real(0.0567);

        assert_eq!(n.to_string(), "0.0567");
    }

    #[test]
    fn with_trailing_zeros() {
        let n = Number::real(234.23000);

        assert_eq!(n.to_string(), "234.23");
    }

    #[test]
    fn large_exponent() {
        let n = Number::real(1e29);

        assert_eq!(n.to_string(), "1e29");
    }

    #[test]
    fn small_exponent() {
        let n = Number::real(1e-29);

        assert_eq!(n.to_string(), "1e-29");
    }

    #[test]
    fn rounding_error() {
        let n = Number::real(0.1 + 0.2);

        assert_eq!(n.to_string(), "0.30000000000000004");
    }

    #[test]
    fn max_float() {
        let n = Number::real(f64::MAX);

        assert_eq!(n.to_string(), "1.7976931348623157e308");
    }

    #[test]
    fn min_float() {
        let n = Number::real(f64::MIN);

        assert_eq!(n.to_string(), "-1.7976931348623157e308");
    }

    #[test]
    fn positive_min_float() {
        let n = Number::real(f64::MIN_POSITIVE);

        assert_eq!(n.to_string(), "2.2250738585072014e-308");
    }

    #[test]
    fn epsilon() {
        let n = Number::real(f64::EPSILON);

        assert_eq!(n.to_string(), "2.220446049250313e-16");
    }

    #[test]
    fn infinity() {
        let n = Number::real(f64::INFINITY);

        assert_eq!(n.to_string(), "+inf.0");
    }

    #[test]
    fn negative_infinity() {
        let n = Number::real(f64::NEG_INFINITY);

        assert_eq!(n.to_string(), "-inf.0");
    }

    #[test]
    fn nan() {
        let n = Number::real(f64::NAN);

        assert_eq!(n.to_string(), "+nan.0");
    }

    #[test]
    fn negative_nan() {
        let n = Number::real(-f64::NAN);

        // NOTE: sign is ignored for NAN
        assert_eq!(n.to_string(), "+nan.0");
    }

    #[test]
    fn positive_rational() {
        let n = Number::Real(ok_or_fail!(Real::reduce(3, 4)));

        assert_eq!(n.to_string(), "3/4");
    }

    #[test]
    fn negative_numerator() {
        let n = Number::Real(ok_or_fail!(Real::reduce(-3, 4)));

        assert_eq!(n.to_string(), "-3/4");
    }

    #[test]
    fn negative_denominator() {
        let n = Number::Real(ok_or_fail!(Real::reduce(3, -4)));

        assert_eq!(n.to_string(), "-3/4");
    }

    #[test]
    fn negative_numerator_and_denominator() {
        let n = Number::Real(ok_or_fail!(Real::reduce(-3, -4)));

        assert_eq!(n.to_string(), "3/4");
    }

    #[test]
    fn greater_than_one_rational() {
        let n = Number::Real(ok_or_fail!(Real::reduce(4, 3)));

        assert_eq!(n.to_string(), "4/3");
    }

    #[test]
    fn basic_complex() {
        let n = Number::complex(4, 5);

        assert_eq!(n.to_string(), "4+5i");
    }

    #[test]
    fn complex_negative_real() {
        let n = Number::complex(-4, 5);

        assert_eq!(n.to_string(), "-4+5i");
    }

    #[test]
    fn complex_negative_imag() {
        let n = Number::complex(4, -5);

        assert_eq!(n.to_string(), "4-5i");
    }

    #[test]
    fn complex_negative() {
        let n = Number::complex(-4, -5);

        assert_eq!(n.to_string(), "-4-5i");
    }

    #[test]
    fn complex_float() {
        let n = Number::complex(4.2, 5.3);

        assert_eq!(n.to_string(), "4.2+5.3i");
    }

    #[test]
    fn complex_rationals() {
        let r = ok_or_fail!(Real::reduce(3, 5));
        let i = ok_or_fail!(Real::reduce(5, 2));
        let n = Number::complex(r, i);

        assert_eq!(n.to_string(), "3/5+5/2i");
    }

    #[test]
    fn complex_negative_rat_real() {
        let r = ok_or_fail!(Real::reduce(-3, 5));
        let i = ok_or_fail!(Real::reduce(5, 2));
        let n = Number::complex(r, i);

        assert_eq!(n.to_string(), "-3/5+5/2i");
    }

    #[test]
    fn complex_negative_rat_imag() {
        let r = ok_or_fail!(Real::reduce(3, 5));
        let i = ok_or_fail!(Real::reduce(-5, 2));
        let n = Number::complex(r, i);

        assert_eq!(n.to_string(), "3/5-5/2i");
    }

    #[test]
    fn complex_negative_rat() {
        let r = ok_or_fail!(Real::reduce(-3, 5));
        let i = ok_or_fail!(Real::reduce(-5, 2));
        let n = Number::complex(r, i);

        assert_eq!(n.to_string(), "-3/5-5/2i");
    }

    #[test]
    fn complex_real_rat() {
        let r = ok_or_fail!(Real::reduce(3, 5));
        let n = Number::complex(r, 5);

        assert_eq!(n.to_string(), "3/5+5i");
    }

    #[test]
    fn complex_imag_rat() {
        let i = ok_or_fail!(Real::reduce(5, 2));
        let n = Number::complex(3, i);

        assert_eq!(n.to_string(), "3+5/2i");
    }

    #[test]
    fn complex_real_float_imag_rat() {
        let i = ok_or_fail!(Real::reduce(5, 2));
        let n = Number::complex(3.032, i);

        assert_eq!(n.to_string(), "3.032+5/2i");
    }

    #[test]
    fn complex_real_rat_imag_float() {
        let r = ok_or_fail!(Real::reduce(3, 5));
        let n = Number::complex(r, -6.34);

        assert_eq!(n.to_string(), "3/5-6.34i");
    }

    #[test]
    fn complex_zero_real() {
        let n = Number::complex(0, 5);

        assert_eq!(n.to_string(), "+5i");
    }

    #[test]
    fn complex_negative_zero_real() {
        let n = Number::complex(0, -5);

        assert_eq!(n.to_string(), "-5i");
    }

    #[test]
    fn complex_zero_imag() {
        let n = Number::complex(4, 0);

        assert_eq!(n.to_string(), "4");
    }

    #[test]
    fn complex_unity_imag() {
        let n = Number::complex(4, 1);

        assert_eq!(n.to_string(), "4+i");
    }

    #[test]
    fn complex_zero_real_unity_imag() {
        let n = Number::complex(0, 1);

        assert_eq!(n.to_string(), "+i");
    }

    #[test]
    fn complex_zero_real_negative_unity_imag() {
        let n = Number::complex(0, -1);

        assert_eq!(n.to_string(), "-i");
    }

    #[test]
    fn complex_zero_real_rat_unity_imag() {
        let i = ok_or_fail!(Real::reduce(5, 5));
        let n = Number::complex(0, i);

        assert_eq!(n.to_string(), "+i");
    }

    #[test]
    fn complex_inexact_unity_imag() {
        let n = Number::complex(4, 1.0);

        assert_eq!(n.to_string(), "4+1.0i");
    }

    #[test]
    fn complex_zero() {
        let n = Number::complex(0, 0);

        assert_eq!(n.to_string(), "0");
    }

    #[test]
    fn complex_type_name() {
        let n = Number::complex(1, 2);

        assert_eq!(n.as_typename().to_string(), "complex");
    }

    #[test]
    fn rational_type_name() {
        let n = Number::real(ok_or_fail!(Real::reduce(4, 5)));

        assert_eq!(n.as_typename().to_string(), "rational");
    }

    #[test]
    fn float_type_name() {
        let n = Number::real(1.2);

        assert_eq!(n.as_typename().to_string(), "floating-point");
    }

    #[test]
    fn integer_type_name() {
        let n = Number::real(5);

        assert_eq!(n.as_typename().to_string(), "integer");
    }
}

mod error {
    use super::*;

    #[test]
    fn display_div_by_zero() {
        let err = NumericError::DivideByZero;

        assert_eq!(err.to_string(), "divide by zero");
    }

    #[test]
    fn display_exp_out_of_range() {
        let err = NumericError::ParseExponentOutOfRange;

        assert_eq!(
            err.to_string(),
            "exponent out of range: [-2147483648, 2147483647]"
        );
    }

    #[test]
    fn display_exp_failure() {
        let err = NumericError::ParseExponentFailure;

        assert_eq!(err.to_string(), "exponent parse failure");
    }

    #[test]
    fn display_parse_failure() {
        let err = NumericError::ParseFailure;

        assert_eq!(err.to_string(), "number parse failure");
    }

    #[test]
    fn display_unimplemented() {
        let err = NumericError::Unimplemented("foo".to_owned());

        assert_eq!(err.to_string(), "unimplemented number parse: 'foo'");
    }

    #[test]
    fn display_byte_invalid_type() {
        let err = NumericError::ByteConversionInvalidType("foobar".to_owned());

        assert_eq!(
            err.to_string(),
            "expected integer literal, got numeric type: foobar"
        );
    }

    #[test]
    fn display_byte_out_of_range() {
        let err = NumericError::ByteConversionInvalidRange;

        assert_eq!(err.to_string(), "integer literal out of range: [0, 255]");
    }

    #[test]
    fn display_int_invalid_type() {
        let err = NumericError::IntConversionInvalidType("foobar".to_owned());

        assert_eq!(
            err.to_string(),
            "expected integer literal, got numeric type: foobar"
        );
    }

    #[test]
    fn display_int_out_of_range() {
        let err = NumericError::IntConversionInvalidRange;

        assert_eq!(
            err.to_string(),
            "integer literal out of range: [-2147483648, 2147483647]"
        );
    }
}

mod integer {
    use super::*;

    #[test]
    fn single_ctor_ignores_sign_for_zero() {
        let n = Integer::single(0, Sign::Positive);

        assert_eq!(n.sign, Sign::Zero);
    }

    #[test]
    fn positive() {
        let n = 42.into();
        let int = extract_or_fail!(n, Real::Integer);

        assert!(!int.is_zero());
        assert!(!int.is_negative());
        assert_eq!(extract_or_fail!(int.precision, Precision::Single), 42);
        assert_eq!(int.sign, Sign::Positive);
    }

    #[test]
    fn zero() {
        let n = 0.into();
        let int = extract_or_fail!(n, Real::Integer);

        assert!(int.is_zero());
        assert!(!int.is_negative());
        assert_eq!(extract_or_fail!(int.precision, Precision::Single), 0);
        assert_eq!(int.sign, Sign::Zero);
    }

    #[test]
    fn negative() {
        let n = (-42).into();
        let int = extract_or_fail!(n, Real::Integer);

        assert!(!int.is_zero());
        assert!(int.is_negative());
        assert_eq!(extract_or_fail!(int.precision, Precision::Single), 42);
        assert_eq!(int.sign, Sign::Negative);
    }

    #[test]
    fn max() {
        let n = i64::MAX.into();
        let int = extract_or_fail!(n, Real::Integer);

        assert_eq!(
            extract_or_fail!(int.precision, Precision::Single),
            9223372036854775807
        );
        assert_eq!(int.sign, Sign::Positive);
    }

    #[test]
    fn min() {
        let n = i64::MIN.into();
        let int = extract_or_fail!(n, Real::Integer);

        assert_eq!(
            extract_or_fail!(int.precision, Precision::Single),
            9223372036854775808
        );
        assert_eq!(int.sign, Sign::Negative);
    }

    #[test]
    fn umax() {
        let u = Integer::single(u64::MAX, Sign::Positive);
        let n = u.into();
        let int = extract_or_fail!(n, Real::Integer);

        assert_eq!(
            extract_or_fail!(int.precision, Precision::Single),
            18446744073709551615
        );
        assert_eq!(int.sign, Sign::Positive);
    }

    #[test]
    fn umin() {
        let u = Integer::single(u64::MAX, Sign::Negative);
        let n = u.into();
        let int = extract_or_fail!(n, Real::Integer);

        assert_eq!(
            extract_or_fail!(int.precision, Precision::Single),
            18446744073709551615
        );
        assert_eq!(int.sign, Sign::Negative);
    }

    #[test]
    fn is_mag_one() {
        let cases = [(-4, false), (-1, true), (0, false), (1, true), (4, false)];
        for (case, expected) in cases {
            let n = case.into();
            let int = extract_or_fail!(n, Real::Integer);

            assert_eq!(int.is_magnitude_one(), expected);
        }
    }

    #[test]
    fn make_positive() {
        let cases = [(-4, Sign::Positive), (0, Sign::Zero), (4, Sign::Positive)];
        for (case, expected) in cases {
            let n = case.into();
            let mut int = extract_or_fail!(n, Real::Integer);

            int.make_positive();

            assert_eq!(int.sign, expected);
        }
    }

    #[test]
    fn make_negative() {
        let cases = [(-4, Sign::Negative), (0, Sign::Zero), (4, Sign::Negative)];
        for (case, expected) in cases {
            let n = case.into();
            let mut int = extract_or_fail!(n, Real::Integer);

            int.make_negative();

            assert_eq!(int.sign, expected);
        }
    }

    #[test]
    fn single_into_float() {
        let n = Real::from(42);

        assert_eq!(n.into_float(), 42.0);
    }

    #[test]
    fn zero_into_float() {
        let n = Real::from(0);

        assert_eq!(n.into_float(), 0.0);
    }

    #[test]
    fn negative_into_float() {
        let n = Real::from(-42);

        assert_eq!(n.into_float(), -42.0);
    }

    #[test]
    fn imax_into_float() {
        let n = Real::from(i64::MAX);

        assert_eq!(n.into_float(), 9.223372036854776e18);
    }

    #[test]
    fn imin_into_float() {
        let n = Real::from(i64::MIN);

        assert_eq!(n.into_float(), -9.223372036854776e18);
    }

    #[test]
    fn umax_into_float() {
        let u = Integer::single(u64::MAX, Sign::Positive);
        let n = Real::from(u);

        assert_eq!(n.into_float(), 1.8446744073709552e19);
    }

    #[test]
    fn umin_into_float() {
        let u = Integer::single(u64::MAX, Sign::Negative);
        let n = Real::from(u);

        assert_eq!(n.into_float(), -1.8446744073709552e19);
    }

    #[test]
    fn single_into_inexact() {
        let n = Integer::single(42, Sign::Positive);

        let r = n.into_inexact();

        let f = extract_or_fail!(r, Real::Float);
        assert_eq!(f, 42.0);
    }

    #[test]
    fn single_into_exact() {
        let n = Real::Integer(Integer::single(42, Sign::Positive));

        let r = n.into_exact();

        let int = extract_or_fail!(r, Real::Integer);

        assert_eq!(extract_or_fail!(int.precision, Precision::Single), 42);
        assert_eq!(int.sign, Sign::Positive);
    }

    #[test]
    fn single_into_byte() {
        let n = Number::real(12);

        let r = n.try_into();

        let b: u8 = ok_or_fail!(r);
        assert_eq!(b, 12);
    }

    #[test]
    fn zero_into_byte() {
        let n = Number::real(0);

        let r = n.try_into();

        let b: u8 = ok_or_fail!(r);
        assert_eq!(b, 0);
    }

    #[test]
    fn max_into_byte() {
        let n = Number::real(255);

        let r = n.try_into();

        let b: u8 = ok_or_fail!(r);
        assert_eq!(b, 255);
    }

    #[test]
    fn negative_into_byte() {
        let n = Number::real(-12);

        let r: Result<u8, _> = n.try_into();

        let err = err_or_fail!(r);
        assert!(matches!(err, NumericError::ByteConversionInvalidRange));
    }

    #[test]
    fn too_large_into_byte() {
        let n = Number::real(256);

        let r: Result<u8, _> = n.try_into();

        let err = err_or_fail!(r);
        assert!(matches!(err, NumericError::ByteConversionInvalidRange));
    }

    #[test]
    fn multiple_into_byte() {
        let i = Integer {
            precision: Precision::Multiple([24].into()),
            sign: Sign::Positive,
        };

        let r = i.try_to_u8();

        let err = err_or_fail!(r);
        assert!(matches!(err, NumericError::ByteConversionInvalidRange));
    }

    #[test]
    fn single_into_int() {
        let n = Number::real(12);

        let r = n.try_into();

        let i: i32 = ok_or_fail!(r);
        assert_eq!(i, 12);
    }

    #[test]
    fn zero_into_int() {
        let n = Number::real(0);

        let r = n.try_into();

        let i: i32 = ok_or_fail!(r);
        assert_eq!(i, 0);
    }

    #[test]
    fn negative_into_int() {
        let n = Number::real(-12);

        let r = n.try_into();

        let i: i32 = ok_or_fail!(r);
        assert_eq!(i, -12);
    }

    #[test]
    fn min_into_int() {
        let n = Number::real(-2147483648);

        let r = n.try_into();

        let i: i32 = ok_or_fail!(r);
        assert_eq!(i, -2147483648);
    }

    #[test]
    fn max_into_int() {
        let n = Number::real(2147483647);

        let r = n.try_into();

        let i: i32 = ok_or_fail!(r);
        assert_eq!(i, 2147483647);
    }

    #[test]
    fn one_above_max_into_int() {
        let n = Number::real(2147483648);

        let r: Result<i32, _> = n.try_into();

        let err = err_or_fail!(r);
        assert!(matches!(err, NumericError::IntConversionInvalidRange));
    }

    #[test]
    fn one_below_min_into_int() {
        let n = Number::real(-2147483649);

        let r: Result<i32, _> = n.try_into();

        let err = err_or_fail!(r);
        assert!(matches!(err, NumericError::IntConversionInvalidRange));
    }

    #[test]
    fn max_u64_into_int() {
        let n = Number::real((Sign::Positive, 18446744073709551615));

        let r: Result<i32, _> = n.try_into();

        let err = err_or_fail!(r);
        assert!(matches!(err, NumericError::IntConversionInvalidRange));
    }

    #[test]
    fn max_negative_u64_into_int() {
        let n = Number::real((Sign::Negative, 18446744073709551615));

        let r: Result<i32, _> = n.try_into();

        let err = err_or_fail!(r);
        assert!(matches!(err, NumericError::IntConversionInvalidRange));
    }

    #[test]
    fn min_i64_into_int() {
        let n = Number::real((Sign::Negative, 9223372036854775808));

        let r: Result<i32, _> = n.try_into();

        let err = err_or_fail!(r);
        assert!(matches!(err, NumericError::IntConversionInvalidRange));
    }

    #[test]
    fn negative_max_i64_into_int() {
        let n = Number::real((Sign::Negative, 9223372036854775807));

        let r: Result<i32, _> = n.try_into();

        let err = err_or_fail!(r);
        assert!(matches!(err, NumericError::IntConversionInvalidRange));
    }

    #[test]
    fn multiple_into_int() {
        let i = Integer {
            precision: Precision::Multiple([24].into()),
            sign: Sign::Positive,
        };

        let r = i.try_to_i32();

        let err = err_or_fail!(r);
        assert!(matches!(err, NumericError::IntConversionInvalidRange));
    }

    #[test]
    fn is_integer() {
        let r = Real::Integer(12.into());

        assert!(r.is_integer());
    }

    #[test]
    fn is_rational() {
        let r = Real::Integer(12.into());

        assert!(r.is_rational());
    }

    #[test]
    fn not_is_inexact() {
        let r = Real::Integer(12.into());

        assert!(!r.is_inexact());
    }

    #[test]
    fn is_exact_zero() {
        let r = Real::Integer(0.into());

        assert!(r.is_exact_zero());
    }

    #[test]
    fn not_is_infinite() {
        let r = Real::Integer(0.into());

        assert!(!r.is_infinite());
    }

    #[test]
    fn not_is_nan() {
        let r = Real::Integer(0.into());

        assert!(!r.is_nan());
    }
}

mod float {
    use super::*;

    #[test]
    fn is_zero() {
        let f = Real::Float(0.0);

        assert!(f.is_zero());
    }

    #[test]
    fn epsilon_is_not_zero() {
        let f = Real::Float(f64::EPSILON);

        assert!(!f.is_zero());
    }

    #[test]
    fn into_float() {
        let n = Real::Float(1.5);

        assert_eq!(n.into_float(), 1.5);
    }

    #[test]
    fn into_inexact() {
        let n = Real::Float(1.5);

        let r = n.into_inexact();

        let f = extract_or_fail!(r, Real::Float);
        assert_eq!(f, 1.5);
    }

    #[test]
    fn into_exact() {
        let n = Real::Float(4.0);

        let n = n.into_exact();

        let int = extract_or_fail!(n, Real::Integer);
        assert_eq!(extract_or_fail!(int.precision, Precision::Single), 4);
        assert_eq!(int.sign, Sign::Positive);
    }

    #[test]
    fn into_exact_rational() {
        let n = Real::Float(1.5);

        let n = n.into_exact();

        let (num, den) = rational_parts!(n);
        assert_eq!(extract_or_fail!(num.precision, Precision::Single), 3);
        assert_eq!(num.sign, Sign::Positive);
        assert_eq!(extract_or_fail!(den.precision, Precision::Single), 2);
        assert_eq!(den.sign, Sign::Positive);
    }

    #[test]
    fn into_exact_zero() {
        let n = Real::Float(0.0);

        let n = n.into_exact();

        let int = extract_or_fail!(n, Real::Integer);
        assert_eq!(extract_or_fail!(int.precision, Precision::Single), 0);
        assert_eq!(int.sign, Sign::Zero);
    }

    #[test]
    fn into_exact_negative_zero() {
        let n = Real::Float(-0.0);

        let n = n.into_exact();

        let int = extract_or_fail!(n, Real::Integer);
        assert_eq!(extract_or_fail!(int.precision, Precision::Single), 0);
        assert_eq!(int.sign, Sign::Zero);
    }

    #[test]
    fn into_exact_exponent() {
        let n = Real::Float(4e2);

        let n = n.into_exact();

        let int = extract_or_fail!(n, Real::Integer);
        assert_eq!(extract_or_fail!(int.precision, Precision::Single), 400);
        assert_eq!(int.sign, Sign::Positive);
    }

    #[test]
    fn into_exact_fraction_exponent() {
        let n = Real::Float(4.2e3);

        let n = n.into_exact();

        let int = extract_or_fail!(n, Real::Integer);
        assert_eq!(extract_or_fail!(int.precision, Precision::Single), 4200);
        assert_eq!(int.sign, Sign::Positive);
    }

    #[test]
    fn into_exact_infinite() {
        let n = Real::Float(f64::INFINITY);

        let n = n.into_exact();

        let flt = extract_or_fail!(n, Real::Float);
        assert!(flt.is_infinite());
    }

    #[test]
    fn into_exact_nan() {
        let n = Real::Float(f64::NAN);

        let n = n.into_exact();

        let flt = extract_or_fail!(n, Real::Float);
        assert!(flt.is_nan());
    }

    #[test]
    fn round_trip() {
        let expected = 4.23452e-2;
        let n = Real::Float(expected);

        let r = n.into_exact();
        assert!(matches!(r, Real::Rational(_)));

        let f = r.into_inexact();
        assert_eq!(f.into_float(), expected);
    }

    #[test]
    fn float_into_byte() {
        let n = Number::real(1.2);

        let r: Result<u8, _> = n.try_into();

        let err = err_or_fail!(r);
        assert!(matches!(
            err,
            NumericError::ByteConversionInvalidType(s)
            if s == "floating-point"));
    }

    #[test]
    fn float_into_int() {
        let n = Number::real(1.2);

        let r: Result<i32, _> = n.try_into();

        let err = err_or_fail!(r);
        assert!(matches!(
            err,
            NumericError::IntConversionInvalidType(s)
            if s == "floating-point"));
    }

    #[test]
    fn not_is_integer() {
        let r = Real::Float(3.5);

        assert!(!r.is_integer());
    }

    #[test]
    fn is_integer_if_zero_fractional() {
        let r = Real::Float(3.0);

        assert!(r.is_integer());
    }

    #[test]
    fn not_is_integer_if_infinite() {
        let r = Real::Float(f64::INFINITY);

        assert!(!r.is_integer());
    }

    #[test]
    fn not_is_integer_if_negative_infinite() {
        let r = Real::Float(f64::NEG_INFINITY);

        assert!(!r.is_integer());
    }

    #[test]
    fn not_is_integer_if_nan() {
        let r = Real::Float(f64::NAN);

        assert!(!r.is_integer());
    }

    #[test]
    fn is_rational_if_finite() {
        let r = Real::Float(3.5);

        assert!(r.is_rational());
    }

    #[test]
    fn not_is_rational_if_infinite() {
        let r = Real::Float(f64::INFINITY);

        assert!(!r.is_rational());
    }

    #[test]
    fn not_is_rational_if_negative_infinite() {
        let r = Real::Float(f64::NEG_INFINITY);

        assert!(!r.is_rational());
    }

    #[test]
    fn not_is_rational_if_nan() {
        let r = Real::Float(f64::NAN);

        assert!(!r.is_rational());
    }

    #[test]
    fn is_inexact() {
        let r = Real::Float(3.5);

        assert!(r.is_inexact());
    }

    #[test]
    fn is_inexact_with_zero_fraction() {
        let r = Real::Float(3.0);

        assert!(r.is_inexact());
    }

    #[test]
    fn not_is_exact_zero() {
        let r = Real::Float(0.0);

        assert!(!r.is_exact_zero());
    }

    #[test]
    fn not_is_exact_negative_zero() {
        let r = Real::Float(-0.0);

        assert!(!r.is_exact_zero());
    }

    #[test]
    fn not_is_infinite() {
        let r = Real::Float(3.4);

        assert!(!r.is_infinite());
    }

    #[test]
    fn is_infinite() {
        let cases = [f64::INFINITY, f64::NEG_INFINITY];
        for case in cases {
            let r = Real::Float(case);

            assert!(r.is_infinite());
        }
    }

    #[test]
    fn not_is_nan() {
        let r = Real::Float(3.4);

        assert!(!r.is_nan());
    }

    #[test]
    fn is_nan() {
        let r = Real::Float(f64::NAN);

        assert!(r.is_nan());
    }
}

mod rational {
    use super::*;

    mod euclid {
        use super::*;

        #[test]
        fn zeros() {
            assert_eq!(gcd_euclidean(0, 0), 0);
        }

        #[test]
        fn numerator_zero() {
            assert_eq!(gcd_euclidean(0, 5), 5);
        }

        #[test]
        fn denominator_zero() {
            assert_eq!(gcd_euclidean(5, 0), 5);
        }

        #[test]
        fn reduce_below_zero() {
            assert_eq!(gcd_euclidean(6, 10), 2);
        }

        #[test]
        fn reduce_above_zero() {
            assert_eq!(gcd_euclidean(15, 10), 5);
        }

        #[test]
        fn reduce_equal() {
            assert_eq!(gcd_euclidean(7, 7), 7);
        }
    }

    #[test]
    fn positive() {
        let n = ok_or_fail!(Real::reduce(4, 5));
        let (num, den) = rational_parts!(n);

        assert_eq!(extract_or_fail!(num.precision, Precision::Single), 4);
        assert_eq!(num.sign, Sign::Positive);
        assert_eq!(extract_or_fail!(den.precision, Precision::Single), 5);
        assert_eq!(den.sign, Sign::Positive);
    }

    #[test]
    fn negative_numerator() {
        let n = ok_or_fail!(Real::reduce(-4, 5));
        let (num, den) = rational_parts!(n);

        assert_eq!(extract_or_fail!(num.precision, Precision::Single), 4);
        assert_eq!(num.sign, Sign::Negative);
        assert_eq!(extract_or_fail!(den.precision, Precision::Single), 5);
        assert_eq!(den.sign, Sign::Positive);
    }

    #[test]
    fn negative_denominator() {
        let n = ok_or_fail!(Real::reduce(4, -5));
        let (num, den) = rational_parts!(n);

        assert_eq!(extract_or_fail!(num.precision, Precision::Single), 4);
        assert_eq!(num.sign, Sign::Negative);
        assert_eq!(extract_or_fail!(den.precision, Precision::Single), 5);
        assert_eq!(den.sign, Sign::Positive);
    }

    #[test]
    fn negative_parts() {
        let n = ok_or_fail!(Real::reduce(-4, -5));
        let (num, den) = rational_parts!(n);

        assert_eq!(extract_or_fail!(num.precision, Precision::Single), 4);
        assert_eq!(num.sign, Sign::Positive);
        assert_eq!(extract_or_fail!(den.precision, Precision::Single), 5);
        assert_eq!(den.sign, Sign::Positive);
    }

    #[test]
    fn improper() {
        let n = ok_or_fail!(Real::reduce(5, 4));
        let (num, den) = rational_parts!(n);

        assert_eq!(extract_or_fail!(num.precision, Precision::Single), 5);
        assert_eq!(num.sign, Sign::Positive);
        assert_eq!(extract_or_fail!(den.precision, Precision::Single), 4);
        assert_eq!(den.sign, Sign::Positive);
    }

    #[test]
    fn gcd() {
        let n = ok_or_fail!(Real::reduce(4, 10));
        let (num, den) = rational_parts!(n);

        assert_eq!(extract_or_fail!(num.precision, Precision::Single), 2);
        assert_eq!(num.sign, Sign::Positive);
        assert_eq!(extract_or_fail!(den.precision, Precision::Single), 5);
        assert_eq!(den.sign, Sign::Positive);
    }

    #[test]
    fn gcd_negative() {
        let n = ok_or_fail!(Real::reduce(-4, 10));
        let (num, den) = rational_parts!(n);

        assert_eq!(extract_or_fail!(num.precision, Precision::Single), 2);
        assert_eq!(num.sign, Sign::Negative);
        assert_eq!(extract_or_fail!(den.precision, Precision::Single), 5);
        assert_eq!(den.sign, Sign::Positive);
    }

    #[test]
    fn unity() {
        let n = ok_or_fail!(Real::reduce(1, 1));
        let int = extract_or_fail!(n, Real::Integer);

        assert_eq!(extract_or_fail!(int.precision, Precision::Single), 1);
        assert_eq!(int.sign, Sign::Positive);
    }

    #[test]
    fn reduce_to_unity() {
        let n = ok_or_fail!(Real::reduce(7, 7));
        let int = extract_or_fail!(n, Real::Integer);

        assert_eq!(extract_or_fail!(int.precision, Precision::Single), 1);
        assert_eq!(int.sign, Sign::Positive);
    }

    #[test]
    fn reduce_to_negative_unity() {
        let n = ok_or_fail!(Real::reduce(-7, 7));
        let int = extract_or_fail!(n, Real::Integer);

        assert_eq!(extract_or_fail!(int.precision, Precision::Single), 1);
        assert_eq!(int.sign, Sign::Negative);
    }

    #[test]
    fn reduce_to_integer() {
        let n = ok_or_fail!(Real::reduce(20, 10));
        let int = extract_or_fail!(n, Real::Integer);

        assert_eq!(extract_or_fail!(int.precision, Precision::Single), 2);
        assert_eq!(int.sign, Sign::Positive);
    }

    #[test]
    fn reduce_to_negative_integer() {
        let n = ok_or_fail!(Real::reduce(-20, 10));
        let int = extract_or_fail!(n, Real::Integer);

        assert_eq!(extract_or_fail!(int.precision, Precision::Single), 2);
        assert_eq!(int.sign, Sign::Negative);
    }

    #[test]
    fn zero_numerator() {
        let n = ok_or_fail!(Real::reduce(0, 7));
        let int = extract_or_fail!(n, Real::Integer);

        assert_eq!(extract_or_fail!(int.precision, Precision::Single), 0);
        assert_eq!(int.sign, Sign::Zero);
    }

    #[test]
    fn unreduced_zero_numerator_is_zero() {
        let r = Rational((0.into(), 7.into()).into());

        assert!(r.is_zero());
    }

    #[test]
    fn zero_denominator() {
        let n = Real::reduce(1, 0);
        let err = err_or_fail!(n);

        assert!(matches!(err, NumericError::DivideByZero));
    }

    #[test]
    fn positive_into_float() {
        let r = ok_or_fail!(Real::reduce(4, 5));

        assert_eq!(r.into_float(), 0.8);
    }

    #[test]
    fn negative_numerator_into_float() {
        let r = ok_or_fail!(Real::reduce(-4, 5));

        assert_eq!(r.into_float(), -0.8);
    }

    #[test]
    fn negative_denominator_into_float() {
        let r = ok_or_fail!(Real::reduce(4, -5));

        assert_eq!(r.into_float(), -0.8);
    }

    #[test]
    fn negative_parts_into_float() {
        let r = ok_or_fail!(Real::reduce(-4, -5));

        assert_eq!(r.into_float(), 0.8);
    }

    #[test]
    fn unreduced_zero_into_float() {
        let r = Rational((0.into(), 7.into()).into());

        assert_eq!(r.into_float(), 0.0);
    }

    #[test]
    fn unreduced_div_by_zero_into_float() {
        let r = Rational((7.into(), 0.into()).into());

        assert_eq!(r.into_float(), f64::INFINITY);
    }

    #[test]
    fn unreduced_negative_div_by_zero_into_float() {
        let r = Rational(((-7).into(), 0.into()).into());

        assert_eq!(r.into_float(), f64::NEG_INFINITY);
    }

    #[test]
    fn positive_into_inexact() {
        let n = ok_or_fail!(Real::reduce(4, 5));

        let r = n.into_inexact();

        let f = extract_or_fail!(r, Real::Float);
        assert_eq!(f, 0.8);
    }

    #[test]
    fn positive_into_exact() {
        let n = ok_or_fail!(Real::reduce(4, 5));

        let (num, den) = rational_parts!(n.into_exact());

        assert_eq!(extract_or_fail!(num.precision, Precision::Single), 4);
        assert_eq!(num.sign, Sign::Positive);
        assert_eq!(extract_or_fail!(den.precision, Precision::Single), 5);
        assert_eq!(den.sign, Sign::Positive);
    }

    #[test]
    fn rational_into_byte() {
        let rat = Real::reduce(4, 5);

        let n = Number::real(ok_or_fail!(rat));

        let r: Result<u8, _> = n.try_into();

        let err = err_or_fail!(r);
        assert!(matches!(
            err,
            NumericError::ByteConversionInvalidType(s)
            if s == "rational"));
    }

    #[test]
    fn rational_into_int() {
        let rat = Real::reduce(4, 5);

        let n = Number::real(ok_or_fail!(rat));

        let r: Result<i32, _> = n.try_into();

        let err = err_or_fail!(r);
        assert!(matches!(
            err,
            NumericError::IntConversionInvalidType(s)
            if s == "rational"));
    }

    #[test]
    fn not_is_integer() {
        let rat = ok_or_fail!(Real::reduce(4, 5));

        assert!(!rat.is_integer());
    }

    #[test]
    fn reduce_is_integer() {
        let rat = ok_or_fail!(Real::reduce(8, 4));

        assert!(rat.is_integer());
    }

    #[test]
    fn is_rational() {
        let rat = ok_or_fail!(Real::reduce(4, 5));

        assert!(rat.is_rational());
    }

    #[test]
    fn not_is_inexact() {
        let rat = ok_or_fail!(Real::reduce(4, 5));

        assert!(!rat.is_inexact());
    }

    #[test]
    fn not_is_infinite() {
        let rat = ok_or_fail!(Real::reduce(4, 5));

        assert!(!rat.is_infinite());
    }

    #[test]
    fn not_is_nan() {
        let rat = ok_or_fail!(Real::reduce(4, 5));

        assert!(!rat.is_nan());
    }
}

mod complex {
    use super::*;

    #[test]
    fn basic() {
        let c = Number::complex(4, 3);

        let ri = extract_or_fail!(c, Number::Complex);
        let r = extract_or_fail!(ri.0.0, Real::Integer);
        assert!(!r.is_zero());
        assert_eq!(extract_or_fail!(r.precision, Precision::Single), 4);
        let i = extract_or_fail!(ri.0.1, Real::Integer);
        assert!(!i.is_zero());
        assert_eq!(extract_or_fail!(i.precision, Precision::Single), 3);
    }

    #[test]
    fn zero_imaginary_reduces_to_int() {
        let c = Number::complex(4, 0);

        let r = extract_or_fail!(c, Number::Real);
        let int = extract_or_fail!(r, Real::Integer);
        assert!(!int.is_zero());
        assert_eq!(extract_or_fail!(int.precision, Precision::Single), 4);
    }

    #[test]
    fn zero_real() {
        let c = Number::complex(0, 3);

        let ri = extract_or_fail!(c, Number::Complex);
        let r = extract_or_fail!(ri.0.0, Real::Integer);
        assert!(r.is_zero());
        assert_eq!(extract_or_fail!(r.precision, Precision::Single), 0);
        let i = extract_or_fail!(ri.0.1, Real::Integer);
        assert!(!i.is_zero());
        assert_eq!(extract_or_fail!(i.precision, Precision::Single), 3);
    }

    #[test]
    fn inexact_zero_imaginary_does_not_reduce() {
        let c = Number::complex(4, 0.0);

        let ri = extract_or_fail!(c, Number::Complex);
        let int = extract_or_fail!(ri.0.0, Real::Integer);
        assert!(!int.is_zero());
        assert_eq!(extract_or_fail!(int.precision, Precision::Single), 4);
        let flt = extract_or_fail!(ri.0.1, Real::Float);
        assert_eq!(0.0, flt);
    }

    #[test]
    fn imaginary_only() {
        let c = Number::imaginary(3);

        let ri = extract_or_fail!(c, Number::Complex);
        let r = extract_or_fail!(ri.0.0, Real::Integer);
        assert!(r.is_zero());
        assert_eq!(extract_or_fail!(r.precision, Precision::Single), 0);
        let i = extract_or_fail!(ri.0.1, Real::Integer);
        assert!(!i.is_zero());
        assert_eq!(extract_or_fail!(i.precision, Precision::Single), 3);
    }

    #[test]
    fn inexact_zero_real_does_not_reduce() {
        let c = Number::complex(0.0, 3);

        let ri = extract_or_fail!(c, Number::Complex);
        let flt = extract_or_fail!(ri.0.0, Real::Float);
        assert_eq!(0.0, flt);
        let int = extract_or_fail!(ri.0.1, Real::Integer);
        assert!(!int.is_zero());
        assert_eq!(extract_or_fail!(int.precision, Precision::Single), 3);
    }

    #[test]
    fn polar() {
        let c = Number::polar(4, 3);

        let ri = extract_or_fail!(c, Number::Complex);
        let r = extract_or_fail!(ri.0.0, Real::Float);
        assert_eq!(r, -3.9599699864017817);
        let i = extract_or_fail!(ri.0.1, Real::Float);
        assert_eq!(i, 0.5644800322394689);
    }

    #[test]
    fn polar_floats() {
        let c = Number::polar(4.0, 3.0);

        let ri = extract_or_fail!(c, Number::Complex);
        let r = extract_or_fail!(ri.0.0, Real::Float);
        assert_eq!(r, -3.9599699864017817);
        let i = extract_or_fail!(ri.0.1, Real::Float);
        assert_eq!(i, 0.5644800322394689);
    }

    #[test]
    fn negative_mag() {
        let c = Number::polar(-4, 3);

        let ri = extract_or_fail!(c, Number::Complex);
        let r = extract_or_fail!(ri.0.0, Real::Float);
        assert_eq!(r, 3.9599699864017817);
        let i = extract_or_fail!(ri.0.1, Real::Float);
        assert_eq!(i, -0.5644800322394689);
    }

    #[test]
    fn negative_angle() {
        let c = Number::polar(4, -3);

        let ri = extract_or_fail!(c, Number::Complex);
        let r = extract_or_fail!(ri.0.0, Real::Float);
        assert_eq!(r, -3.9599699864017817);
        let i = extract_or_fail!(ri.0.1, Real::Float);
        assert_eq!(i, -0.5644800322394689);
    }

    #[test]
    fn negatives() {
        let c = Number::polar(-4, -3);

        let ri = extract_or_fail!(c, Number::Complex);
        let r = extract_or_fail!(ri.0.0, Real::Float);
        assert_eq!(r, 3.9599699864017817);
        let i = extract_or_fail!(ri.0.1, Real::Float);
        assert_eq!(i, 0.5644800322394689);
    }

    #[test]
    fn zero_mag() {
        let c = Number::polar(0, 3);

        let r = extract_or_fail!(c, Number::Real);
        let int = extract_or_fail!(r, Real::Integer);
        assert!(int.is_zero());
        assert_eq!(extract_or_fail!(int.precision, Precision::Single), 0);
    }

    #[test]
    fn zero_mag_float_does_not_reduce() {
        let c = Number::polar(0.0, 3);

        let ri = extract_or_fail!(c, Number::Complex);
        let flt = extract_or_fail!(ri.0.0, Real::Float);
        assert_eq!(flt, 0.0);
        let flt = extract_or_fail!(ri.0.1, Real::Float);
        assert_eq!(flt, 0.0);
    }

    #[test]
    fn zero_angle() {
        let c = Number::polar(4, 0);

        let r = extract_or_fail!(c, Number::Real);
        let int = extract_or_fail!(r, Real::Integer);
        assert!(!int.is_zero());
        assert_eq!(extract_or_fail!(int.precision, Precision::Single), 4);
    }

    #[test]
    fn zero_angle_float_does_not_reduce() {
        let c = Number::polar(4, 0.0);

        let ri = extract_or_fail!(c, Number::Complex);
        let flt = extract_or_fail!(ri.0.0, Real::Float);
        assert_eq!(flt, 4.0);
        let flt = extract_or_fail!(ri.0.1, Real::Float);
        assert_eq!(flt, 0.0);
    }

    #[test]
    fn zero_angle_rational() {
        let m = ok_or_fail!(Real::reduce(4, 5));
        let rad = ok_or_fail!(Real::reduce(0, 2));
        let c = Number::polar(m, rad);

        let (num, den) = rational_parts!(extract_or_fail!(c, Number::Real));

        assert_eq!(extract_or_fail!(num.precision, Precision::Single), 4);
        assert_eq!(num.sign, Sign::Positive);
        assert_eq!(extract_or_fail!(den.precision, Precision::Single), 5);
        assert_eq!(den.sign, Sign::Positive);
    }

    #[test]
    fn into_exact() {
        let c = Number::complex(4.0, 3.0);

        let c = c.into_exact();

        let ri = extract_or_fail!(c, Number::Complex);
        let r = extract_or_fail!(ri.0.0, Real::Integer);
        assert!(!r.is_zero());
        assert_eq!(extract_or_fail!(r.precision, Precision::Single), 4);
        let i = extract_or_fail!(ri.0.1, Real::Integer);
        assert!(!i.is_zero());
        assert_eq!(extract_or_fail!(i.precision, Precision::Single), 3);
    }

    #[test]
    fn into_exact_rational() {
        let c = Number::complex(1.5, 0.8);

        let c = c.into_exact();

        let ri = extract_or_fail!(c, Number::Complex);
        let (num, den) = rational_parts!(ri.0.0);
        assert_eq!(extract_or_fail!(num.precision, Precision::Single), 3);
        assert_eq!(num.sign, Sign::Positive);
        assert_eq!(extract_or_fail!(den.precision, Precision::Single), 2);
        assert_eq!(den.sign, Sign::Positive);
        let (num, den) = rational_parts!(ri.0.1);
        assert_eq!(extract_or_fail!(num.precision, Precision::Single), 4);
        assert_eq!(num.sign, Sign::Positive);
        assert_eq!(extract_or_fail!(den.precision, Precision::Single), 5);
        assert_eq!(den.sign, Sign::Positive);
    }

    #[test]
    fn into_inexact() {
        let c = Number::complex(4, 3);

        let c = c.into_inexact();

        let ri = extract_or_fail!(c, Number::Complex);
        let r = extract_or_fail!(ri.0.0, Real::Float);
        assert_eq!(r, 4.0);
        let i = extract_or_fail!(ri.0.1, Real::Float);
        assert_eq!(i, 3.0);
    }

    #[test]
    fn complex_into_byte() {
        let n = Number::complex(4, 5);

        let r: Result<u8, _> = n.try_into();

        let err = err_or_fail!(r);
        assert!(matches!(
            err,
            NumericError::ByteConversionInvalidType(s)
            if s == "complex"));
    }

    #[test]
    fn complex_into_int() {
        let n = Number::complex(4, 5);

        let r: Result<i32, _> = n.try_into();

        let err = err_or_fail!(r);
        assert!(matches!(
            err,
            NumericError::IntConversionInvalidType(s)
            if s == "complex"));
    }

    #[test]
    fn inexact_parts_is_inexact() {
        let n = Number::complex(4.3, 5.6);

        assert!(n.is_inexact());
    }

    #[test]
    fn inexact_parts_with_zero_frac_is_inexact() {
        let n = Number::complex(4.0, 5.0);

        assert!(n.is_inexact());
    }

    #[test]
    fn not_inexact_parts_with_exact_parts() {
        let n = Number::complex(4, ok_or_fail!(Real::reduce(4, 5)));

        assert!(!n.is_inexact());
    }

    #[test]
    fn inexact_if_inexact_real() {
        let n = Number::complex(4.3, 6);

        assert!(n.is_inexact());
    }

    #[test]
    fn inexact_if_inexact_imag() {
        let n = Number::complex(4, 5.6);

        assert!(n.is_inexact());
    }

    #[test]
    fn not_is_infinite() {
        let r = Number::complex(4.5, 5.6);

        assert!(!r.is_infinite());
    }

    #[test]
    fn not_is_nan() {
        let r = Number::complex(4.5, 5.6);

        assert!(!r.is_nan());
    }

    #[test]
    fn is_infinite() {
        let cases = [
            (4.5, f64::INFINITY),
            (4.5, f64::NEG_INFINITY),
            (f64::INFINITY, 5.6),
            (f64::NEG_INFINITY, 5.6),
            (f64::INFINITY, f64::INFINITY),
            (f64::INFINITY, f64::NEG_INFINITY),
            (f64::NEG_INFINITY, f64::INFINITY),
            (f64::NEG_INFINITY, f64::NEG_INFINITY),
        ];
        for case in cases {
            let r = Number::complex(case.0, case.1);

            assert!(r.is_infinite());
        }
    }

    #[test]
    fn is_nan() {
        let cases = [(4.5, f64::NAN), (f64::NAN, 5.6), (f64::NAN, f64::NAN)];
        for case in cases {
            let r = Number::complex(case.0, case.1);

            assert!(r.is_nan());
        }
    }
}

mod specs {
    use super::*;

    #[test]
    fn int_empty() {
        let espec = IntSpec::<Decimal>::default();
        let ispec = espec.clone();

        assert!(espec.is_empty());

        let err = err_or_fail!(espec.into_exact("1234"));

        assert!(matches!(err, NumericError::ParseFailure));

        let err = err_or_fail!(ispec.into_inexact("1234"));

        assert!(matches!(err, NumericError::ParseFailure));
    }

    #[test]
    fn int_blank_string() {
        let espec = IntSpec::<Decimal> {
            magnitude: 1..3,
            ..Default::default()
        };
        let ispec = espec.clone();

        assert!(!espec.is_empty());

        let err = err_or_fail!(espec.into_exact(""));

        assert!(matches!(err, NumericError::ParseFailure));

        let err = err_or_fail!(ispec.into_inexact(""));

        assert!(matches!(err, NumericError::ParseFailure));
    }

    #[test]
    fn float_empty() {
        let espec = FloatSpec::default();
        let ispec = espec.clone();

        assert!(espec.is_empty());

        let err = err_or_fail!(espec.into_exact("1234.456e3"));

        assert!(matches!(err, NumericError::ParseFailure));

        let err = err_or_fail!(ispec.into_inexact("1234.456e3"));

        assert!(matches!(err, NumericError::ParseFailure));
    }

    #[test]
    fn float_blank_string() {
        let espec = FloatSpec {
            exponent: 4..5,
            fraction: 2..3,
            integral: IntSpec {
                magnitude: 0..1,
                ..Default::default()
            },
        };
        let ispec = espec.clone();

        assert!(!espec.is_empty());

        let err = err_or_fail!(espec.into_exact(""));

        assert!(matches!(err, NumericError::ParseExponentFailure));

        let err = err_or_fail!(ispec.into_inexact(""));

        assert!(matches!(err, NumericError::ParseFailure));
    }
}
