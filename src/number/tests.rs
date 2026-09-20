macro_rules! rational_parts {
    ($real:expr) => {{
        let q = extract_or_fail!($real, Real::Rational);
        *q.0
    }};
}

use super::*;
use crate::testutil::{err_or_fail, extract_or_fail, ok_or_fail};
use std::assert_matches;

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

    #[test]
    fn mul_matrix() {
        let cases = [
            (Sign::Negative, Sign::Negative, Sign::Positive),
            (Sign::Negative, Sign::Zero, Sign::Zero),
            (Sign::Negative, Sign::Positive, Sign::Negative),
            (Sign::Zero, Sign::Negative, Sign::Zero),
            (Sign::Zero, Sign::Zero, Sign::Zero),
            (Sign::Zero, Sign::Positive, Sign::Zero),
            (Sign::Positive, Sign::Negative, Sign::Negative),
            (Sign::Positive, Sign::Zero, Sign::Zero),
            (Sign::Positive, Sign::Positive, Sign::Positive),
        ];
        for (a, b, expected) in cases {
            assert_eq!(a * b, expected);
        }
    }

    #[test]
    fn mul_is_commutative() {
        let cases = [
            (Sign::Negative, Sign::Negative),
            (Sign::Negative, Sign::Zero),
            (Sign::Negative, Sign::Positive),
            (Sign::Zero, Sign::Zero),
            (Sign::Zero, Sign::Positive),
            (Sign::Positive, Sign::Positive),
        ];
        for (a, b) in cases {
            assert_eq!(a * b, b * a);
        }
    }

    #[test]
    fn flip_matrix() {
        let cases = [
            (Sign::Negative, Sign::Positive),
            (Sign::Zero, Sign::Zero),
            (Sign::Positive, Sign::Negative),
        ];
        for (s, expected) in cases {
            assert_eq!(s.flip(), expected);
        }
    }

    #[test]
    fn flip_is_an_involution() {
        let cases = [Sign::Negative, Sign::Zero, Sign::Positive];
        for s in cases {
            assert_eq!(s.flip().flip(), s);
        }
    }

    #[test]
    fn flip_negates_value() {
        let cases = [Sign::Negative, Sign::Zero, Sign::Positive];
        for s in cases {
            assert_eq!(s.flip() as i32, -(s as i32));
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
        let q = Number::Real(ok_or_fail!(Real::reduce(4, 5)));

        assert_eq!(q.as_token_descriptor().to_string(), "RAT");
    }

    #[test]
    fn complex() {
        let z = Number::complex(3, 5);

        assert_eq!(z.as_token_descriptor().to_string(), "CPX");
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

        // sign is ignored for NAN
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
        let z = Number::complex(4, 5);

        assert_eq!(z.to_string(), "4+5i");
    }

    #[test]
    fn complex_negative_real() {
        let z = Number::complex(-4, 5);

        assert_eq!(z.to_string(), "-4+5i");
    }

    #[test]
    fn complex_negative_imag() {
        let z = Number::complex(4, -5);

        assert_eq!(z.to_string(), "4-5i");
    }

    #[test]
    fn complex_negative() {
        let z = Number::complex(-4, -5);

        assert_eq!(z.to_string(), "-4-5i");
    }

    #[test]
    fn complex_float() {
        let z = Number::complex(4.2, 5.3);

        assert_eq!(z.to_string(), "4.2+5.3i");
    }

    #[test]
    fn complex_rationals() {
        let r = ok_or_fail!(Real::reduce(3, 5));
        let i = ok_or_fail!(Real::reduce(5, 2));
        let z = Number::complex(r, i);

        assert_eq!(z.to_string(), "3/5+5/2i");
    }

    #[test]
    fn complex_negative_rat_real() {
        let r = ok_or_fail!(Real::reduce(-3, 5));
        let i = ok_or_fail!(Real::reduce(5, 2));
        let z = Number::complex(r, i);

        assert_eq!(z.to_string(), "-3/5+5/2i");
    }

    #[test]
    fn complex_negative_rat_imag() {
        let r = ok_or_fail!(Real::reduce(3, 5));
        let i = ok_or_fail!(Real::reduce(-5, 2));
        let z = Number::complex(r, i);

        assert_eq!(z.to_string(), "3/5-5/2i");
    }

    #[test]
    fn complex_negative_rat() {
        let r = ok_or_fail!(Real::reduce(-3, 5));
        let i = ok_or_fail!(Real::reduce(-5, 2));
        let z = Number::complex(r, i);

        assert_eq!(z.to_string(), "-3/5-5/2i");
    }

    #[test]
    fn complex_real_rat() {
        let r = ok_or_fail!(Real::reduce(3, 5));
        let z = Number::complex(r, 5);

        assert_eq!(z.to_string(), "3/5+5i");
    }

    #[test]
    fn complex_imag_rat() {
        let i = ok_or_fail!(Real::reduce(5, 2));
        let z = Number::complex(3, i);

        assert_eq!(z.to_string(), "3+5/2i");
    }

    #[test]
    fn complex_real_float_imag_rat() {
        let i = ok_or_fail!(Real::reduce(5, 2));
        let z = Number::complex(3.032, i);

        assert_eq!(z.to_string(), "3.032+5/2i");
    }

    #[test]
    fn complex_real_rat_imag_float() {
        let r = ok_or_fail!(Real::reduce(3, 5));
        let z = Number::complex(r, -6.34);

        assert_eq!(z.to_string(), "3/5-6.34i");
    }

    #[test]
    fn complex_zero_real() {
        let z = Number::complex(0, 5);

        assert_eq!(z.to_string(), "+5i");
    }

    #[test]
    fn complex_negative_zero_real() {
        let z = Number::complex(0, -5);

        assert_eq!(z.to_string(), "-5i");
    }

    #[test]
    fn complex_zero_imag() {
        let z = Number::complex(4, 0);

        assert_eq!(z.to_string(), "4");
    }

    #[test]
    fn complex_unity_imag() {
        let z = Number::complex(4, 1);

        assert_eq!(z.to_string(), "4+i");
    }

    #[test]
    fn complex_zero_real_unity_imag() {
        let z = Number::complex(0, 1);

        assert_eq!(z.to_string(), "+i");
    }

    #[test]
    fn complex_zero_real_negative_unity_imag() {
        let z = Number::complex(0, -1);

        assert_eq!(z.to_string(), "-i");
    }

    #[test]
    fn complex_zero_real_rat_unity_imag() {
        let i = ok_or_fail!(Real::reduce(5, 5));
        let z = Number::complex(0, i);

        assert_eq!(z.to_string(), "+i");
    }

    #[test]
    fn complex_inexact_unity_imag() {
        let z = Number::complex(4, 1.0);

        assert_eq!(z.to_string(), "4+1.0i");
    }

    #[test]
    fn complex_zero() {
        let z = Number::complex(0, 0);

        assert_eq!(z.to_string(), "0");
    }

    #[test]
    fn complex_type_name() {
        let z = Number::complex(1, 2);

        assert_eq!(z.as_typename().to_string(), "complex");
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
    fn display_no_exact_representation() {
        let err = NumericError::NoExactRepresentation("+inf.0".to_owned());

        assert_eq!(err.to_string(), "no exact representation for: +inf.0");
    }

    #[test]
    fn display_not_exact_integer() {
        let err = NumericError::NotExactInteger("4.5".to_owned());

        assert_eq!(err.to_string(), "expected exact integer, got: 4.5");
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
        let err = NumericError::Int32ConversionInvalidRange;

        assert_eq!(
            err.to_string(),
            "integer literal out of range: [-2147483648, 2147483647]"
        );
    }

    #[test]
    fn display_uint_out_of_range() {
        let err = NumericError::Uint32ConversionInvalidRange;

        assert_eq!(
            err.to_string(),
            "integer literal out of range: [0, 4294967295]"
        );
    }

    #[test]
    fn display_usize_out_of_range() {
        let err = NumericError::UsizeConversionInvalidRange;

        assert_eq!(
            err.to_string(),
            "integer literal out of range: [0, 18446744073709551615]"
        );
    }
}

mod integer {
    use super::*;

    #[test]
    fn single_ctor_ignores_sign_for_zero() {
        let n = Integer::new(0, Sign::Positive);

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
        let u = Integer::new(u64::MAX, Sign::Positive);
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
        let u = Integer::new(u64::MAX, Sign::Negative);
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

        assert_eq!(n.to_float(), 42.0);
    }

    #[test]
    fn zero_into_float() {
        let n = Real::from(0);

        assert_eq!(n.to_float(), 0.0);
    }

    #[test]
    fn negative_into_float() {
        let n = Real::from(-42);

        assert_eq!(n.to_float(), -42.0);
    }

    #[test]
    fn imax_into_float() {
        let n = Real::from(i64::MAX);

        assert_eq!(n.to_float(), 9.223372036854776e18);
    }

    #[test]
    fn imin_into_float() {
        let n = Real::from(i64::MIN);

        assert_eq!(n.to_float(), -9.223372036854776e18);
    }

    #[test]
    fn umax_into_float() {
        let u = Integer::new(u64::MAX, Sign::Positive);
        let n = Real::from(u);

        assert_eq!(n.to_float(), 1.8446744073709552e19);
    }

    #[test]
    fn umin_into_float() {
        let u = Integer::new(u64::MAX, Sign::Negative);
        let n = Real::from(u);

        assert_eq!(n.to_float(), -1.8446744073709552e19);
    }

    #[test]
    fn single_into_inexact() {
        let n = Integer::new(42, Sign::Positive);

        let r = n.into_inexact();

        let f = extract_or_fail!(r, Real::Float);
        assert_eq!(f, 42.0);
    }

    #[test]
    fn single_into_exact() {
        let n = Real::Integer(Integer::new(42, Sign::Positive));

        let r = n.try_into_exact();

        let int = extract_or_fail!(ok_or_fail!(r), Real::Integer);

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
        assert_matches!(err, NumericError::ByteConversionInvalidRange);
    }

    #[test]
    fn too_large_into_byte() {
        let n = Number::real(256);

        let r: Result<u8, _> = n.try_into();

        let err = err_or_fail!(r);
        assert_matches!(err, NumericError::ByteConversionInvalidRange);
    }

    #[test]
    fn multiple_into_byte() {
        let i = Integer {
            precision: Precision::Multiple([24].into()),
            sign: Sign::Positive,
        };

        let r = i.try_to_u8();

        let err = err_or_fail!(r);
        assert_matches!(err, NumericError::ByteConversionInvalidRange);
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
        assert_matches!(err, NumericError::Int32ConversionInvalidRange);
    }

    #[test]
    fn one_below_min_into_int() {
        let n = Number::real(-2147483649);

        let r: Result<i32, _> = n.try_into();

        let err = err_or_fail!(r);
        assert_matches!(err, NumericError::Int32ConversionInvalidRange);
    }

    #[test]
    fn max_u64_into_int() {
        let n = Number::real((Sign::Positive, 18446744073709551615));

        let r: Result<i32, _> = n.try_into();

        let err = err_or_fail!(r);
        assert_matches!(err, NumericError::Int32ConversionInvalidRange);
    }

    #[test]
    fn max_negative_u64_into_int() {
        let n = Number::real((Sign::Negative, 18446744073709551615));

        let r: Result<i32, _> = n.try_into();

        let err = err_or_fail!(r);
        assert_matches!(err, NumericError::Int32ConversionInvalidRange);
    }

    #[test]
    fn min_i64_into_int() {
        let n = Number::real(-9223372036854775808);

        let r: Result<i32, _> = n.try_into();

        let err = err_or_fail!(r);
        assert_matches!(err, NumericError::Int32ConversionInvalidRange);
    }

    #[test]
    fn negative_max_i64_into_int() {
        let n = Number::real(-9223372036854775807);

        let r: Result<i32, _> = n.try_into();

        let err = err_or_fail!(r);
        assert_matches!(err, NumericError::Int32ConversionInvalidRange);
    }

    #[test]
    fn multiple_into_int() {
        let i = Integer {
            precision: Precision::Multiple([24].into()),
            sign: Sign::Positive,
        };

        let r = i.try_to_i32();

        let err = err_or_fail!(r);
        assert_matches!(err, NumericError::Int32ConversionInvalidRange);
    }

    #[test]
    fn single_into_int64() {
        let n = Number::real(12);

        let r = n.try_into();

        let i: i64 = ok_or_fail!(r);
        assert_eq!(i, 12);
    }

    #[test]
    fn zero_into_int64() {
        let n = Number::real(0);

        let r = n.try_into();

        let i: i64 = ok_or_fail!(r);
        assert_eq!(i, 0);
    }

    #[test]
    fn negative_into_int64() {
        let n = Number::real(-12);

        let r = n.try_into();

        let i: i64 = ok_or_fail!(r);
        assert_eq!(i, -12);
    }

    #[test]
    fn min_into_int64() {
        let n = Number::real(-9223372036854775808);

        let r = n.try_into();

        let i: i64 = ok_or_fail!(r);
        assert_eq!(i, -9223372036854775808);
    }

    #[test]
    fn max_into_int64() {
        let n = Number::real(9223372036854775807);

        let r = n.try_into();

        let i: i64 = ok_or_fail!(r);
        assert_eq!(i, 9223372036854775807);
    }

    #[test]
    fn one_above_max_into_int64() {
        let n = Number::real((Sign::Positive, 9223372036854775808));

        let r: Result<i64, _> = n.try_into();

        let err = err_or_fail!(r);
        assert_matches!(err, NumericError::Int64ConversionInvalidRange);
    }

    #[test]
    fn one_below_min_into_int64() {
        let n = Number::real((Sign::Negative, 9223372036854775809));

        let r: Result<i64, _> = n.try_into();

        let err = err_or_fail!(r);
        assert_matches!(err, NumericError::Int64ConversionInvalidRange);
    }

    #[test]
    fn max_u64_into_int64() {
        let n = Number::real((Sign::Positive, 18446744073709551615));

        let r: Result<i64, _> = n.try_into();

        let err = err_or_fail!(r);
        assert_matches!(err, NumericError::Int64ConversionInvalidRange);
    }

    #[test]
    fn max_negative_u64_into_int64() {
        let n = Number::real((Sign::Negative, 18446744073709551615));

        let r: Result<i64, _> = n.try_into();

        let err = err_or_fail!(r);
        assert_matches!(err, NumericError::Int64ConversionInvalidRange);
    }

    #[test]
    fn multiple_into_int64() {
        let i = Integer {
            precision: Precision::Multiple([24].into()),
            sign: Sign::Positive,
        };

        let r = i.try_to_i64();

        let err = err_or_fail!(r);
        assert_matches!(err, NumericError::Int64ConversionInvalidRange);
    }

    #[test]
    fn single_into_uint() {
        let n = Number::real(12);

        let r = n.try_into();

        let u: u32 = ok_or_fail!(r);
        assert_eq!(u, 12);
    }

    #[test]
    fn zero_into_uint() {
        let n = Number::real(0);

        let r = n.try_into();

        let u: u32 = ok_or_fail!(r);
        assert_eq!(u, 0);
    }

    #[test]
    fn negative_into_uint() {
        let n = Number::real(-12);

        let r: Result<u32, _> = n.try_into();

        let err = err_or_fail!(r);
        assert_matches!(err, NumericError::Uint32ConversionInvalidRange);
    }

    #[test]
    fn max_into_uint() {
        let n = Number::real(4294967295);

        let r = n.try_into();

        let u: u32 = ok_or_fail!(r);
        assert_eq!(u, 4294967295);
    }

    #[test]
    fn one_above_max_into_uint() {
        let n = Number::real(4294967296);

        let r: Result<u32, _> = n.try_into();

        let err = err_or_fail!(r);
        assert_matches!(err, NumericError::Uint32ConversionInvalidRange);
    }

    #[test]
    fn max_u64_into_uint() {
        let n = Number::real((Sign::Positive, 18446744073709551615));

        let r: Result<u32, _> = n.try_into();

        let err = err_or_fail!(r);
        assert_matches!(err, NumericError::Uint32ConversionInvalidRange);
    }

    #[test]
    fn max_negative_u64_into_uint() {
        let n = Number::real((Sign::Negative, 18446744073709551615));

        let r: Result<u32, _> = n.try_into();

        let err = err_or_fail!(r);
        assert_matches!(err, NumericError::Uint32ConversionInvalidRange);
    }

    #[test]
    fn min_i64_into_uint() {
        let n = Number::real((Sign::Negative, 9223372036854775808));

        let r: Result<u32, _> = n.try_into();

        let err = err_or_fail!(r);
        assert_matches!(err, NumericError::Uint32ConversionInvalidRange);
    }

    #[test]
    fn negative_max_i64_into_uint() {
        let n = Number::real((Sign::Negative, 9223372036854775807));

        let r: Result<u32, _> = n.try_into();

        let err = err_or_fail!(r);
        assert_matches!(err, NumericError::Uint32ConversionInvalidRange);
    }

    #[test]
    fn multiple_into_uint() {
        let i = Integer {
            precision: Precision::Multiple([24].into()),
            sign: Sign::Positive,
        };

        let r = i.try_to_u32();

        let err = err_or_fail!(r);
        assert_matches!(err, NumericError::Uint32ConversionInvalidRange);
    }

    #[test]
    fn single_into_usize() {
        let n = Number::real(12);

        let r = n.try_into();

        let u: usize = ok_or_fail!(r);
        assert_eq!(u, 12);
    }

    #[test]
    fn zero_into_usize() {
        let n = Number::real(0);

        let r = n.try_into();

        let u: usize = ok_or_fail!(r);
        assert_eq!(u, 0);
    }

    #[test]
    fn negative_into_usize() {
        let n = Number::real(-12);

        let r: Result<usize, _> = n.try_into();

        let err = err_or_fail!(r);
        assert_matches!(err, NumericError::UsizeConversionInvalidRange);
    }

    #[test]
    fn max_into_usize() {
        let n = Number::real((Sign::Positive, 18446744073709551615));

        let r = n.try_into();

        let u: usize = ok_or_fail!(r);
        assert_eq!(u, 18446744073709551615);
    }

    #[test]
    fn max_negative_into_usize() {
        let n = Number::real((Sign::Negative, 18446744073709551615));

        let r: Result<usize, _> = n.try_into();

        let err = err_or_fail!(r);
        assert_matches!(err, NumericError::UsizeConversionInvalidRange);
    }

    #[test]
    fn multiple_into_usize() {
        let i = Integer {
            precision: Precision::Multiple([24].into()),
            sign: Sign::Positive,
        };

        let r = i.try_to_usize();

        let err = err_or_fail!(r);
        assert_matches!(err, NumericError::UsizeConversionInvalidRange);
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

    #[test]
    fn is_positive() {
        let r = Real::Integer(4.into());

        assert!(r.is_positive());
    }

    #[test]
    fn zero_is_not_positive() {
        let r = Real::Integer(0.into());

        assert!(!r.is_positive());
    }

    #[test]
    fn negative_is_not_positive() {
        let r = Real::Integer((-4).into());

        assert!(!r.is_positive());
    }

    #[test]
    fn is_negative() {
        let r = Real::Integer((-4).into());

        assert!(r.is_negative());
    }

    #[test]
    fn zero_is_not_negative() {
        let r = Real::Integer(0.into());

        assert!(!r.is_negative());
    }

    #[test]
    fn positive_is_not_negative() {
        let r = Real::Integer(4.into());

        assert!(!r.is_negative());
    }

    #[test]
    fn is_even() {
        let cases = [4, 0, -4];
        for case in cases {
            let n = Integer::from(case);

            assert!(n.is_even())
        }
    }

    #[test]
    fn is_odd() {
        let cases = [3, -3];
        for case in cases {
            let n = Integer::from(case);

            assert!(!n.is_even())
        }
    }

    #[test]
    fn positive_try_into_exact_integer() {
        let r = Real::Integer(4.into());

        let res = r.try_into_exact_integer();

        let n = ok_or_fail!(res);
        assert_eq!(extract_or_fail!(n.precision, Precision::Single), 4);
        assert_eq!(n.sign, Sign::Positive);
    }

    #[test]
    fn zero_try_into_exact_integer() {
        let r = Real::Integer(0.into());

        let res = r.try_into_exact_integer();

        let n = ok_or_fail!(res);
        assert_eq!(extract_or_fail!(n.precision, Precision::Single), 0);
        assert_eq!(n.sign, Sign::Zero);
    }

    #[test]
    fn negative_try_into_exact_integer() {
        let r = Real::Integer((-4).into());

        let res = r.try_into_exact_integer();

        let n = ok_or_fail!(res);
        assert_eq!(extract_or_fail!(n.precision, Precision::Single), 4);
        assert_eq!(n.sign, Sign::Negative);
    }

    #[test]
    fn from_usize() {
        let n = Integer::from_usize(5_usize);

        assert_eq!(extract_or_fail!(n.precision, Precision::Single), 5);
        assert_eq!(n.sign, Sign::Positive);
    }

    #[test]
    fn from_usize_min() {
        let n = Integer::from_usize(usize::MIN);

        assert_eq!(extract_or_fail!(n.precision, Precision::Single), 0);
        assert_eq!(n.sign, Sign::Zero);
    }

    #[test]
    fn from_usize_max() {
        let n = Integer::from_usize(usize::MAX);

        assert_eq!(
            extract_or_fail!(n.precision, Precision::Single),
            18446744073709551615
        );
        assert_eq!(n.sign, Sign::Positive);
    }

    #[test]
    fn from_u64() {
        let x = Number::from_u64(5_u64);

        let n = extract_or_fail!(extract_or_fail!(x, Number::Real), Real::Integer);
        assert_eq!(extract_or_fail!(n.precision, Precision::Single), 5);
        assert_eq!(n.sign, Sign::Positive);
    }

    #[test]
    fn from_u64_min() {
        let x = Number::from_u64(u64::MIN);

        let n = extract_or_fail!(extract_or_fail!(x, Number::Real), Real::Integer);
        assert_eq!(extract_or_fail!(n.precision, Precision::Single), 0);
        assert_eq!(n.sign, Sign::Zero);
    }

    #[test]
    fn from_u64_max() {
        let x = Number::from_u64(u64::MAX);

        let n = extract_or_fail!(extract_or_fail!(x, Number::Real), Real::Integer);
        assert_eq!(
            extract_or_fail!(n.precision, Precision::Single),
            18446744073709551615
        );
        assert_eq!(n.sign, Sign::Positive);
    }

    #[test]
    fn negative_abs() {
        let n = Integer::from(-4);

        let a = n.into_abs();

        assert_eq!(a.sign, Sign::Positive);
    }

    #[test]
    fn positive_abs() {
        let n = Integer::from(4);

        let a = n.into_abs();

        assert_eq!(a.sign, Sign::Positive);
    }

    #[test]
    fn zero_abs() {
        let n = Integer::from(0);

        let a = n.into_abs();

        assert_eq!(a.sign, Sign::Zero);
    }

    #[test]
    fn positive_into_numerator() {
        let r = Real::Integer(4.into());

        let num = ok_or_fail!(r.try_into_numerator());

        let n = extract_or_fail!(num, Real::Integer);
        assert_eq!(extract_or_fail!(n.precision, Precision::Single), 4);
        assert_eq!(n.sign, Sign::Positive);
    }

    #[test]
    fn negative_into_numerator() {
        let r = Real::Integer((-4).into());

        let num = ok_or_fail!(r.try_into_numerator());

        let n = extract_or_fail!(num, Real::Integer);
        assert_eq!(extract_or_fail!(n.precision, Precision::Single), 4);
        assert_eq!(n.sign, Sign::Negative);
    }

    #[test]
    fn positive_into_denominator() {
        let r = Real::Integer(4.into());

        let denom = ok_or_fail!(r.try_into_denominator());

        let n = extract_or_fail!(denom, Real::Integer);
        assert_eq!(extract_or_fail!(n.precision, Precision::Single), 1);
        assert_eq!(n.sign, Sign::Positive);
    }

    #[test]
    fn negative_into_denominator() {
        let r = Real::Integer((-4).into());

        let denom = ok_or_fail!(r.try_into_denominator());

        let n = extract_or_fail!(denom, Real::Integer);
        assert_eq!(extract_or_fail!(n.precision, Precision::Single), 1);
        assert_eq!(n.sign, Sign::Positive);
    }

    #[test]
    fn zero_into_denominator() {
        let r = Real::Integer(0.into());

        let denom = ok_or_fail!(r.try_into_denominator());

        let n = extract_or_fail!(denom, Real::Integer);
        assert_eq!(extract_or_fail!(n.precision, Precision::Single), 1);
        assert_eq!(n.sign, Sign::Positive);
    }

    mod gcd {
        use super::*;

        #[test]
        fn common_divisor() {
            let n = Integer::from(32).gcd(&36.into());

            assert_eq!(extract_or_fail!(n.precision, Precision::Single), 4);
            assert_eq!(n.sign, Sign::Positive);
        }

        #[test]
        fn coprime() {
            let n = Integer::from(9).gcd(&28.into());

            assert_eq!(extract_or_fail!(n.precision, Precision::Single), 1);
            assert_eq!(n.sign, Sign::Positive);
        }

        #[test]
        fn one_divides_the_other() {
            let n = Integer::from(6).gcd(&18.into());

            assert_eq!(extract_or_fail!(n.precision, Precision::Single), 6);
            assert_eq!(n.sign, Sign::Positive);
        }

        #[test]
        fn equal_operands() {
            let n = Integer::from(7).gcd(&7.into());

            assert_eq!(extract_or_fail!(n.precision, Precision::Single), 7);
            assert_eq!(n.sign, Sign::Positive);
        }

        #[test]
        fn zero_and_positive() {
            let n = Integer::from(0).gcd(&5.into());

            assert_eq!(extract_or_fail!(n.precision, Precision::Single), 5);
            assert_eq!(n.sign, Sign::Positive);
        }

        #[test]
        fn positive_and_zero() {
            let n = Integer::from(5).gcd(&0.into());

            assert_eq!(extract_or_fail!(n.precision, Precision::Single), 5);
            assert_eq!(n.sign, Sign::Positive);
        }

        #[test]
        fn both_zero() {
            let n = Integer::from(0).gcd(&0.into());

            assert_eq!(extract_or_fail!(n.precision, Precision::Single), 0);
            assert_eq!(n.sign, Sign::Zero);
        }

        #[test]
        fn zero_and_negative() {
            let n = Integer::from(0).gcd(&(-5).into());

            assert_eq!(extract_or_fail!(n.precision, Precision::Single), 5);
            assert_eq!(n.sign, Sign::Positive);
        }

        #[test]
        fn negative_first_operand() {
            let n = Integer::from(-32).gcd(&36.into());

            assert_eq!(extract_or_fail!(n.precision, Precision::Single), 4);
            assert_eq!(n.sign, Sign::Positive);
        }

        #[test]
        fn negative_second_operand() {
            let n = Integer::from(32).gcd(&(-36).into());

            assert_eq!(extract_or_fail!(n.precision, Precision::Single), 4);
            assert_eq!(n.sign, Sign::Positive);
        }

        #[test]
        fn both_negative() {
            let n = Integer::from(-32).gcd(&(-36).into());

            assert_eq!(extract_or_fail!(n.precision, Precision::Single), 4);
            assert_eq!(n.sign, Sign::Positive);
        }

        #[test]
        fn commutative() {
            let cases = [(32, 36), (9, 28), (0, 5), (7, 7), (-32, 36)];
            for (a, b) in cases {
                let x = Integer::from(a).gcd(&b.into());
                let y = Integer::from(b).gcd(&a.into());

                assert_eq!(x, y);
            }
        }

        #[test]
        fn unit_operand() {
            let cases = [-32, 0, 1, 32];
            for case in cases {
                let n = Integer::from(case).gcd(&1.into());

                assert_eq!(extract_or_fail!(n.precision, Precision::Single), 1);
                assert_eq!(n.sign, Sign::Positive);
            }
        }

        #[test]
        fn beyond_i64_magnitude() {
            let n = Integer::from(i64::MIN).gcd(&2.into());

            assert_eq!(extract_or_fail!(n.precision, Precision::Single), 2);
            assert_eq!(n.sign, Sign::Positive);
        }

        #[test]
        fn large_u64_operands() {
            let a = Integer::new(u64::MAX, Sign::Positive);
            let b = Integer::new(3, Sign::Positive);

            let n = a.gcd(&b);

            assert_eq!(extract_or_fail!(n.precision, Precision::Single), 3);
            assert_eq!(n.sign, Sign::Positive);
        }

        #[test]
        #[ignore = "multi-precision gcd not yet implemented"]
        fn multi_precision() {
            let a = Integer {
                precision: Precision::Multiple([4, 6].into()),
                sign: Sign::Positive,
            };
            let b = Integer::from(4);

            let n = a.gcd(&b);

            assert_eq!(extract_or_fail!(n.precision, Precision::Single), 4);
            assert_eq!(n.sign, Sign::Positive);
        }
    }

    mod lcm {
        use super::*;

        #[test]
        fn common_multiple() {
            let n = Integer::from(4).lcm(&6.into());

            assert_eq!(extract_or_fail!(n.precision, Precision::Single), 12);
            assert_eq!(n.sign, Sign::Positive);
        }

        #[test]
        fn coprime_operands_multiply() {
            let n = Integer::from(9).lcm(&28.into());

            assert_eq!(extract_or_fail!(n.precision, Precision::Single), 252);
            assert_eq!(n.sign, Sign::Positive);
        }

        #[test]
        fn one_divides_the_other() {
            let n = Integer::from(6).lcm(&18.into());

            assert_eq!(extract_or_fail!(n.precision, Precision::Single), 18);
            assert_eq!(n.sign, Sign::Positive);
        }

        #[test]
        fn equal_operands() {
            let n = Integer::from(7).lcm(&7.into());

            assert_eq!(extract_or_fail!(n.precision, Precision::Single), 7);
            assert_eq!(n.sign, Sign::Positive);
        }

        #[test]
        fn zero_and_positive() {
            let n = Integer::from(0).lcm(&5.into());

            assert_eq!(extract_or_fail!(n.precision, Precision::Single), 0);
            assert_eq!(n.sign, Sign::Zero);
        }

        #[test]
        fn positive_and_zero() {
            let n = Integer::from(5).lcm(&0.into());

            assert_eq!(extract_or_fail!(n.precision, Precision::Single), 0);
            assert_eq!(n.sign, Sign::Zero);
        }

        #[test]
        fn both_zero() {
            let n = Integer::from(0).lcm(&0.into());

            assert_eq!(extract_or_fail!(n.precision, Precision::Single), 0);
            assert_eq!(n.sign, Sign::Zero);
        }

        #[test]
        fn negative_first_operand() {
            let n = Integer::from(-32).lcm(&36.into());

            assert_eq!(extract_or_fail!(n.precision, Precision::Single), 288);
            assert_eq!(n.sign, Sign::Positive);
        }

        #[test]
        fn negative_second_operand() {
            let n = Integer::from(32).lcm(&(-36).into());

            assert_eq!(extract_or_fail!(n.precision, Precision::Single), 288);
            assert_eq!(n.sign, Sign::Positive);
        }

        #[test]
        fn both_negative() {
            let n = Integer::from(-32).lcm(&(-36).into());

            assert_eq!(extract_or_fail!(n.precision, Precision::Single), 288);
            assert_eq!(n.sign, Sign::Positive);
        }

        #[test]
        fn commutative() {
            let cases = [(32, 36), (9, 28), (0, 5), (7, 7), (-32, 36)];
            for (a, b) in cases {
                let x = Integer::from(a).lcm(&b.into());
                let y = Integer::from(b).lcm(&a.into());

                assert_eq!(x, y);
            }
        }

        #[test]
        fn unit_operand() {
            let cases = [-32, 0, 1, 32];
            for case in cases {
                let n = Integer::from(case).lcm(&1.into());

                let expected = if case == 0 { 0 } else { case.unsigned_abs() };
                assert_eq!(extract_or_fail!(n.precision, Precision::Single), expected);
                assert_eq!(
                    n.sign,
                    if case == 0 {
                        Sign::Zero
                    } else {
                        Sign::Positive
                    }
                );
            }
        }

        // defining relationship between gcd and lcm: gcd(a,b) * lcm(a,b) = |a*b|
        #[test]
        fn gcd_lcm_product_identity() {
            let cases = [(4, 6), (9, 28), (6, 18), (7, 7), (-32, 36), (0, 5)];
            for (a, b) in cases {
                let x = Integer::from(a);
                let y = Integer::from(b);

                let gcd = x.gcd(&y);
                let lcm = x.lcm(&y);
                let product = gcd * lcm;

                let expected = Integer::from(a) * Integer::from(b);
                assert_eq!(
                    extract_or_fail!(product.precision, Precision::Single),
                    extract_or_fail!(expected.into_abs().precision, Precision::Single)
                );
            }
        }

        #[test]
        #[ignore = "multi-precision lcm not yet implemented"]
        fn multi_precision() {
            let a = Integer {
                precision: Precision::Multiple([4, 6].into()),
                sign: Sign::Positive,
            };
            let b = Integer::from(4);

            let n = a.lcm(&b);

            assert_eq!(extract_or_fail!(n.precision, Precision::Single), 4);
            assert_eq!(n.sign, Sign::Positive);
        }

        #[test]
        #[ignore = "multi-precision lcm not yet implemented"]
        fn overflows_precision() {
            let a: Integer = (Sign::Positive, u64::MAX).into();
            let b = Integer::from(2);

            let n = a.lcm(&b);

            assert_eq!(n.sign, Sign::Positive);
        }
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

        assert_eq!(n.to_float(), 1.5);
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

        let n = n.try_into_exact();

        let int = extract_or_fail!(ok_or_fail!(n), Real::Integer);
        assert_eq!(extract_or_fail!(int.precision, Precision::Single), 4);
        assert_eq!(int.sign, Sign::Positive);
    }

    #[test]
    fn into_exact_rational() {
        let n = Real::Float(1.5);

        let n = n.try_into_exact();

        let (num, den) = rational_parts!(ok_or_fail!(n));
        assert_eq!(extract_or_fail!(num.precision, Precision::Single), 3);
        assert_eq!(num.sign, Sign::Positive);
        assert_eq!(extract_or_fail!(den.precision, Precision::Single), 2);
        assert_eq!(den.sign, Sign::Positive);
    }

    #[test]
    fn into_exact_uses_binary_value_not_decimal_shorthand() {
        let n = Real::Float(0.1);

        let n = n.try_into_exact();

        let (num, den) = rational_parts!(ok_or_fail!(n));
        assert_eq!(
            extract_or_fail!(num.precision, Precision::Single),
            3602879701896397
        );
        assert_eq!(num.sign, Sign::Positive);
        assert_eq!(
            extract_or_fail!(den.precision, Precision::Single),
            36028797018963968
        );
        assert_eq!(den.sign, Sign::Positive);
    }

    #[test]
    fn into_exact_one_third_float() {
        let n = Real::Float(1.0 / 3.0);

        let n = n.try_into_exact();

        let (num, den) = rational_parts!(ok_or_fail!(n));
        assert_eq!(
            extract_or_fail!(num.precision, Precision::Single),
            6004799503160661
        );
        assert_eq!(num.sign, Sign::Positive);
        assert_eq!(
            extract_or_fail!(den.precision, Precision::Single),
            18014398509481984
        );
        assert_eq!(den.sign, Sign::Positive);
    }

    #[test]
    fn into_exact_denominator_is_always_a_power_of_two() {
        let cases = [0.1, 0.2, 1.0 / 3.0, 4.23452e-2];
        for case in cases {
            let n = Real::Float(case);

            let n = n.try_into_exact();

            let (_, den) = rational_parts!(ok_or_fail!(n));
            let d = extract_or_fail!(den.precision, Precision::Single);
            assert_eq!(d & (d - 1), 0, "denominator {d} is not a power of two");
        }
    }

    #[test]
    fn into_exact_zero() {
        let n = Real::Float(0.0);

        let n = n.try_into_exact();

        let int = extract_or_fail!(ok_or_fail!(n), Real::Integer);
        assert_eq!(extract_or_fail!(int.precision, Precision::Single), 0);
        assert_eq!(int.sign, Sign::Zero);
    }

    #[test]
    fn into_exact_negative_zero() {
        let n = Real::Float(-0.0);

        let n = n.try_into_exact();

        let int = extract_or_fail!(ok_or_fail!(n), Real::Integer);
        assert_eq!(extract_or_fail!(int.precision, Precision::Single), 0);
        assert_eq!(int.sign, Sign::Zero);
    }

    #[test]
    fn into_exact_exponent() {
        let n = Real::Float(4e2);

        let n = n.try_into_exact();

        let int = extract_or_fail!(ok_or_fail!(n), Real::Integer);
        assert_eq!(extract_or_fail!(int.precision, Precision::Single), 400);
        assert_eq!(int.sign, Sign::Positive);
    }

    #[test]
    fn into_exact_fraction_exponent() {
        let n = Real::Float(4.2e3);

        let n = n.try_into_exact();

        let int = extract_or_fail!(ok_or_fail!(n), Real::Integer);
        assert_eq!(extract_or_fail!(int.precision, Precision::Single), 4200);
        assert_eq!(int.sign, Sign::Positive);
    }

    #[test]
    fn into_exact_infinite() {
        let n = Real::Float(f64::INFINITY);

        let n = n.try_into_exact();

        let err = err_or_fail!(n);
        assert_matches!(err, NumericError::NoExactRepresentation(s) if s.contains("+inf.0"));
    }

    #[test]
    fn into_exact_nan() {
        let n = Real::Float(f64::NAN);

        let n = n.try_into_exact();

        let err = err_or_fail!(n);
        assert_matches!(err, NumericError::NoExactRepresentation(s) if s.contains("+nan.0"));
    }

    #[test]
    fn round_trip() {
        let expected = 4.23452e-2;
        let n = Real::Float(expected);

        let r = n.try_into_exact();

        let r = ok_or_fail!(r);
        assert_matches!(r, Real::Rational(_));

        let f = r.into_inexact();
        assert_eq!(f.to_float(), expected);
    }

    #[test]
    fn float_into_byte() {
        let n = Number::real(1.2);

        let r: Result<u8, _> = n.try_into();

        let err = err_or_fail!(r);
        assert_matches!(
            err,
            NumericError::IntConversionInvalidType(s)
            if s == "floating-point");
    }

    #[test]
    fn float_into_int() {
        let n = Number::real(1.2);

        let r: Result<i32, _> = n.try_into();

        let err = err_or_fail!(r);
        assert_matches!(
            err,
            NumericError::IntConversionInvalidType(s)
            if s == "floating-point");
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

    #[test]
    fn is_positive() {
        let r = Real::Float(4.0);

        assert!(r.is_positive());
    }

    #[test]
    fn zero_is_not_positive() {
        let r = Real::Float(0.0);

        assert!(!r.is_positive());
    }

    #[test]
    fn negative_zero_is_not_positive() {
        let r = Real::Float(-0.0);

        assert!(!r.is_positive());
    }

    #[test]
    fn negative_is_not_positive() {
        let r = Real::Float(-4.0);

        assert!(!r.is_positive());
    }

    #[test]
    fn positive_infinity_is_positive() {
        let r = Real::Float(f64::INFINITY);

        assert!(r.is_positive());
    }

    #[test]
    fn negative_infinity_is_not_positive() {
        let r = Real::Float(f64::NEG_INFINITY);

        assert!(!r.is_positive());
    }

    #[test]
    fn nan_is_not_positive() {
        let r = Real::Float(f64::NAN);

        assert!(!r.is_positive());
    }

    #[test]
    fn is_negative() {
        let r = Real::Float(-4.0);

        assert!(r.is_negative());
    }

    #[test]
    fn zero_is_not_negative() {
        let r = Real::Float(0.0);

        assert!(!r.is_negative());
    }

    #[test]
    fn negative_zero_is_not_negative() {
        let r = Real::Float(-0.0);

        assert!(!r.is_negative());
    }

    #[test]
    fn positive_is_not_negative() {
        let r = Real::Float(4.0);

        assert!(!r.is_negative());
    }

    #[test]
    fn positive_infinity_is_not_negative() {
        let r = Real::Float(f64::INFINITY);

        assert!(!r.is_negative());
    }

    #[test]
    fn negative_infinity_is_negative() {
        let r = Real::Float(f64::NEG_INFINITY);

        assert!(r.is_negative());
    }

    #[test]
    fn nan_is_not_negative() {
        let r = Real::Float(f64::NAN);

        assert!(!r.is_negative());
    }

    #[test]
    fn zero_frac_try_into_exact_integer() {
        let r = Real::Float(4.0);

        let res = r.try_into_exact_integer();

        let n = ok_or_fail!(res);
        assert_eq!(extract_or_fail!(n.precision, Precision::Single), 4);
        assert_eq!(n.sign, Sign::Positive);
    }

    #[test]
    fn negative_zero_frac_try_into_exact_integer() {
        let r = Real::Float(-4.0);

        let res = r.try_into_exact_integer();

        let n = ok_or_fail!(res);
        assert_eq!(extract_or_fail!(n.precision, Precision::Single), 4);
        assert_eq!(n.sign, Sign::Negative);
    }

    #[test]
    fn zeros_try_into_exact_integer() {
        let cases = [0.0, -0.0];
        for case in cases {
            let r = Real::Float(case);

            let res = r.try_into_exact_integer();

            let n = ok_or_fail!(res);
            assert_eq!(extract_or_fail!(n.precision, Precision::Single), 0);
            assert_eq!(n.sign, Sign::Zero);
        }
    }

    #[test]
    fn max_continuous_integer_try_into_exact_integer() {
        let r = Real::Float(FMAX_INT);

        let res = r.try_into_exact_integer();

        let n = ok_or_fail!(res);
        assert_eq!(
            extract_or_fail!(n.precision, Precision::Single),
            9007199254740991
        );
        assert_eq!(n.sign, Sign::Positive);
    }

    #[test]
    fn min_continuous_integer_try_into_exact_integer() {
        let r = Real::Float(-FMAX_INT);

        let res = r.try_into_exact_integer();

        let n = ok_or_fail!(res);
        assert_eq!(
            extract_or_fail!(n.precision, Precision::Single),
            9007199254740991
        );
        assert_eq!(n.sign, Sign::Negative);
    }

    #[test]
    fn non_zero_frac_try_into_exact_integer() {
        let r = Real::Float(4.2);

        let res = r.try_into_exact_integer();

        let err = err_or_fail!(res);
        assert_matches!(err, NumericError::NotExactInteger(s) if s == "4.2");
    }

    #[test]
    fn inf_nan_try_into_exact_integer() {
        let cases = [
            (f64::INFINITY, "+inf.0"),
            (f64::NEG_INFINITY, "-inf.0"),
            (f64::NAN, "+nan.0"),
        ];
        for (case, expected) in cases {
            let r = Real::Float(case);

            let res = r.try_into_exact_integer();

            let err = err_or_fail!(res);
            assert_matches!(err, NumericError::NotExactInteger(s) if s == expected);
        }
    }

    #[test]
    fn positive_try_into_numerator() {
        let r = Real::Float(4.0);

        let num = ok_or_fail!(r.try_into_numerator());

        let flt = extract_or_fail!(num, Real::Float);
        assert_eq!(flt, 4.0);
    }

    #[test]
    fn negative_try_into_numerator() {
        let r = Real::Float(-4.0);

        let num = ok_or_fail!(r.try_into_numerator());

        let flt = extract_or_fail!(num, Real::Float);
        assert_eq!(flt, -4.0);
    }

    #[test]
    fn fractional_try_into_numerator() {
        let r = Real::Float(4.5); // 9/2

        let num = ok_or_fail!(r.try_into_numerator());

        let flt = extract_or_fail!(num, Real::Float);
        assert_eq!(flt, 9.0);
    }

    #[test]
    fn inf_try_into_numerator() {
        let r = Real::Float(f64::INFINITY);

        let err = err_or_fail!(r.try_into_numerator());

        assert_matches!(err, NumericError::NoExactRepresentation(s) if s == "+inf.0");
    }

    #[test]
    fn nan_try_into_numerator() {
        let r = Real::Float(f64::NAN);

        let err = err_or_fail!(r.try_into_numerator());

        assert_matches!(err, NumericError::NoExactRepresentation(s) if s == "+nan.0");
    }

    #[test]
    fn positive_try_into_denominator() {
        let r = Real::Float(4.0);

        let denom = ok_or_fail!(r.try_into_denominator());

        let flt = extract_or_fail!(denom, Real::Float);
        assert_eq!(flt, 1.0);
    }

    #[test]
    fn negative_try_into_denominator() {
        let r = Real::Float(4.0);

        let denom = ok_or_fail!(r.try_into_denominator());

        let flt = extract_or_fail!(denom, Real::Float);
        assert_eq!(flt, 1.0);
    }

    #[test]
    fn fractional_try_into_denominator() {
        let r = Real::Float(4.5); // 9/2

        let denom = ok_or_fail!(r.try_into_denominator());

        let flt = extract_or_fail!(denom, Real::Float);
        assert_eq!(flt, 2.0);
    }

    #[test]
    fn inf_try_into_denominator() {
        let r = Real::Float(f64::INFINITY);

        let err = err_or_fail!(r.try_into_denominator());

        assert_matches!(err, NumericError::NoExactRepresentation(s) if s == "+inf.0");
    }

    #[test]
    fn nan_try_into_denominator() {
        let r = Real::Float(f64::NAN);

        let err = err_or_fail!(r.try_into_denominator());

        assert_matches!(err, NumericError::NoExactRepresentation(s) if s == "+nan.0");
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
        let q = ok_or_fail!(Real::reduce(4, 5));
        let (num, den) = rational_parts!(q);

        assert_eq!(extract_or_fail!(num.precision, Precision::Single), 4);
        assert_eq!(num.sign, Sign::Positive);
        assert_eq!(extract_or_fail!(den.precision, Precision::Single), 5);
        assert_eq!(den.sign, Sign::Positive);
    }

    #[test]
    fn negative_numerator() {
        let q = ok_or_fail!(Real::reduce(-4, 5));
        let (num, den) = rational_parts!(q);

        assert_eq!(extract_or_fail!(num.precision, Precision::Single), 4);
        assert_eq!(num.sign, Sign::Negative);
        assert_eq!(extract_or_fail!(den.precision, Precision::Single), 5);
        assert_eq!(den.sign, Sign::Positive);
    }

    #[test]
    fn negative_denominator() {
        let q = ok_or_fail!(Real::reduce(4, -5));
        let (num, den) = rational_parts!(q);

        assert_eq!(extract_or_fail!(num.precision, Precision::Single), 4);
        assert_eq!(num.sign, Sign::Negative);
        assert_eq!(extract_or_fail!(den.precision, Precision::Single), 5);
        assert_eq!(den.sign, Sign::Positive);
    }

    #[test]
    fn negative_parts() {
        let q = ok_or_fail!(Real::reduce(-4, -5));
        let (num, den) = rational_parts!(q);

        assert_eq!(extract_or_fail!(num.precision, Precision::Single), 4);
        assert_eq!(num.sign, Sign::Positive);
        assert_eq!(extract_or_fail!(den.precision, Precision::Single), 5);
        assert_eq!(den.sign, Sign::Positive);
    }

    #[test]
    fn improper() {
        let q = ok_or_fail!(Real::reduce(5, 4));
        let (num, den) = rational_parts!(q);

        assert_eq!(extract_or_fail!(num.precision, Precision::Single), 5);
        assert_eq!(num.sign, Sign::Positive);
        assert_eq!(extract_or_fail!(den.precision, Precision::Single), 4);
        assert_eq!(den.sign, Sign::Positive);
    }

    #[test]
    fn gcd() {
        let q = ok_or_fail!(Real::reduce(4, 10));
        let (num, den) = rational_parts!(q);

        assert_eq!(extract_or_fail!(num.precision, Precision::Single), 2);
        assert_eq!(num.sign, Sign::Positive);
        assert_eq!(extract_or_fail!(den.precision, Precision::Single), 5);
        assert_eq!(den.sign, Sign::Positive);
    }

    #[test]
    fn gcd_negative() {
        let q = ok_or_fail!(Real::reduce(-4, 10));
        let (num, den) = rational_parts!(q);

        assert_eq!(extract_or_fail!(num.precision, Precision::Single), 2);
        assert_eq!(num.sign, Sign::Negative);
        assert_eq!(extract_or_fail!(den.precision, Precision::Single), 5);
        assert_eq!(den.sign, Sign::Positive);
    }

    #[test]
    fn unity() {
        let q = ok_or_fail!(Real::reduce(1, 1));
        let int = extract_or_fail!(q, Real::Integer);

        assert_eq!(extract_or_fail!(int.precision, Precision::Single), 1);
        assert_eq!(int.sign, Sign::Positive);
    }

    #[test]
    fn reduce_to_unity() {
        let q = ok_or_fail!(Real::reduce(7, 7));
        let int = extract_or_fail!(q, Real::Integer);

        assert_eq!(extract_or_fail!(int.precision, Precision::Single), 1);
        assert_eq!(int.sign, Sign::Positive);
    }

    #[test]
    fn reduce_to_negative_unity() {
        let q = ok_or_fail!(Real::reduce(-7, 7));
        let int = extract_or_fail!(q, Real::Integer);

        assert_eq!(extract_or_fail!(int.precision, Precision::Single), 1);
        assert_eq!(int.sign, Sign::Negative);
    }

    #[test]
    fn reduce_to_integer() {
        let q = ok_or_fail!(Real::reduce(20, 10));
        let int = extract_or_fail!(q, Real::Integer);

        assert_eq!(extract_or_fail!(int.precision, Precision::Single), 2);
        assert_eq!(int.sign, Sign::Positive);
    }

    #[test]
    fn reduce_to_negative_integer() {
        let q = ok_or_fail!(Real::reduce(-20, 10));
        let int = extract_or_fail!(q, Real::Integer);

        assert_eq!(extract_or_fail!(int.precision, Precision::Single), 2);
        assert_eq!(int.sign, Sign::Negative);
    }

    #[test]
    fn zero_numerator() {
        let q = ok_or_fail!(Real::reduce(0, 7));
        let int = extract_or_fail!(q, Real::Integer);

        assert_eq!(extract_or_fail!(int.precision, Precision::Single), 0);
        assert_eq!(int.sign, Sign::Zero);
    }

    #[test]
    fn unreduced_zero_numerator_is_zero() {
        let q = Rational((0.into(), 7.into()).into());

        assert!(q.is_zero());
    }

    #[test]
    fn zero_denominator() {
        let q = Real::reduce(1, 0);
        let err = err_or_fail!(q);

        assert_matches!(err, NumericError::DivideByZero);
    }

    #[test]
    fn unreduced_zero_denomintor_ignored_for_is_zero() {
        let q = Rational((7.into(), 0.into()).into());

        assert!(!q.is_zero());
    }

    #[test]
    fn positive_into_float() {
        let q = ok_or_fail!(Real::reduce(4, 5));

        assert_eq!(q.to_float(), 0.8);
    }

    #[test]
    fn negative_numerator_into_float() {
        let q = ok_or_fail!(Real::reduce(-4, 5));

        assert_eq!(q.to_float(), -0.8);
    }

    #[test]
    fn negative_denominator_into_float() {
        let q = ok_or_fail!(Real::reduce(4, -5));

        assert_eq!(q.to_float(), -0.8);
    }

    #[test]
    fn negative_parts_into_float() {
        let q = ok_or_fail!(Real::reduce(-4, -5));

        assert_eq!(q.to_float(), 0.8);
    }

    #[test]
    fn unreduced_zero_into_float() {
        let q = Rational((0.into(), 7.into()).into());

        assert_eq!(q.to_float(), 0.0);
    }

    #[test]
    fn unreduced_div_by_zero_into_float() {
        let q = Rational((7.into(), 0.into()).into());

        assert_eq!(q.to_float(), f64::INFINITY);
    }

    #[test]
    fn unreduced_negative_div_by_zero_into_float() {
        let q = Rational(((-7).into(), 0.into()).into());

        assert_eq!(q.to_float(), f64::NEG_INFINITY);
    }

    #[test]
    fn positive_into_inexact() {
        let q = ok_or_fail!(Real::reduce(4, 5));

        let r = q.into_inexact();

        let f = extract_or_fail!(r, Real::Float);
        assert_eq!(f, 0.8);
    }

    #[test]
    fn positive_into_exact() {
        let q = ok_or_fail!(Real::reduce(4, 5));

        let (num, den) = rational_parts!(ok_or_fail!(q.try_into_exact()));

        assert_eq!(extract_or_fail!(num.precision, Precision::Single), 4);
        assert_eq!(num.sign, Sign::Positive);
        assert_eq!(extract_or_fail!(den.precision, Precision::Single), 5);
        assert_eq!(den.sign, Sign::Positive);
    }

    #[test]
    fn rational_into_byte() {
        let q = Real::reduce(4, 5);

        let n = Number::real(ok_or_fail!(q));

        let r: Result<u8, _> = n.try_into();

        let err = err_or_fail!(r);
        assert_matches!(
            err,
            NumericError::IntConversionInvalidType(s)
            if s == "rational");
    }

    #[test]
    fn rational_into_int() {
        let q = Real::reduce(4, 5);

        let n = Number::real(ok_or_fail!(q));

        let r: Result<i32, _> = n.try_into();

        let err = err_or_fail!(r);
        assert_matches!(
            err,
            NumericError::IntConversionInvalidType(s)
            if s == "rational");
    }

    #[test]
    fn not_is_integer() {
        let q = ok_or_fail!(Real::reduce(4, 5));

        assert!(!q.is_integer());
    }

    #[test]
    fn reduce_is_integer() {
        let q = ok_or_fail!(Real::reduce(8, 4));

        assert!(q.is_integer());
    }

    #[test]
    fn is_rational() {
        let q = ok_or_fail!(Real::reduce(4, 5));

        assert!(q.is_rational());
    }

    #[test]
    fn not_is_inexact() {
        let q = ok_or_fail!(Real::reduce(4, 5));

        assert!(!q.is_inexact());
    }

    #[test]
    fn not_is_infinite() {
        let q = ok_or_fail!(Real::reduce(4, 5));

        assert!(!q.is_infinite());
    }

    #[test]
    fn not_is_nan() {
        let q = ok_or_fail!(Real::reduce(4, 5));

        assert!(!q.is_nan());
    }

    #[test]
    fn is_positive() {
        let q = ok_or_fail!(Real::reduce(4, 5));

        assert!(q.is_positive());
    }

    #[test]
    fn zero_is_not_positive() {
        let q = ok_or_fail!(Real::reduce(0, 5));

        assert!(!q.is_positive());
    }

    #[test]
    fn negative_is_not_positive() {
        let q = ok_or_fail!(Real::reduce(-4, 5));

        assert!(!q.is_positive());
    }

    #[test]
    fn negative_denom_not_checked_for_is_positive() {
        let q = Rational((4.into(), (-5).into()).into());

        assert!(q.is_positive());
    }

    #[test]
    fn is_negative() {
        let q = ok_or_fail!(Real::reduce(-4, 5));

        assert!(q.is_negative());
    }

    #[test]
    fn zero_is_not_negative() {
        let q = ok_or_fail!(Real::reduce(0, 5));

        assert!(!q.is_negative());
    }

    #[test]
    fn positive_is_not_negative() {
        let q = ok_or_fail!(Real::reduce(4, 5));

        assert!(!q.is_negative());
    }

    #[test]
    fn negative_denom_not_checked_for_is_negative() {
        let q = Rational((4.into(), (-5).into()).into());

        assert!(!q.is_negative());
    }

    #[test]
    fn into_exact_integer() {
        let q = ok_or_fail!(Real::reduce(4, 5));

        let r = q.try_into_exact_integer();

        let err = err_or_fail!(r);
        assert_matches!(err, NumericError::NotExactInteger(s) if s == "4/5");
    }

    #[test]
    fn negative_abs() {
        let q = Rational(((-4).into(), 5.into()).into());

        let abs = q.into_abs();

        assert_eq!(abs.0.0.sign, Sign::Positive);
    }

    #[test]
    fn positive_abs() {
        let q = Rational((4.into(), 5.into()).into());

        let abs = q.into_abs();

        assert_eq!(abs.0.0.sign, Sign::Positive);
    }

    #[test]
    fn zero_abs() {
        let q = Rational((0.into(), 5.into()).into());

        let abs = q.into_abs();

        assert_eq!(abs.0.0.sign, Sign::Zero);
    }

    #[test]
    fn negative_denom_not_checked_for_abs() {
        let q = Rational((4.into(), (-5).into()).into());

        let abs = q.into_abs();

        assert_eq!(abs.0.0.sign, Sign::Positive);
        assert_eq!(abs.0.1.sign, Sign::Negative);
    }

    #[test]
    fn positive_into_numerator() {
        let q = ok_or_fail!(Real::reduce(4, 5));

        let num = ok_or_fail!(q.try_into_numerator());

        let n = extract_or_fail!(num, Real::Integer);
        assert_eq!(extract_or_fail!(n.precision, Precision::Single), 4);
        assert_eq!(n.sign, Sign::Positive);
    }

    #[test]
    fn negative_into_numerator() {
        let q = ok_or_fail!(Real::reduce(-4, 5));

        let num = ok_or_fail!(q.try_into_numerator());

        let n = extract_or_fail!(num, Real::Integer);
        assert_eq!(extract_or_fail!(n.precision, Precision::Single), 4);
        assert_eq!(n.sign, Sign::Negative);
    }

    #[test]
    fn positive_into_denominator() {
        let q = ok_or_fail!(Real::reduce(4, 5));

        let denom = ok_or_fail!(q.try_into_denominator());

        let n = extract_or_fail!(denom, Real::Integer);
        assert_eq!(extract_or_fail!(n.precision, Precision::Single), 5);
        assert_eq!(n.sign, Sign::Positive);
    }

    #[test]
    fn negative_into_denominator() {
        let q = ok_or_fail!(Real::reduce(-4, 5));

        let denom = ok_or_fail!(q.try_into_denominator());

        let n = extract_or_fail!(denom, Real::Integer);
        assert_eq!(extract_or_fail!(n.precision, Precision::Single), 5);
        assert_eq!(n.sign, Sign::Positive);
    }

    #[test]
    fn negative_denom_not_checked_for_denominator() {
        let q = Rational((4.into(), (-5).into()).into());

        let n = q.into_denominator();

        assert_eq!(extract_or_fail!(n.precision, Precision::Single), 5);
        assert_eq!(n.sign, Sign::Negative);
    }
}

mod complex {
    use super::*;

    #[test]
    fn basic() {
        let z = Number::complex(4, 3);

        let ri = extract_or_fail!(z, Number::Complex);
        let r = extract_or_fail!(ri.0.0, Real::Integer);
        assert!(!r.is_zero());
        assert_eq!(extract_or_fail!(r.precision, Precision::Single), 4);
        let i = extract_or_fail!(ri.0.1, Real::Integer);
        assert!(!i.is_zero());
        assert_eq!(extract_or_fail!(i.precision, Precision::Single), 3);
    }

    #[test]
    fn zero_imaginary_reduces_to_int() {
        let z = Number::complex(4, 0);

        let r = extract_or_fail!(z, Number::Real);
        let int = extract_or_fail!(r, Real::Integer);
        assert!(!int.is_zero());
        assert_eq!(extract_or_fail!(int.precision, Precision::Single), 4);
    }

    #[test]
    fn zero_real() {
        let z = Number::complex(0, 3);

        assert!(!z.is_zero());
        let ri = extract_or_fail!(z, Number::Complex);
        let r = extract_or_fail!(ri.0.0, Real::Integer);
        assert!(r.is_zero());
        assert_eq!(extract_or_fail!(r.precision, Precision::Single), 0);
        let i = extract_or_fail!(ri.0.1, Real::Integer);
        assert!(!i.is_zero());
        assert_eq!(extract_or_fail!(i.precision, Precision::Single), 3);
    }

    #[test]
    fn inexact_zero_imaginary_does_not_reduce() {
        let z = Number::complex(4, 0.0);

        assert!(!z.is_zero());
        let ri = extract_or_fail!(z, Number::Complex);
        let int = extract_or_fail!(ri.0.0, Real::Integer);
        assert!(!int.is_zero());
        assert_eq!(extract_or_fail!(int.precision, Precision::Single), 4);
        let flt = extract_or_fail!(ri.0.1, Real::Float);
        assert_eq!(0.0, flt);
    }

    #[test]
    fn all_inexact_zeros() {
        let z = Number::complex(0.0, 0.0);

        assert!(z.is_zero());
        let ri = extract_or_fail!(z, Number::Complex);
        let flt = extract_or_fail!(ri.0.0, Real::Float);
        assert_eq!(0.0, flt);
        let flt = extract_or_fail!(ri.0.1, Real::Float);
        assert_eq!(0.0, flt);
    }

    #[test]
    fn imaginary_only() {
        let z = Number::imaginary(3);

        let ri = extract_or_fail!(z, Number::Complex);
        let r = extract_or_fail!(ri.0.0, Real::Integer);
        assert!(r.is_zero());
        assert_eq!(extract_or_fail!(r.precision, Precision::Single), 0);
        let i = extract_or_fail!(ri.0.1, Real::Integer);
        assert!(!i.is_zero());
        assert_eq!(extract_or_fail!(i.precision, Precision::Single), 3);
    }

    #[test]
    fn inexact_zero_real_does_not_reduce() {
        let z = Number::complex(0.0, 3);

        assert!(!z.is_zero());
        let ri = extract_or_fail!(z, Number::Complex);
        let flt = extract_or_fail!(ri.0.0, Real::Float);
        assert_eq!(0.0, flt);
        let int = extract_or_fail!(ri.0.1, Real::Integer);
        assert!(!int.is_zero());
        assert_eq!(extract_or_fail!(int.precision, Precision::Single), 3);
    }

    #[test]
    fn polar() {
        let z = Number::polar(4, 3);

        let ri = extract_or_fail!(z, Number::Complex);
        let r = extract_or_fail!(ri.0.0, Real::Float);
        assert_eq!(r, -3.9599699864017817);
        let i = extract_or_fail!(ri.0.1, Real::Float);
        assert_eq!(i, 0.5644800322394689);
    }

    #[test]
    fn polar_floats() {
        let z = Number::polar(4.0, 3.0);

        let ri = extract_or_fail!(z, Number::Complex);
        let r = extract_or_fail!(ri.0.0, Real::Float);
        assert_eq!(r, -3.9599699864017817);
        let i = extract_or_fail!(ri.0.1, Real::Float);
        assert_eq!(i, 0.5644800322394689);
    }

    #[test]
    fn negative_mag() {
        let z = Number::polar(-4, 3);

        let ri = extract_or_fail!(z, Number::Complex);
        let r = extract_or_fail!(ri.0.0, Real::Float);
        assert_eq!(r, 3.9599699864017817);
        let i = extract_or_fail!(ri.0.1, Real::Float);
        assert_eq!(i, -0.5644800322394689);
    }

    #[test]
    fn negative_angle() {
        let z = Number::polar(4, -3);

        let ri = extract_or_fail!(z, Number::Complex);
        let r = extract_or_fail!(ri.0.0, Real::Float);
        assert_eq!(r, -3.9599699864017817);
        let i = extract_or_fail!(ri.0.1, Real::Float);
        assert_eq!(i, -0.5644800322394689);
    }

    #[test]
    fn negatives() {
        let z = Number::polar(-4, -3);

        let ri = extract_or_fail!(z, Number::Complex);
        let r = extract_or_fail!(ri.0.0, Real::Float);
        assert_eq!(r, 3.9599699864017817);
        let i = extract_or_fail!(ri.0.1, Real::Float);
        assert_eq!(i, 0.5644800322394689);
    }

    #[test]
    fn zero_mag() {
        let z = Number::polar(0, 3);

        let r = extract_or_fail!(z, Number::Real);
        let int = extract_or_fail!(r, Real::Integer);
        assert!(int.is_zero());
        assert_eq!(extract_or_fail!(int.precision, Precision::Single), 0);
    }

    #[test]
    fn zero_mag_float_does_not_reduce() {
        let z = Number::polar(0.0, 3);

        assert!(z.is_zero());
        let ri = extract_or_fail!(z, Number::Complex);
        let flt = extract_or_fail!(ri.0.0, Real::Float);
        assert_eq!(flt, 0.0);
        let flt = extract_or_fail!(ri.0.1, Real::Float);
        assert_eq!(flt, 0.0);
    }

    #[test]
    fn zero_angle() {
        let z = Number::polar(4, 0);

        let r = extract_or_fail!(z, Number::Real);
        let int = extract_or_fail!(r, Real::Integer);
        assert!(!int.is_zero());
        assert_eq!(extract_or_fail!(int.precision, Precision::Single), 4);
    }

    #[test]
    fn zero_angle_float_does_not_reduce() {
        let z = Number::polar(4, 0.0);

        assert!(!z.is_zero());
        let ri = extract_or_fail!(z, Number::Complex);
        let flt = extract_or_fail!(ri.0.0, Real::Float);
        assert_eq!(flt, 4.0);
        let flt = extract_or_fail!(ri.0.1, Real::Float);
        assert_eq!(flt, 0.0);
    }

    #[test]
    fn polar_all_inexact_zeros() {
        let z = Number::polar(0.0, 0.0);

        assert!(z.is_zero());
        let ri = extract_or_fail!(z, Number::Complex);
        let flt = extract_or_fail!(ri.0.0, Real::Float);
        assert_eq!(0.0, flt);
        let flt = extract_or_fail!(ri.0.1, Real::Float);
        assert_eq!(0.0, flt);
    }

    #[test]
    fn zero_angle_rational() {
        let m = ok_or_fail!(Real::reduce(4, 5));
        let rad = ok_or_fail!(Real::reduce(0, 2));
        let z = Number::polar(m, rad);

        let (num, den) = rational_parts!(extract_or_fail!(z, Number::Real));

        assert_eq!(extract_or_fail!(num.precision, Precision::Single), 4);
        assert_eq!(num.sign, Sign::Positive);
        assert_eq!(extract_or_fail!(den.precision, Precision::Single), 5);
        assert_eq!(den.sign, Sign::Positive);
    }

    #[test]
    fn into_exact() {
        let z = Number::complex(4.0, 3.0);

        let z = z.try_into_exact();

        let ri = extract_or_fail!(ok_or_fail!(z), Number::Complex);
        let r = extract_or_fail!(ri.0.0, Real::Integer);
        assert!(!r.is_zero());
        assert_eq!(extract_or_fail!(r.precision, Precision::Single), 4);
        let i = extract_or_fail!(ri.0.1, Real::Integer);
        assert!(!i.is_zero());
        assert_eq!(extract_or_fail!(i.precision, Precision::Single), 3);
    }

    #[test]
    fn into_exact_rational() {
        let z = Number::complex(1.5, 0.875);

        let z = z.try_into_exact();

        let ri = extract_or_fail!(ok_or_fail!(z), Number::Complex);
        let (num, den) = rational_parts!(ri.0.0);
        assert_eq!(extract_or_fail!(num.precision, Precision::Single), 3);
        assert_eq!(num.sign, Sign::Positive);
        assert_eq!(extract_or_fail!(den.precision, Precision::Single), 2);
        assert_eq!(den.sign, Sign::Positive);
        let (num, den) = rational_parts!(ri.0.1);
        assert_eq!(extract_or_fail!(num.precision, Precision::Single), 7);
        assert_eq!(num.sign, Sign::Positive);
        assert_eq!(extract_or_fail!(den.precision, Precision::Single), 8);
        assert_eq!(den.sign, Sign::Positive);
    }

    #[test]
    fn into_inexact() {
        let z = Number::complex(4, 3);

        let z = z.into_inexact();

        let ri = extract_or_fail!(z, Number::Complex);
        let r = extract_or_fail!(ri.0.0, Real::Float);
        assert_eq!(r, 4.0);
        let i = extract_or_fail!(ri.0.1, Real::Float);
        assert_eq!(i, 3.0);
    }

    #[test]
    fn complex_into_byte() {
        let z = Number::complex(4, 5);

        let r: Result<u8, _> = z.try_into();

        let err = err_or_fail!(r);
        assert_matches!(
            err,
            NumericError::IntConversionInvalidType(s)
            if s == "complex");
    }

    #[test]
    fn complex_into_int() {
        let z = Number::complex(4, 5);

        let r: Result<i32, _> = z.try_into();

        let err = err_or_fail!(r);
        assert_matches!(
            err,
            NumericError::IntConversionInvalidType(s)
            if s == "complex");
    }

    #[test]
    fn inexact_parts_is_inexact() {
        let z = Number::complex(4.3, 5.6);

        assert!(z.is_inexact());
    }

    #[test]
    fn inexact_parts_with_zero_frac_is_inexact() {
        let z = Number::complex(4.0, 5.0);

        assert!(z.is_inexact());
    }

    #[test]
    fn not_inexact_parts_with_exact_parts() {
        let z = Number::complex(4, ok_or_fail!(Real::reduce(4, 5)));

        assert!(!z.is_inexact());
    }

    #[test]
    fn inexact_if_inexact_real() {
        let z = Number::complex(4.3, 6);

        assert!(z.is_inexact());
    }

    #[test]
    fn inexact_if_inexact_imag() {
        let z = Number::complex(4, 5.6);

        assert!(z.is_inexact());
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

        let err = err_or_fail!(espec.try_into_exact("1234"));

        assert_matches!(err, NumericError::ParseFailure);

        let err = err_or_fail!(ispec.try_into_inexact("1234"));

        assert_matches!(err, NumericError::ParseFailure);
    }

    #[test]
    fn int_blank_string() {
        let espec = IntSpec::<Decimal> {
            magnitude: 1..3,
            ..Default::default()
        };
        let ispec = espec.clone();

        assert!(!espec.is_empty());

        let err = err_or_fail!(espec.try_into_exact(""));

        assert_matches!(err, NumericError::ParseFailure);

        let err = err_or_fail!(ispec.try_into_inexact(""));

        assert_matches!(err, NumericError::ParseFailure);
    }

    #[test]
    fn float_empty() {
        let espec = FloatSpec::default();
        let ispec = espec.clone();

        assert!(espec.is_empty());

        let err = err_or_fail!(espec.try_into_exact("1234.456e3"));

        assert_matches!(err, NumericError::ParseFailure);

        let err = err_or_fail!(ispec.try_into_inexact("1234.456e3"));

        assert_matches!(err, NumericError::ParseFailure);
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

        let err = err_or_fail!(espec.try_into_exact(""));

        assert_matches!(err, NumericError::ParseExponentFailure);

        let err = err_or_fail!(ispec.try_into_inexact(""));

        assert_matches!(err, NumericError::ParseFailure);
    }
}

mod equivalence {
    use super::*;

    #[test]
    fn real_not_equivalent_to_complex() {
        let z = Number::complex(4, 5);
        let r = Number::real(4);

        assert!(!z.is_eqv(&r));
    }

    #[test]
    fn inexact_equivalent_to_inexact() {
        let a = Number::real(4.2);
        let b = Number::real(4.2);

        assert!(a.is_eqv(&b));
    }

    #[test]
    fn exact_equivalent_to_exact() {
        let a = Number::real(4);
        let b = Number::real(4);

        assert!(a.is_eqv(&b));
    }

    #[test]
    fn exact_zeros_equivalent() {
        let a = Number::real(0);
        let b = Number::real(0);

        assert!(a.is_eqv(&b));
    }

    #[test]
    fn exact_negatives_equivalent() {
        let a = Number::real(-4);
        let b = Number::real(-4);

        assert!(a.is_eqv(&b));
    }

    #[test]
    fn mixed_sign_not_equivalent() {
        let a = Number::real(4);
        let b = Number::real(-4);

        assert!(!a.is_eqv(&b));
    }

    #[test]
    fn exact_rational_equivalent_to_exact() {
        let a = Number::real(ok_or_fail!(Real::reduce(4, 5)));
        let b = Number::real(ok_or_fail!(Real::reduce(8, 10)));

        assert!(a.is_eqv(&b));
    }

    #[test]
    fn inexact_not_equivalent_to_exact() {
        let a = Number::real(4.0);
        let b = Number::real(4);

        assert!(!a.is_eqv(&b));
    }

    #[test]
    fn inexact_zeros_of_opposite_sign_not_equivalent() {
        let a = Number::real(0.0);
        let b = Number::real(-0.0);

        assert!(!a.is_eqv(&b));
        assert!(!b.is_eqv(&a));
    }

    #[test]
    fn matching_inexact_zeros_are_equivalent() {
        let cases = [(0.0, 0.0), (-0.0, -0.0)];
        for (a, b) in cases {
            let a = Number::real(a);
            let b = Number::real(b);

            assert!(a.is_eqv(&b));
        }
    }

    #[test]
    fn infinities_are_equivalent() {
        let a = Number::real(f64::INFINITY);
        let b = Number::real(f64::INFINITY);

        assert!(a.is_eqv(&b));
    }

    #[test]
    fn nans_are_equivalent() {
        let a = Number::real(f64::NAN);
        let b = Number::real(f64::NAN);

        assert!(a.is_eqv(&b));
    }

    #[test]
    fn inexact_different_values() {
        let a = Number::real(4.2);
        let b = Number::real(5.2);

        assert!(!a.is_eqv(&b));
    }

    #[test]
    fn exact_different_values() {
        let a = Number::real(4);
        let b = Number::real(5);

        assert!(!a.is_eqv(&b));
    }

    #[test]
    fn exact_rational_different_values() {
        let a = Number::real(ok_or_fail!(Real::reduce(4, 5)));
        let b = Number::real(ok_or_fail!(Real::reduce(3, 4)));

        assert!(!a.is_eqv(&b));
    }

    #[test]
    fn complex_inexact_equivalent() {
        let a = Number::complex(4.2, 5.3);
        let b = Number::complex(4.2, 5.3);

        assert!(a.is_eqv(&b));
    }

    #[test]
    fn complex_exact_equivalent() {
        let a = Number::complex(4, 5);
        let b = Number::complex(4, 5);

        assert!(a.is_eqv(&b));
    }

    #[test]
    fn complex_mixed_equivalent() {
        let a = Number::complex(4, 5.3);
        let b = Number::complex(4, 5.3);

        assert!(a.is_eqv(&b));
    }

    #[test]
    fn complex_mismatched_not_equivalent() {
        let a = Number::complex(4.0, 5);
        let b = Number::complex(4, 5.0);

        assert!(!a.is_eqv(&b));
    }
}

mod equality {
    use super::*;

    #[test]
    fn nan_not_equal_to_itself() {
        let n = Real::Float(f64::NAN);

        assert_ne!(n, n);
    }

    #[test]
    fn nan_not_equal_to_other_float() {
        let n = Real::Float(f64::NAN);
        let f = Real::Float(4.0);

        assert_ne!(n, f);
    }

    #[test]
    fn nan_not_equal_to_integer() {
        let n = Real::Float(f64::NAN);
        let i = Real::Integer(4.into());

        assert_ne!(n, i);
    }

    #[test]
    fn nan_not_equal_to_rational() {
        let n = Real::Float(f64::NAN);
        let q = ok_or_fail!(Real::reduce(4, 5));

        assert_ne!(n, q);
    }

    #[test]
    fn nan_numbers_not_equal() {
        let a = Number::real(f64::NAN);
        let b = Number::real(f64::NAN);

        assert_ne!(a, b);
    }

    #[test]
    fn positive_infinity_equal_to_itself() {
        let a = Real::Float(f64::INFINITY);
        let b = Real::Float(f64::INFINITY);

        assert_eq!(a, b);
    }

    #[test]
    fn negative_infinity_equal_to_itself() {
        let a = Real::Float(f64::NEG_INFINITY);
        let b = Real::Float(f64::NEG_INFINITY);

        assert_eq!(a, b);
    }

    #[test]
    fn positive_and_negative_infinity_not_equal() {
        let a = Real::Float(f64::INFINITY);
        let b = Real::Float(f64::NEG_INFINITY);

        assert_ne!(a, b);
    }

    #[test]
    fn zero_equal_to_negative_zero() {
        let a = Real::Float(0.0);
        let b = Real::Float(-0.0);

        assert_eq!(a, b);
    }

    #[test]
    fn integer_equal_to_matching_float() {
        let i = Real::Integer(4.into());
        let f = Real::Float(4.0);

        assert_eq!(i, f);
        assert_eq!(f, i);
    }

    #[test]
    fn integer_not_equal_to_mismatched_float() {
        let i = Real::Integer(4.into());
        let f = Real::Float(4.2);

        assert_ne!(i, f);
    }

    #[test]
    fn rational_equal_to_matching_float() {
        let q = ok_or_fail!(Real::reduce(1, 2));
        let f = Real::Float(0.5);

        assert_eq!(q, f);
        assert_eq!(f, q);
    }

    #[test]
    fn rational_not_equal_to_nearest_float() {
        let q = ok_or_fail!(Real::reduce(1, 3));
        let f = Real::Float(1.0 / 3.0);

        assert_ne!(q, f);
        assert_ne!(f, q);
    }

    #[test]
    fn rational_not_equal_to_mismatched_float() {
        let q = ok_or_fail!(Real::reduce(1, 2));
        let f = Real::Float(0.6);

        assert_ne!(q, f);
    }

    #[test]
    fn equal_rationals_with_different_reduction_paths() {
        let a = ok_or_fail!(Real::reduce(4, 8));
        let b = ok_or_fail!(Real::reduce(1, 2));

        assert_eq!(a, b);
    }

    #[test]
    fn different_rationals_not_equal() {
        let a = ok_or_fail!(Real::reduce(4, 5));
        let b = ok_or_fail!(Real::reduce(3, 4));

        assert_ne!(a, b);
    }

    #[test]
    fn different_integers_not_equal() {
        let a = Real::Integer(4.into());
        let b = Real::Integer(5.into());

        assert_ne!(a, b);
    }

    #[test]
    fn reduced_rational_equals_integer_of_same_value() {
        // Real::reduce normalizes 4/2 down to a bare Integer(2), so this
        // is really an Integer-vs-Integer comparison, not a cross-variant one.
        let q = ok_or_fail!(Real::reduce(4, 2));
        let n = Real::Integer(2.into());

        assert_eq!(q, n);
    }

    #[test]
    fn unreduced_rational_not_equal_to_equivalent_integer() {
        // Built directly, bypassing Real::reduce, so the Rational stays 4/2
        // instead of normalizing to Integer(2). PartialEq only coerces
        // through Float; Integer and Rational have no cross-variant case,
        // so this is false even though 4/2 == 2 mathematically.
        let q = Real::Rational(Rational((4.into(), 2.into()).into()));
        let n = Real::Integer(2.into());

        assert_ne!(q, n);
    }

    #[test]
    fn unreduced_rational_equal_to_matching_unreduced_rational() {
        let a = Real::Rational(Rational((4.into(), 2.into()).into()));
        let b = Real::Rational(Rational((4.into(), 2.into()).into()));

        assert_eq!(a, b);
    }

    #[test]
    fn complex_equal() {
        let a = Number::complex(4, 5);
        let b = Number::complex(4, 5);

        assert_eq!(a, b);
    }

    #[test]
    fn complex_not_equal_different_real() {
        let a = Number::complex(4, 5);
        let b = Number::complex(6, 5);

        assert_ne!(a, b);
    }

    #[test]
    fn complex_not_equal_different_imag() {
        let a = Number::complex(4, 5);
        let b = Number::complex(4, 6);

        assert_ne!(a, b);
    }

    #[test]
    fn complex_parts_convert_across_exactness() {
        let r = ok_or_fail!(Real::reduce(1, 2));
        let a = Number::complex(r, 5);
        let b = Number::complex(0.5, 5);

        assert_eq!(a, b);
    }

    #[test]
    fn complex_integer_equal_to_complex_float() {
        let a = Number::complex(4, 5);
        let b = Number::complex(4.0, 5.0);

        assert_eq!(a, b);
    }

    #[test]
    fn complex_infinite_equal_to_itself() {
        let a = Number::complex(f64::INFINITY, f64::NEG_INFINITY);
        let b = Number::complex(f64::INFINITY, f64::NEG_INFINITY);

        assert_eq!(a, b);
    }

    #[test]
    fn complex_mismatched_infinities_not_equal() {
        let a = Number::complex(f64::INFINITY, 5);
        let b = Number::complex(f64::NEG_INFINITY, 5);

        assert_ne!(a, b);
    }

    #[test]
    fn complex_nan_not_equal() {
        let a = Number::complex(f64::NAN, 5);
        let b = Number::complex(f64::NAN, 5);

        assert_ne!(a, b);
    }

    #[test]
    fn complex_not_equal_to_real() {
        let z = Number::complex(4, 5);
        let r = Number::real(4);

        assert_ne!(z, r);
    }

    #[test]
    fn real_equal_across_exactness() {
        let a = Number::real(4);
        let b = Number::real(4.0);

        assert_eq!(a, b);
    }

    #[test]
    fn real_not_equal_different_values() {
        let a = Number::real(4);
        let b = Number::real(5);

        assert_ne!(a, b);
    }
}

mod ordering {
    use super::*;

    #[test]
    fn integer_cmp_matrix() {
        let cases = [
            (0, 0, Ordering::Equal),
            (5, 5, Ordering::Equal),
            (-5, -5, Ordering::Equal),
            (3, 5, Ordering::Less),
            (5, 3, Ordering::Greater),
            (-5, -3, Ordering::Less),
            (-3, -5, Ordering::Greater),
            (5, -3, Ordering::Greater),
            (-3, 5, Ordering::Less),
            (0, 5, Ordering::Less),
            (5, 0, Ordering::Greater),
            (0, -5, Ordering::Greater),
            (-5, 0, Ordering::Less),
            (5, -5, Ordering::Greater),
            (-5, 5, Ordering::Less),
        ];
        for (a, b, expected) in cases {
            let x: Integer = a.into();
            let y: Integer = b.into();

            assert_eq!(x.cmp(&y), expected);
        }
    }

    #[test]
    fn integer_partial_cmp_matches_cmp() {
        let cases = [(0, 0), (3, 5), (5, 3), (-5, -3), (-3, -5), (5, -5)];
        for (a, b) in cases {
            let x: Integer = a.into();
            let y: Integer = b.into();

            assert_eq!(x.partial_cmp(&y), Some(x.cmp(&y)));
        }
    }

    #[test]
    fn integer_large_magnitude() {
        let max = Integer::new(u64::MAX, Sign::Positive);
        let min = Integer::new(u64::MAX, Sign::Negative);

        assert_eq!(max.cmp(&min), Ordering::Greater);
        assert_eq!(min.cmp(&max), Ordering::Less);
        assert_eq!(max.cmp(&max), Ordering::Equal);
        assert_eq!(min.cmp(&min), Ordering::Equal);
    }

    #[test]
    fn float_finite_ordering() {
        let cases = [
            (1.0, 2.0, Some(Ordering::Less)),
            (2.0, 1.0, Some(Ordering::Greater)),
            (2.0, 2.0, Some(Ordering::Equal)),
            (0.0, -0.0, Some(Ordering::Equal)),
            (-2.0, -1.0, Some(Ordering::Less)),
        ];
        for (a, b, expected) in cases {
            let x = Real::Float(a);
            let y = Real::Float(b);

            assert_eq!(x.partial_cmp(&y), expected);
        }
    }

    #[test]
    fn float_infinite_ordering() {
        let cases = [
            (f64::INFINITY, 5.0, Some(Ordering::Greater)),
            (5.0, f64::INFINITY, Some(Ordering::Less)),
            (f64::NEG_INFINITY, 5.0, Some(Ordering::Less)),
            (5.0, f64::NEG_INFINITY, Some(Ordering::Greater)),
            (f64::INFINITY, f64::INFINITY, Some(Ordering::Equal)),
            (f64::NEG_INFINITY, f64::NEG_INFINITY, Some(Ordering::Equal)),
            (f64::INFINITY, f64::NEG_INFINITY, Some(Ordering::Greater)),
            (f64::NEG_INFINITY, f64::INFINITY, Some(Ordering::Less)),
        ];
        for (a, b, expected) in cases {
            let x = Real::Float(a);
            let y = Real::Float(b);

            assert_eq!(x.partial_cmp(&y), expected);
        }
    }

    #[test]
    fn float_nan_is_unordered() {
        let cases = [
            (f64::NAN, 5.0),
            (5.0, f64::NAN),
            (f64::NAN, f64::NAN),
            (f64::NAN, f64::INFINITY),
            (f64::INFINITY, f64::NAN),
            (f64::NAN, f64::NEG_INFINITY),
        ];
        for (a, b) in cases {
            let x = Real::Float(a);
            let y = Real::Float(b);

            assert_eq!(x.partial_cmp(&y), None);
        }
    }

    #[test]
    fn float_vs_integer_float_first() {
        let cases = [
            (3.0, 5, Some(Ordering::Less)),
            (5.0, 3, Some(Ordering::Greater)),
            (5.0, 5, Some(Ordering::Equal)),
            (3.0, -5, Some(Ordering::Greater)),
            (-3.0, 5, Some(Ordering::Less)),
            (0.0, 0, Some(Ordering::Equal)),
            (-0.0, 0, Some(Ordering::Equal)),
        ];
        for (f, n, expected) in cases {
            let x = Real::Float(f);
            let y = Real::Integer(n.into());

            assert_eq!(x.partial_cmp(&y), expected);
        }
    }

    #[test]
    fn float_vs_integer_integer_first() {
        let cases = [
            (5, 3.0, Some(Ordering::Greater)),
            (3, 5.0, Some(Ordering::Less)),
            (5, 5.0, Some(Ordering::Equal)),
            (-5, 3.0, Some(Ordering::Less)),
            (5, -3.0, Some(Ordering::Greater)),
            (0, 0.0, Some(Ordering::Equal)),
            (0, -0.0, Some(Ordering::Equal)),
        ];
        for (n, f, expected) in cases {
            let x = Real::Integer(n.into());
            let y = Real::Float(f);

            assert_eq!(x.partial_cmp(&y), expected);
        }
    }

    #[test]
    fn float_vs_integer_infinite() {
        let inf = Real::Float(f64::INFINITY);
        let neg_inf = Real::Float(f64::NEG_INFINITY);
        let n = Real::Integer(5.into());

        assert_eq!(inf.partial_cmp(&n), Some(Ordering::Greater));
        assert_eq!(n.partial_cmp(&inf), Some(Ordering::Less));
        assert_eq!(neg_inf.partial_cmp(&n), Some(Ordering::Less));
        assert_eq!(n.partial_cmp(&neg_inf), Some(Ordering::Greater));
    }

    #[test]
    fn float_vs_integer_nan_is_unordered() {
        let nan = Real::Float(f64::NAN);
        let n = Real::Integer(5.into());

        assert_eq!(nan.partial_cmp(&n), None);
        assert_eq!(n.partial_cmp(&nan), None);
    }

    #[test]
    fn integer_vs_integer_ordering() {
        let cases = [
            (3, 5, Some(Ordering::Less)),
            (5, 3, Some(Ordering::Greater)),
            (5, 5, Some(Ordering::Equal)),
            (-5, 3, Some(Ordering::Less)),
            (5, -3, Some(Ordering::Greater)),
            (-5, -3, Some(Ordering::Less)),
            (0, 0, Some(Ordering::Equal)),
        ];
        for (a, b, expected) in cases {
            let x = Real::Integer(a.into());
            let y = Real::Integer(b.into());

            assert_eq!(x.partial_cmp(&y), expected);
        }
    }

    #[test]
    fn rational_cmp_matrix() {
        let cases = [
            ((1, 2), (3, 4), Some(Ordering::Less)),
            ((3, 4), (1, 2), Some(Ordering::Greater)),
            ((1, 2), (1, 2), Some(Ordering::Equal)),
            ((-1, 2), (1, 2), Some(Ordering::Less)),
            ((-3, 4), (-1, 2), Some(Ordering::Less)),
            ((-1, 2), (-3, 4), Some(Ordering::Greater)),
            ((1, 2), (-1, 2), Some(Ordering::Greater)),
        ];
        for ((an, ad), (bn, bd), expected) in cases {
            let a = ok_or_fail!(Real::reduce(an, ad));
            let b = ok_or_fail!(Real::reduce(bn, bd));

            assert_eq!(a.partial_cmp(&b), expected);
        }
    }

    #[test]
    fn rational_cmp_matches_cross_multiplication_definition() {
        // a/b < c/d iff a*d < c*b -- the textbook definition of comparing
        // fractions, computed here independently of Ord for Rational.
        let cases = [
            ((1, 2), (3, 4)),
            ((-3, 4), (-1, 2)),
            ((5, 6), (5, 6)),
            ((7, 3), (2, 5)),
        ];
        for ((an, ad), (bn, bd)) in cases {
            let a = ok_or_fail!(Real::reduce(an, ad));
            let b = ok_or_fail!(Real::reduce(bn, bd));

            let expected = (an * bd).cmp(&(bn * ad));

            assert_eq!(a.partial_cmp(&b), Some(expected));
        }
    }

    #[test]
    fn equal_value_different_construction_orders_as_equal() {
        let a = ok_or_fail!(Real::reduce(4, 8));
        let b = ok_or_fail!(Real::reduce(1, 2));

        assert_eq!(a.partial_cmp(&b), Some(Ordering::Equal));
    }

    #[test]
    fn rational_vs_integer_ordering() {
        let cases = [
            ((3, 2), 1, Some(Ordering::Greater)),
            ((3, 2), 2, Some(Ordering::Less)),
            ((-3, 2), -1, Some(Ordering::Less)),
            ((5, 2), 2, Some(Ordering::Greater)),
        ];
        for ((qn, qd), n, expected) in cases {
            let q = ok_or_fail!(Real::reduce(qn, qd));
            let i = Real::Integer(n.into());

            assert_eq!(q.partial_cmp(&i), expected);
        }
    }

    #[test]
    fn integer_vs_rational_ordering() {
        let cases = [
            (1, (3, 2), Some(Ordering::Less)),
            (2, (3, 2), Some(Ordering::Greater)),
            (-1, (-3, 2), Some(Ordering::Greater)),
            (2, (5, 2), Some(Ordering::Less)),
        ];
        for (n, (qn, qd), expected) in cases {
            let i = Real::Integer(n.into());
            let q = ok_or_fail!(Real::reduce(qn, qd));

            assert_eq!(i.partial_cmp(&q), expected);
        }
    }

    #[test]
    fn rational_vs_float_ordering() {
        let q = ok_or_fail!(Real::reduce(1, 2));

        assert_eq!(q.partial_cmp(&Real::Float(0.5)), Some(Ordering::Equal));
        assert_eq!(Real::Float(0.5).partial_cmp(&q), Some(Ordering::Equal));
        assert_eq!(q.partial_cmp(&Real::Float(0.4)), Some(Ordering::Greater));
        assert_eq!(
            q.partial_cmp(&Real::Float(f64::INFINITY)),
            Some(Ordering::Less)
        );
        assert_eq!(
            q.partial_cmp(&Real::Float(f64::NEG_INFINITY)),
            Some(Ordering::Greater)
        );
    }

    #[test]
    fn rational_vs_nan_is_unordered() {
        let q = ok_or_fail!(Real::reduce(1, 2));

        assert_eq!(q.partial_cmp(&Real::Float(f64::NAN)), None);
        assert_eq!(Real::Float(f64::NAN).partial_cmp(&q), None);
    }

    #[test]
    fn rational_ordering_is_antisymmetric() {
        let cases = [((1, 2), (3, 4)), ((-1, 3), (1, 3)), ((5, 6), (5, 6))];
        for ((an, ad), (bn, bd)) in cases {
            let a = ok_or_fail!(Real::reduce(an, ad));
            let b = ok_or_fail!(Real::reduce(bn, bd));

            assert_eq!(a.partial_cmp(&b), b.partial_cmp(&a).map(Ordering::reverse));
        }
    }

    #[test]
    fn rational_ordering_is_transitive() {
        let a = ok_or_fail!(Real::reduce(1, 3));
        let b = ok_or_fail!(Real::reduce(1, 2));
        let c = ok_or_fail!(Real::reduce(2, 3));

        assert_eq!(a.partial_cmp(&b), Some(Ordering::Less));
        assert_eq!(b.partial_cmp(&c), Some(Ordering::Less));
        assert_eq!(a.partial_cmp(&c), Some(Ordering::Less));
    }

    #[test]
    #[ignore = "multi-precision multiplication not yet implemented"]
    fn cross_product_overflows_precision() {
        // a/7 vs b/11 cross-multiplies to (a*11) vs (b*7), both overflowing
        // u64::MAX; multi-precision integers would be needed to compare
        // them.
        let big: Integer = (Sign::Positive, u64::MAX).into();
        let a = ok_or_fail!(Real::reduce(big.clone(), 7));
        let b = ok_or_fail!(Real::reduce(big, 11));

        assert_eq!(a.partial_cmp(&b), Some(Ordering::Greater));
    }
}

mod add {
    use super::*;

    #[test]
    fn integer_matrix() {
        let cases = [
            (2, 3, "5"),
            (0, 5, "5"),
            (5, 0, "5"),
            (0, 0, "0"),
            (-2, -3, "-5"),
            (5, -3, "2"),
            (3, -5, "-2"),
            (-5, 3, "-2"),
            (-3, 5, "2"),
            (5, -5, "0"),
            (-5, 5, "0"),
        ];
        for (a, b, expected) in cases {
            let sum = Number::real(a) + Number::real(b);

            assert_eq!(sum.to_string(), expected);
        }
    }

    #[test]
    fn integer_sum_stays_exact() {
        let sum = Number::real(2) + Number::real(3);

        let n = extract_or_fail!(sum, Number::Real);
        assert_matches!(n, Real::Integer(_));
    }

    #[test]
    fn integer_sum_beyond_i64_max() {
        let sum = Number::real(i64::MAX) + Number::real(1);

        assert_eq!(sum.to_string(), "9223372036854775808");
    }

    #[test]
    fn integer_sum_beyond_i64_min() {
        let sum = Number::real(i64::MIN) + Number::real(-1);

        assert_eq!(sum.to_string(), "-9223372036854775809");
    }

    #[test]
    fn integer_addition_is_commutative() {
        let cases = [(4, 7), (-4, 7), (4, -7), (-4, -7), (0, 7)];
        for (a, b) in cases {
            let ab = Number::real(a) + Number::real(b);
            let ba = Number::real(b) + Number::real(a);

            assert_eq!(ab.to_string(), ba.to_string());
        }
    }

    #[test]
    fn float_matrix() {
        let cases = [
            (1.5, 2.5, "4.0"),
            (0.1, 0.2, "0.30000000000000004"),
            (-1.5, 0.5, "-1.0"),
            (0.0, -0.0, "0.0"),
            (-0.0, -0.0, "-0.0"),
        ];
        for (a, b, expected) in cases {
            let sum = Number::real(a) + Number::real(b);

            assert_eq!(sum.to_string(), expected);
        }
    }

    #[test]
    fn integer_and_float_either_order() {
        let a = Number::real(2) + Number::real(1.5);
        let b = Number::real(1.5) + Number::real(2);

        assert_eq!(a.to_string(), "3.5");
        assert_eq!(b.to_string(), "3.5");
    }

    #[test]
    fn zero_integer_and_zero_float_either_order() {
        let a = Number::real(0) + Number::real(0.0);
        let b = Number::real(0.0) + Number::real(0);

        assert_eq!(a.to_string(), "0.0");
        assert_eq!(b.to_string(), "0.0");
    }

    #[test]
    fn integer_and_float_cancel_to_inexact_zero() {
        let sum = Number::real(5) + Number::real(-5.0);

        assert_eq!(sum.to_string(), "0.0");
    }

    #[test]
    fn float_sum_is_inexact() {
        let sum = Number::real(2) + Number::real(1.5);

        let n = extract_or_fail!(sum, Number::Real);
        assert_matches!(n, Real::Float(_));
    }

    #[test]
    fn float_infinities() {
        let cases = [
            (f64::INFINITY, 1.0, "+inf.0"),
            (f64::NEG_INFINITY, 1.0, "-inf.0"),
            (f64::INFINITY, f64::INFINITY, "+inf.0"),
        ];
        for (a, b, expected) in cases {
            let sum = Number::real(a) + Number::real(b);

            assert_eq!(sum.to_string(), expected);
        }
    }

    #[test]
    fn opposite_infinities_sum_to_nan() {
        let sum = Number::real(f64::INFINITY) + Number::real(f64::NEG_INFINITY);

        assert!(sum.is_nan());
    }

    #[test]
    fn nan_propagates_as_first_operand() {
        let sum = Number::real(f64::NAN) + Number::real(1.0);

        assert!(sum.is_nan());
    }

    #[test]
    fn nan_propagates_as_second_operand() {
        let sum = Number::real(1.0) + Number::real(f64::NAN);

        assert!(sum.is_nan());
    }

    #[test]
    fn rational_and_float_either_order() {
        let q = ok_or_fail!(Real::reduce(1, 2));
        let a = Number::real(q.clone()) + Number::real(0.5);
        let b = Number::real(0.5) + Number::real(q);

        assert_eq!(a.to_string(), "1.0");
        assert_eq!(b.to_string(), "1.0");
    }

    #[test]
    fn rational_and_float_matrix() {
        let cases = [((3, 4), 0.25, "1.0"), ((-1, 2), 0.5, "0.0")];
        for ((n, d), f, expected) in cases {
            let q = ok_or_fail!(Real::reduce(n, d));
            let sum = Number::real(q) + Number::real(f);

            assert_eq!(sum.to_string(), expected);
        }
    }

    #[test]
    fn rational_and_float_sum_is_inexact() {
        let q = ok_or_fail!(Real::reduce(1, 2));
        let sum = Number::real(q) + Number::real(0.5);

        let n = extract_or_fail!(sum, Number::Real);
        assert_matches!(n, Real::Float(_));
    }

    #[test]
    fn rational_matrix() {
        let cases = [
            ((1, 2), (1, 3), "5/6"),
            ((1, 6), (1, 6), "1/3"),
            ((1, 2), (1, 2), "1"),
            ((3, 4), (1, 4), "1"),
            ((1, 2), (-1, 3), "1/6"),
            ((-1, 2), (-1, 3), "-5/6"),
            ((2, 3), (5, 6), "3/2"),
        ];
        for ((an, ad), (bn, bd), expected) in cases {
            let a = ok_or_fail!(Real::reduce(an, ad));
            let b = ok_or_fail!(Real::reduce(bn, bd));

            let sum = Number::real(a) + Number::real(b);

            assert_eq!(sum.to_string(), expected);
        }
    }

    #[test]
    fn rational_sum_collapsing_to_integer_is_an_integer() {
        let a = ok_or_fail!(Real::reduce(1, 2));
        let b = ok_or_fail!(Real::reduce(1, 2));

        let sum = Number::real(a) + Number::real(b);

        let n = extract_or_fail!(sum, Number::Real);
        assert_matches!(n, Real::Integer(_));
    }

    #[test]
    fn rational_sum_stays_exact() {
        let a = ok_or_fail!(Real::reduce(1, 2));
        let b = ok_or_fail!(Real::reduce(1, 3));

        let sum = Number::real(a) + Number::real(b);

        let n = extract_or_fail!(sum, Number::Real);
        assert!(!matches!(n, Real::Float(_)));
    }

    #[test]
    fn rational_and_integer_either_order() {
        let q = ok_or_fail!(Real::reduce(1, 2));
        let a = Number::real(q.clone()) + Number::real(3);
        let b = Number::real(3) + Number::real(q);

        assert_eq!(a.to_string(), "7/2");
        assert_eq!(b.to_string(), "7/2");
    }

    #[test]
    fn rational_cancels_to_exact_zero() {
        let a = ok_or_fail!(Real::reduce(1, 2));
        let b = ok_or_fail!(Real::reduce(-1, 2));

        let sum = Number::real(a) + Number::real(b);

        assert_eq!(sum.to_string(), "0");
        let n = extract_or_fail!(sum, Number::Real);
        assert_matches!(n, Real::Integer(_));
    }

    #[test]
    fn rational_addition_is_commutative() {
        let cases = [
            ((1, 2), (1, 3)),
            ((1, 6), (1, 6)),
            ((1, 2), (-1, 3)),
            ((2, 3), (5, 6)),
        ];
        for ((an, ad), (bn, bd)) in cases {
            let a = ok_or_fail!(Real::reduce(an, ad));
            let b = ok_or_fail!(Real::reduce(bn, bd));

            let ab = Number::real(a.clone()) + Number::real(b.clone());
            let ba = Number::real(b) + Number::real(a);

            assert_eq!(ab.to_string(), ba.to_string());
        }
    }

    #[test]
    fn rational_addition_is_associative() {
        let cases = [
            ((1, 2), (1, 3), (1, 6)),
            ((1, 4), (1, 6), (1, 3)),
            ((-1, 2), (1, 3), (1, 5)),
        ];
        for ((an, ad), (bn, bd), (cn, cd)) in cases {
            let a = ok_or_fail!(Real::reduce(an, ad));
            let b = ok_or_fail!(Real::reduce(bn, bd));
            let c = ok_or_fail!(Real::reduce(cn, cd));

            let ab_c =
                (Number::real(a.clone()) + Number::real(b.clone())) + Number::real(c.clone());
            let a_bc = Number::real(a) + (Number::real(b) + Number::real(c));

            assert_eq!(ab_c.to_string(), a_bc.to_string());
        }
    }

    #[test]
    fn shared_denominator_avoids_intermediate_overflow() {
        let big: Integer = (Sign::Positive, u64::MAX).into();
        let a = ok_or_fail!(Real::reduce(1, big.clone()));
        let b = ok_or_fail!(Real::reduce(1, big));

        let sum = Number::real(a) + Number::real(b);

        assert_eq!(sum.to_string(), "2/18446744073709551615");
    }

    #[test]
    #[ignore = "multi-precision lcm not yet implemented"]
    fn coprime_denominators_overflow_precision() {
        // 7 shares no factor with u64::MAX (== 3*5*17*257*65537*641*6700417),
        // so lcm(big, 7) == big*7, which overflows u64 precision.
        let big: Integer = (Sign::Positive, u64::MAX).into();
        let a = ok_or_fail!(Real::reduce(1, big));
        let b = ok_or_fail!(Real::reduce(1, 7));

        let sum = Number::real(a) + Number::real(b);

        assert_matches!(sum, Number::Real(Real::Rational(_)));
    }

    #[test]
    fn complex_plus_complex() {
        let a = Number::complex(3, 2);
        let b = Number::complex(1, 4);

        let sum = a + b;

        assert_eq!(sum.to_string(), "4+6i");
    }

    #[test]
    fn complex_imaginary_parts_cancel_to_real() {
        let a = Number::complex(3, 2);
        let b = Number::complex(1, -2);

        let sum = a + b;

        assert_eq!(sum.to_string(), "4");
        assert_matches!(sum, Number::Real(_));
    }

    #[test]
    fn complex_real_parts_cancel() {
        let a = Number::complex(3, 2);
        let b = Number::complex(-3, 4);

        let sum = a + b;

        assert_eq!(sum.to_string(), "+6i");
    }

    #[test]
    fn complex_fully_cancels_to_zero() {
        let a = Number::complex(3, 2);
        let b = Number::complex(-3, -2);

        let sum = a + b;

        assert_eq!(sum.to_string(), "0");
    }

    #[test]
    fn complex_inexact_imaginary_cancels_to_inexact_zero_stays_complex() {
        let a = Number::complex(3, 2.0);
        let b = Number::complex(1, -2.0);

        let sum = a + b;

        assert_eq!(sum.to_string(), "4+0.0i");
        assert_matches!(sum, Number::Complex(_));
    }

    #[test]
    fn complex_plus_exact_real_either_order() {
        let a = Number::complex(3, 2) + Number::real(5);
        let b = Number::real(5) + Number::complex(3, 2);

        assert_eq!(a.to_string(), "8+2i");
        assert_eq!(b.to_string(), "8+2i");
    }

    #[test]
    fn complex_plus_inexact_real_keeps_mixed_exactness() {
        let sum = Number::complex(3, 2) + Number::real(1.5);

        assert_eq!(sum.to_string(), "4.5+2i");
    }

    #[test]
    fn all_inexact_complex() {
        let a = Number::complex(3.0, 2.0);
        let b = Number::complex(1.0, 1.0);

        let sum = a + b;

        assert_eq!(sum.to_string(), "4.0+3.0i");
    }

    #[test]
    fn zero_is_exact_integer() {
        let z = Number::zero();

        assert_eq!(z.to_string(), "0");
        assert_matches!(z, Number::Real(Real::Integer(_)));
    }

    #[test]
    fn zero_is_additive_identity_for_integer() {
        let sum = Number::zero() + Number::real(7);

        assert_eq!(sum.to_string(), "7");
    }

    #[test]
    fn zero_is_additive_identity_for_float() {
        let sum = Number::zero() + Number::real(4.5);

        assert_eq!(sum.to_string(), "4.5");
    }

    #[test]
    fn zero_is_additive_identity_for_complex() {
        let sum = Number::zero() + Number::complex(3, 2);

        assert_eq!(sum.to_string(), "3+2i");
    }
}

mod sub {
    use super::*;

    #[test]
    fn integer_matrix() {
        let cases = [
            (5, 3, "2"),
            (3, 5, "-2"),
            (0, 5, "-5"),
            (5, 0, "5"),
            (0, 0, "0"),
            (-2, -3, "1"),
            (-3, -2, "-1"),
            (5, -3, "8"),
            (3, -5, "8"),
            (-5, 3, "-8"),
            (5, 5, "0"),
            (-5, -5, "0"),
            (-5, 5, "-10"),
        ];
        for (a, b, expected) in cases {
            let diff = Number::real(a) - Number::real(b);

            assert_eq!(diff.to_string(), expected);
        }
    }

    #[test]
    fn integer_difference_stays_exact() {
        let diff = Number::real(5) - Number::real(3);

        let n = extract_or_fail!(diff, Number::Real);
        assert_matches!(n, Real::Integer(_));
    }

    #[test]
    fn integer_difference_beyond_i64_max() {
        let diff = Number::real(i64::MAX) - Number::real(-1);

        assert_eq!(diff.to_string(), "9223372036854775808");
    }

    #[test]
    fn integer_difference_beyond_i64_min() {
        let diff = Number::real(i64::MIN) - Number::real(1);

        assert_eq!(diff.to_string(), "-9223372036854775809");
    }

    #[test]
    fn equal_magnitude_difference_has_zero_sign() {
        let cases = [(5, 5), (-5, -5)];
        for (a, b) in cases {
            let diff = Integer::from(a) - Integer::from(b);

            assert_eq!(diff.sign, Sign::Zero);
        }
    }

    #[test]
    #[ignore = "multi-precision subtraction not yet implemented"]
    fn integer_difference_overflows_precision() {
        let a = Integer::new(u64::MAX, Sign::Positive);
        let b = Integer::from(-1);

        let diff = a - b;

        assert_eq!(diff.to_string(), "18446744073709551616");
    }

    #[test]
    #[ignore = "multi-precision subtraction not yet implemented"]
    fn multi_precision() {
        let a = Integer {
            precision: Precision::Multiple([4, 6].into()),
            sign: Sign::Positive,
        };
        let b = Integer::from(4);

        let diff = a - b;

        assert_eq!(extract_or_fail!(diff.precision, Precision::Single), 0);
        assert_eq!(diff.sign, Sign::Zero);
    }

    #[test]
    fn float_matrix() {
        let cases = [
            (2.5, 1.5, "1.0"),
            (0.3, 0.1, "0.19999999999999998"),
            (-1.5, 0.5, "-2.0"),
            (1.5, -0.5, "2.0"),
            (0.0, 0.0, "0.0"),
            (0.0, -0.0, "0.0"),
            (-0.0, 0.0, "-0.0"),
            (-0.0, -0.0, "0.0"),
        ];
        for (a, b, expected) in cases {
            let diff = Number::real(a) - Number::real(b);

            assert_eq!(diff.to_string(), expected);
        }
    }

    #[test]
    fn float_difference_is_inexact() {
        let diff = Number::real(2.5) - Number::real(1.5);

        let n = extract_or_fail!(diff, Number::Real);
        assert_matches!(n, Real::Float(_));
    }

    #[test]
    fn float_infinities() {
        let cases = [
            (f64::INFINITY, 1.0, "+inf.0"),
            (f64::NEG_INFINITY, 1.0, "-inf.0"),
            (1.0, f64::INFINITY, "-inf.0"),
            (f64::INFINITY, f64::NEG_INFINITY, "+inf.0"),
            (f64::NEG_INFINITY, f64::INFINITY, "-inf.0"),
        ];
        for (a, b, expected) in cases {
            let diff = Number::real(a) - Number::real(b);

            assert_eq!(diff.to_string(), expected);
        }
    }

    #[test]
    fn like_infinities_difference_is_nan() {
        let cases = [
            (f64::INFINITY, f64::INFINITY),
            (f64::NEG_INFINITY, f64::NEG_INFINITY),
        ];
        for (a, b) in cases {
            let diff = Number::real(a) - Number::real(b);

            assert!(diff.is_nan());
        }
    }

    #[test]
    fn nan_propagates_as_first_operand() {
        let diff = Number::real(f64::NAN) - Number::real(1.0);

        assert!(diff.is_nan());
    }

    #[test]
    fn nan_propagates_as_second_operand() {
        let diff = Number::real(1.0) - Number::real(f64::NAN);

        assert!(diff.is_nan());
    }

    #[test]
    fn integer_and_float_either_order() {
        let a = Number::real(2) - Number::real(1.5);
        let b = Number::real(1.5) - Number::real(2);

        assert_eq!(a.to_string(), "0.5");
        assert_eq!(b.to_string(), "-0.5");
    }

    #[test]
    fn mixed_difference_is_inexact() {
        let diff = Number::real(2) - Number::real(1.5);

        let n = extract_or_fail!(diff, Number::Real);
        assert_matches!(n, Real::Float(_));
    }

    #[test]
    fn integer_and_float_cancel_to_inexact_zero() {
        let diff = Number::real(5) - Number::real(5.0);

        assert_eq!(diff.to_string(), "0.0");
    }

    // exact zero acts as a pure sign flip here, not an IEEE coercion to
    // 0.0 - 0.0 == 0.0; this is the sub equivalent of exact zero's additive identity
    #[test]
    fn exact_zero_minus_inexact_zero_flips_sign() {
        let diff = Number::real(0) - Number::real(0.0);

        assert_eq!(diff.to_string(), "-0.0");
    }

    #[test]
    fn exact_zero_minus_negative_inexact_zero() {
        let diff = Number::real(0) - Number::real(-0.0);

        assert_eq!(diff.to_string(), "0.0");
    }

    #[test]
    fn exact_zero_minus_infinity_negates() {
        let diff = Number::real(0) - Number::real(f64::INFINITY);

        assert_eq!(diff.to_string(), "-inf.0");
    }

    #[test]
    fn inexact_zero_minus_exact_zero_preserves_sign() {
        let cases = [(0.0, "0.0"), (-0.0, "-0.0")];
        for (f, expected) in cases {
            let diff = Number::real(f) - Number::real(0);

            assert_eq!(diff.to_string(), expected);
        }
    }

    #[test]
    fn rational_matrix() {
        let cases = [
            ((1, 2), (1, 3), "1/6"),
            ((1, 3), (1, 2), "-1/6"),
            ((1, 6), (1, 6), "0"),
            ((3, 4), (1, 4), "1/2"),
            ((1, 2), (-1, 3), "5/6"),
            ((-1, 2), (-1, 3), "-1/6"),
            ((2, 3), (5, 6), "-1/6"),
            ((5, 6), (2, 3), "1/6"),
        ];
        for ((an, ad), (bn, bd), expected) in cases {
            let a = ok_or_fail!(Real::reduce(an, ad));
            let b = ok_or_fail!(Real::reduce(bn, bd));

            let diff = Number::real(a) - Number::real(b);

            assert_eq!(diff.to_string(), expected);
        }
    }

    #[test]
    fn rational_difference_collapsing_to_integer_is_an_integer() {
        let a = ok_or_fail!(Real::reduce(3, 2));
        let b = ok_or_fail!(Real::reduce(1, 2));

        let diff = Number::real(a) - Number::real(b);

        let n = extract_or_fail!(diff, Number::Real);
        assert_matches!(n, Real::Integer(_));
    }

    #[test]
    fn rational_difference_stays_exact() {
        let a = ok_or_fail!(Real::reduce(1, 2));
        let b = ok_or_fail!(Real::reduce(1, 3));

        let diff = Number::real(a) - Number::real(b);

        let n = extract_or_fail!(diff, Number::Real);
        assert!(!matches!(n, Real::Float(_)));
    }

    #[test]
    fn rational_cancels_to_exact_zero() {
        let a = ok_or_fail!(Real::reduce(1, 2));
        let b = ok_or_fail!(Real::reduce(1, 2));

        let diff = Number::real(a) - Number::real(b);

        assert_eq!(diff.to_string(), "0");
        let n = extract_or_fail!(diff, Number::Real);
        assert_matches!(n, Real::Integer(_));
    }

    #[test]
    fn rational_and_integer_either_order() {
        let q = ok_or_fail!(Real::reduce(1, 2));
        let a = Number::real(q.clone()) - Number::real(3);
        let b = Number::real(3) - Number::real(q);

        assert_eq!(a.to_string(), "-5/2");
        assert_eq!(b.to_string(), "5/2");
    }

    #[test]
    fn rational_and_float_either_order() {
        let q = ok_or_fail!(Real::reduce(1, 2));
        let a = Number::real(q.clone()) - Number::real(0.25);
        let b = Number::real(0.25) - Number::real(q);

        assert_eq!(a.to_string(), "0.25");
        assert_eq!(b.to_string(), "-0.25");
    }

    #[test]
    fn rational_and_float_difference_is_inexact() {
        let q = ok_or_fail!(Real::reduce(1, 2));
        let diff = Number::real(q) - Number::real(0.25);

        let n = extract_or_fail!(diff, Number::Real);
        assert_matches!(n, Real::Float(_));
    }

    #[test]
    fn shared_denominator_avoids_intermediate_overflow() {
        let big: Integer = (Sign::Positive, u64::MAX).into();
        let a = ok_or_fail!(Real::reduce(2, big.clone()));
        let b = ok_or_fail!(Real::reduce(1, big));

        let diff = Number::real(a) - Number::real(b);

        assert_eq!(diff.to_string(), "1/18446744073709551615");
    }

    #[test]
    #[ignore = "multi-precision lcm not yet implemented"]
    fn coprime_denominators_overflow_precision() {
        // 7 shares no factor with u64::MAX (== 3*5*17*257*65537*641*6700417),
        // so lcm(big, 7) == big*7, which overflows u64 precision.
        let big: Integer = (Sign::Positive, u64::MAX).into();
        let a = ok_or_fail!(Real::reduce(1, big));
        let b = ok_or_fail!(Real::reduce(1, 7));

        let diff = Number::real(a) - Number::real(b);

        assert_matches!(diff, Number::Real(Real::Rational(_)));
    }

    #[test]
    fn complex_minus_complex() {
        let a = Number::complex(3, 2);
        let b = Number::complex(1, 4);

        let diff = a - b;

        assert_eq!(diff.to_string(), "2-2i");
    }

    // Add for Number uses an order-erasing (Complex, n) | (n, Complex) pattern,
    // since addition is commutative; Sub must not do the same, or this would
    // wrongly come out as -2+2i.
    #[test]
    fn real_minus_complex_subtracts_in_operand_order() {
        let diff = Number::real(5) - Number::complex(3, 2);

        assert_eq!(diff.to_string(), "2-2i");
    }

    #[test]
    fn complex_minus_real() {
        let diff = Number::complex(3, 2) - Number::real(5);

        assert_eq!(diff.to_string(), "-2+2i");
    }

    #[test]
    fn complex_and_real_are_anticommutative() {
        let z = Number::complex(3, 2);
        let a = Number::real(5) - z.clone();
        let b = -(z - Number::real(5));

        assert_eq!(a.to_string(), b.to_string());
    }

    #[test]
    fn complex_imaginary_parts_cancel_to_real() {
        let a = Number::complex(3, 2);
        let b = Number::complex(1, 2);

        let diff = a - b;

        assert_eq!(diff.to_string(), "2");
        assert_matches!(diff, Number::Real(_));
    }

    #[test]
    fn complex_real_parts_cancel() {
        let a = Number::complex(3, 2);
        let b = Number::complex(3, 4);

        let diff = a - b;

        assert_eq!(diff.to_string(), "-2i");
    }

    #[test]
    fn complex_fully_cancels_to_zero() {
        let a = Number::complex(3, 2);
        let b = Number::complex(3, 2);

        let diff = a - b;

        assert_eq!(diff.to_string(), "0");
    }

    #[test]
    fn complex_inexact_imaginary_cancels_to_inexact_zero_stays_complex() {
        let a = Number::complex(3, 2.0);
        let b = Number::complex(1, 2.0);

        let diff = a - b;

        assert_eq!(diff.to_string(), "2+0.0i");
        assert_matches!(diff, Number::Complex(_));
    }

    #[test]
    fn complex_minus_inexact_real_keeps_mixed_exactness() {
        let diff = Number::complex(3, 2) - Number::real(1.5);

        assert_eq!(diff.to_string(), "1.5+2i");
    }

    #[test]
    fn all_inexact_complex() {
        let a = Number::complex(3.0, 2.0);
        let b = Number::complex(1.0, 1.0);

        let diff = a - b;

        assert_eq!(diff.to_string(), "2.0+1.0i");
    }

    #[test]
    fn zero_minus_complex_is_negation() {
        let diff = Number::zero() - Number::complex(3, 2);

        assert_eq!(diff.to_string(), "-3-2i");
    }

    #[test]
    fn zero_is_right_identity() {
        let q = ok_or_fail!(Real::reduce(1, 2));
        let cases = [
            Number::real(7),
            Number::real(4.5),
            Number::real(q),
            Number::complex(3, 2),
        ];
        for a in cases {
            let diff = a.clone() - Number::zero();

            assert_eq!(diff.to_string(), a.to_string());
        }
    }

    #[test]
    fn zero_minus_operand_is_negation() {
        let q = ok_or_fail!(Real::reduce(1, 2));
        let cases = [
            Number::real(7),
            Number::real(4.5),
            Number::real(q),
            Number::complex(3, 2),
        ];
        for a in cases {
            let diff = Number::zero() - a.clone();

            assert_eq!(diff.to_string(), (-a).to_string());
        }
    }

    #[test]
    fn self_subtraction_is_zero() {
        let cases = [
            (Number::real(7), "0"),
            (Number::real(1.5), "0.0"),
            (Number::real(ok_or_fail!(Real::reduce(1, 2))), "0"),
            (Number::complex(3, 4), "0"),
        ];
        for (x, expected) in cases {
            let diff = x.clone() - x;

            assert_eq!(diff.to_string(), expected);
        }
    }

    #[test]
    fn subtraction_is_addition_of_negation() {
        let q = ok_or_fail!(Real::reduce(1, 2));
        let cases: [(Number, Number); 7] = [
            (Number::real(5), Number::real(3)),
            (Number::real(5), Number::real(1.5)),
            (Number::real(1.5), Number::real(5)),
            (Number::real(q), Number::real(3)),
            (Number::complex(3, 2), Number::real(5)),
            (Number::real(5), Number::complex(3, 2)),
            (Number::real(0), Number::real(0.0)),
        ];
        for (a, b) in cases {
            let via_sub = a.clone() - b.clone();
            let via_add_neg = a + (-b);

            assert_eq!(via_sub.to_string(), via_add_neg.to_string());
        }
    }

    #[test]
    fn subtraction_is_anticommutative() {
        // exact operands only: float cases legitimately differ in zero sign,
        // e.g. 1.5 - 1.5 == 0.0 but -(1.5 - 1.5) == -0.0.
        let q = ok_or_fail!(Real::reduce(1, 2));
        let cases = [
            (Number::real(5), Number::real(3)),
            (Number::real(q), Number::real(3)),
            (Number::complex(3, 2), Number::real(5)),
        ];
        for (a, b) in cases {
            let ab = a.clone() - b.clone();
            let neg_ba = -(b - a);

            assert_eq!(ab.to_string(), neg_ba.to_string());
        }
    }

    #[test]
    fn subtraction_is_not_associative() {
        let ab_c = (Number::real(10) - Number::real(3)) - Number::real(2);
        let a_bc = Number::real(10) - (Number::real(3) - Number::real(2));

        assert_eq!(ab_c.to_string(), "5");
        assert_eq!(a_bc.to_string(), "9");
    }
}

mod mult {
    use super::*;

    #[test]
    fn integer_matrix() {
        let cases = [
            (2, 3, "6"),
            (-2, 3, "-6"),
            (2, -3, "-6"),
            (-2, -3, "6"),
            (0, 5, "0"),
            (5, 0, "0"),
            (0, 0, "0"),
            (1, 7, "7"),
            (-1, 7, "-7"),
        ];
        for (a, b, expected) in cases {
            let product = Number::real(a) * Number::real(b);

            assert_eq!(product.to_string(), expected);
        }
    }

    #[test]
    fn integer_product_stays_exact() {
        let product = Number::real(2) * Number::real(3);

        let n = extract_or_fail!(product, Number::Real);
        assert_matches!(n, Real::Integer(_));
    }

    #[test]
    fn integer_product_beyond_i64_max() {
        let product = Number::real(i64::MAX) * Number::real(2);

        assert_eq!(product.to_string(), "18446744073709551614");
    }

    #[test]
    fn integer_multiplication_is_commutative() {
        let cases = [(4, 7), (-4, 7), (4, -7), (-4, -7), (0, 7)];
        for (a, b) in cases {
            let ab = Number::real(a) * Number::real(b);
            let ba = Number::real(b) * Number::real(a);

            assert_eq!(ab.to_string(), ba.to_string());
        }
    }

    #[test]
    fn integer_multiplication_is_associative() {
        let ab_c = (Number::real(2) * Number::real(3)) * Number::real(4);
        let a_bc = Number::real(2) * (Number::real(3) * Number::real(4));

        assert_eq!(ab_c.to_string(), a_bc.to_string());
    }

    #[test]
    #[ignore = "multi-precision multiplication not yet implemented"]
    fn integer_product_overflows_precision() {
        let product = Number::real(i64::MIN) * Number::real(2);

        assert_eq!(product.to_string(), "18446744073709551616");
    }

    #[test]
    fn float_matrix() {
        let cases = [
            (1.5, 2.0, "3.0"),
            (0.1, 0.2, "0.020000000000000004"),
            (-1.5, 2.0, "-3.0"),
            (0.0, -0.0, "-0.0"),
            (-0.0, -0.0, "0.0"),
        ];
        for (a, b, expected) in cases {
            let product = Number::real(a) * Number::real(b);

            assert_eq!(product.to_string(), expected);
        }
    }

    #[test]
    fn float_infinities() {
        let cases = [
            (f64::INFINITY, 1.0, "+inf.0"),
            (f64::NEG_INFINITY, 1.0, "-inf.0"),
            (f64::INFINITY, f64::INFINITY, "+inf.0"),
            (f64::INFINITY, f64::NEG_INFINITY, "-inf.0"),
        ];
        for (a, b, expected) in cases {
            let product = Number::real(a) * Number::real(b);

            assert_eq!(product.to_string(), expected);
        }
    }

    #[test]
    fn nan_propagates_as_first_operand() {
        let product = Number::real(f64::NAN) * Number::real(2.0);

        assert!(product.is_nan());
    }

    #[test]
    fn nan_propagates_as_second_operand() {
        let product = Number::real(2.0) * Number::real(f64::NAN);

        assert!(product.is_nan());
    }

    #[test]
    fn inexact_zero_times_infinity_is_nan() {
        let product = Number::real(0.0) * Number::real(f64::INFINITY);

        assert!(product.is_nan());
    }

    #[test]
    fn integer_and_float_either_order() {
        let a = Number::real(3) * Number::real(1.5);
        let b = Number::real(1.5) * Number::real(3);

        assert_eq!(a.to_string(), "4.5");
        assert_eq!(b.to_string(), "4.5");
    }

    #[test]
    fn float_product_is_inexact() {
        let product = Number::real(3) * Number::real(1.5);

        let n = extract_or_fail!(product, Number::Real);
        assert_matches!(n, Real::Float(_));
    }

    #[test]
    fn exact_zero_overrides_float_taint() {
        let a = Number::real(0) * Number::real(1.5);
        let b = Number::real(1.5) * Number::real(0);

        assert_eq!(a.to_string(), "0");
        assert!(matches!(
            extract_or_fail!(a, Number::Real),
            Real::Integer(_)
        ));
        assert_eq!(b.to_string(), "0");
        assert!(matches!(
            extract_or_fail!(b, Number::Real),
            Real::Integer(_)
        ));
    }

    #[test]
    fn exact_zero_overrides_infinity() {
        let a = Number::real(0) * Number::real(f64::INFINITY);
        let b = Number::real(f64::INFINITY) * Number::real(0);

        assert_eq!(a.to_string(), "0");
        assert_eq!(b.to_string(), "0");
    }

    #[test]
    fn exact_zero_overrides_nan() {
        let a = Number::real(0) * Number::real(f64::NAN);
        let b = Number::real(f64::NAN) * Number::real(0);

        assert_eq!(a.to_string(), "0");
        assert_eq!(b.to_string(), "0");
    }

    #[test]
    fn inexact_zero_does_not_override() {
        let a = Number::real(0.0) * Number::real(2);
        let b = Number::real(2) * Number::real(0.0);

        assert_eq!(a.to_string(), "0.0");
        assert_eq!(b.to_string(), "0.0");
    }

    #[test]
    fn rational_and_float_either_order() {
        let q = ok_or_fail!(Real::reduce(1, 2));
        let a = Number::real(q.clone()) * Number::real(0.5);
        let b = Number::real(0.5) * Number::real(q);

        assert_eq!(a.to_string(), "0.25");
        assert_eq!(b.to_string(), "0.25");
    }

    #[test]
    fn rational_and_float_product_is_inexact() {
        let q = ok_or_fail!(Real::reduce(1, 2));
        let product = Number::real(q) * Number::real(0.5);

        let n = extract_or_fail!(product, Number::Real);
        assert_matches!(n, Real::Float(_));
    }

    #[test]
    fn rational_matrix() {
        let a = ok_or_fail!(Real::reduce(1, 2));
        let b = ok_or_fail!(Real::reduce(2, 3));
        let product = Number::real(a) * Number::real(b);

        assert_eq!(product.to_string(), "1/3");
    }

    #[test]
    fn integer_and_rational_either_order() {
        let q = ok_or_fail!(Real::reduce(1, 2));
        let a = Number::real(3) * Number::real(q.clone());
        let b = Number::real(q) * Number::real(3);

        assert_eq!(a.to_string(), "3/2");
        assert_eq!(b.to_string(), "3/2");
    }

    #[test]
    fn rational_times_exact_zero() {
        let q = ok_or_fail!(Real::reduce(1, 2));
        let product = Number::real(0) * Number::real(q);

        assert_eq!(product.to_string(), "0");
    }

    #[test]
    fn rational_sign_matrix() {
        let cases = [
            ((1, 2), (1, 3), "1/6"),
            ((-1, 2), (1, 3), "-1/6"),
            ((1, 2), (-1, 3), "-1/6"),
            ((-1, 2), (-1, 3), "1/6"),
        ];
        for ((an, ad), (bn, bd), expected) in cases {
            let a = ok_or_fail!(Real::reduce(an, ad));
            let b = ok_or_fail!(Real::reduce(bn, bd));

            let product = Number::real(a) * Number::real(b);

            assert_eq!(product.to_string(), expected);
        }
    }

    #[test]
    fn rational_product_reduces_to_integer() {
        let a = ok_or_fail!(Real::reduce(2, 3));
        let b = ok_or_fail!(Real::reduce(3, 2));

        let product = Number::real(a) * Number::real(b);

        assert_eq!(product.to_string(), "1");
        assert!(matches!(
            extract_or_fail!(product, Number::Real),
            Real::Integer(_)
        ));
    }

    #[test]
    fn rational_times_integer_reduces_to_integer() {
        let q = ok_or_fail!(Real::reduce(1, 2));

        let product = Number::real(q) * Number::real(4);

        assert_eq!(product.to_string(), "2");
        assert!(matches!(
            extract_or_fail!(product, Number::Real),
            Real::Integer(_)
        ));
    }

    #[test]
    fn rational_multiplication_is_commutative() {
        let cases = [((1, 2), (2, 3)), ((-1, 2), (3, 4)), ((5, 6), (7, 8))];
        for ((an, ad), (bn, bd)) in cases {
            let a = ok_or_fail!(Real::reduce(an, ad));
            let b = ok_or_fail!(Real::reduce(bn, bd));

            let ab = Number::real(a.clone()) * Number::real(b.clone());
            let ba = Number::real(b) * Number::real(a);

            assert_eq!(ab.to_string(), ba.to_string());
        }
    }

    #[test]
    fn rational_multiplication_is_associative() {
        let a = ok_or_fail!(Real::reduce(1, 2));
        let b = ok_or_fail!(Real::reduce(2, 3));
        let c = ok_or_fail!(Real::reduce(3, 4));

        let ab_c = (Number::real(a.clone()) * Number::real(b.clone())) * Number::real(c.clone());
        let a_bc = Number::real(a) * (Number::real(b) * Number::real(c));

        assert_eq!(ab_c.to_string(), a_bc.to_string());
    }

    #[test]
    fn one_is_multiplicative_identity_for_rational() {
        let q = ok_or_fail!(Real::reduce(3, 4));

        let product = Number::one() * Number::real(q);

        assert_eq!(product.to_string(), "3/4");
    }

    #[test]
    fn cross_reduction_avoids_naive_overflow() {
        // a*c here would be big*3, well beyond u64::MAX if computed before
        // cross-reducing by the shared `big` factor; Mul for Rational's
        // gcd-first strategy (src/number.rs:824) avoids it entirely and
        // produces the exact answer.
        let big: Integer = (Sign::Positive, u64::MAX - 1).into();
        let a = ok_or_fail!(Real::reduce(big.clone(), 3));
        let b = ok_or_fail!(Real::reduce(3, big));

        let product = Number::real(a) * Number::real(b);

        assert_eq!(product.to_string(), "1");
    }

    #[test]
    #[ignore = "multi-precision multiplication not yet implemented"]
    fn cross_product_overflows_precision() {
        // both denominators are coprime with both numerators, so no
        // cross-reduction is possible and the numerator computation
        // big*big overflows u64::MAX; multi-precision integers would be
        // needed to carry the result.
        let big: Integer = (Sign::Positive, u64::MAX).into();
        let a = ok_or_fail!(Real::reduce(big.clone(), 7));
        let b = ok_or_fail!(Real::reduce(big, 11));

        let product = Number::real(a) * Number::real(b);

        assert_eq!(
            product.to_string(),
            "340282366920938463426481119284349108225/77"
        );
    }

    #[test]
    fn complex_times_complex() {
        let a = Number::complex(3, 2);
        let b = Number::complex(1, 4);

        let product = a * b;

        assert_eq!(product.to_string(), "-5+14i");
    }

    #[test]
    fn imaginary_unit_squared_is_negative_one() {
        let a = Number::imaginary(1);
        let b = Number::imaginary(1);

        let product = a * b;

        assert_eq!(product.to_string(), "-1");
        assert_matches!(product, Number::Real(_));
    }

    #[test]
    fn conjugates_multiply_to_real() {
        let a = Number::complex(3, 2);
        let b = Number::complex(3, -2);

        let product = a * b;

        assert_eq!(product.to_string(), "13");
        assert_matches!(product, Number::Real(_));
    }

    #[test]
    fn complex_times_exact_real_either_order() {
        let a = Number::complex(3, 2) * Number::real(5);
        let b = Number::real(5) * Number::complex(3, 2);

        assert_eq!(a.to_string(), "15+10i");
        assert_eq!(b.to_string(), "15+10i");
    }

    #[test]
    fn complex_times_inexact_real() {
        let product = Number::complex(3, 2) * Number::real(1.5);

        assert_eq!(product.to_string(), "4.5+3.0i");
    }

    #[test]
    fn complex_times_exact_zero() {
        let product = Number::complex(3, 2) * Number::real(0);

        assert_eq!(product.to_string(), "0");
    }

    #[test]
    fn all_inexact_complex() {
        let a = Number::complex(3.0, 2.0);
        let b = Number::complex(1.0, 1.0);

        let product = a * b;

        assert_eq!(product.to_string(), "1.0+5.0i");
    }

    #[test]
    fn complex_multiplication_is_commutative() {
        let a = Number::complex(3, 2);
        let b = Number::complex(1, 4);

        let ab = a.clone() * b.clone();
        let ba = b * a;

        assert_eq!(ab.to_string(), ba.to_string());
    }

    // Verify Smith's algorithm optimization for division, and specifically the
    // zero-sign preservation, is not applied to multiplication; this aligns with
    // other scheme implementations like Guile and Chez Scheme.
    #[test]
    fn complex_zero_times_complex_does_not_preserve_sign_of_zero() {
        let cases = [
            ((3, 2), "0.0+0.0i"),
            ((-3, 2), "-0.0+0.0i"),
            ((3, -2), "0.0+0.0i"),
            ((-3, -2), "0.0-0.0i"),
        ];
        for ((re, im), expected) in cases {
            let zero = Number::complex(0.0, 0.0);
            let w = Number::complex(re, im);

            let product = zero * w;

            assert_eq!(product.to_string(), expected);
        }
    }

    #[test]
    fn one_is_exact_integer() {
        let one = Number::one();

        assert_eq!(one.to_string(), "1");
        assert_matches!(one, Number::Real(Real::Integer(_)));
    }

    #[test]
    fn one_is_multiplicative_identity_for_integer() {
        let product = Number::one() * Number::real(7);

        assert_eq!(product.to_string(), "7");
    }

    #[test]
    fn one_is_multiplicative_identity_for_float() {
        let product = Number::one() * Number::real(4.5);

        assert_eq!(product.to_string(), "4.5");
    }

    #[test]
    fn one_is_multiplicative_identity_for_complex() {
        let product = Number::one() * Number::complex(3, 2);

        assert_eq!(product.to_string(), "3+2i");
    }

    #[test]
    fn zero_annihilates_integer() {
        let product = Number::zero() * Number::real(7);

        assert_eq!(product.to_string(), "0");
    }

    #[test]
    fn zero_annihilates_float() {
        // exact zero overrides float-taint, so the result stays exact
        let product = Number::zero() * Number::real(4.5);

        assert_eq!(product.to_string(), "0");
    }
}

mod div {
    use super::*;

    mod integer {
        use super::*;

        #[test]
        fn exact_matrix() {
            let cases = [
                (6, 3, "2"),
                (3, 4, "3/4"),
                (-3, 4, "-3/4"),
                (3, -4, "-3/4"),
                (-3, -4, "3/4"),
                (0, 5, "0"),
                (7, 1, "7"),
                (8, -2, "-4"),
                (4, 8, "1/2"),
            ];
            for (a, b, expected) in cases {
                let quotient = ok_or_fail!(Number::real(a) / Number::real(b));

                assert_eq!(quotient.to_string(), expected);
            }
        }

        #[test]
        fn divisible_pair_stays_integer() {
            let quotient = ok_or_fail!(Number::real(6) / Number::real(3));

            let n = extract_or_fail!(quotient, Number::Real);
            assert_matches!(n, Real::Integer(_));
        }

        #[test]
        fn non_divisible_pair_becomes_rational() {
            let quotient = ok_or_fail!(Number::real(3) / Number::real(4));

            let n = extract_or_fail!(quotient, Number::Real);
            assert_matches!(n, Real::Rational(_));
        }

        #[test]
        fn one_is_divisive_identity() {
            let cases = [4, -4, 0, 7];
            for n in cases {
                let quotient = ok_or_fail!(Number::real(n) / Number::one());

                assert_eq!(quotient.to_string(), Number::real(n).to_string());
            }
        }

        #[test]
        fn nonzero_self_division_is_one() {
            let cases = [4, -4, 7, 1];
            for n in cases {
                let quotient = ok_or_fail!(Number::real(n) / Number::real(n));

                assert_eq!(quotient.to_string(), "1");
            }
        }

        #[test]
        fn zero_dividend_is_zero() {
            let cases = [4, -4, 7];
            for n in cases {
                let quotient = ok_or_fail!(Number::zero() / Number::real(n));

                assert_eq!(quotient.to_string(), "0");
            }
        }

        #[test]
        fn division_by_exact_zero_is_an_error() {
            let cases = [4, -4, 0];
            for n in cases {
                let quotient = Number::real(n) / Number::zero();

                let err = err_or_fail!(quotient);
                assert_matches!(err, NumericError::DivideByZero);
            }
        }

        #[test]
        fn not_commutative() {
            let ab = ok_or_fail!(Number::real(3) / Number::real(4));
            let ba = ok_or_fail!(Number::real(4) / Number::real(3));

            assert_ne!(ab.to_string(), ba.to_string());
        }

        #[test]
        fn agrees_with_multiply_by_reciprocal() {
            let cases = [(7, 2), (-7, 2), (7, -2), (1, 3)];
            for (a, b) in cases {
                let quotient = ok_or_fail!(Number::real(a) / Number::real(b));
                let reciprocal_product =
                    Number::real(a) * ok_or_fail!(Number::real(b).try_into_reciprocal());

                assert_eq!(quotient.to_string(), reciprocal_product.to_string());
            }
        }

        #[test]
        fn beyond_i64_magnitude() {
            let quotient = ok_or_fail!(Number::real(i64::MIN) / Number::real(-1));

            assert_eq!(quotient.to_string(), "9223372036854775808");
        }

        #[test]
        fn reciprocal_beyond_i64_magnitude() {
            let quotient = ok_or_fail!(Number::one() / Number::real(i64::MIN));

            assert_eq!(quotient.to_string(), "-1/9223372036854775808");
        }
    }

    mod float {
        use super::*;

        #[test]
        fn matrix() {
            let cases = [
                (3.0, 2.0, "1.5"),
                (-3.0, 2.0, "-1.5"),
                (3.0, -2.0, "-1.5"),
                (-3.0, -2.0, "1.5"),
            ];
            for (a, b, expected) in cases {
                let quotient = ok_or_fail!(Number::real(a) / Number::real(b));

                assert_eq!(quotient.to_string(), expected);
            }
        }

        #[test]
        fn positive_over_positive_zero_is_positive_infinity() {
            let quotient = ok_or_fail!(Number::real(1.0) / Number::real(0.0));

            assert_eq!(quotient.to_string(), "+inf.0");
        }

        #[test]
        fn negative_over_positive_zero_is_negative_infinity() {
            let quotient = ok_or_fail!(Number::real(-1.0) / Number::real(0.0));

            assert_eq!(quotient.to_string(), "-inf.0");
        }

        #[test]
        fn positive_over_negative_zero_is_negative_infinity() {
            let quotient = ok_or_fail!(Number::real(1.0) / Number::real(-0.0));

            assert_eq!(quotient.to_string(), "-inf.0");
        }

        #[test]
        fn negative_over_negative_zero_is_positive_infinity() {
            let quotient = ok_or_fail!(Number::real(-1.0) / Number::real(-0.0));

            assert_eq!(quotient.to_string(), "+inf.0");
        }

        #[test]
        fn finite_over_positive_infinity_is_positive_zero() {
            let quotient = ok_or_fail!(Number::real(5.0) / Number::real(f64::INFINITY));

            assert_eq!(quotient.to_string(), "0.0");
        }

        #[test]
        fn finite_over_negative_infinity_is_negative_zero() {
            let quotient = ok_or_fail!(Number::real(5.0) / Number::real(f64::NEG_INFINITY));

            assert_eq!(quotient.to_string(), "-0.0");
        }

        #[test]
        fn zero_over_zero_is_nan() {
            let quotient = ok_or_fail!(Number::real(0.0) / Number::real(0.0));

            assert!(quotient.is_nan());
        }

        #[test]
        fn infinity_over_infinity_is_nan() {
            let quotient = ok_or_fail!(Number::real(f64::INFINITY) / Number::real(f64::INFINITY));

            assert!(quotient.is_nan());
        }

        #[test]
        fn nan_propagates_as_dividend() {
            let quotient = ok_or_fail!(Number::real(f64::NAN) / Number::real(2.0));

            assert!(quotient.is_nan());
        }

        #[test]
        fn nan_propagates_as_divisor() {
            let quotient = ok_or_fail!(Number::real(2.0) / Number::real(f64::NAN));

            assert!(quotient.is_nan());
        }

        #[test]
        fn quotient_is_inexact() {
            let quotient = ok_or_fail!(Number::real(3.0) / Number::real(2.0));

            let n = extract_or_fail!(quotient, Number::Real);
            assert_matches!(n, Real::Float(_));
        }

        #[test]
        fn exact_zero_divisor_is_an_error_even_for_a_float_dividend() {
            // R7RS: division by exact zero is an error, regardless of the
            // dividend's exactness; contrast with 1.0 / 0.0 above.
            let quotient = Number::real(1.0) / Number::zero();

            let err = err_or_fail!(quotient);
            assert_matches!(err, NumericError::DivideByZero);
        }
    }

    mod mixed {
        use super::*;

        #[test]
        fn integer_over_float_and_float_over_integer_agree() {
            let a = ok_or_fail!(Number::real(7) / Number::real(2.0));
            let b = ok_or_fail!(Number::real(7.0) / Number::real(2));

            assert_eq!(a.to_string(), "3.5");
            assert_eq!(b.to_string(), "3.5");
        }

        #[test]
        fn mixed_quotient_is_inexact() {
            let quotient = ok_or_fail!(Number::real(7) / Number::real(2.0));

            let n = extract_or_fail!(quotient, Number::Real);
            assert_matches!(n, Real::Float(_));
        }

        #[test]
        fn integer_over_signed_zero_float() {
            let cases = [(1, "+inf.0"), (-1, "-inf.0")];
            for (n, expected) in cases {
                let quotient = ok_or_fail!(Number::real(n) / Number::real(0.0));

                assert_eq!(quotient.to_string(), expected);
            }
        }

        #[test]
        fn exact_zero_over_inexact_zero_stays_exact() {
            let quotient = ok_or_fail!(Number::zero() / Number::real(0.0));

            assert_eq!(quotient.to_string(), "0");
            assert!(matches!(
                extract_or_fail!(quotient, Number::Real),
                Real::Integer(_)
            ));
        }

        #[test]
        fn exact_zero_over_nan_is_nan() {
            // nan overrides the exact-zero shortcut above
            let quotient = ok_or_fail!(Number::zero() / Number::real(f64::NAN));

            assert!(quotient.is_nan());
        }
    }

    mod rational {
        use super::*;

        #[test]
        fn exact_matrix() {
            let cases = [
                ((1, 2), (3, 4), "2/3"),
                ((3, 4), (3, 4), "1"),
                ((-1, 2), (3, 4), "-2/3"),
                ((1, 2), (-3, 4), "-2/3"),
                ((3, 4), (3, 8), "2"),
            ];
            for ((an, ad), (bn, bd), expected) in cases {
                let a = ok_or_fail!(Real::reduce(an, ad));
                let b = ok_or_fail!(Real::reduce(bn, bd));

                let quotient = ok_or_fail!(Number::real(a) / Number::real(b));

                assert_eq!(quotient.to_string(), expected);
            }
        }

        #[test]
        fn integer_over_rational() {
            let q = ok_or_fail!(Real::reduce(1, 2));

            let quotient = ok_or_fail!(Number::real(3) / Number::real(q));

            assert_eq!(quotient.to_string(), "6");
        }

        #[test]
        fn rational_over_integer() {
            let q = ok_or_fail!(Real::reduce(1, 2));

            let quotient = ok_or_fail!(Number::real(q) / Number::real(3));

            assert_eq!(quotient.to_string(), "1/6");
        }

        #[test]
        fn division_by_exact_zero_is_an_error() {
            let q = ok_or_fail!(Real::reduce(1, 2));

            let quotient = Number::real(q) / Number::zero();

            let err = err_or_fail!(quotient);
            assert_matches!(err, NumericError::DivideByZero);
        }

        #[test]
        fn zero_dividend_is_zero() {
            let q = ok_or_fail!(Real::reduce(1, 2));

            let quotient = ok_or_fail!(Number::zero() / Number::real(q));

            assert_eq!(quotient.to_string(), "0");
        }

        #[test]
        fn exact_operands_stay_exact() {
            let a = ok_or_fail!(Real::reduce(1, 2));
            let b = ok_or_fail!(Real::reduce(3, 4));

            let quotient = ok_or_fail!(Number::real(a) / Number::real(b));

            let n = extract_or_fail!(quotient, Number::Real);
            assert!(!matches!(n, Real::Float(_)));
        }

        #[test]
        fn rational_over_infinity_is_positive_zero() {
            let q = ok_or_fail!(Real::reduce(1, 2));

            let quotient = ok_or_fail!(Number::real(q) / Number::real(f64::INFINITY));

            assert_eq!(quotient.to_string(), "0.0");
        }

        #[test]
        fn rational_over_nan_is_nan() {
            let q = ok_or_fail!(Real::reduce(1, 2));

            let quotient = ok_or_fail!(Number::real(q) / Number::real(f64::NAN));

            assert!(quotient.is_nan());
        }

        #[test]
        fn float_over_rational_matches_direct_float_division() {
            let q = ok_or_fail!(Real::reduce(1, 3));

            let quotient = ok_or_fail!(Number::real(0.5) / Number::real(q));

            let expected = 0.5_f64 / (1.0 / 3.0);
            assert_eq!(quotient.to_string(), expected.to_string());
        }

        #[test]
        fn rational_over_float_is_correctly_rounded() {
            let q = ok_or_fail!(Real::reduce(1, 3));

            let quotient = ok_or_fail!(Number::real(q) / Number::real(49.0));

            let expected = (1.0_f64 / 3.0) / 49.0;
            assert_eq!(quotient.to_string(), expected.to_string());
        }

        #[test]
        #[ignore = "multi-precision multiplication not yet implemented"]
        fn cross_product_overflows_precision() {
            // dividing routes through reciprocal-then-multiply; a's
            // numerator and b's numerator are coprime (as are their
            // denominators), so Mul for Rational finds no shared factor to
            // cross-reduce and big*big overflows u64::MAX.
            let big: Integer = (Sign::Positive, u64::MAX).into();
            let a = ok_or_fail!(Real::reduce(big.clone(), 7));
            let b = ok_or_fail!(Real::reduce(11, big));

            let quotient = Number::real(a) / Number::real(b);

            ok_or_fail!(quotient);
        }
    }

    mod complex {
        use super::*;

        #[test]
        fn basic() {
            // (a+bi)/(c+di) = ((ac+bd) + (bc-ad)i) / (c^2+d^2)
            let a = Number::complex(3, 2);
            let b = Number::complex(1, 4);

            let quotient = ok_or_fail!(a / b);

            assert_eq!(quotient.to_string(), "11/17-10/17i");
        }

        #[test]
        fn dividing_by_one_is_identity() {
            let z = Number::complex(3, 2);

            let quotient = ok_or_fail!(z.clone() / Number::one());

            assert_eq!(quotient.to_string(), z.to_string());
        }

        #[test]
        fn dividing_by_self_is_one() {
            let z = Number::complex(3, 2);

            let quotient = ok_or_fail!(z.clone() / z);

            assert_eq!(quotient.to_string(), "1");
        }

        #[test]
        fn division_by_large_magnitude_divisor_does_not_overflow() {
            let quotient = ok_or_fail!(Number::one() / Number::complex(1e200, 1e200));

            assert!(!quotient.is_zero());
        }

        #[test]
        fn dividing_large_magnitude_complex_by_itself_is_one() {
            let z = Number::complex(1e200, 1e200);

            let quotient = ok_or_fail!(z.clone() / z);

            assert_eq!(quotient.to_string(), "1.0+0.0i");
        }

        #[test]
        fn real_divided_by_imaginary_unit_is_not_the_reverse() {
            let one_over_i = ok_or_fail!(Number::one() / Number::imaginary(1));
            let i_over_one = ok_or_fail!(Number::imaginary(1) / Number::one());

            assert_eq!(one_over_i.to_string(), "-i");
            assert_eq!(i_over_one.to_string(), "+i");
        }

        #[test]
        fn exact_matrix() {
            // (a+bi)/(c+di) = ((ac+bd) + (bc-ad)i) / (c^2+d^2)
            let cases = [
                ((1, 1), (1, -1), "+i"),
                ((4, 2), (2, 0), "2+i"),
                ((-3, 4), (1, 2), "1+2i"),
            ];
            for ((are, aim), (bre, bim), expected) in cases {
                let a = Number::complex(are, aim);
                let b = Number::complex(bre, bim);

                let quotient = ok_or_fail!(a / b);

                assert_eq!(quotient.to_string(), expected);
            }
        }

        #[test]
        fn complex_over_exact_real() {
            let z = Number::complex(3, 2);

            let quotient = ok_or_fail!(z / Number::real(2));

            assert_eq!(quotient.to_string(), "3/2+i");
        }

        #[test]
        fn complex_over_inexact_real() {
            let z = Number::complex(3, 2);

            let quotient = ok_or_fail!(z / Number::real(2.0));

            assert_eq!(quotient.to_string(), "1.5+1.0i");
        }

        #[test]
        fn real_over_complex() {
            let quotient = ok_or_fail!(Number::real(5) / Number::complex(1, 2));

            assert_eq!(quotient.to_string(), "1-2i");
        }

        #[test]
        fn real_over_complex_with_exact_zero_real_part_and_inexact_zero_magnitude_is_nan() {
            let quotient = ok_or_fail!(Number::real(5) / Number::complex(0, 0.0));

            let c = extract_or_fail!(quotient, Number::Complex);
            let (re, im) = (c.clone().into_real(), c.into_imag());
            assert!(re.is_nan());
            assert!(im.is_nan());
        }

        #[test]
        fn real_over_complex_preserves_sign_of_zero() {
            let quotient = ok_or_fail!(Number::real(0.0) / Number::complex(3, 2));

            assert_eq!(quotient.to_string(), "0.0-0.0i");
        }

        #[test]
        fn real_over_complex_preserves_sign_of_zero_in_real_part() {
            let quotient = ok_or_fail!(Number::real(0.0) / Number::complex(-3, 2));

            assert_eq!(quotient.to_string(), "-0.0-0.0i");
        }

        #[test]
        fn non_commutative() {
            let a = Number::complex(3, 2);
            let b = Number::complex(1, 4);

            let a_over_b = ok_or_fail!(a.clone() / b.clone());
            let b_over_a = ok_or_fail!(b.clone() / a.clone());

            assert_ne!(a_over_b.to_string(), b_over_a.to_string());

            let product = a_over_b * b_over_a;
            assert_eq!(product.to_string(), "1");
        }

        #[test]
        fn quotient_times_divisor_recovers_dividend() {
            let cases = [
                ((1, 1), (1, -1)),
                ((4, 2), (2, 0)),
                ((-3, 4), (1, 2)),
                ((3, 2), (1, 4)),
            ];
            for ((are, aim), (bre, bim)) in cases {
                let a = Number::complex(are, aim);
                let b = Number::complex(bre, bim);

                let quotient = ok_or_fail!(a.clone() / b.clone());
                let recovered = quotient * b;

                assert_eq!(recovered.to_string(), a.to_string());
            }
        }

        #[test]
        fn dividing_into_zero_is_zero() {
            let quotient = ok_or_fail!(Number::zero() / Number::complex(3, 4));

            assert_eq!(quotient.to_string(), "0");
            assert_matches!(quotient, Number::Real(Real::Integer(_)));
        }

        #[test]
        fn division_by_exact_zero_is_an_error() {
            let z = Number::complex(3, 2);

            let quotient = z / Number::zero();

            let err = err_or_fail!(quotient);
            assert_matches!(err, NumericError::DivideByZero);
        }

        #[test]
        fn all_inexact_operands() {
            let a = Number::complex(3.0, 4.0);
            let b = Number::complex(1.0, 1.0);

            let quotient = ok_or_fail!(a / b);

            assert_eq!(quotient.to_string(), "3.5+0.5i");
        }

        #[test]
        fn mixed_exactness_taints_result() {
            let z = Number::complex(3, 2);

            let quotient = ok_or_fail!(z / Number::real(2.0));

            let c = extract_or_fail!(quotient, Number::Complex);
            let (re, im) = c.into_parts();
            assert_matches!(re, Real::Float(_));
            assert_matches!(im, Real::Float(_));
        }

        #[test]
        fn exact_operands_stay_exact() {
            let a = Number::complex(3, 2);
            let b = Number::complex(1, 4);

            let quotient = ok_or_fail!(a / b);

            let c = extract_or_fail!(quotient, Number::Complex);
            let (re, im) = c.into_parts();
            assert!(!matches!(re, Real::Float(_)));
            assert!(!matches!(im, Real::Float(_)));
        }

        #[test]
        fn infinite_part_propagates() {
            let z = Number::complex(f64::INFINITY, 1.0);

            let quotient = ok_or_fail!(z / Number::real(2));

            assert!(quotient.is_infinite());
        }

        #[test]
        fn nan_part_propagates() {
            let z = Number::complex(f64::NAN, 1.0);

            let quotient = ok_or_fail!(z / Number::real(2));

            assert!(quotient.is_nan());
        }

        // Reproduces a real-divisor bug: `Div for Number`'s `(Complex, Real)`
        // arm promotes the real divisor to a Complex and routes through the
        // general conjugate/magnitude-squared reciprocal algorithm
        // (src/number.rs:319) instead of dividing each component directly.
        // For a real divisor, direct component-wise division
        // `(x+yi)/r = x/r + (y/r)i` is both simpler and IEEE-correct;
        // the magnitude-squared detour instead produces 0.0/0.0 or
        // inf/inf internally, yielding NaN+NaNi where Chez Scheme and
        // Guile (dividing componentwise) give +inf.0+inf.0i / 0.0+0.0i
        // respectively.
        #[test]
        fn division_by_inexact_zero_is_infinite() {
            let cases = [Number::complex(3, 2), Number::complex(3.0, 2.0)];
            for z in cases {
                let quotient = ok_or_fail!(z / Number::real(0.0));

                assert_eq!(quotient.to_string(), "+inf.0+inf.0i");
            }
        }

        #[test]
        fn division_by_infinity_is_zero() {
            let cases = [Number::complex(3, 2), Number::complex(3.0, 2.0)];
            for z in cases {
                let quotient = ok_or_fail!(z / Number::real(f64::INFINITY));

                assert_eq!(quotient.to_string(), "0.0+0.0i");
            }
        }

        // Same bug as division_by_inexact_zero_is_infinite, mirrored to a
        // negative-signed zero divisor: componentwise division would give
        // 3/-0.0 = -inf.0 and 2/-0.0 = -inf.0, so a fix that only handles
        // +0.0 but mishandles -0.0 (an easy slip given how much sign-of-zero
        // subtlety is at play here) would still fail this one.
        #[test]
        fn division_by_negative_inexact_zero_is_negative_infinite() {
            let cases = [Number::complex(3, 2), Number::complex(3.0, 2.0)];
            for z in cases {
                let quotient = ok_or_fail!(z / Number::real(-0.0));

                assert_eq!(quotient.to_string(), "-inf.0-inf.0i");
            }
        }

        // Mirror of division_by_infinity_is_zero: 3/-inf.0 = -0.0 and
        // 2/-inf.0 = -0.0 componentwise.
        #[test]
        fn division_by_negative_infinity_is_negative_zero() {
            let cases = [Number::complex(3, 2), Number::complex(3.0, 2.0)];
            for z in cases {
                let quotient = ok_or_fail!(z / Number::real(f64::NEG_INFINITY));

                assert_eq!(quotient.to_string(), "-0.0-0.0i");
            }
        }

        // An exact zero imaginary part collapses Number::complex straight to a
        // Real (see Number::complex), so this never touches Complex::div at all
        // -- contrast with the inexact case below.
        #[test]
        fn real_over_complex_with_exact_zero_imaginary_is_real() {
            let quotient = ok_or_fail!(Number::real(1) / Number::complex(2.0, 0));

            assert_eq!(quotient.to_string(), "0.5");
            assert_matches!(quotient, Number::Real(_));
        }

        // Smith's algorithm computes the imaginary part as (b - a*r)/denom; here
        // b is the exact int 0, so 0 - 1*0.0 relies on exact-zero-minus-inexact
        // flipping the sign to -0.0, matching (0*2 - 1*0)/4's mathematical sign.
        #[test]
        fn real_over_complex_with_inexact_zero_imaginary_keeps_signed_zero() {
            let quotient = ok_or_fail!(Number::real(1) / Number::complex(2.0, 0.0));

            assert_eq!(quotient.to_string(), "0.5-0.0i");
        }
    }
}

mod negate {
    use super::*;

    #[test]
    fn integer_matrix() {
        let cases = [(4, "-4"), (-4, "4"), (0, "0")];
        for (n, expected) in cases {
            let neg = -Number::real(n);

            assert_eq!(neg.to_string(), expected);
        }
    }

    #[test]
    fn integer_negation_stays_exact_integer() {
        let neg = -Number::real(4);

        assert_matches!(neg, Number::Real(Real::Integer(_)));
    }

    #[test]
    fn zero_negation_stays_zero_sign() {
        let neg = -Integer::from(0);

        assert_eq!(neg.sign, Sign::Zero);
    }

    #[test]
    fn integer_negation_is_an_involution() {
        let cases = [4, -4, 0];
        for n in cases {
            let x = Number::real(n);
            let double_neg = -(-x.clone());

            assert_eq!(double_neg.to_string(), x.to_string());
        }
    }

    #[test]
    fn integer_negation_beyond_i64_max() {
        let neg = -Number::real(i64::MIN);

        assert_eq!(neg.to_string(), "9223372036854775808");
    }

    #[test]
    fn float_matrix() {
        let cases = [
            (1.5, "-1.5"),
            (-1.5, "1.5"),
            (0.0, "-0.0"),
            (-0.0, "0.0"),
            (f64::INFINITY, "-inf.0"),
            (f64::NEG_INFINITY, "+inf.0"),
        ];
        for (f, expected) in cases {
            let neg = -Number::real(f);

            assert_eq!(neg.to_string(), expected);
        }
    }

    #[test]
    fn float_negation_stays_inexact() {
        let neg = -Number::real(1.5);

        assert_matches!(neg, Number::Real(Real::Float(_)));
    }

    #[test]
    fn nan_negation_is_still_nan() {
        let neg = -Number::real(f64::NAN);

        assert!(neg.is_nan());
    }

    #[test]
    fn float_negation_is_an_involution() {
        let cases = [1.5, -1.5, 0.0, -0.0];
        for f in cases {
            let x = Number::real(f);
            let double_neg = -(-x.clone());

            assert_eq!(double_neg.to_string(), x.to_string());
        }
    }

    #[test]
    fn rational_matrix() {
        let cases = [((1, 2), "-1/2"), ((-1, 2), "1/2"), ((-3, 4), "3/4")];
        for ((n, d), expected) in cases {
            let q = ok_or_fail!(Real::reduce(n, d));
            let neg = -Number::real(q);

            assert_eq!(neg.to_string(), expected);
        }
    }

    #[test]
    fn rational_negation_stays_rational() {
        let q = ok_or_fail!(Real::reduce(1, 2));
        let neg = -Number::real(q);

        assert_matches!(neg, Number::Real(Real::Rational(_)));
    }

    #[test]
    fn rational_negation_leaves_denominator_untouched() {
        let q = ok_or_fail!(Real::reduce(1, 2));
        let neg = -Number::real(q);

        let r = extract_or_fail!(neg, Number::Real);
        let q = extract_or_fail!(r, Real::Rational);
        assert_eq!(q.0.1, Integer::from(2));
    }

    #[test]
    fn rational_negation_is_an_involution() {
        let q = ok_or_fail!(Real::reduce(1, 2));
        let x = Number::real(q);
        let double_neg = -(-x.clone());

        assert_eq!(double_neg.to_string(), x.to_string());
    }

    #[test]
    fn complex_matrix() {
        let cases = [
            ((3, 4), "-3-4i"),
            ((-3, -4), "3+4i"),
            ((0, 2), "-2i"),
            ((0, -2), "+2i"),
        ];
        for ((re, im), expected) in cases {
            let neg = -Number::complex(re, im);

            assert_eq!(neg.to_string(), expected);
        }
    }

    #[test]
    fn complex_with_inexact_zero_imag_stays_complex_when_negated() {
        let z = Number::complex(3, 0.0);

        let neg = -z;

        assert_eq!(neg.to_string(), "-3-0.0i");
        assert_matches!(neg, Number::Complex(_));
    }

    #[test]
    fn complex_negation_is_an_involution() {
        let x = Number::complex(3, 4);
        let double_neg = -(-x.clone());

        assert_eq!(double_neg.to_string(), x.to_string());
    }

    #[test]
    fn integer_inverse_law() {
        let x = Number::real(7);
        let sum = x.clone() + -x;

        assert_eq!(sum.to_string(), "0");
    }

    #[test]
    fn float_inverse_law() {
        let x = Number::real(1.5);
        let sum = x.clone() + -x;

        assert_eq!(sum.to_string(), "0.0");
    }

    #[test]
    fn complex_inverse_law() {
        let x = Number::complex(3, 4);
        let sum = x.clone() + -x;

        assert_eq!(sum.to_string(), "0");
    }

    #[test]
    fn infinity_has_no_additive_inverse() {
        let x = Number::real(f64::INFINITY);
        let sum = x.clone() + -x;

        assert!(sum.is_nan());
    }

    #[test]
    fn rational_inverse_law() {
        let q = ok_or_fail!(Real::reduce(1, 2));
        let x = Number::real(q);
        let sum = x.clone() + -x;

        assert_eq!(sum.to_string(), "0");
    }
}

mod reciprocal {
    use super::*;

    mod integer {
        use super::*;

        #[test]
        fn matrix() {
            let cases = [(1, "1"), (-1, "-1"), (4, "1/4"), (-4, "-1/4"), (2, "1/2")];
            for (n, expected) in cases {
                let r = ok_or_fail!(Number::real(n).try_into_reciprocal());

                assert_eq!(r.to_string(), expected);
            }
        }

        #[test]
        fn nonunit_reciprocal_is_exact_rational() {
            let r = ok_or_fail!(Number::real(4).try_into_reciprocal());

            assert_matches!(r, Number::Real(Real::Rational(_)));
        }

        #[test]
        fn unity_reciprocal_stays_an_integer() {
            let cases = [1, -1];
            for n in cases {
                let r = ok_or_fail!(Number::real(n).try_into_reciprocal());

                assert_matches!(r, Number::Real(Real::Integer(_)));
            }
        }

        #[test]
        fn beyond_i64_magnitude() {
            let r = ok_or_fail!(Number::real(i64::MIN).try_into_reciprocal());

            assert_eq!(r.to_string(), "-1/9223372036854775808");
        }

        #[test]
        fn zero_has_no_reciprocal() {
            let r = Number::real(0).try_into_reciprocal();

            let err = err_or_fail!(r);
            assert_matches!(err, NumericError::DivideByZero);
        }

        #[test]
        fn is_an_involution() {
            let cases = [4, -4, 1, -1, 7];
            for n in cases {
                let x = Number::real(n);

                let r = ok_or_fail!(x.clone().try_into_reciprocal());
                let r2 = ok_or_fail!(r.try_into_reciprocal());

                assert_eq!(r2.to_string(), x.to_string());
            }
        }

        #[test]
        fn preserves_sign() {
            let positive = Real::Integer(4.into());
            let r = ok_or_fail!(positive.try_into_reciprocal());
            assert!(r.is_positive());

            let negative = Real::Integer((-4).into());
            let r = ok_or_fail!(negative.try_into_reciprocal());
            assert!(r.is_negative());
        }

        #[test]
        fn inverse_law_for_unity() {
            let cases = [1, -1];
            for n in cases {
                let x = Number::real(n);
                let r = ok_or_fail!(x.clone().try_into_reciprocal());

                let product = x * r;

                assert_eq!(product.to_string(), "1");
            }
        }

        #[test]
        fn inverse_law_for_nonunit() {
            let x = Number::real(4);
            let r = ok_or_fail!(x.clone().try_into_reciprocal());

            let product = x * r;

            assert_eq!(product.to_string(), "1");
        }
    }

    mod rational {
        use super::*;

        #[test]
        fn matrix() {
            let cases = [
                ((3, 4), "4/3"),
                ((-3, 4), "-4/3"),
                ((5, 2), "2/5"),
                ((-5, 2), "-2/5"),
            ];
            for ((n, d), expected) in cases {
                let q = ok_or_fail!(Real::reduce(n, d));

                let r = ok_or_fail!(Number::real(q).try_into_reciprocal());

                assert_eq!(r.to_string(), expected);
            }
        }

        #[test]
        fn unit_numerator_reciprocal_is_an_integer() {
            let cases = [((1, 5), "5"), ((-1, 5), "-5")];
            for ((n, d), expected) in cases {
                let q = ok_or_fail!(Real::reduce(n, d));

                let r = ok_or_fail!(Number::real(q).try_into_reciprocal());

                assert_eq!(r.to_string(), expected);
                assert_matches!(r, Number::Real(Real::Integer(_)));
            }
        }

        #[test]
        fn is_an_involution() {
            let cases = [(3, 4), (-3, 4)];
            for (n, d) in cases {
                let q = ok_or_fail!(Real::reduce(n, d));
                let x = Number::real(q);

                let r = ok_or_fail!(x.clone().try_into_reciprocal());
                let r2 = ok_or_fail!(r.try_into_reciprocal());

                assert_eq!(r2.to_string(), x.to_string());
            }
        }

        #[test]
        fn denominator_stays_positive_after_inverting_negative() {
            let q = ok_or_fail!(Real::reduce(-3, 4));

            let r = ok_or_fail!(Number::real(q).try_into_reciprocal());

            let (_, den) = rational_parts!(extract_or_fail!(r, Number::Real));
            assert_eq!(den.sign, Sign::Positive);
        }

        #[test]
        fn inverse_law() {
            let q = ok_or_fail!(Real::reduce(3, 4));
            let x = Number::real(q);
            let r = ok_or_fail!(x.clone().try_into_reciprocal());

            let product = x * r;

            assert_eq!(product.to_string(), "1");
        }
    }

    mod float {
        use super::*;

        #[test]
        fn matrix() {
            let cases = [(2.0, "0.5"), (0.5, "2.0"), (-8.0, "-0.125"), (1.0, "1.0")];
            for (f, expected) in cases {
                let r = ok_or_fail!(Number::real(f).try_into_reciprocal());

                assert_eq!(r.to_string(), expected);
            }
        }

        #[test]
        fn positive_zero_reciprocal_is_positive_infinity() {
            let r = ok_or_fail!(Number::real(0.0).try_into_reciprocal());

            assert_eq!(r.to_string(), "+inf.0");
        }

        #[test]
        fn negative_zero_reciprocal_is_negative_infinity() {
            let r = ok_or_fail!(Number::real(-0.0).try_into_reciprocal());

            assert_eq!(r.to_string(), "-inf.0");
        }

        #[test]
        fn positive_infinity_reciprocal_is_positive_zero() {
            let r = ok_or_fail!(Number::real(f64::INFINITY).try_into_reciprocal());

            assert_eq!(r.to_string(), "0.0");
        }

        #[test]
        fn negative_infinity_reciprocal_is_negative_zero() {
            let r = ok_or_fail!(Number::real(f64::NEG_INFINITY).try_into_reciprocal());

            assert_eq!(r.to_string(), "-0.0");
        }

        #[test]
        fn nan_reciprocal_is_nan() {
            let r = ok_or_fail!(Number::real(f64::NAN).try_into_reciprocal());

            assert!(r.is_nan());
        }

        #[test]
        fn stays_inexact_even_for_whole_number_result() {
            let r = ok_or_fail!(Number::real(0.5).try_into_reciprocal());

            assert_matches!(r, Number::Real(Real::Float(_)));
        }

        #[test]
        fn small_normal_reciprocal_is_finite() {
            let r = ok_or_fail!(Number::real(f64::MIN_POSITIVE).try_into_reciprocal());

            assert!(!r.is_infinite());
        }

        #[test]
        fn smallest_subnormal_reciprocal_overflows_to_infinity() {
            let r = ok_or_fail!(Number::real(f64::from_bits(1)).try_into_reciprocal());

            assert!(r.is_infinite());
        }

        #[test]
        fn is_an_involution_for_exact_binary_fractions() {
            let cases = [2.0, 0.25, -8.0];
            for f in cases {
                let x = Number::real(f);

                let r = ok_or_fail!(x.clone().try_into_reciprocal());
                let r2 = ok_or_fail!(r.try_into_reciprocal());

                assert_eq!(r2.to_string(), x.to_string());
            }
        }

        #[test]
        fn is_not_an_involution_in_general() {
            // 1/(1/f64::MAX) overflows to infinity rather than round-tripping,
            // per IEEE 754 semantics -- this documents that limit, not a bug.
            let x = Number::real(f64::MAX);

            let r = ok_or_fail!(x.try_into_reciprocal());
            let r2 = ok_or_fail!(r.try_into_reciprocal());

            assert_eq!(r2.to_string(), "+inf.0");
        }

        #[test]
        fn inverse_law() {
            let x = Number::real(2.0);
            let r = ok_or_fail!(x.clone().try_into_reciprocal());

            let product = x * r;

            assert_eq!(product.to_string(), "1.0");
        }

        #[test]
        fn inverse_law_fails_for_infinity() {
            let x = Number::real(f64::INFINITY);
            let r = ok_or_fail!(x.clone().try_into_reciprocal());

            let product = x * r;

            assert!(product.is_nan());
        }
    }

    mod complex {
        use super::*;

        #[test]
        fn conjugate_over_magnitude_squared() {
            // 1/(a+bi) = (a-bi) / (a^2+b^2)
            let z = Number::complex(3, 4);

            let r = ok_or_fail!(z.try_into_reciprocal());

            let expected_re = ok_or_fail!(Real::reduce(3, 25));
            let expected_im = ok_or_fail!(Real::reduce(-4, 25));
            assert_eq!(r, Number::complex(expected_re, expected_im));
        }

        #[test]
        fn matrix() {
            let cases = [
                ((0, 1), "-i"),
                ((1, 1), "1/2-1/2i"),
                ((-3, 4), "-3/25-4/25i"),
                ((1, -1), "1/2+1/2i"),
            ];
            for ((re, im), expected) in cases {
                let z = Number::complex(re, im);

                let r = ok_or_fail!(z.try_into_reciprocal());

                assert_eq!(r.to_string(), expected);
            }
        }

        #[test]
        fn inexact_parts_stay_inexact() {
            let z = Number::complex(3.0, 4.0);

            let r = ok_or_fail!(z.try_into_reciprocal());

            assert_eq!(r.to_string(), "0.12-0.16i");
            assert!(r.is_inexact());
        }

        #[test]
        fn is_an_involution() {
            let cases = [(3, 4), (-3, 4), (1, -1)];
            for (re, im) in cases {
                let z = Number::complex(re, im);

                let r = ok_or_fail!(z.clone().try_into_reciprocal());
                let r2 = ok_or_fail!(r.try_into_reciprocal());

                assert_eq!(r2.to_string(), z.to_string());
            }
        }

        #[test]
        fn inverse_law() {
            let z = Number::complex(3, 4);
            let r = ok_or_fail!(z.clone().try_into_reciprocal());

            let product = z * r;

            assert_eq!(product.to_string(), "1");
            assert_matches!(product, Number::Real(Real::Integer(_)));
        }

        #[test]
        fn preserves_conjugate_symmetry() {
            // conj(z)^-1 == conj(z^-1)
            let z = Number::complex(3, 4);

            let conj_then_recip =
                ok_or_fail!(z.clone().into_complex_conjugate().try_into_reciprocal());
            let recip_then_conj = ok_or_fail!(z.try_into_reciprocal()).into_complex_conjugate();

            assert_eq!(conj_then_recip.to_string(), recip_then_conj.to_string());
        }

        #[test]
        fn purely_imaginary() {
            let z = Number::imaginary(2);

            let r = ok_or_fail!(z.try_into_reciprocal());

            assert_eq!(r.to_string(), "-1/2i");
        }

        #[test]
        fn exact_zero_real_part_with_inexact_zero_magnitude_is_nan() {
            let z = Number::complex(0, 0.0);

            let r = ok_or_fail!(z.try_into_reciprocal());

            let c = extract_or_fail!(r, Number::Complex);
            let (re, im) = c.into_parts();
            assert!(re.is_nan());
            assert!(im.is_nan());
        }

        #[test]
        fn large_magnitude_reciprocal_does_not_overflow_to_zero() {
            let z = Number::complex(1e200, 1e200);

            let r = ok_or_fail!(z.clone().try_into_reciprocal());
            let expected = ok_or_fail!(Number::real(1) / z);

            assert!(!r.is_zero());
            assert_eq!(r.to_string(), expected.to_string());
        }
    }
}
