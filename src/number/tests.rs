use super::*;
use crate::testutil::{extract_or_fail, ok_or_fail};

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
        let n = ok_or_fail!(Number::rational(4, 5));

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

        assert_eq!(n.as_datum().to_string(), "0");
    }

    #[test]
    fn negative_int_zero() {
        let n = Number::real(-0);

        assert_eq!(n.as_datum().to_string(), "0");
    }

    #[test]
    fn positive_int() {
        let n = Number::real(23);

        assert_eq!(n.as_datum().to_string(), "23");
    }

    #[test]
    fn negative_int() {
        let n = Number::real(-32);

        assert_eq!(n.as_datum().to_string(), "-32");
    }

    #[test]
    fn int_max() {
        let n = Number::real(i64::MAX);

        assert_eq!(n.as_datum().to_string(), "9223372036854775807");
    }

    #[test]
    fn int_min() {
        let n = Number::real(i64::MIN);

        assert_eq!(n.as_datum().to_string(), "-9223372036854775808");
    }

    #[test]
    fn positive_zero() {
        let n = Number::real(0.0);

        assert_eq!(n.as_datum().to_string(), "0.0");
    }

    #[test]
    fn negative_zero() {
        let n = Number::real(-0.0);

        assert_eq!(n.as_datum().to_string(), "-0.0");
    }

    #[test]
    fn whole_float() {
        let n = Number::real(1.0);

        assert_eq!(n.as_datum().to_string(), "1.0");
    }

    #[test]
    fn positive_float() {
        let n = Number::real(234.23);

        assert_eq!(n.as_datum().to_string(), "234.23");
    }

    #[test]
    fn negative_float() {
        let n = Number::real(-789.34);

        assert_eq!(n.as_datum().to_string(), "-789.34");
    }

    #[test]
    fn fractional_float() {
        let n = Number::real(0.0567);

        assert_eq!(n.as_datum().to_string(), "0.0567");
    }

    #[test]
    fn with_trailing_zeros() {
        let n = Number::real(234.23000);

        assert_eq!(n.as_datum().to_string(), "234.23");
    }

    #[test]
    fn large_exponent() {
        let n = Number::real(1e29);

        assert_eq!(n.as_datum().to_string(), "1e29");
    }

    #[test]
    fn small_exponent() {
        let n = Number::real(1e-29);

        assert_eq!(n.as_datum().to_string(), "1e-29");
    }

    #[test]
    fn rounding_error() {
        let n = Number::real(0.1 + 0.2);

        assert_eq!(n.as_datum().to_string(), "0.30000000000000004");
    }

    #[test]
    fn max_float() {
        let n = Number::real(f64::MAX);

        assert_eq!(n.as_datum().to_string(), "1.7976931348623157e308");
    }

    #[test]
    fn min_float() {
        let n = Number::real(f64::MIN);

        assert_eq!(n.as_datum().to_string(), "-1.7976931348623157e308");
    }

    #[test]
    fn positive_min_float() {
        let n = Number::real(f64::MIN_POSITIVE);

        assert_eq!(n.as_datum().to_string(), "2.2250738585072014e-308");
    }

    #[test]
    fn epsilon() {
        let n = Number::real(f64::EPSILON);

        assert_eq!(n.as_datum().to_string(), "2.220446049250313e-16");
    }

    #[test]
    fn infinity() {
        let n = Number::real(f64::INFINITY);

        assert_eq!(n.as_datum().to_string(), "+inf.0");
    }

    #[test]
    fn negative_infinity() {
        let n = Number::real(f64::NEG_INFINITY);

        assert_eq!(n.as_datum().to_string(), "-inf.0");
    }

    #[test]
    fn nan() {
        let n = Number::real(f64::NAN);

        assert_eq!(n.as_datum().to_string(), "+nan.0");
    }

    #[test]
    fn negative_nan() {
        let n = Number::real(-f64::NAN);

        // NOTE: sign is ignored for NAN
        assert_eq!(n.as_datum().to_string(), "+nan.0");
    }

    #[test]
    fn positive_rational() {
        let n = ok_or_fail!(Number::rational(3, 4));

        assert_eq!(n.as_datum().to_string(), "3/4");
    }

    #[test]
    fn negative_numerator() {
        let n = ok_or_fail!(Number::rational(-3, 4));

        assert_eq!(n.as_datum().to_string(), "-3/4");
    }

    #[test]
    fn negative_denominator() {
        let n = ok_or_fail!(Number::rational(3, -4));

        assert_eq!(n.as_datum().to_string(), "-3/4");
    }

    #[test]
    fn negative_numerator_and_denominator() {
        let n = ok_or_fail!(Number::rational(-3, -4));

        assert_eq!(n.as_datum().to_string(), "3/4");
    }

    #[test]
    fn greater_than_one_rational() {
        let n = ok_or_fail!(Number::rational(4, 3));

        assert_eq!(n.as_datum().to_string(), "4/3");
    }

    #[test]
    fn basic_complex() {
        let n = Number::complex(4, 5);

        assert_eq!(n.as_datum().to_string(), "4+5i");
    }

    #[test]
    fn complex_negative_real() {
        let n = Number::complex(-4, 5);

        assert_eq!(n.as_datum().to_string(), "-4+5i");
    }

    #[test]
    fn complex_negative_imag() {
        let n = Number::complex(4, -5);

        assert_eq!(n.as_datum().to_string(), "4-5i");
    }

    #[test]
    fn complex_negative() {
        let n = Number::complex(-4, -5);

        assert_eq!(n.as_datum().to_string(), "-4-5i");
    }

    #[test]
    fn complex_float() {
        let n = Number::complex(4.2, 5.3);

        assert_eq!(n.as_datum().to_string(), "4.2+5.3i");
    }

    #[test]
    fn complex_rationals() {
        let r = ok_or_fail!(Real::reduce(3, 5));
        let i = ok_or_fail!(Real::reduce(5, 2));
        let n = Number::complex(r, i);

        assert_eq!(n.as_datum().to_string(), "3/5+5/2i");
    }

    #[test]
    fn complex_negative_rat_real() {
        let r = ok_or_fail!(Real::reduce(-3, 5));
        let i = ok_or_fail!(Real::reduce(5, 2));
        let n = Number::complex(r, i);

        assert_eq!(n.as_datum().to_string(), "-3/5+5/2i");
    }

    #[test]
    fn complex_negative_rat_imag() {
        let r = ok_or_fail!(Real::reduce(3, 5));
        let i = ok_or_fail!(Real::reduce(-5, 2));
        let n = Number::complex(r, i);

        assert_eq!(n.as_datum().to_string(), "3/5-5/2i");
    }

    #[test]
    fn complex_negative_rat() {
        let r = ok_or_fail!(Real::reduce(-3, 5));
        let i = ok_or_fail!(Real::reduce(-5, 2));
        let n = Number::complex(r, i);

        assert_eq!(n.as_datum().to_string(), "-3/5-5/2i");
    }

    #[test]
    fn complex_real_rat() {
        let r = ok_or_fail!(Real::reduce(3, 5));
        let n = Number::complex(r, 5);

        assert_eq!(n.as_datum().to_string(), "3/5+5i");
    }

    #[test]
    fn complex_imag_rat() {
        let i = ok_or_fail!(Real::reduce(5, 2));
        let n = Number::complex(3, i);

        assert_eq!(n.as_datum().to_string(), "3+5/2i");
    }

    #[test]
    fn complex_real_float_imag_rat() {
        let i = ok_or_fail!(Real::reduce(5, 2));
        let n = Number::complex(3.032, i);

        assert_eq!(n.as_datum().to_string(), "3.032+5/2i");
    }

    #[test]
    fn complex_real_rat_imag_float() {
        let r = ok_or_fail!(Real::reduce(3, 5));
        let n = Number::complex(r, -6.34);

        assert_eq!(n.as_datum().to_string(), "3/5-6.34i");
    }

    #[test]
    fn complex_zero_real() {
        let n = Number::complex(0, 5);

        assert_eq!(n.as_datum().to_string(), "+5i");
    }

    #[test]
    fn complex_negative_zero_real() {
        let n = Number::complex(0, -5);

        assert_eq!(n.as_datum().to_string(), "-5i");
    }

    #[test]
    fn complex_zero_imag() {
        let n = Number::complex(4, 0);

        assert_eq!(n.as_datum().to_string(), "4");
    }

    #[test]
    fn complex_unity_imag() {
        let n = Number::complex(4, 1);

        assert_eq!(n.as_datum().to_string(), "4+i");
    }

    #[test]
    fn complex_zero_real_unity_imag() {
        let n = Number::complex(0, 1);

        assert_eq!(n.as_datum().to_string(), "+i");
    }

    #[test]
    fn complex_zero_real_negative_unity_imag() {
        let n = Number::complex(0, -1);

        assert_eq!(n.as_datum().to_string(), "-i");
    }

    #[test]
    fn complex_zero_real_rat_unity_imag() {
        let i = ok_or_fail!(Real::reduce(5, 5));
        let n = Number::complex(0, i);

        assert_eq!(n.as_datum().to_string(), "+i");
    }

    #[test]
    fn complex_inexact_unity_imag() {
        let n = Number::complex(4, 1.0);

        assert_eq!(n.as_datum().to_string(), "4+1.0i");
    }
}

mod error {
    use super::*;

    #[test]
    fn display_div_by_zero() {
        let err = NumericError::DivideByZero;

        assert_eq!(err.to_string(), "divide by zero");
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
        assert_eq!(extract_or_fail!(int.precision, Precision::Single), 42);
        assert_eq!(int.sign, Sign::Positive);
    }

    #[test]
    fn zero() {
        let n = 0.into();
        let int = extract_or_fail!(n, Real::Integer);

        assert!(int.is_zero());
        assert_eq!(extract_or_fail!(int.precision, Precision::Single), 0);
        assert_eq!(int.sign, Sign::Zero);
    }

    #[test]
    fn negative() {
        let n = (-42).into();
        let int = extract_or_fail!(n, Real::Integer);

        assert!(!int.is_zero());
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
        let n: Real = 42.into();

        assert_eq!(n.into_float(), 42.0);
    }

    #[test]
    fn zero_into_float() {
        let n: Real = 0.into();

        assert_eq!(n.into_float(), 0.0);
    }

    #[test]
    fn negative_into_float() {
        let n: Real = (-42).into();

        assert_eq!(n.into_float(), -42.0);
    }

    #[test]
    fn imax_into_float() {
        let n: Real = i64::MAX.into();

        assert_eq!(n.into_float(), 9.223372036854776e18);
    }

    #[test]
    fn imin_into_float() {
        let n: Real = i64::MIN.into();

        assert_eq!(n.into_float(), -9.223372036854776e18);
    }

    #[test]
    fn umax_into_float() {
        let u = Integer::single(u64::MAX, Sign::Positive);
        let n: Real = u.into();

        assert_eq!(n.into_float(), 1.8446744073709552e19);
    }

    #[test]
    fn umin_into_float() {
        let u = Integer::single(u64::MAX, Sign::Negative);
        let n: Real = u.into();

        assert_eq!(n.into_float(), -1.8446744073709552e19);
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
        let n: Real = Real::Float(1.5);

        assert_eq!(n.into_float(), 1.5);
    }
}

mod rational {
    use super::*;
    use crate::testutil::err_or_fail;

    macro_rules! rational_parts {
        ($num:expr) => {{
            let r = extract_or_fail!($num, Number::Real);
            let rat = extract_or_fail!(r, Real::Rational);
            *rat.0
        }};
    }

    macro_rules! rational_integer {
        ($num:expr) => {{
            let r = extract_or_fail!($num, Number::Real);
            extract_or_fail!(r, Real::Integer)
        }};
    }

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
        let n = ok_or_fail!(Number::rational(4, 5));
        let (num, den) = rational_parts!(n);

        assert_eq!(extract_or_fail!(num.precision, Precision::Single), 4);
        assert_eq!(num.sign, Sign::Positive);
        assert_eq!(extract_or_fail!(den.precision, Precision::Single), 5);
        assert_eq!(den.sign, Sign::Positive);
    }

    #[test]
    fn negative_numerator() {
        let n = ok_or_fail!(Number::rational(-4, 5));
        let (num, den) = rational_parts!(n);

        assert_eq!(extract_or_fail!(num.precision, Precision::Single), 4);
        assert_eq!(num.sign, Sign::Negative);
        assert_eq!(extract_or_fail!(den.precision, Precision::Single), 5);
        assert_eq!(den.sign, Sign::Positive);
    }

    #[test]
    fn negative_denominator() {
        let n = ok_or_fail!(Number::rational(4, -5));
        let (num, den) = rational_parts!(n);

        assert_eq!(extract_or_fail!(num.precision, Precision::Single), 4);
        assert_eq!(num.sign, Sign::Negative);
        assert_eq!(extract_or_fail!(den.precision, Precision::Single), 5);
        assert_eq!(den.sign, Sign::Positive);
    }

    #[test]
    fn negative_parts() {
        let n = ok_or_fail!(Number::rational(-4, -5));
        let (num, den) = rational_parts!(n);

        assert_eq!(extract_or_fail!(num.precision, Precision::Single), 4);
        assert_eq!(num.sign, Sign::Positive);
        assert_eq!(extract_or_fail!(den.precision, Precision::Single), 5);
        assert_eq!(den.sign, Sign::Positive);
    }

    #[test]
    fn improper() {
        let n = ok_or_fail!(Number::rational(5, 4));
        let (num, den) = rational_parts!(n);

        assert_eq!(extract_or_fail!(num.precision, Precision::Single), 5);
        assert_eq!(num.sign, Sign::Positive);
        assert_eq!(extract_or_fail!(den.precision, Precision::Single), 4);
        assert_eq!(den.sign, Sign::Positive);
    }

    #[test]
    fn gcd() {
        let n = ok_or_fail!(Number::rational(4, 10));
        let (num, den) = rational_parts!(n);

        assert_eq!(extract_or_fail!(num.precision, Precision::Single), 2);
        assert_eq!(num.sign, Sign::Positive);
        assert_eq!(extract_or_fail!(den.precision, Precision::Single), 5);
        assert_eq!(den.sign, Sign::Positive);
    }

    #[test]
    fn gcd_negative() {
        let n = ok_or_fail!(Number::rational(-4, 10));
        let (num, den) = rational_parts!(n);

        assert_eq!(extract_or_fail!(num.precision, Precision::Single), 2);
        assert_eq!(num.sign, Sign::Negative);
        assert_eq!(extract_or_fail!(den.precision, Precision::Single), 5);
        assert_eq!(den.sign, Sign::Positive);
    }

    #[test]
    fn unity() {
        let n = ok_or_fail!(Number::rational(1, 1));
        let int = rational_integer!(n);

        assert_eq!(extract_or_fail!(int.precision, Precision::Single), 1);
        assert_eq!(int.sign, Sign::Positive);
    }

    #[test]
    fn reduce_to_unity() {
        let n = ok_or_fail!(Number::rational(7, 7));
        let int = rational_integer!(n);

        assert_eq!(extract_or_fail!(int.precision, Precision::Single), 1);
        assert_eq!(int.sign, Sign::Positive);
    }

    #[test]
    fn reduce_to_negative_unity() {
        let n = ok_or_fail!(Number::rational(-7, 7));
        let int = rational_integer!(n);

        assert_eq!(extract_or_fail!(int.precision, Precision::Single), 1);
        assert_eq!(int.sign, Sign::Negative);
    }

    #[test]
    fn reduce_to_integer() {
        let n = ok_or_fail!(Number::rational(20, 10));
        let int = rational_integer!(n);

        assert_eq!(extract_or_fail!(int.precision, Precision::Single), 2);
        assert_eq!(int.sign, Sign::Positive);
    }

    #[test]
    fn reduce_to_negative_integer() {
        let n = ok_or_fail!(Number::rational(-20, 10));
        let int = rational_integer!(n);

        assert_eq!(extract_or_fail!(int.precision, Precision::Single), 2);
        assert_eq!(int.sign, Sign::Negative);
    }

    #[test]
    fn zero_numerator() {
        let n = ok_or_fail!(Number::rational(0, 7));
        let int = rational_integer!(n);

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
        let n = Number::rational(1, 0);
        let err = err_or_fail!(n);

        assert!(matches!(err, NumericError::DivideByZero));
    }

    #[test]
    fn positive_into_float() {
        let n = ok_or_fail!(Number::rational(4, 5));
        let r = extract_or_fail!(n, Number::Real);

        assert_eq!(r.into_float(), 0.8);
    }

    #[test]
    fn negative_numerator_into_float() {
        let n = ok_or_fail!(Number::rational(-4, 5));
        let r = extract_or_fail!(n, Number::Real);

        assert_eq!(r.into_float(), -0.8);
    }

    #[test]
    fn negative_denominator_into_float() {
        let n = ok_or_fail!(Number::rational(4, -5));
        let r = extract_or_fail!(n, Number::Real);

        assert_eq!(r.into_float(), -0.8);
    }

    #[test]
    fn negative_parts_into_float() {
        let n = ok_or_fail!(Number::rational(-4, -5));
        let r = extract_or_fail!(n, Number::Real);

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
}

mod complex {
    use super::*;

    #[test]
    fn basic() {
        let c = Number::complex(4, 3);

        let ri = extract_or_fail!(c, Number::Complex);
        let r = extract_or_fail!(ri.0 .0, Real::Integer);
        assert!(!r.is_zero());
        assert_eq!(extract_or_fail!(r.precision, Precision::Single), 4);
        let i = extract_or_fail!(ri.0 .1, Real::Integer);
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
        let r = extract_or_fail!(ri.0 .0, Real::Integer);
        assert!(r.is_zero());
        assert_eq!(extract_or_fail!(r.precision, Precision::Single), 0);
        let i = extract_or_fail!(ri.0 .1, Real::Integer);
        assert!(!i.is_zero());
        assert_eq!(extract_or_fail!(i.precision, Precision::Single), 3);
    }
}
