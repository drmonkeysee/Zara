use std::{
    cmp::Ordering,
    error::Error,
    fmt::{self, Display, Formatter, Write},
    result::Result,
};

#[derive(Debug)]
pub(crate) enum Number {
    Complex(Box<(Real, Real)>), // NOTE: Boxed to keep enum size down
    Real(Real),
}

impl Number {
    // TODO: need full combo of ctors for Rational
    // TODO: polar-coordinates
    pub(crate) fn complex(real: impl Into<Real>, imag: impl Into<Real>) -> Self {
        Self::Complex((real.into(), imag.into()).into())
    }

    pub(crate) fn real(value: impl Into<Real>) -> Self {
        Self::Real(value.into())
    }

    pub(crate) fn rational(
        numerator: impl Into<Integer>,
        denominator: impl Into<Integer>,
    ) -> Result<Self, NumericError> {
        Ok(Self::Real((numerator, denominator).try_into()?))
    }

    pub(crate) fn as_datum(&self) -> Datum {
        Datum(self)
    }

    pub(crate) fn as_token_descriptor(&self) -> TokenDescriptor {
        TokenDescriptor(self)
    }
}

#[derive(Debug)]
pub(crate) enum Real {
    Float(f64),
    Integer(Integer),
    Rational(Rational),
}

impl Real {
    fn reduce(
        numerator: impl Into<Integer>,
        denominator: impl Into<Integer>,
    ) -> Result<Self, NumericError> {
        let mut d = denominator.into();
        if d.is_zero() {
            return Err(NumericError::DivideByZero);
        }
        let mut n = numerator.into();
        if n.sign == d.sign {
            n.make_positive();
            d.make_positive();
        } else {
            n.make_negative();
            d.make_positive();
        }
        if n.is_zero() || d.is_one() {
            return Ok(Self::Integer(n));
        }
        if n.cmp_magnitude(&d) == Ordering::Equal {
            return Ok(Self::Integer(Integer::single(1, n.sign)));
        }
        n.reduce(&mut d);
        if d.is_one() {
            return Ok(Self::Integer(n));
        }
        Ok(Self::Rational(Rational((n, d).into())))
    }
}

impl Display for Real {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Real::Float(d) => FloatDatum(d).fmt(f),
            Real::Integer(n) => n.fmt(f),
            Real::Rational(r) => r.fmt(f),
        }
    }
}

impl From<f64> for Real {
    fn from(value: f64) -> Self {
        Self::Float(value)
    }
}

impl<T: Into<Integer>> From<T> for Real {
    fn from(value: T) -> Self {
        Self::Integer(value.into())
    }
}

impl<N: Into<Integer>, D: Into<Integer>> TryFrom<(N, D)> for Real {
    type Error = NumericError;

    fn try_from(value: (N, D)) -> Result<Self, Self::Error> {
        let (numerator, denominator) = value;
        Self::reduce(numerator, denominator)
    }
}

// NOTE: Boxed to keep struct size down
#[derive(Debug)]
pub(crate) struct Rational(Box<(Integer, Integer)>);

impl Display for Rational {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.0 .0.fmt(f)?;
        write!(f, "/{}", self.0 .1)
    }
}

#[derive(Debug)]
pub(crate) struct Integer {
    precision: Precision,
    sign: Sign,
}

impl Integer {
    fn single(magnitude: u64, mut sign: Sign) -> Self {
        if magnitude == 0 {
            sign = Sign::Zero;
        }
        Self {
            precision: Precision::Single(magnitude),
            sign,
        }
    }

    fn is_zero(&self) -> bool {
        self.sign == Sign::Zero
    }

    fn is_one(&self) -> bool {
        self.sign == Sign::Positive
            && match &self.precision {
                Precision::Single(u) => *u == 1,
                Precision::Multiple(_) => todo!(),
            }
    }

    fn cmp_magnitude(&self, other: &Self) -> Ordering {
        self.precision.cmp(&other.precision)
    }

    fn make_positive(&mut self) {
        if self.sign == Sign::Negative {
            self.sign = Sign::Positive;
        }
    }

    fn make_negative(&mut self) {
        if self.sign == Sign::Positive {
            self.sign = Sign::Negative;
        }
    }

    fn reduce(&mut self, other: &mut Self) {
        self.precision.reduce(&mut other.precision);
    }
}

impl Display for Integer {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.sign.fmt(f)?;
        match self.precision {
            Precision::Single(u) => write!(f, "{u}"),
            Precision::Multiple(_) => todo!(),
        }
    }
}

// TODO: handle multi-precision later
impl From<i64> for Integer {
    fn from(value: i64) -> Self {
        Self {
            precision: Precision::Single(value.unsigned_abs()),
            sign: value.into(),
        }
    }
}

#[derive(Debug)]
pub(crate) enum NumericError {
    DivideByZero,
}

impl Display for NumericError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::DivideByZero => f.write_str("divide by zero"),
        }
    }
}

impl Error for NumericError {}

pub(crate) struct Datum<'a>(&'a Number);

impl Display for Datum<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.0 {
            Number::Complex(c) => write!(f, "{}{:+}i", c.0, c.1),
            Number::Real(r) => write!(f, "{r}"),
        }
    }
}

pub(crate) struct TokenDescriptor<'a>(&'a Number);

impl Display for TokenDescriptor<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.0 {
            Number::Complex(_) => f.write_str("CPX"),
            Number::Real(r) => match r {
                Real::Float(_) => f.write_str("FLT"),
                Real::Integer(_) => f.write_str("INT"),
                Real::Rational(_) => f.write_str("RAT"),
            },
        }
    }
}

#[derive(Debug, Eq)]
enum Precision {
    Single(u64),
    Multiple(Vec<u64>), // TODO: can this be Box<[u64]>
}

impl Precision {
    fn reduce(&mut self, other: &mut Self) {
        match self {
            Self::Single(a) => match other {
                Self::Single(b) => {
                    let gcd = gcd_euclidean(*a, *b);
                    *self = Self::Single(*a / gcd);
                    *other = Self::Single(*b / gcd);
                }
                Self::Multiple(_) => todo!(),
            },
            Self::Multiple(_) => todo!(),
        }
    }
}

impl PartialEq for Precision {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Precision::Single(a) => match other {
                Precision::Single(b) => a.eq(b),
                Precision::Multiple(_) => todo!(),
            },
            Precision::Multiple(_) => todo!(),
        }
    }
}

impl PartialOrd for Precision {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Precision {
    fn cmp(&self, other: &Self) -> Ordering {
        match self {
            Precision::Single(a) => match other {
                Precision::Single(b) => a.cmp(b),
                Precision::Multiple(_) => todo!(),
            },
            Precision::Multiple(_) => todo!(),
        }
    }
}

// NOTE: enum expression of the signum function
#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
enum Sign {
    Negative = -1,
    Zero,
    Positive,
}

impl Display for Sign {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if *self == Sign::Negative {
            f.write_char('-')
        } else if f.sign_plus() {
            f.write_char('+')
        } else {
            Ok(())
        }
    }
}

macro_rules! sign_from {
    ($type:ty) => {
        impl From<$type> for Sign {
            fn from(value: $type) -> Self {
                match value.signum() as i32 {
                    -1 => Self::Negative,
                    0 => Self::Zero,
                    _ => Self::Positive,
                }
            }
        }
    };
}

sign_from!(i64);
sign_from!(f64);

struct FloatDatum<'a>(&'a f64);

impl Display for FloatDatum<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let d = self.0;
        if d.is_infinite() {
            let s = Sign::from(*d);
            write!(f, "{s:+}inf.0")
        } else if d.is_nan() {
            f.write_str("+nan.0")
        } else {
            fmt::Debug::fmt(d, f)
        }
    }
}

// NOTE: https://en.wikipedia.org/wiki/Euclidean_algorithm
fn gcd_euclidean(mut a: u64, mut b: u64) -> u64 {
    while b > 0 {
        let t = b;
        b = a % b;
        a = t;
    }
    a
}

#[cfg(test)]
mod tests {
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
            let r = ok_or_fail!(Real::try_from((3, 5)));
            let i = ok_or_fail!(Real::try_from((5, 2)));
            let n = Number::complex(r, i);

            assert_eq!(n.as_datum().to_string(), "3/5+5/2i");
        }

        #[test]
        fn complex_negative_rat_real() {
            let r = ok_or_fail!(Real::try_from((-3, 5)));
            let i = ok_or_fail!(Real::try_from((5, 2)));
            let n = Number::complex(r, i);

            assert_eq!(n.as_datum().to_string(), "-3/5+5/2i");
        }

        #[test]
        fn complex_negative_rat_imag() {
            let r = ok_or_fail!(Real::try_from((3, 5)));
            let i = ok_or_fail!(Real::try_from((-5, 2)));
            let n = Number::complex(r, i);

            assert_eq!(n.as_datum().to_string(), "3/5-5/2i");
        }

        #[test]
        fn complex_negative_rat() {
            let r = ok_or_fail!(Real::try_from((-3, 5)));
            let i = ok_or_fail!(Real::try_from((-5, 2)));
            let n = Number::complex(r, i);

            assert_eq!(n.as_datum().to_string(), "-3/5-5/2i");
        }

        #[test]
        fn complex_real_rat() {
            let r = ok_or_fail!(Real::try_from((3, 5)));
            let n = Number::complex(r, 5);

            assert_eq!(n.as_datum().to_string(), "3/5+5i");
        }

        #[test]
        fn complex_imag_rat() {
            let i = ok_or_fail!(Real::try_from((5, 2)));
            let n = Number::complex(3, i);

            assert_eq!(n.as_datum().to_string(), "3+5/2i");
        }

        #[test]
        fn complex_real_float_imag_rat() {
            let i = ok_or_fail!(Real::try_from((5, 2)));
            let n = Number::complex(3.032, i);

            assert_eq!(n.as_datum().to_string(), "3.032+5/2i");
        }

        #[test]
        fn complex_real_rat_imag_float() {
            let r = ok_or_fail!(Real::try_from((3, 5)));
            let n = Number::complex(r, -6.34);

            assert_eq!(n.as_datum().to_string(), "3/5-6.34i");
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
        fn is_one() {
            let cases = [(-4, false), (-1, false), (0, false), (1, true), (4, false)];
            for (case, expected) in cases {
                let n = case.into();
                let int = extract_or_fail!(n, Real::Integer);

                assert_eq!(int.is_one(), expected);
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
        fn zero_denominator() {
            let n = Number::rational(1, 0);
            let err = err_or_fail!(n);

            assert!(matches!(err, NumericError::DivideByZero));
        }
    }
}
