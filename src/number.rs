use std::{
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
        let mut d: Integer = denominator.into();
        if d.is_zero() {
            return Err(NumericError::DivideByZero);
        }
        let mut n: Integer = numerator.into();
        if n.is_zero() {
            return Ok(Self::Integer(n));
        }
        if n.sign == d.sign {
            n.make_positive();
            d.make_positive();
        } else {
            n.make_negative();
            d.make_positive();
        }
        Ok(Self::Rational(Rational((n, d).into())))
    }
}

impl Display for Real {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Real::Float(_) => todo!(),
            Real::Integer(n) => write!(f, "{n}"),
            Real::Rational(_) => todo!(),
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
        Ok(Self::reduce(numerator, denominator)?)
    }
}

#[derive(Debug)]
pub(crate) struct Integer {
    precision: Precision,
    sign: Sign,
}

impl Integer {
    fn is_zero(&self) -> bool {
        self.sign == Sign::Zero
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
}

impl Display for Integer {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}{}",
            self.sign,
            match self.precision {
                Precision::Single(u) => u,
                Precision::Multiple(_) => todo!(),
            }
        )
    }
}

// TODO: handle multi-precision later
impl From<i64> for Integer {
    fn from(value: i64) -> Self {
        Self {
            precision: Precision::Single(value.unsigned_abs()),
            sign: Sign::from_signed(value),
        }
    }
}

// NOTE: Boxed to keep struct size down
#[derive(Debug)]
pub(crate) struct Rational(Box<(Integer, Integer)>);

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
            Number::Complex(_) => todo!(),
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

#[derive(Debug)]
enum Precision {
    Single(u64),
    Multiple(Vec<u64>), // TODO: can this be Box<[u64]>
}

// NOTE: enum expression of the signum function
#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
enum Sign {
    Negative = -1,
    Zero,
    Positive,
}

impl Sign {
    fn from_signed(n: i64) -> Self {
        match n.signum() {
            -1 => Self::Negative,
            0 => Self::Zero,
            // NOTE: *technically* this could be fallible but signum is
            // guaranteed to only return (-1, 0, 1) so this won't actually fail.
            _ => Self::Positive,
        }
    }
}

impl Display for Sign {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if *self == Sign::Negative {
            f.write_char('-')
        } else if f.sign_plus() {
            f.write_char('+')
        } else {
            f.write_str("")
        }
    }
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
                let s = Sign::from_signed(case);
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

            assert_eq!(n.as_datum().to_string(), "234.23");
        }

        #[test]
        fn small_exponent() {
            let n = Number::real(1e-29);

            assert_eq!(n.as_datum().to_string(), "234.23");
        }

        #[test]
        fn rounding_error() {
            let n = Number::real(0.1 + 0.2);

            assert_eq!(n.as_datum().to_string(), "234.23");
        }

        #[test]
        fn max_float() {
            let n = Number::real(f64::MAX);

            assert_eq!(n.as_datum().to_string(), "234.23");
        }

        #[test]
        fn min_float() {
            let n = Number::real(f64::MIN);

            assert_eq!(n.as_datum().to_string(), "234.23");
        }

        #[test]
        fn positive_min_float() {
            let n = Number::real(f64::MIN_POSITIVE);

            assert_eq!(n.as_datum().to_string(), "234.23");
        }

        #[test]
        fn epsilon() {
            let n = Number::real(f64::EPSILON);

            assert_eq!(n.as_datum().to_string(), "234.23");
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
        fn positive() {
            let n: Real = 42.into();
            let int = extract_or_fail!(n, Real::Integer);

            assert_eq!(extract_or_fail!(int.precision, Precision::Single), 42);
            assert_eq!(int.sign, Sign::Positive);
        }

        #[test]
        fn zero() {
            let n: Real = 0.into();
            let int = extract_or_fail!(n, Real::Integer);

            assert_eq!(extract_or_fail!(int.precision, Precision::Single), 0);
            assert_eq!(int.sign, Sign::Zero);
        }

        #[test]
        fn negative() {
            let n: Real = (-42).into();
            let int = extract_or_fail!(n, Real::Integer);

            assert_eq!(extract_or_fail!(int.precision, Precision::Single), 42);
            assert_eq!(int.sign, Sign::Negative);
        }

        #[test]
        fn max() {
            let n: Real = i64::MAX.into();
            let int = extract_or_fail!(n, Real::Integer);

            assert_eq!(
                extract_or_fail!(int.precision, Precision::Single),
                9223372036854775807
            );
            assert_eq!(int.sign, Sign::Positive);
        }

        #[test]
        fn min() {
            let n: Real = i64::MIN.into();
            let int = extract_or_fail!(n, Real::Integer);

            assert_eq!(
                extract_or_fail!(int.precision, Precision::Single),
                9223372036854775808
            );
            assert_eq!(int.sign, Sign::Negative);
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
        fn greater_than_one() {
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
        fn reduce_to_integer() {
            let n = ok_or_fail!(Number::rational(20, 10));
            let int = rational_integer!(n);

            assert_eq!(extract_or_fail!(int.precision, Precision::Single), 2);
            assert_eq!(int.sign, Sign::Positive);
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
