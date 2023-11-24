use std::{
    error::Error,
    fmt::{self, Display, Formatter},
    result::Result,
};

#[derive(Debug)]
pub enum Number {
    Complex(Box<(Real, Real)>), // NOTE: Boxed to keep enum size down
    Real(Real),
}

impl Number {
    // TODO: need full combo of ctors for Rational
    pub(crate) fn complex(real: impl Into<Real>, imag: impl Into<Real>) -> Self {
        Self::Complex((real.into(), imag.into()).into())
    }

    pub(crate) fn real(value: impl Into<Real>) -> Self {
        Self::Real(value.into())
    }

    pub(crate) fn rational(
        value: impl TryInto<Real, Error = RationalError>,
    ) -> Result<Self, RationalError> {
        Ok(Self::Real(value.try_into()?))
    }

    pub(crate) fn as_datum(&self) -> Datum {
        Datum(self)
    }

    pub(crate) fn as_token_descriptor(&self) -> TokenDescriptor {
        TokenDescriptor(self)
    }
}

#[derive(Debug)]
pub enum Real {
    Inexact(f64),
    Integer(Exact),
    Rational(Rational),
}

impl From<f64> for Real {
    fn from(value: f64) -> Self {
        Self::Inexact(value)
    }
}

impl From<i64> for Real {
    fn from(value: i64) -> Self {
        Self::Integer(Exact::Native(value))
    }
}

// TODO: need full combo of converters for Rational
impl From<BigInt> for Real {
    fn from(value: BigInt) -> Self {
        Self::Integer(Exact::Big(value))
    }
}

impl TryFrom<(i64, i64)> for Real {
    type Error = RationalError;

    fn try_from(value: (i64, i64)) -> Result<Self, Self::Error> {
        todo!()
    }
}

#[derive(Debug)]
pub enum Exact {
    Native(i64),
    Big(BigInt),
}

#[derive(Debug)]
pub struct Rational(Box<(Exact, Exact)>); // NOTE: Boxed to keep struct size down

#[derive(Debug)]
pub struct RationalError;

impl Display for RationalError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        todo!()
    }
}

impl Error for RationalError {}

#[derive(Debug)]
pub struct BigInt {
    digits: Vec<u64>,
    sign: Sign,
}

impl BigInt {
    pub(crate) fn tempctor(v: i64) -> Self {
        // TODO: should this use signum instead?
        let sign = if v < 0 {
            Sign::Negative
        } else {
            Sign::Positive
        };
        Self {
            digits: vec![v.try_into().unwrap()],
            sign,
        }
    }
}

pub(crate) struct Datum<'a>(&'a Number);

impl Display for Datum<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        todo!()
    }
}

pub(crate) struct TokenDescriptor<'a>(&'a Number);

impl Display for TokenDescriptor<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.0 {
            Number::Complex(_) => f.write_str("CPX"),
            Number::Real(r) => match r {
                Real::Inexact(_) => f.write_str("FLT"),
                Real::Integer(_) => f.write_str("INT"),
                Real::Rational(_) => f.write_str("RAT"),
            },
        }
    }
}

#[derive(Debug)]
enum Sign {
    Negative,
    Positive,
}

#[cfg(test)]
mod tests {
    use super::*;

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
            let n = Number::rational((4, 5));
            assert!(n.is_ok());
            let n = n.unwrap();

            assert_eq!(n.as_token_descriptor().to_string(), "RAT");
        }

        #[test]
        fn complex() {
            let n = Number::complex(3, 5);

            assert_eq!(n.as_token_descriptor().to_string(), "CPX");
        }

        #[test]
        fn bigint() {
            let n = Number::real(BigInt::tempctor(30));

            assert_eq!(n.as_token_descriptor().to_string(), "INT");
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

            assert_eq!(n.as_datum().to_string(), "");
        }

        #[test]
        fn int_min() {
            let n = Number::real(i64::MIN);

            assert_eq!(n.as_datum().to_string(), "");
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
            let n = Number::rational((3, 4));
            assert!(n.is_ok());
            let n = n.unwrap();

            assert_eq!(n.as_datum().to_string(), "3/4");
        }

        #[test]
        fn negative_numerator() {
            let n = Number::rational((-3, 4));
            assert!(n.is_ok());
            let n = n.unwrap();

            assert_eq!(n.as_datum().to_string(), "-3/4");
        }

        #[test]
        fn negative_denominator() {
            let n = Number::rational((3, -4));
            assert!(n.is_ok());
            let n = n.unwrap();

            assert_eq!(n.as_datum().to_string(), "-3/4");
        }

        #[test]
        fn negative_numerator_and_denominator() {
            let n = Number::rational((-3, -4));
            assert!(n.is_ok());
            let n = n.unwrap();

            assert_eq!(n.as_datum().to_string(), "3/4");
        }

        #[test]
        fn greater_than_one_rational() {
            let n = Number::rational((4, 3));
            assert!(n.is_ok());
            let n = n.unwrap();

            assert_eq!(n.as_datum().to_string(), "4/3");
        }
    }

    mod rational {
        use super::*;

        #[test]
        fn min_over_max() {
            let n = Number::rational((i64::MIN, i64::MAX));
            assert!(n.is_ok());
            let n = n.unwrap();

            assert_eq!(n.as_datum().to_string(), "-3/4");
        }

        #[test]
        fn max_over_min() {
            let n = Number::rational((i64::MAX, i64::MIN));
            assert!(n.is_ok());
            let n = n.unwrap();

            assert_eq!(n.as_datum().to_string(), "-3/4");
        }

        #[test]
        fn min_forced_to_positive() {
            let n = Number::rational((i64::MIN, -4));
            assert!(n.is_ok());
            let n = n.unwrap();

            assert_eq!(n.as_datum().to_string(), "-3/4");
        }

        #[test]
        fn rational_integer() {
            let n = Number::rational((3, 1));
            assert!(n.is_ok());
            let n = n.unwrap();

            // TODO: should this reduce to an error?
            assert_eq!(n.as_datum().to_string(), "3/1");
        }

        #[test]
        fn unity() {
            let n = Number::rational((1, 1));
            assert!(n.is_ok());
            let n = n.unwrap();

            // TODO: should this reduce to an error?
            assert_eq!(n.as_datum().to_string(), "1/1");
        }

        #[test]
        fn zero_numerator() {
            let n = Number::rational((0, 1));
            assert!(n.is_ok());
            let n = n.unwrap();

            // TODO: should this reduce to an error?
            assert_eq!(n.as_datum().to_string(), "0/1");
        }
    }
}
