#[cfg(test)]
mod tests;

use std::{
    cmp::Ordering,
    error::Error,
    fmt::{self, Display, Formatter, Write},
    result::Result,
};

#[derive(Debug)]
pub(crate) enum Number {
    Complex(Complex),
    Real(Real),
}

impl Number {
    pub(crate) fn complex(real: impl Into<Real>, imag: impl Into<Real>) -> Self {
        let (real, imag): (Real, Real) = (real.into(), imag.into());
        if imag.is_zero() {
            Self::real(real)
        } else {
            Self::Complex(Complex((real, imag).into()))
        }
    }

    pub(crate) fn polar(magnitude: impl Into<Real>, radians: impl Into<Real>) -> Self {
        let (mag, rad) = (magnitude.into(), radians.into());
        if mag.is_zero() {
            Self::real(0)
        } else if rad.is_zero() {
            Self::real(mag)
        } else {
            let (mag, rad) = (mag.into_float(), rad.into_float());
            let (rsin, rcos) = rad.sin_cos();
            Self::complex(mag * rcos, mag * rsin)
        }
    }

    pub(crate) fn real(value: impl Into<Real>) -> Self {
        Self::Real(value.into())
    }

    pub(crate) fn rational(
        numerator: impl Into<Integer>,
        denominator: impl Into<Integer>,
    ) -> Result<Self, NumericError> {
        Ok(Self::real(Real::reduce(numerator, denominator)?))
    }

    pub(crate) fn as_datum(&self) -> Datum {
        Datum(self)
    }

    pub(crate) fn as_token_descriptor(&self) -> TokenDescriptor {
        TokenDescriptor(self)
    }
}

#[derive(Debug)]
pub(crate) struct Complex(Box<(Real, Real)>);

#[derive(Debug)]
pub(crate) enum Real {
    Float(f64),
    Integer(Integer),
    Rational(Rational),
}

impl Real {
    pub(crate) fn as_token_descriptor(&self) -> RealTokenDescriptor {
        RealTokenDescriptor(self)
    }

    pub(crate) fn reduce(
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
        if n.is_zero() || d.is_magnitude_one() {
            return Ok(Self::Integer(n));
        }
        if n.cmp_magnitude(&d) == Ordering::Equal {
            return Ok(Self::Integer(Integer::single(1, n.sign)));
        }
        n.reduce(&mut d);
        if d.is_magnitude_one() {
            return Ok(Self::Integer(n));
        }
        Ok(Self::Rational(Rational((n, d).into())))
    }

    fn is_zero(&self) -> bool {
        match self {
            Real::Float(f) => *f == 0.0,
            Real::Integer(n) => n.is_zero(),
            Real::Rational(r) => r.is_zero(),
        }
    }

    fn into_float(self) -> f64 {
        match self {
            Self::Float(f) => f,
            Self::Integer(n) => n.into_float(),
            Self::Rational(r) => r.into_float(),
        }
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

#[derive(Debug)]
pub(crate) struct Rational(Box<(Integer, Integer)>);

impl Rational {
    pub(crate) fn into_inexact(self) -> Real {
        Real::Float(self.into_float())
    }

    fn is_zero(&self) -> bool {
        self.0 .0.is_zero()
    }

    fn into_float(self) -> f64 {
        let (num, denom) = (self.0 .0.into_float(), self.0 .1.into_float());
        num / denom
    }
}

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

    pub(crate) fn into_inexact(self) -> Real {
        Real::Float(self.into_float())
    }

    fn is_zero(&self) -> bool {
        self.sign == Sign::Zero
    }

    fn is_magnitude_one(&self) -> bool {
        match &self.precision {
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

    fn into_float(self) -> f64 {
        match self.precision {
            Precision::Single(u) => {
                let f = u as f64;
                if self.sign == Sign::Negative {
                    -f
                } else {
                    f
                }
            }
            Precision::Multiple(_) => todo!(),
        }
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

impl From<(Sign, u64)> for Integer {
    fn from(value: (Sign, u64)) -> Self {
        Self::single(value.1, value.0)
    }
}

// NOTE: enum expression of the signum function
#[derive(Clone, Copy, Debug, Default, Eq, Ord, PartialEq, PartialOrd)]
pub(crate) enum Sign {
    Negative = -1,
    Zero,
    #[default]
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
                    1 => Self::Positive,
                    _ => unreachable!(),
                }
            }
        }
    };
}

sign_from!(i64);
sign_from!(f64);

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
            Number::Complex(Complex(c)) => {
                ComplexRealDatum(&c.0).fmt(f)?;
                ComplexImagDatum(&c.1).fmt(f)
            }
            Number::Real(r) => r.fmt(f),
        }
    }
}

pub(crate) struct TokenDescriptor<'a>(&'a Number);

impl Display for TokenDescriptor<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.0 {
            Number::Complex(_) => f.write_str("CPX"),
            Number::Real(r) => r.as_token_descriptor().fmt(f),
        }
    }
}

pub(crate) struct RealTokenDescriptor<'a>(&'a Real);

impl Display for RealTokenDescriptor<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.0 {
            Real::Float(_) => f.write_str("FLT"),
            Real::Integer(_) => f.write_str("INT"),
            Real::Rational(_) => f.write_str("RAT"),
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

struct ComplexRealDatum<'a>(&'a Real);

impl Display for ComplexRealDatum<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let r = self.0;
        if let Real::Integer(n) = r {
            if n.is_zero() {
                return Ok(());
            }
        }
        r.fmt(f)
    }
}

struct ComplexImagDatum<'a>(&'a Real);

impl Display for ComplexImagDatum<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let r = self.0;
        if let Real::Integer(n) = r {
            if n.is_zero() {
                return Ok(());
            }
            if n.is_magnitude_one() {
                return write!(f, "{:+}i", n.sign);
            }
        }
        write!(f, "{r:+}i")
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
