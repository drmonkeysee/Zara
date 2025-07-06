macro_rules! try_int_conversion {
    ($type:ty, $convert:ident) => {
        impl TryFrom<Number> for $type {
            type Error = NumericError;

            fn try_from(value: Number) -> Result<Self, Self::Error> {
                <Self as TryFrom<&Number>>::try_from(&value)
            }
        }

        impl TryFrom<&Number> for $type {
            type Error = NumericError;

            fn try_from(value: &Number) -> Result<Self, Self::Error> {
                match value {
                    Number::Real(Real::Integer(n)) => n.$convert(),
                    _ => Err(Self::Error::IntConversionInvalidType(
                        value.as_typename().to_string(),
                    )),
                }
            }
        }
    };
}

macro_rules! uint_convert {
    ($name:ident, $type:ty, $err:expr) => {
        fn $name(&self) -> Result<$type, NumericError> {
            if self.is_negative() {
                return Err($err);
            }
            let Precision::Single(u) = self.precision else {
                return Err($err);
            };
            u.try_into().map_err(|_| $err)
        }
    };
}

macro_rules! sign_from {
    ($type:ty) => {
        impl From<$type> for Sign {
            fn from(value: $type) -> Self {
                #[allow(
                    clippy::cast_possible_truncation,
                    reason = "conversion is never called for invalid value"
                )]
                match value.signum() as i32 {
                    -1 => Self::Negative,
                    0 => Self::Zero,
                    1 => Self::Positive,
                    _ => unreachable!("unexpected value from signum()"),
                }
            }
        }
    };
}

#[cfg(test)]
mod tests;

use crate::txt::TxtSpan;
use std::{
    cmp::Ordering,
    error::Error,
    fmt::{self, Display, Formatter, Write},
    num::{IntErrorKind, ParseFloatError, ParseIntError},
    rc::Rc,
    result::Result,
};

pub(crate) const INF_STR: &str = "inf.0";
pub(crate) const NAN_STR: &str = "nan.0";
// NOTE: 2^53 - 1; maximum safe integer in f64 format
// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/MAX_SAFE_INTEGER
const FMAX_INT: f64 = 9_007_199_254_740_991.0;

pub(crate) type NumResult = Result<Number, NumericError>;
pub(crate) type RealResult = Result<Real, NumericError>;
pub(crate) type IntResult = Result<Integer, NumericError>;

/*
 * All predicates, equivalence relationships, and operators assume normalized
 * numeric values such as:
 * - sign attached to numerator
 * - no zero denominator
 * - rationals reduced to integers
 * - single-item MPs reduced to single precision
 * - etc
 * This way we don't have to worry about whether 4/5 == 8/10, 5/1 is an Integer
 * or a Rational, or MP([1234]) is equivalent to SP(1234).
 */

#[derive(Clone, Debug)]
pub(crate) enum Number {
    Complex(Complex),
    Real(Real),
}

impl Number {
    pub(crate) fn nan() -> Self {
        Self::Real(Real::nan())
    }

    pub(crate) fn complex(real: impl Into<Real>, imag: impl Into<Real>) -> Self {
        let (real, imag) = (real.into(), imag.into());
        if imag.is_exact_zero() {
            Self::real(real)
        } else {
            Self::Complex(Complex((real, imag).into()))
        }
    }

    pub(crate) fn polar(magnitude: impl Into<Real>, radians: impl Into<Real>) -> Self {
        let (mag, rad) = (magnitude.into(), radians.into());
        if mag.is_exact_zero() || rad.is_exact_zero() {
            Self::real(mag)
        } else {
            let (mag, rad) = (mag.to_float(), rad.to_float());
            let (rsin, rcos) = rad.sin_cos();
            Self::complex(mag * rcos, mag * rsin)
        }
    }

    pub(crate) fn imaginary(value: impl Into<Real>) -> Self {
        Self::complex(0, value)
    }

    pub(crate) fn real(value: impl Into<Real>) -> Self {
        Self::Real(value.into())
    }

    // NOTE: From<usize> would clash with existing Integer From<i64>
    pub(crate) fn from_usize(val: usize) -> Self {
        Self::real(Integer::from_usize(val))
    }

    pub(crate) fn is_inexact(&self) -> bool {
        match self {
            Self::Complex(Complex(z)) => z.0.is_inexact() || z.1.is_inexact(),
            Self::Real(r) => r.is_inexact(),
        }
    }

    pub(crate) fn is_infinite(&self) -> bool {
        match self {
            Self::Complex(Complex(z)) => z.0.is_infinite() || z.1.is_infinite(),
            Self::Real(r) => r.is_infinite(),
        }
    }

    pub(crate) fn is_nan(&self) -> bool {
        match self {
            Self::Complex(Complex(z)) => z.0.is_nan() || z.1.is_nan(),
            Self::Real(r) => r.is_nan(),
        }
    }

    pub(crate) fn is_zero(&self) -> bool {
        match self {
            Self::Complex(Complex(z)) => z.0.is_zero() && z.1.is_zero(),
            Self::Real(r) => r.is_zero(),
        }
    }

    pub(crate) fn is_eqv(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Complex(Complex(a)), Self::Complex(Complex(b))) => {
                a.0.is_eqv(&b.0) && a.1.is_eqv(&b.1)
            }
            (Self::Real(a), Self::Real(b)) => a.is_eqv(b),
            _ => false,
        }
    }

    pub(crate) fn as_token_descriptor(&self) -> TokenDescriptor {
        TokenDescriptor(self)
    }

    pub(crate) fn as_typename(&self) -> NumericTypeName {
        NumericTypeName(self)
    }

    pub(crate) fn into_inexact(self) -> Self {
        match self {
            Self::Complex(Complex(z)) => Self::complex(z.0.into_inexact(), z.1.into_inexact()),
            Self::Real(r) => Self::Real(r.into_inexact()),
        }
    }

    pub(crate) fn try_into_exact(self) -> NumResult {
        Ok(match self {
            Self::Complex(Complex(z)) => {
                Self::complex(z.0.try_into_exact()?, z.1.try_into_exact()?)
            }
            Self::Real(r) => Self::Real(r.try_into_exact()?),
        })
    }
}

impl Display for Number {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::Complex(Complex(z)) => {
                ComplexRealDatum(&z.0).fmt(f)?;
                ComplexImagDatum(&z.1).fmt(f)
            }
            Self::Real(r) => r.fmt(f),
        }
    }
}

try_int_conversion!(u8, try_to_u8);
try_int_conversion!(i32, try_to_i32);
try_int_conversion!(u32, try_to_u32);
try_int_conversion!(usize, try_to_usize);

#[derive(Clone, Debug)]
pub(crate) struct Complex(Box<(Real, Real)>);

impl Complex {
    pub(crate) fn real_part(&self) -> &Real {
        &self.0.0
    }

    pub(crate) fn imag_part(&self) -> &Real {
        &self.0.1
    }

    pub(crate) fn to_magnitude(&self) -> Real {
        let x = self.0.0.to_float();
        let y = self.0.1.to_float();
        Real::Float(x.hypot(y))
    }

    pub(crate) fn to_angle(&self) -> Real {
        let x = self.0.0.to_float();
        let y = self.0.1.to_float();
        Real::Float(y.atan2(x))
    }
}

#[derive(Clone, Debug)]
pub(crate) enum Real {
    Float(f64),
    Integer(Integer),
    Rational(Rational),
}

impl Real {
    pub(crate) fn zero() -> Self {
        Integer::zero().into()
    }

    pub(crate) fn nan() -> Self {
        f64::NAN.into()
    }

    pub(crate) fn reduce(
        numerator: impl Into<Integer>,
        denominator: impl Into<Integer>,
    ) -> RealResult {
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
            return Ok(n.into());
        }
        if n.cmp_magnitude(&d) == Ordering::Equal {
            return Ok(Integer::single(1, n.sign).into());
        }
        n.reduce(&mut d);
        if d.is_magnitude_one() {
            return Ok(n.into());
        }
        Ok(Self::Rational(Rational((n, d).into())))
    }

    pub(crate) fn is_rational(&self) -> bool {
        if let Self::Float(f) = self {
            f.is_finite()
        } else {
            true
        }
    }

    pub(crate) fn is_integer(&self) -> bool {
        match self {
            Self::Float(f) => f.fract() == 0.0,
            Self::Integer(_) => true,
            Self::Rational(_) => false,
        }
    }

    pub(crate) fn is_positive(&self) -> bool {
        match self {
            Self::Float(f) => *f > 0.0,
            Self::Integer(n) => n.is_positive(),
            Self::Rational(q) => q.is_positive(),
        }
    }

    pub(crate) fn is_negative(&self) -> bool {
        match self {
            Self::Float(f) => *f < 0.0,
            Self::Integer(n) => n.is_negative(),
            Self::Rational(q) => q.is_negative(),
        }
    }

    pub(crate) fn as_token_descriptor(&self) -> RealTokenDescriptor {
        RealTokenDescriptor(self)
    }

    pub(crate) fn into_inexact(self) -> Self {
        match self {
            Self::Float(_) => self,
            Self::Integer(n) => n.into_inexact(),
            Self::Rational(q) => q.into_inexact(),
        }
    }

    pub(crate) fn into_abs(self) -> Self {
        match self {
            Self::Float(f) => f.abs().into(),
            Self::Integer(n) => n.into_abs().into(),
            Self::Rational(q) => Self::Rational(q.into_abs()),
        }
    }

    pub(crate) fn try_into_exact(self) -> RealResult {
        match self {
            Self::Float(f) => FloatSpec::try_float_to_exact(f),
            _ => Ok(self),
        }
    }

    pub(crate) fn try_into_exact_integer(self) -> IntResult {
        match self {
            Self::Float(f) if f.fract() == 0.0 => {
                if (-FMAX_INT..=FMAX_INT).contains(&f) {
                    #[allow(
                        clippy::cast_possible_truncation,
                        reason = "guarded against truncation"
                    )]
                    Ok((f as i64).into())
                } else {
                    todo!("convert f64 to multi-precision integer somehow")
                }
            }
            Self::Integer(n) => Ok(n.clone()),
            _ => Err(NumericError::NotExactInteger(self.to_string())),
        }
    }

    pub(crate) fn try_into_numerator(self) -> RealResult {
        Ok(match self {
            Self::Float(_) => self.try_into_exact()?.try_into_numerator()?.into_inexact(),
            Self::Integer(n) => n.into(),
            Self::Rational(q) => q.into_numerator().into(),
        })
    }

    pub(crate) fn try_into_denominator(self) -> RealResult {
        Ok(match self {
            Self::Float(_) => self
                .try_into_exact()?
                .try_into_denominator()?
                .into_inexact(),
            Self::Integer(_) => Integer::one().into(),
            Self::Rational(q) => q.into_denominator().into(),
        })
    }

    fn is_eqv(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Float(a), Self::Float(b)) if a.is_nan() && b.is_nan() => true,
            #[allow(
                clippy::float_cmp,
                reason = "underlying implementation does not hide epsilon inequality"
            )]
            (Self::Float(a), Self::Float(b)) => a == b,
            (Self::Integer(a), Self::Integer(b)) => a == b,
            (Self::Rational(a), Self::Rational(b)) => a == b,
            _ => false,
        }
    }

    fn is_zero(&self) -> bool {
        match self {
            Self::Float(f) => *f == 0.0,
            Self::Integer(n) => n.is_zero(),
            Self::Rational(q) => q.is_zero(),
        }
    }

    fn is_exact_zero(&self) -> bool {
        !self.is_inexact() && self.is_zero()
    }

    fn is_inexact(&self) -> bool {
        matches!(self, Self::Float(_))
    }

    fn is_infinite(&self) -> bool {
        if let Self::Float(f) = self {
            f.is_infinite()
        } else {
            false
        }
    }

    fn is_nan(&self) -> bool {
        if let Self::Float(f) = self {
            f.is_nan()
        } else {
            false
        }
    }

    // TODO: if this becomes public rewrite to TryFrom<&Number>
    fn to_float(&self) -> f64 {
        match self {
            Self::Float(f) => *f,
            Self::Integer(n) => n.to_float(),
            Self::Rational(q) => q.to_float(),
        }
    }
}

impl Display for Real {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::Float(d) => FloatDatum(d).fmt(f),
            Self::Integer(n) => n.fmt(f),
            Self::Rational(q) => q.fmt(f),
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

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct Rational(Box<(Integer, Integer)>);

impl Rational {
    fn is_positive(&self) -> bool {
        self.0.0.is_positive()
    }

    fn is_zero(&self) -> bool {
        self.0.0.is_zero()
    }

    fn is_negative(&self) -> bool {
        self.0.0.is_negative()
    }

    fn to_float(&self) -> f64 {
        let r = &self.0;
        let (num, denom) = (r.0.to_float(), r.1.to_float());
        num / denom
    }

    fn into_inexact(self) -> Real {
        self.to_float().into()
    }

    fn into_abs(mut self) -> Self {
        self.0.0 = self.0.0.into_abs();
        self
    }

    fn into_numerator(self) -> Integer {
        self.0.0
    }

    fn into_denominator(self) -> Integer {
        self.0.1
    }
}

impl Display for Rational {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let r = &self.0;
        r.0.fmt(f)?;
        write!(f, "/{}", r.1)
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct Integer {
    precision: Precision,
    sign: Sign,
}

impl Integer {
    fn zero() -> Self {
        0.into()
    }

    fn one() -> Self {
        1.into()
    }

    fn single(magnitude: u64, mut sign: Sign) -> Self {
        if magnitude == 0 {
            sign = Sign::Zero;
        }
        Self {
            precision: Precision::Single(magnitude),
            sign,
        }
    }

    fn from_usize(val: usize) -> Self {
        let r = u64::try_from(val);
        if let Ok(u) = r {
            (Sign::Positive, u).into()
        } else {
            todo!("handle multi-precision");
        }
    }

    pub(crate) fn is_even(&self) -> bool {
        self.precision.is_even()
    }

    fn is_positive(&self) -> bool {
        self.sign == Sign::Positive
    }

    fn is_zero(&self) -> bool {
        self.sign == Sign::Zero
    }

    fn is_negative(&self) -> bool {
        self.sign == Sign::Negative
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

    fn to_float(&self) -> f64 {
        match self.precision {
            Precision::Single(u) => {
                #[allow(clippy::cast_precision_loss)]
                let f = u as f64;
                if self.sign == Sign::Negative { -f } else { f }
            }
            Precision::Multiple(_) => todo!(),
        }
    }

    fn try_to_i32(&self) -> Result<i32, NumericError> {
        let Precision::Single(u) = self.precision else {
            return Err(NumericError::Int32ConversionInvalidRange);
        };
        if self.is_negative() {
            if u <= i32::MIN.unsigned_abs().into() {
                #[allow(clippy::cast_possible_wrap, reason = "guarded against wrapping")]
                (-(u as i64)).try_into()
            } else {
                return Err(NumericError::Int32ConversionInvalidRange);
            }
        } else {
            u.try_into()
        }
        .map_err(|_| NumericError::Int32ConversionInvalidRange)
    }

    uint_convert!(try_to_u8, u8, NumericError::ByteConversionInvalidRange);
    uint_convert!(try_to_u32, u32, NumericError::Uint32ConversionInvalidRange);
    uint_convert!(
        try_to_usize,
        usize,
        NumericError::UsizeConversionInvalidRange
    );

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

    fn into_inexact(self) -> Real {
        Real::Float(self.to_float())
    }

    fn into_abs(mut self) -> Self {
        self.make_positive();
        self
    }
}

impl Display for Integer {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        self.sign.fmt(f)?;
        self.precision.fmt(f)
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
    fn from((sign, val): (Sign, u64)) -> Self {
        Self::single(val, sign)
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
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        if *self == Self::Negative {
            f.write_char('-')
        } else if f.sign_plus() {
            f.write_char('+')
        } else {
            Ok(())
        }
    }
}

sign_from!(i64);
sign_from!(f64);

#[allow(private_bounds)]
pub(crate) trait Radix: RadixPrivate {
    const BASE: u32;
    const NAME: &'static str;

    fn is_digit(&self, ch: char) -> bool;
}

#[derive(Clone, Copy, Default)]
pub(crate) struct Binary;

impl Radix for Binary {
    const BASE: u32 = 2;
    const NAME: &'static str = "binary";

    fn is_digit(&self, ch: char) -> bool {
        matches!(ch, '0'..='1')
    }
}

#[derive(Clone, Copy, Default)]
pub(crate) struct Octal;

impl Radix for Octal {
    const BASE: u32 = 8;
    const NAME: &'static str = "octal";

    fn is_digit(&self, ch: char) -> bool {
        // TODO: nightly-only experimental API.
        // (is_ascii_octdigit https://github.com/rust-lang/rust/issues/101288)
        matches!(ch, '0'..='7')
    }
}

#[derive(Clone, Copy, Default)]
pub(crate) struct Decimal;

impl Radix for Decimal {
    const BASE: u32 = 10;
    const NAME: &'static str = "decimal";

    fn is_digit(&self, ch: char) -> bool {
        ch.is_ascii_digit()
    }
}

#[derive(Clone, Copy, Default)]
pub(crate) struct Hexadecimal;

impl Radix for Hexadecimal {
    const BASE: u32 = 16;
    const NAME: &'static str = "hexadecimal";

    fn is_digit(&self, ch: char) -> bool {
        ch.is_ascii_hexdigit()
    }
}

#[derive(Clone, Default)]
pub(crate) struct IntSpec<R> {
    pub(crate) magnitude: TxtSpan,
    pub(crate) radix: R,
    pub(crate) sign: Option<Sign>,
}

impl<R: Radix> IntSpec<R> {
    pub(crate) fn is_empty(&self) -> bool {
        self.magnitude.is_empty()
    }

    pub(crate) fn has_sign(&self) -> bool {
        self.sign.is_some()
    }

    pub(crate) fn try_into_exact(self, input: &str) -> IntResult {
        parse_signed(&self, input)
    }

    pub(crate) fn try_into_inexact(self, input: &str) -> RealResult {
        R::parse_inexact(self, input)
    }
}

#[derive(Clone, Default)]
pub(crate) struct FloatSpec {
    pub(crate) exponent: TxtSpan,
    pub(crate) fraction: TxtSpan,
    pub(crate) integral: IntSpec<Decimal>,
}

impl FloatSpec {
    fn try_float_to_exact(flt: f64) -> RealResult {
        if flt.is_finite() {
            let (spec, flt_str) = Self::prep_parse(flt);
            // NOTE: this should never fail since the string input comes from
            // an f64 but if something real weird happens return parse error.
            spec.try_into_exact(&flt_str)
        } else {
            Err(NumericError::NoExactRepresentation(
                FloatDatum(&flt).to_string(),
            ))
        }
    }

    fn prep_parse(flt: f64) -> (Self, String) {
        let flt_str = FloatDatum(&flt).to_string();
        let mut spec = Self {
            integral: IntSpec {
                magnitude: 0..flt_str.len(),
                ..Default::default()
            },
            ..Default::default()
        };
        if flt.is_sign_negative() {
            spec.integral.sign = Some(Sign::Negative);
            spec.integral.magnitude.start = 1;
        }
        if let Some(idx) = flt_str.find('.') {
            spec.integral.magnitude.end = idx;
            let next = idx + 1;
            if next < flt_str.len() {
                spec.fraction = next..flt_str.len();
            }
        }
        if let Some(idx) = flt_str.find('e') {
            if spec.fraction.is_empty() {
                spec.integral.magnitude.end = idx;
            } else {
                spec.fraction.end = idx;
            }
            let next = idx + 1;
            if next < flt_str.len() {
                spec.exponent = next..flt_str.len();
            }
        }
        (spec, flt_str)
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.exponent.is_empty() && self.fraction.is_empty() && self.integral.is_empty()
    }

    pub(crate) fn try_into_exact(self, input: &str) -> RealResult {
        let mut buf = String::new();
        let mut num = IntSpec::<Decimal>::default();
        if self.integral.has_sign() {
            buf += input.get(0..1).unwrap_or_default();
            num.sign = self.integral.sign;
            num.magnitude = 1..1;
        }
        buf += input
            .get(self.integral.magnitude.clone())
            .unwrap_or_default();
        let frac = input.get(self.fraction.clone()).unwrap_or_default();
        buf += frac;
        let exponent = self.parse_exponent(input)?;
        let scale = exponent - i32::try_from(frac.len()).unwrap_or_default();
        let adjustment = "0".repeat(scale.abs().try_into().unwrap_or_default());
        if scale < 0 {
            num.magnitude.end = buf.len();
            let adjustment = format!("1{adjustment}");
            let denom = IntSpec::<Decimal> {
                magnitude: 0..adjustment.len(),
                ..Default::default()
            };
            Real::reduce(
                num.try_into_exact(&buf)?,
                denom.try_into_exact(&adjustment)?,
            )
        } else {
            buf += &adjustment;
            num.magnitude.end = buf.len();
            Ok(num.try_into_exact(&buf)?.into())
        }
    }

    pub(crate) fn try_into_inexact(self, input: &str) -> RealResult {
        let end = if !self.exponent.is_empty() {
            self.exponent.end
        } else if !self.fraction.is_empty() {
            self.fraction.end
        } else {
            self.integral.magnitude.end
        };
        input
            .get(..end)
            .map_or(Err(NumericError::ParseFailure), |fstr| {
                Ok(fstr.parse::<f64>()?.into())
            })
    }

    fn parse_exponent(&self, input: &str) -> Result<i32, NumericError> {
        if self.exponent.is_empty() {
            Ok(0)
        } else {
            input
                .get(self.exponent.clone())
                .unwrap_or_default()
                .parse()
                .map_err(|err: ParseIntError| match err.kind() {
                    IntErrorKind::PosOverflow | IntErrorKind::NegOverflow => {
                        NumericError::ParseExponentOutOfRange
                    }
                    _ => NumericError::ParseExponentFailure,
                })
        }
    }
}

#[derive(Debug)]
pub(crate) enum NumericError {
    ByteConversionInvalidRange,
    DivideByZero,
    Int32ConversionInvalidRange,
    IntConversionInvalidType(String),
    NoExactRepresentation(String),
    NotExactInteger(String),
    ParseExponentFailure,
    ParseExponentOutOfRange,
    ParseFailure,
    Uint32ConversionInvalidRange,
    Unimplemented(String),
    UsizeConversionInvalidRange,
}

impl Display for NumericError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::ByteConversionInvalidRange => {
                write_intconversion_range_error(u8::MIN, u8::MAX, f)
            }
            Self::DivideByZero => f.write_str("divide by zero"),
            Self::Int32ConversionInvalidRange => {
                write_intconversion_range_error(i32::MIN, i32::MAX, f)
            }
            Self::IntConversionInvalidType(n) => {
                write!(f, "expected integer literal, got numeric type: {n}")
            }
            Self::NoExactRepresentation(s) => write!(f, "no exact representation for: {s}"),
            Self::NotExactInteger(s) => write!(f, "expected exact integer, got: {s}"),
            Self::ParseExponentOutOfRange => {
                write!(f, "exponent out of range: [{}, {}]", i32::MIN, i32::MAX)
            }
            Self::ParseExponentFailure => f.write_str("exponent parse failure"),
            Self::ParseFailure => f.write_str("number parse failure"),
            Self::Uint32ConversionInvalidRange => {
                write_intconversion_range_error(u32::MIN, u32::MAX, f)
            }
            Self::Unimplemented(s) => write!(f, "unimplemented number parse: '{s}'"),
            Self::UsizeConversionInvalidRange => {
                write_intconversion_range_error(usize::MIN, usize::MAX, f)
            }
        }
    }
}

impl Error for NumericError {}

impl From<ParseFloatError> for NumericError {
    fn from(_value: ParseFloatError) -> Self {
        Self::ParseFailure
    }
}

pub(crate) struct TokenDescriptor<'a>(&'a Number);

impl Display for TokenDescriptor<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self.0 {
            Number::Complex(_) => f.write_str("CPX"),
            Number::Real(r) => r.as_token_descriptor().fmt(f),
        }
    }
}

pub(crate) struct RealTokenDescriptor<'a>(&'a Real);

impl Display for RealTokenDescriptor<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self.0 {
            Real::Float(_) => f.write_str("FLT"),
            Real::Integer(_) => f.write_str("INT"),
            Real::Rational(_) => f.write_str("RAT"),
        }
    }
}

pub(crate) struct NumericTypeName<'a>(&'a Number);

impl NumericTypeName<'_> {
    pub(crate) const INTEGER: &'static str = "integer";
    pub(crate) const RATIONAL: &'static str = "rational";
    pub(crate) const REAL: &'static str = "real";
}

impl Display for NumericTypeName<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.0 {
            Number::Complex(_) => f.write_str("complex"),
            Number::Real(Real::Float(_)) => f.write_str("floating-point"),
            Number::Real(Real::Integer(_)) => f.write_str(Self::INTEGER),
            Number::Real(Real::Rational(_)) => f.write_str(Self::RATIONAL),
        }
    }
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
enum Precision {
    Single(u64),
    #[allow(dead_code, reason = "not yet implemented")]
    Multiple(Rc<[u64]>),
}

impl Precision {
    fn is_even(&self) -> bool {
        match self {
            Self::Single(u) => u % 2 == 0,
            Self::Multiple(_) => todo!(),
        }
    }

    fn reduce(&mut self, other: &mut Self) {
        match (&self, &other) {
            (Self::Single(a), Self::Single(b)) => {
                let gcd = gcd_euclidean(*a, *b);
                *self = Self::Single(*a / gcd);
                *other = Self::Single(*b / gcd);
            }
            _ => todo!(),
        }
    }
}

impl Display for Precision {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Single(u) => write!(f, "{u}"),
            Self::Multiple(_) => todo!(),
        }
    }
}

struct FloatDatum<'a>(&'a f64);

impl Display for FloatDatum<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let d = self.0;
        if d.is_infinite() {
            let s = Sign::from(*d);
            write!(f, "{s:+}{INF_STR}")
        } else if d.is_nan() {
            write!(f, "+{NAN_STR}")
        } else {
            fmt::Debug::fmt(d, f)
        }
    }
}

struct ComplexRealDatum<'a>(&'a Real);

impl Display for ComplexRealDatum<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let r = self.0;
        if let Real::Integer(n) = r
            && n.is_zero()
        {
            Ok(())
        } else {
            r.fmt(f)
        }
    }
}

struct ComplexImagDatum<'a>(&'a Real);

impl Display for ComplexImagDatum<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
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

trait RadixPrivate {
    fn parse_inexact<R: Radix>(spec: IntSpec<R>, input: &str) -> RealResult {
        // NOTE: always parse exact magnitude first to account for radix
        Ok(parse_signed(&spec, input)?.into_inexact())
    }
}

impl RadixPrivate for Binary {}
impl RadixPrivate for Octal {}
impl RadixPrivate for Hexadecimal {}

impl RadixPrivate for Decimal {
    fn parse_inexact<R>(spec: IntSpec<R>, input: &str) -> RealResult {
        input
            .get(..spec.magnitude.end)
            .map_or(Err(NumericError::ParseFailure), |fstr| {
                Ok(fstr.parse::<f64>()?.into())
            })
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

fn parse_signed<R: Radix>(spec: &IntSpec<R>, input: &str) -> IntResult {
    if spec.is_empty() {
        Err(NumericError::ParseFailure)
    } else {
        input
            .get(..spec.magnitude.end)
            .map_or(Err(NumericError::ParseFailure), |signed_num| {
                i64::from_str_radix(signed_num, R::BASE)
                    .map_or_else(|_| parse_sign_magnitude(spec, signed_num), |n| Ok(n.into()))
            })
    }
}

fn parse_sign_magnitude<R: Radix>(spec: &IntSpec<R>, input: &str) -> IntResult {
    input
        .get(spec.magnitude.start..)
        .map_or(Err(NumericError::ParseFailure), |mag| {
            u64::from_str_radix(mag, R::BASE).map_or_else(
                |_| parse_multi_precision(spec, input),
                |n| {
                    let sign_mag = (spec.sign.unwrap_or(Sign::Positive), n);
                    Ok(sign_mag.into())
                },
            )
        })
}

fn parse_multi_precision<R: Radix>(_spec: &IntSpec<R>, input: &str) -> IntResult {
    Err(NumericError::Unimplemented(input.to_owned()))
}

fn write_intconversion_range_error(
    min: impl Display,
    max: impl Display,
    f: &mut Formatter,
) -> fmt::Result {
    write!(f, "integer literal out of range: [{min}, {max}]")
}
