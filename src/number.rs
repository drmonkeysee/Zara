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

pub(crate) type RealResult = Result<Real, NumericError>;
pub(crate) type IntResult = Result<Integer, NumericError>;

#[derive(Clone, Debug)]
pub(crate) enum Number {
    Complex(Complex),
    Real(Real),
}

impl Number {
    pub(crate) fn complex(real: impl Into<Real>, imag: impl Into<Real>) -> Self {
        let (real, imag) = (real.into(), imag.into());
        if imag.is_zero() {
            Self::real(real)
        } else {
            Self::Complex(Complex((real, imag).into()))
        }
    }

    pub(crate) fn polar(magnitude: impl Into<Real>, radians: impl Into<Real>) -> Self {
        let (mag, rad) = (magnitude.into(), radians.into());
        if mag.is_zero() || rad.is_zero() {
            Self::real(mag)
        } else {
            let (mag, rad) = (mag.into_float(), rad.into_float());
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

    pub(crate) fn as_token_descriptor(&self) -> TokenDescriptor {
        TokenDescriptor(self)
    }

    pub(crate) fn into_exact(self) -> Self {
        match self {
            Self::Complex(Complex(c)) => Self::complex(c.0.into_exact(), c.1.into_exact()),
            Self::Real(r) => Self::Real(r.into_exact()),
        }
    }

    pub(crate) fn into_inexact(self) -> Self {
        match self {
            Self::Complex(Complex(c)) => Self::complex(c.0.into_inexact(), c.1.into_inexact()),
            Self::Real(r) => Self::Real(r.into_inexact()),
        }
    }

    fn as_typename(&self) -> NumericTypeName {
        NumericTypeName(self)
    }
}

impl Display for Number {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::Complex(Complex(c)) => {
                ComplexRealDatum(&c.0).fmt(f)?;
                ComplexImagDatum(&c.1).fmt(f)
            }
            Self::Real(r) => r.fmt(f),
        }
    }
}

macro_rules! try_int_conversion {
    ($type:ty, $convert:ident, $type_err:ident) => {
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
                    _ => Err(Self::Error::$type_err(value.as_typename().to_string())),
                }
            }
        }
    };
}

try_int_conversion!(u8, try_to_u8, ByteConversionInvalidType);
try_int_conversion!(i32, try_to_i32, IntConversionInvalidType);

#[derive(Clone, Debug)]
pub(crate) struct Complex(Box<(Real, Real)>);

#[derive(Clone, Debug)]
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

    pub(crate) fn into_exact(self) -> Self {
        match self {
            Self::Float(f) => FloatSpec::float_to_exact(f),
            _ => self,
        }
    }

    pub(crate) fn into_inexact(self) -> Self {
        match self {
            Self::Float(_) => self,
            Self::Integer(n) => n.into_inexact(),
            Self::Rational(r) => r.into_inexact(),
        }
    }

    fn is_zero(&self) -> bool {
        match self {
            Self::Float(f) => *f == 0.0,
            Self::Integer(n) => n.is_zero(),
            Self::Rational(r) => r.is_zero(),
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
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::Float(d) => FloatDatum(d).fmt(f),
            Self::Integer(n) => n.fmt(f),
            Self::Rational(r) => r.fmt(f),
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

#[derive(Clone, Debug)]
pub(crate) struct Rational(Box<(Integer, Integer)>);

impl Rational {
    fn is_zero(&self) -> bool {
        self.0.0.is_zero()
    }

    fn into_inexact(self) -> Real {
        Real::Float(self.into_float())
    }

    fn into_float(self) -> f64 {
        let r = self.0;
        let (num, denom) = (r.0.into_float(), r.1.into_float());
        num / denom
    }
}

impl Display for Rational {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let r = &self.0;
        r.0.fmt(f)?;
        write!(f, "/{}", r.1)
    }
}

#[derive(Clone, Debug)]
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

    fn into_inexact(self) -> Real {
        Real::Float(self.into_float())
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
                #[allow(clippy::cast_precision_loss)]
                let f = u as f64;
                if self.sign == Sign::Negative { -f } else { f }
            }
            Precision::Multiple(_) => todo!(),
        }
    }

    fn try_to_u8(&self) -> Result<u8, NumericError> {
        if self.is_negative() {
            return Err(NumericError::ByteConversionInvalidRange);
        }
        let Precision::Single(u) = self.precision else {
            return Err(NumericError::ByteConversionInvalidRange);
        };
        u.try_into()
            .map_err(|_| NumericError::ByteConversionInvalidRange)
    }

    fn try_to_i32(&self) -> Result<i32, NumericError> {
        let Precision::Single(u) = self.precision else {
            return Err(NumericError::IntConversionInvalidRange);
        };
        if self.is_negative() {
            if u <= i32::MIN.unsigned_abs().into() {
                #[allow(clippy::cast_possible_wrap, reason = "guarded against wrapping")]
                (-(u as i64)).try_into()
            } else {
                return Err(NumericError::IntConversionInvalidRange);
            }
        } else {
            u.try_into()
        }
        .map_err(|_| NumericError::IntConversionInvalidRange)
    }
}

impl Display for Integer {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
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

    pub(crate) fn into_exact(self, input: &str) -> IntResult {
        parse_signed(&self, input)
    }

    pub(crate) fn into_inexact(self, input: &str) -> RealResult {
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
    fn float_to_exact(flt: f64) -> Real {
        if flt.is_infinite() || flt.is_nan() {
            Real::Float(flt)
        } else {
            let (spec, flt_str) = Self::prep_parse(flt);
            // NOTE: this should never fail since the string input comes from
            // an f64 but if something real weird happens treat it as NaN.
            spec.into_exact(&flt_str).unwrap_or(Real::Float(f64::NAN))
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

    pub(crate) fn into_exact(self, input: &str) -> RealResult {
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
            Real::reduce(num.into_exact(&buf)?, denom.into_exact(&adjustment)?)
        } else {
            buf += &adjustment;
            num.magnitude.end = buf.len();
            Ok(num.into_exact(&buf)?.into())
        }
    }

    pub(crate) fn into_inexact(self, input: &str) -> RealResult {
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
    ByteConversionInvalidType(String),
    DivideByZero,
    IntConversionInvalidRange,
    IntConversionInvalidType(String),
    ParseExponentFailure,
    ParseExponentOutOfRange,
    ParseFailure,
    Unimplemented(String),
}

impl Display for NumericError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::ByteConversionInvalidRange => {
                write_intconversion_range_error(u8::MIN, u8::MAX, f)
            }
            Self::ByteConversionInvalidType(n) | Self::IntConversionInvalidType(n) => {
                write!(f, "expected integer literal, got numeric type: {n}")
            }
            Self::DivideByZero => f.write_str("divide by zero"),
            Self::IntConversionInvalidRange => {
                write_intconversion_range_error(i32::MIN, i32::MAX, f)
            }
            Self::ParseExponentOutOfRange => {
                write!(f, "exponent out of range: [{}, {}]", i32::MIN, i32::MAX)
            }
            Self::ParseExponentFailure => f.write_str("exponent parse failure"),
            Self::ParseFailure => f.write_str("number parse failure"),
            Self::Unimplemented(s) => write!(f, "unimplemented number parse: '{s}'"),
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

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
enum Precision {
    Single(u64),
    #[allow(dead_code, reason = "not yet implemented")]
    Multiple(Rc<[u64]>),
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

struct FloatDatum<'a>(&'a f64);

impl Display for FloatDatum<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
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
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
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

struct NumericTypeName<'a>(&'a Number);

impl Display for NumericTypeName<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.0 {
            Number::Complex(_) => f.write_str("complex"),
            Number::Real(Real::Float(_)) => f.write_str("floating-point"),
            Number::Real(Real::Integer(_)) => f.write_str("integer"),
            Number::Real(Real::Rational(_)) => f.write_str("rational"),
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
