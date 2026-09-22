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
                if let Number::Real(Real::Integer(n)) = value {
                    n.$convert()
                } else {
                    Err(Self::Error::IntConversionInvalidType(
                        value.as_typename().to_string(),
                    ))
                }
            }
        }
    };
}

macro_rules! int_convert {
    ($name:ident, $type: ty, $err:expr, $conv:expr) => {
        fn $name(&self) -> Result<$type, NumericError> {
            let Precision::Single(u) = self.precision else {
                return Err($err);
            };
            if self.is_negative() {
                if u <= <$type>::MIN.unsigned_abs().into() {
                    #[allow(clippy::cast_possible_wrap, reason = "guarded against wrapping")]
                    $conv(u)
                } else {
                    return Err($err);
                }
            } else {
                u.try_into()
            }
            .map_err(|_| $err)
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

macro_rules! inexact_cmp_exact {
    ($inexact:expr, $flt:expr, $cmp:ident, $exact:expr) => {
        $inexact
            .clone()
            .try_into_exact()
            .map_or_else(|_| f64::$cmp($flt, &$exact.to_float()), |n| n.$cmp($exact))
    };
}

macro_rules! exact_cmp_inexact {
    ($exact:expr, $cmp:ident, $inexact:expr, $flt:expr) => {
        $inexact
            .clone()
            .try_into_exact()
            .map_or_else(|_| f64::$cmp(&$exact.to_float(), $flt), |n| $exact.$cmp(&n))
    };
}

macro_rules! assume_safe_div {
    ($div:expr) => {
        $div.expect("denominator cannot be zero")
    };
}

#[cfg(test)]
mod tests;

use crate::txt::TxtSpan;
use std::{
    cmp::Ordering,
    f64,
    fmt::{self, Display, Formatter, Write},
    num::{IntErrorKind, ParseFloatError, ParseIntError},
    ops::{Add, Div, Mul, Neg, Sub},
    rc::Rc,
    result::Result,
};

pub(crate) const INF_STR: &str = "inf.0";
pub(crate) const NAN_STR: &str = "nan.0";
// 2^53 - 1; maximum safe integer in f64 format
// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/MAX_SAFE_INTEGER
// TODO: https://github.com/rust-lang/rust/issues/152466
const FMAX_INT: f64 = 9_007_199_254_740_991.0;

pub(crate) type NumResult = Result<Number, NumericError>;
pub(crate) type RealResult = Result<Real, NumericError>;
pub(crate) type IntResult = Result<Integer, NumericError>;

/*
 * All predicates, equivalence relationships, and operators assume normalized
 * numeric values such as:
 * - sign attached to numerator
 * - no zero denominator
 * - rationals reduced to canonical form
 * - rationals with divisor denominators are reduced to integers
 * - complex with exact-zero imaginaries are reduced to reals
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
    pub(crate) fn zero() -> Self {
        Self::real(Real::zero())
    }

    pub(crate) fn one() -> Self {
        Self::real(Real::one())
    }

    pub(crate) fn nan() -> Self {
        Self::real(Real::nan())
    }

    pub(crate) fn float_max() -> Self {
        Self::real(Real::float_max())
    }

    pub(crate) fn float_min() -> Self {
        Self::real(Real::float_min())
    }

    pub(crate) fn float_min_positive() -> Self {
        Self::real(Real::float_min_positive())
    }

    pub(crate) fn epsilon() -> Self {
        Self::real(Real::epsilon())
    }

    pub(crate) fn float_max_int() -> Self {
        Self::real(Real::float_max_int())
    }

    pub(crate) fn float_min_int() -> Self {
        Self::real(Real::float_min_int())
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

    // Explicit conversions that would clash with Integer From<i64>
    pub(crate) fn from_usize(val: usize) -> Self {
        Self::real(Integer::from_usize(val))
    }

    pub(crate) fn from_u64(val: u64) -> Self {
        Self::real((Sign::Positive, val))
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
            Self::Complex(z) => z.is_zero(),
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

    pub(crate) fn as_token_descriptor(&self) -> TokenDescriptor<'_> {
        TokenDescriptor(self)
    }

    pub(crate) fn as_typename(&self) -> NumericTypeName<'_> {
        NumericTypeName(self)
    }

    pub(crate) fn into_inexact(self) -> Self {
        match self {
            Self::Complex(Complex(z)) => Self::complex(z.0.into_inexact(), z.1.into_inexact()),
            Self::Real(r) => Self::real(r.into_inexact()),
        }
    }

    pub(crate) fn into_real(self) -> Real {
        match self {
            Self::Complex(z) => z.into_real(),
            Self::Real(r) => r,
        }
    }

    pub(crate) fn into_imag(self) -> Real {
        match self {
            Self::Complex(z) => z.into_imag(),
            Self::Real(_) => Real::zero(),
        }
    }

    pub(crate) fn into_magnitude(self) -> Real {
        match self {
            Self::Complex(z) => z.into_magnitude(),
            // complex magnitude of a real is just √r² = |r|
            Self::Real(r) => r.into_abs(),
        }
    }

    pub(crate) fn into_angle(self) -> Real {
        match self {
            Self::Complex(z) => z.into_angle(),
            Self::Real(r) => {
                // positive real angles are always zero, negative are always π
                if r.is_positive() {
                    Real::zero()
                } else {
                    f64::consts::PI.into()
                }
            }
        }
    }

    pub(crate) fn into_complex_conjugate(self) -> Self {
        match self {
            Self::Complex(z) => z.into_conjugate(),
            Self::Real(_) => self,
        }
    }

    pub(crate) fn try_into_exact(self) -> NumResult {
        Ok(match self {
            Self::Complex(Complex(z)) => {
                Self::complex(z.0.try_into_exact()?, z.1.try_into_exact()?)
            }
            Self::Real(r) => Self::real(r.try_into_exact()?),
        })
    }

    pub(crate) fn try_into_reciprocal(self) -> NumResult {
        match self {
            Self::Complex(z) => z.try_into_reciprocal(),
            Self::Real(r) => Ok(Self::real(r.try_into_reciprocal()?)),
        }
    }

    pub(crate) fn sqrt(self) -> Self {
        match self {
            Self::Complex(z) => z.sqrt(),
            Self::Real(r) => {
                let rt = r.sqrt();
                if rt.is_negative() {
                    Self::imaginary(-rt)
                } else {
                    Self::real(rt)
                }
            }
        }
    }
}

impl PartialEq for Number {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Complex(a), Self::Complex(b)) => a == b,
            (Self::Real(a), Self::Real(b)) => a == b,
            _ => false,
        }
    }
}

impl Neg for Number {
    type Output = Self;

    fn neg(self) -> Self::Output {
        match self {
            Self::Complex(Complex(z)) => Self::complex(-z.0, -z.1),
            Self::Real(r) => Self::real(-r),
        }
    }
}

impl Add for Number {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Complex(z), n) | (n, Self::Complex(z)) => z + n,
            (Self::Real(a), Self::Real(b)) => Self::real(a + b),
        }
    }
}

impl Sub for Number {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Complex(z), n) => z - n,
            (Self::Real(r), Self::Complex(z)) => r.into_complex() - z,
            (Self::Real(a), Self::Real(b)) => Self::real(a - b),
        }
    }
}

impl Mul for Number {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Complex(z), x) | (x, Self::Complex(z)) => z * x,
            (Self::Real(a), Self::Real(b)) => Self::real(a * b),
        }
    }
}

impl Div for Number {
    type Output = NumResult;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Complex(a), x) => a / x,
            (Self::Real(r), Self::Complex(z)) => r.into_complex() / z,
            (Self::Real(a), Self::Real(b)) => Ok(Self::real((a / b)?)),
        }
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
try_int_conversion!(i64, try_to_i64);
try_int_conversion!(usize, try_to_usize);

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct Complex(Box<(Real, Real)>);

impl Complex {
    fn is_zero(&self) -> bool {
        self.0.0.is_zero() && self.0.1.is_zero()
    }

    fn into_real(self) -> Real {
        self.0.0
    }

    fn into_imag(self) -> Real {
        self.0.1
    }

    fn into_magnitude(self) -> Real {
        let (x, y) = self.into_parts();
        if x.is_inexact() || y.is_inexact() {
            Real::Float(x.to_float().hypot(y.to_float()))
        } else {
            ((x.clone() * x) + (y.clone() * y)).sqrt()
        }
    }

    fn into_angle(self) -> Real {
        let (x, y) = self.into_parts();
        Real::Float(y.to_float().atan2(x.to_float()))
    }

    fn into_parts(self) -> (Real, Real) {
        (self.0.0, self.0.1)
    }

    fn into_conjugate(self) -> Number {
        let (x, y) = self.into_parts();
        Number::complex(x, -y)
    }

    fn try_into_reciprocal(self) -> NumResult {
        Real::one().into_complex() / self
    }

    /*
     * Square root of Complex is defined as:
     * given x+yi and r = √(x² + y²)
     * √x+yi = √((r+x)/2) + sign(y)i√((r-x)/2)
     * split on whether x < 0 to avoid cancellation issues for (r-x)/2; x > 0, |y| << x
     * used by C99's csqrt, which also includes some special casing for signed zeros and infs.
     */
    fn sqrt(self) -> Number {
        // Square root of zero always sets x to + and keeps sign of y; if we calculated
        // this with the below algorithm instead, the zero signs go wonky due to IEEE rules.
        // Complex is_zero implies float values (exact zeros would have reduced to Integer),
        // so we can safely hardcode the answer without losing exactness.
        if self.is_zero() {
            return Number::complex(0.0, 0.0f64.copysign(self.0.1.signum()));
        }
        let (x, y) = self.clone().into_parts();
        // According to C99-Annex-G inf y always sets x to +inf and keeps y
        // because IEEE infinities are not limits but actual values, which means
        // the math doesn't work out without special-casing it.
        if y.is_infinite() {
            return Number::complex(f64::INFINITY, f64::INFINITY.copysign(self.0.1.signum()));
        }
        let r = self.into_magnitude();
        let (u, v) = if x.is_negative() {
            let t = scaled_re(r, x, Real::sub);
            (
                assume_safe_div!(y.clone().into_abs() / t.clone()),
                assume_safe_div!(t / Real::two()).copysign(&y),
            )
        } else {
            let t = scaled_re(r, x, Real::add);
            (
                assume_safe_div!(t.clone() / Real::two()),
                assume_safe_div!(y / t),
            )
        };
        Number::complex(u, v)
    }
}

impl Sub for Complex {
    type Output = Number;

    fn sub(self, rhs: Self) -> Self::Output {
        let (x, y) = self.into_parts();
        let (u, v) = rhs.into_parts();
        Number::complex(x - u, y - v)
    }
}

impl Div for Complex {
    type Output = NumResult;

    /*
     * Smith's Algorithm for (a + bi) / (c + di)
     * https://dl.acm.org/doi/abs/10.1145/368637.368661
     *  if |c| < |d|:
     *      r = c / d
     *      denom = c*r + d
     *      real = (a*r + b) / denom
     *      imag = (b*r - a) / denom
     *  else:
     *      r = d / c
     *      denom = c + d*r
     *      real = (a + b*r) / denom
     *      imag = (b - a*r) / denom
     * The textbook formula for complex division is:
     * (a + bi) / ( c + di ) = (ac + bd) / (c² + d²) + (bc - ad) / (c² + d²)i
     * which causes issues with IEEE-754 where the squares could overflow to inf
     * even if the final answer would be within range, as well as losing sign-of-zero
     * in the event the numerators sum over opposite-sign zeros.
     * Smith's algorithm avoids this by avoiding squares and using ratios of the
     * imaginary parts, as well as avoiding addition/subtraction of two products.
     */
    #[allow(clippy::many_single_char_names)]
    fn div(self, rhs: Self) -> Self::Output {
        let (a, b) = self.into_parts();
        let (mut c, mut d) = rhs.into_parts();
        // Don't mix exact/inexact in the ratio/denominator elements as exact values
        // can interact strangely with signed zeros, nan, and inf; apply float-taint
        // to quotient calculations to avoid these issues.
        if c.is_inexact() || d.is_inexact() {
            (c, d) = (c.into_inexact(), d.into_inexact());
        }
        let (re, im) = if c.clone().into_abs() < d.clone().into_abs() {
            let r = (c.clone() / d.clone())?;
            let denom = (c.clone() * r.clone()) + d.clone();
            (
                (((a.clone() * r.clone()) + b.clone()) / denom.clone())?,
                (((b * r) - a) / denom)?,
            )
        } else {
            let r = (d.clone() / c.clone())?;
            let denom = c.clone() + (d.clone() * r.clone());
            (
                ((a.clone() + (b.clone() * r.clone())) / denom.clone())?,
                ((b - (a * r)) / denom)?,
            )
        };
        Ok(Number::complex(re, im))
    }
}

impl Add<Number> for Complex {
    type Output = Number;

    #[allow(clippy::many_single_char_names)]
    fn add(self, rhs: Number) -> Self::Output {
        let (x, y) = self.into_parts();
        match rhs {
            Number::Complex(w) => {
                let (u, v) = w.into_parts();
                Number::complex(x + u, y + v)
            }
            Number::Real(r) => Number::complex(x + r, y),
        }
    }
}

impl Sub<Number> for Complex {
    type Output = Number;

    fn sub(self, rhs: Number) -> Self::Output {
        match rhs {
            Number::Complex(z) => self.sub(z),
            Number::Real(r) => self.sub(r.into_complex()),
        }
    }
}

impl Mul<Number> for Complex {
    type Output = Number;

    // Complex multiplication: (a + bi) * (c + di) = (ac - bd) + (ad + bc)i
    fn mul(self, rhs: Number) -> Self::Output {
        let (a, b) = self.into_parts();
        let (c, d) = match rhs {
            Number::Complex(z) => z.into_parts(),
            Number::Real(r) => r.into_complex().into_parts(),
        };
        Number::complex(
            (a.clone() * c.clone()) - (b.clone() * d.clone()),
            (a * d) + (b * c),
        )
    }
}

impl Div<Number> for Complex {
    type Output = NumResult;

    fn div(self, rhs: Number) -> Self::Output {
        match rhs {
            Number::Complex(z) => self.div(z),
            Number::Real(r) => {
                let (x, y) = self.into_parts();
                Ok(Number::complex((x / r.clone())?, (y / r)?))
            }
        }
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

    pub(crate) fn float_max() -> Self {
        f64::MAX.into()
    }

    pub(crate) fn float_min() -> Self {
        f64::MIN.into()
    }

    pub(crate) fn float_min_positive() -> Self {
        f64::MIN_POSITIVE.into()
    }

    pub(crate) fn epsilon() -> Self {
        f64::EPSILON.into()
    }

    pub(crate) fn float_max_int() -> Self {
        FMAX_INT.into()
    }

    pub(crate) fn float_min_int() -> Self {
        (-FMAX_INT).into()
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
        } else {
            n.make_negative();
        }
        d.make_positive();
        if n.is_zero() || d.is_magnitude_one() {
            return Ok(n.into());
        }
        if n.cmp_magnitude(&d) == Ordering::Equal {
            return Ok(Integer::new(1, n.sign).into());
        }
        n.reduce(&mut d);
        if d.is_magnitude_one() {
            return Ok(n.into());
        }
        Ok(Self::Rational(Rational((n, d).into())))
    }

    // assume divisor is a factor of dividend, so reduction ensures an Integer;
    // will panic if assumption does not hold.
    fn exact_quotient(dividend: impl Into<Integer>, divisor: impl Into<Integer>) -> Integer {
        let r = assume_safe_div!(Self::reduce(dividend, divisor));
        let Self::Integer(n) = r else {
            unreachable!("unexpected non-factor divisor");
        };
        n
    }

    pub(crate) fn is_nan(&self) -> bool {
        if let Self::Float(f) = self {
            f.is_nan()
        } else {
            false
        }
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

    pub(crate) fn is_inexact(&self) -> bool {
        matches!(self, Self::Float(_))
    }

    pub(crate) fn strict_lt(&self, other: &Self) -> bool {
        self.strict_ordering(other, &Self::lt, &f64::lt)
    }

    pub(crate) fn strict_gt(&self, other: &Self) -> bool {
        self.strict_ordering(other, &Self::gt, &f64::gt)
    }

    pub(crate) fn as_token_descriptor(&self) -> RealTokenDescriptor<'_> {
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

    pub(crate) fn into_floor(self) -> Self {
        match self {
            Self::Float(f) => f.floor().into(),
            Self::Integer(n) => n.into(),
            Self::Rational(q) => todo!(),
        }
    }

    pub(crate) fn into_ceiling(self) -> Self {
        match self {
            Self::Float(f) => f.ceil().into(),
            Self::Integer(n) => n.into(),
            Self::Rational(q) => todo!(),
        }
    }

    pub(crate) fn into_truncate(self) -> Self {
        match self {
            Self::Float(f) => f.trunc().into(),
            Self::Integer(n) => n.into(),
            Self::Rational(q) => todo!(),
        }
    }

    pub(crate) fn into_round(self) -> Self {
        match self {
            Self::Float(f) => f.round().into(),
            Self::Integer(n) => n.into(),
            Self::Rational(q) => todo!(),
        }
    }

    pub(crate) fn try_into_exact(self) -> RealResult {
        if let Self::Float(f) = self {
            FloatSpec::try_float_to_exact(f)
        } else {
            Ok(self)
        }
    }

    pub(crate) fn try_into_exact_integer(self) -> IntResult {
        match self {
            Self::Float(f) if f.fract() == 0.0 => Ok(Integer::from_exact_float(f)),
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

    fn one() -> Self {
        Integer::one().into()
    }

    fn two() -> Self {
        Integer::two().into()
    }

    fn is_eqv(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Float(a), Self::Float(b)) if a.is_nan() && b.is_nan() => true,
            (Self::Float(a), Self::Float(b))
                if *a == 0.0 && *b == 0.0 && a.signum() != b.signum() =>
            {
                false
            }
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

    fn is_infinite(&self) -> bool {
        if let Self::Float(f) = self {
            f.is_infinite()
        } else {
            false
        }
    }

    fn signum(&self) -> f64 {
        match self {
            Self::Float(f) => f.signum(),
            Self::Integer(n) => n.signum(),
            Self::Rational(q) => q.signum(),
        }
    }

    /*
     * For comparison sequences (e.g. max/min) the normal float comparison rules
     * don't result in the correct result:
     * - nan should render the entire set undefined
     * - negative and positive zero should order according to sign;
     *     IEEE-754 defines an ascending order of (-0.0, +0.0), despite -0.0 ≮ +0.0
     */
    fn strict_ordering(
        &self,
        other: &Self,
        cmp: &impl Fn(&Real, &Real) -> bool,
        fcmp: &impl Fn(&f64, &f64) -> bool,
    ) -> bool {
        cmp(self, other) || other.is_nan() || self.strict_zeros(other, fcmp)
    }

    fn strict_zeros(&self, other: &Self, cmp: &impl Fn(&f64, &f64) -> bool) -> bool {
        match (self, other) {
            (Self::Float(a), Self::Float(b)) if *a == 0.0 && *b == 0.0 => {
                cmp(&a.signum(), &b.signum())
            }
            (Self::Float(f), Self::Integer(n)) if *f == 0.0 && n.is_zero() => {
                cmp(&f.signum(), &1.0)
            }
            (Self::Integer(n), Self::Float(f)) if n.is_zero() && *f == 0.0 => {
                cmp(&1.0, &f.signum())
            }
            _ => false,
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

    fn copysign(self, sign: &Real) -> Self {
        let s = sign.signum();
        match self {
            Self::Float(f) => f.copysign(s).into(),
            Self::Integer(n) => n.copysign(s).into(),
            Self::Rational(q) => Self::Rational(q.copysign(s)),
        }
    }

    fn into_complex(self) -> Complex {
        Complex((self, Self::zero()).into())
    }

    fn try_into_reciprocal(self) -> RealResult {
        match self {
            Self::Float(f) => Ok(f.recip().into()),
            Self::Integer(n) => n.try_into_reciprocal(),
            Self::Rational(q) => q.try_into_reciprocal(),
        }
    }

    // Number handles negative sign so this function technically returns the
    // wrong value for negative roots (e.g. √-4 = -2 instead of +2i)
    fn sqrt(self) -> Self {
        match self {
            Self::Float(f) if f == 0.0 => self,
            Self::Float(f) => sign_preserving_sqrt(f).into(),
            Self::Integer(n) => n.sqrt(),
            Self::Rational(q) => q.sqrt(),
        }
    }
}

impl PartialEq for Real {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Self::Float(a) if let Self::Float(b) = other => a == b,
            Self::Float(f) => inexact_cmp_exact!(self, f, eq, other),
            Self::Integer(n) => n == other,
            Self::Rational(q) => q == other,
        }
    }
}

impl PartialOrd for Real {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match self {
            Self::Float(a) if let Self::Float(b) = other => a.partial_cmp(b),
            Self::Float(f) => inexact_cmp_exact!(self, f, partial_cmp, other),
            Self::Integer(n) => n.partial_cmp(other),
            Self::Rational(q) => q.partial_cmp(other),
        }
    }
}

impl Neg for Real {
    type Output = Self;

    fn neg(self) -> Self::Output {
        match self {
            Self::Float(f) => (-f).into(),
            Self::Integer(n) => (-n).into(),
            Self::Rational(q) => Self::Rational(-q),
        }
    }
}

impl Add for Real {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        match self {
            // integral additive identity should not affect a float;
            // if we convert to float we get -0.0 + 0.0 = 0.0 which is wrong!
            // exact zero shouldn't affect the sign of inexact zero.
            Self::Float(_) if rhs.is_exact_zero() => self,
            Self::Float(f) => (f + rhs.to_float()).into(),
            Self::Integer(n) => n + rhs,
            Self::Rational(q) => q + rhs,
        }
    }
}

impl Sub for Real {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        match self {
            Self::Float(f) => (f - rhs.to_float()).into(),
            Self::Integer(n) => n - rhs,
            Self::Rational(q) => q - rhs,
        }
    }
}

impl Mul for Real {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        match self {
            // exact zero overrides float-taint
            Self::Float(_) if rhs.is_exact_zero() => rhs,
            Self::Float(f) => (f * rhs.to_float()).into(),
            Self::Integer(n) => n * rhs,
            Self::Rational(q) => q * rhs,
        }
    }
}

impl Div for Real {
    type Output = RealResult;

    fn div(self, rhs: Self) -> Self::Output {
        match self {
            // exact zero overrides float-taint
            Self::Float(_) if rhs.is_exact_zero() => Err(NumericError::DivideByZero),
            Self::Float(f) => Ok((f / rhs.to_float()).into()),
            Self::Integer(n) => n / rhs,
            Self::Rational(q) => q / rhs,
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

    fn signum(&self) -> f64 {
        self.0.0.signum()
    }

    fn to_float(&self) -> f64 {
        let r = &self.0;
        r.0.to_float() / r.1.to_float()
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

    fn into_parts(self) -> (Integer, Integer) {
        (self.0.0, self.0.1)
    }

    fn try_into_reciprocal(self) -> RealResult {
        Real::reduce(self.0.1, self.0.0)
    }

    // a/b ± c/d = (ad ± cb)/bd except cross-reduce with gcd and lcm to lessen
    // likelihood of intermediate result overflow.
    // div/0 should be a programmer error here, hence the panics. if everything
    // is wired up correctly this will only be called with canonical rationals
    // or integer reciprocals.
    #[allow(clippy::many_single_char_names)]
    fn additive_op(self, rhs: Self, op: impl FnOnce(Integer, Integer) -> Integer) -> Real {
        let (a, b) = self.into_parts();
        let (c, d) = rhs.into_parts();
        let g = b.gcd(&d);
        let m = b.lcm(&d);
        debug_assert!(!g.is_zero());
        debug_assert!(!m.is_zero());
        let ad = a * Real::exact_quotient(d, g.clone());
        let cb = c * Real::exact_quotient(b, g);
        assume_safe_div!(op(ad, cb) / m)
    }

    fn sqrt(self) -> Real {
        let (n, d) = self.into_parts();
        match (n.clone().sqrt(), d.clone().sqrt()) {
            (Real::Float(_), _) | (_, Real::Float(_)) => {
                sign_preserving_sqrt(n.to_float() / d.to_float()).into()
            }
            (Real::Integer(a), Real::Integer(b)) => {
                debug_assert!(!b.is_zero());
                assume_safe_div!(Real::reduce(a.clone(), b.clone()))
            }
            _ => unreachable!("sqrt of rational cannot result in two rational parts"),
        }
    }

    fn copysign(self, sign: f64) -> Self {
        let (n, d) = self.into_parts();
        Self((n.copysign(sign), d).into())
    }
}

impl PartialOrd for Rational {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Rational {
    // a/b < c/d => ad < cb
    fn cmp(&self, other: &Self) -> Ordering {
        let (a, b) = self.clone().into_parts();
        let (c, d) = other.clone().into_parts();
        (a * d).cmp(&(c * b))
    }
}

impl Neg for Rational {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Self((-self.0.0, self.0.1).into())
    }
}

impl Add for Rational {
    type Output = Real;

    fn add(self, rhs: Self) -> Self::Output {
        self.additive_op(rhs, Integer::add)
    }
}

impl Sub for Rational {
    type Output = Real;

    fn sub(self, rhs: Self) -> Self::Output {
        self.additive_op(rhs, Integer::sub)
    }
}

impl Mul for Rational {
    type Output = Real;

    // a/b * c/d = ac/bd except cross-reduce with gcds first to lessen
    // likelihood of intermediate result overflow.
    // div/0 should be a programmer error here, hence the panics. if everything
    // is wired up correctly this will only be called with canonical rationals
    // or integer reciprocals.
    fn mul(self, rhs: Self) -> Self::Output {
        let (a, b) = self.into_parts();
        let (c, d) = rhs.into_parts();
        let (g1, g2) = (a.gcd(&d), c.gcd(&b));
        debug_assert!(!g1.is_zero());
        debug_assert!(!g2.is_zero());
        assume_safe_div!(Real::reduce(
            Real::exact_quotient(a, g1.clone()) * Real::exact_quotient(c, g2.clone()),
            Real::exact_quotient(b, g2) * Real::exact_quotient(d, g1),
        ))
    }
}

impl Display for Rational {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let r = &self.0;
        r.0.fmt(f)?;
        write!(f, "/{}", r.1)
    }
}

impl PartialEq<Real> for Rational {
    fn eq(&self, other: &Real) -> bool {
        match other {
            Real::Float(f) => exact_cmp_inexact!(self, eq, other, f),
            Real::Integer(_) => false,
            Real::Rational(q) => self.eq(q),
        }
    }
}

impl PartialOrd<Real> for Rational {
    fn partial_cmp(&self, other: &Real) -> Option<Ordering> {
        match other {
            Real::Float(f) => exact_cmp_inexact!(self, partial_cmp, other, f),
            Real::Integer(n) => self.partial_cmp(&n.clone().into_rational()),
            Real::Rational(q) => self.partial_cmp(q),
        }
    }
}

impl Add<Real> for Rational {
    type Output = Real;

    fn add(self, rhs: Real) -> Self::Output {
        match rhs {
            Real::Float(f) => (self.to_float() + f).into(),
            Real::Integer(n) => self.add(n.into_rational()),
            Real::Rational(q) => self.add(q),
        }
    }
}

impl Sub<Real> for Rational {
    type Output = Real;

    fn sub(self, rhs: Real) -> Self::Output {
        match rhs {
            Real::Float(f) => (self.to_float() - f).into(),
            Real::Integer(n) => self.sub(n.into_rational()),
            Real::Rational(q) => self.sub(q),
        }
    }
}

impl Mul<Real> for Rational {
    type Output = Real;

    fn mul(self, rhs: Real) -> Self::Output {
        match rhs {
            Real::Float(f) => (self.to_float() * f).into(),
            Real::Integer(n) => self.mul(n.into_rational()),
            Real::Rational(q) => self.mul(q),
        }
    }
}

impl Div<Real> for Rational {
    type Output = RealResult;

    fn div(self, rhs: Real) -> Self::Output {
        Ok(match rhs {
            // need this because (q * f.recip()) ends up losing precision
            Real::Float(f) => (self.to_float() / f).into(),
            Real::Integer(n) => self.mul(n.try_into_reciprocal()?),
            Real::Rational(q) => self.mul(q.try_into_reciprocal()?),
        })
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct Integer {
    precision: Precision,
    sign: Sign,
}

impl Integer {
    pub(crate) fn zero() -> Self {
        0.into()
    }

    pub(crate) fn one() -> Self {
        1.into()
    }

    fn new(precision: impl Into<Precision>, sign: impl Into<Sign>) -> Self {
        let mut sign = sign.into();
        let precision = precision.into();
        if precision.is_zero() {
            sign = Sign::Zero;
        }
        Self { precision, sign }
    }

    fn from_usize(val: usize) -> Self {
        match u64::try_from(val) {
            Ok(u) => (Sign::Positive, u).into(),
            _ => todo!("handle multi-precision"),
        }
    }

    fn from_exact_float(val: f64) -> Self {
        if (-FMAX_INT..=FMAX_INT).contains(&val) {
            #[allow(
                clippy::cast_possible_truncation,
                reason = "guarded against truncation"
            )]
            (val as i64).into()
        } else {
            todo!("convert f64 to multi-precision integer somehow")
        }
    }

    pub(crate) fn is_even(&self) -> bool {
        self.precision.is_even()
    }

    pub(crate) fn gcd(&self, rhs: &Self) -> Self {
        Self::new(self.precision.gcd(&rhs.precision), Sign::Positive)
    }

    pub(crate) fn lcm(&self, rhs: &Self) -> Self {
        if self.is_zero() && rhs.is_zero() {
            Self::zero()
        } else {
            Self::new(self.precision.lcm(&rhs.precision), Sign::Positive)
        }
    }

    pub(crate) fn into_inexact(self) -> Real {
        Real::Float(self.to_float())
    }

    fn two() -> Self {
        2.into()
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

    fn signum(&self) -> f64 {
        self.sign.into()
    }

    fn cmp_magnitude(&self, other: &Self) -> Ordering {
        self.precision.cmp(&other.precision)
    }

    fn to_float(&self) -> f64 {
        match self.precision {
            Precision::Single(u) =>
            {
                #[allow(clippy::cast_precision_loss)]
                (u as f64).copysign(self.sign.into())
            }
            Precision::Multiple(_) => todo!(),
        }
    }

    int_convert!(
        try_to_i32,
        i32,
        NumericError::Int32ConversionInvalidRange,
        |u| (-(u as i64)).try_into()
    );
    int_convert!(
        try_to_i64,
        i64,
        NumericError::Int64ConversionInvalidRange,
        |u| {
            Ok(if u == i64::MIN.unsigned_abs() {
                i64::MIN
            } else {
                -(u as i64)
            })
        }
    );
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

    fn into_rational(self) -> Rational {
        Rational((self, Self::one()).into())
    }

    fn into_abs(mut self) -> Self {
        self.make_positive();
        self
    }

    fn try_into_reciprocal(self) -> RealResult {
        Real::reduce(Self::one(), self)
    }

    fn safe_sum(self, rhs: Self) -> Self {
        let sum = self.precision + rhs.precision;
        Self::new(sum, self.sign)
    }

    fn overflowing_sum(self, rhs: Self, ovf_sign: Sign) -> Self {
        let (sign, sum) = if self.precision < rhs.precision {
            (ovf_sign, rhs.precision - self.precision)
        } else {
            (self.sign, self.precision - rhs.precision)
        };
        Self::new(sum, sign)
    }

    fn sqrt(self) -> Real {
        if self.is_zero() {
            self.into()
        } else if let Some(p) = self.precision.isqrt() {
            Self::new(p, self.sign).into()
        } else {
            sign_preserving_sqrt(self.to_float()).into()
        }
    }

    fn copysign(self, sign: f64) -> Self {
        Self::new(self.precision, sign)
    }
}

impl PartialOrd for Integer {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Integer {
    fn cmp(&self, other: &Self) -> Ordering {
        self.sign.cmp(&other.sign).then_with(|| {
            let mag = self.cmp_magnitude(other);
            if self.is_negative() {
                mag.reverse()
            } else {
                mag
            }
        })
    }
}

impl Neg for Integer {
    type Output = Self;

    fn neg(mut self) -> Self::Output {
        self.sign = -self.sign;
        self
    }
}

impl Add for Integer {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        match (&self.sign, &rhs.sign) {
            (_, Sign::Zero) => self,
            (Sign::Zero, _) => rhs,
            (Sign::Positive, Sign::Positive) | (Sign::Negative, Sign::Negative) => {
                self.safe_sum(rhs)
            }
            (Sign::Positive, Sign::Negative) | (Sign::Negative, Sign::Positive) => {
                let s = rhs.sign;
                self.overflowing_sum(rhs, s)
            }
        }
    }
}

impl Sub for Integer {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        match (&self.sign, &rhs.sign) {
            (_, Sign::Zero) => self,
            (Sign::Zero, _) => -rhs,
            (Sign::Positive, Sign::Positive) | (Sign::Negative, Sign::Negative) => {
                let s = -self.sign;
                self.overflowing_sum(rhs, s)
            }
            (Sign::Positive, Sign::Negative) | (Sign::Negative, Sign::Positive) => {
                self.safe_sum(rhs)
            }
        }
    }
}

impl Mul for Integer {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        match self.sign * rhs.sign {
            Sign::Zero => Self::zero(),
            s => Self::new(self.precision * rhs.precision, s),
        }
    }
}

impl Div for Integer {
    type Output = RealResult;

    fn div(self, rhs: Self) -> Self::Output {
        Real::reduce(self, rhs)
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
        Self::new(val, sign)
    }
}

impl PartialEq<Real> for Integer {
    fn eq(&self, other: &Real) -> bool {
        match other {
            Real::Float(f) => exact_cmp_inexact!(self, eq, other, f),
            Real::Integer(n) => self.eq(n),
            Real::Rational(_) => false,
        }
    }
}

impl PartialOrd<Real> for Integer {
    fn partial_cmp(&self, other: &Real) -> Option<Ordering> {
        match other {
            Real::Float(f) => exact_cmp_inexact!(self, partial_cmp, other, f),
            Real::Integer(n) => self.partial_cmp(n),
            Real::Rational(q) => self.clone().into_rational().partial_cmp(q),
        }
    }
}

impl Add<Real> for Integer {
    type Output = Real;

    fn add(self, rhs: Real) -> Self::Output {
        match rhs {
            // integral additive identity should not affect a float;
            // if we convert to float we get 0.0 + -0.0 = 0.0 which is wrong!
            // exact zero shouldn't affect the sign of inexact zero.
            Real::Float(_) if self.is_zero() => rhs,
            Real::Float(f) => (self.to_float() + f).into(),
            Real::Integer(n) => self.add(n).into(),
            Real::Rational(q) => self.into_rational() + q,
        }
    }
}

impl Sub<Real> for Integer {
    type Output = Real;

    fn sub(self, rhs: Real) -> Self::Output {
        match rhs {
            // Integral additive reciprical should flip the sign of a float;
            // this ends up being relevant for 0 - 0.0 = -0.0
            Real::Float(_) if self.is_zero() => -rhs,
            Real::Float(f) => (self.to_float() - f).into(),
            Real::Integer(n) => self.sub(n).into(),
            Real::Rational(q) => self.into_rational() - q,
        }
    }
}

impl Mul<Real> for Integer {
    type Output = Real;

    fn mul(self, rhs: Real) -> Self::Output {
        match rhs {
            // exact zero overrides float-taint
            Real::Float(_) if self.is_zero() => self.into(),
            Real::Float(f) => (self.to_float() * f).into(),
            Real::Integer(n) => self.mul(n).into(),
            Real::Rational(q) => self.into_rational() * q,
        }
    }
}

impl Div<Real> for Integer {
    type Output = RealResult;

    fn div(self, rhs: Real) -> Self::Output {
        match rhs {
            // nan overrides exact zero which overrides float-taint
            Real::Float(f) if f.is_nan() => Ok(rhs),
            Real::Float(_) if self.is_zero() => Ok(self.into()),
            Real::Float(f) => Ok((self.to_float() / f).into()),
            Real::Integer(n) => self.div(n),
            Real::Rational(q) => Ok(self * q.try_into_reciprocal()?),
        }
    }
}

// enum expression of the signum function
#[derive(Clone, Copy, Debug, Default, Eq, Ord, PartialEq, PartialOrd)]
pub(crate) enum Sign {
    Negative = -1,
    Zero,
    #[default]
    Positive,
}

impl Neg for Sign {
    type Output = Self;

    fn neg(self) -> Self::Output {
        match self {
            Self::Negative => Self::Positive,
            Self::Positive => Self::Negative,
            Self::Zero => self,
        }
    }
}

impl Mul for Sign {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        (self as i64 * rhs as i64).into()
    }
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

impl From<Sign> for f64 {
    fn from(value: Sign) -> Self {
        match value {
            Sign::Negative => -1.0,
            Sign::Zero => 0.0,
            Sign::Positive => 1.0,
        }
    }
}

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
        // TODO: experimental https://doc.rust-lang.org/std/primitive.char.html#method.is_ascii_octdigit
        ch.is_digit(8)
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
    // The size of the mantissa in bits is one less than digits due to the
    // implicit leading one.
    const MANTISSA_SIZE: u32 = f64::MANTISSA_DIGITS - 1;

    fn try_float_to_exact(flt: f64) -> RealResult {
        if flt == 0.0 {
            // both +/- zero converts to integer zero
            Ok(Real::zero())
        } else if flt.is_finite() {
            Self::try_to_dyadic_rational(flt)
        } else {
            Err(NumericError::NoExactRepresentation(
                FloatDatum(&flt).to_string(),
            ))
        }
    }

    // Convert IEEE-754 floating point into dyadic rational by bit-decomposition;
    // every finite f64 is exactly sign * mantissa * 2^exponent
    #[allow(clippy::similar_names, reason = "bits and bias have clear semantics")]
    fn try_to_dyadic_rational(flt: f64) -> RealResult {
        #[allow(
            clippy::cast_possible_truncation,
            reason = "only values are -1.0 or 1.0"
        )]
        let sign = flt.signum() as i64;
        let bits = flt.to_bits();
        // TODO: https://doc.rust-lang.org/std/primitive.f64.html#associatedconstant.EXPONENT_MASK
        let exp_bits = ((bits & 0x7ff0_0000_0000_0000) >> Self::MANTISSA_SIZE) as i32;
        // TODO: https://doc.rust-lang.org/std/primitive.f64.html#associatedconstant.MANTISSA_MASK
        let mantissa_bits = bits & 0x000f_ffff_ffff_ffff;

        let (mantissa, exp) = if flt.is_subnormal() {
            // Subnormal: no implicit leading bit, but raw_mantissa is scaled up by
            // 2^52 (integer, not fractional), so exponent is -1021 - 53 = -1074
            (
                mantissa_bits.cast_signed(),
                f64::MIN_EXP - f64::MANTISSA_DIGITS.cast_signed(),
            )
        } else {
            // Normal: mantissa | (1<<52) scales the true significand up by 2^52 to
            // make it an integer (adding the implicit leading 1 back in),
            // so exponent is (raw_exp - 1023) - 52 = raw_exp - 1075
            let bias = f64::MAX_EXP - 1;
            (
                mantissa_bits.cast_signed() | (1 << Self::MANTISSA_SIZE),
                exp_bits - bias - Self::MANTISSA_SIZE.cast_signed(),
            )
        };

        if exp < 0 {
            // negative exponent: sign * mantissa * 2^exponent = (sign * mantissa) / 2^-exponent
            let numerator = sign * mantissa;
            let denom = 2i64.pow((-exp).cast_unsigned());
            Real::reduce(numerator, denom)
        } else {
            // positive exponent: sign * mantissa * 2^exponent
            Ok(Real::Integer(
                (sign * mantissa * 2i64.pow(exp.cast_unsigned())).into(),
            ))
        }
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.exponent.is_empty() && self.fraction.is_empty() && self.integral.is_empty()
    }

    pub(crate) fn try_into_exact(self, input: &str) -> RealResult {
        let mut buf = String::new();
        let mut num = IntSpec::<Decimal>::default();
        if self.integral.has_sign() {
            buf += input.get(..1).unwrap_or_default();
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
    Int64ConversionInvalidRange,
    IntConversionInvalidType(String),
    IsizeConversionInvalidRange,
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
            Self::Int64ConversionInvalidRange => {
                write_intconversion_range_error(i64::MIN, i64::MAX, f)
            }
            Self::IntConversionInvalidType(n) => {
                write!(f, "expected integer literal, got numeric type: {n}")
            }
            Self::IsizeConversionInvalidRange => {
                write_intconversion_range_error(isize::MIN, isize::MAX, f)
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
    fn is_zero(&self) -> bool {
        match self {
            Self::Single(u) => *u == 0,
            Self::Multiple(_) => false,
        }
    }

    fn is_even(&self) -> bool {
        match self {
            Self::Single(u) => u % 2 == 0,
            Self::Multiple(_) => todo!(),
        }
    }

    fn gcd(&self, rhs: &Self) -> Self {
        match (self, rhs) {
            (Self::Single(a), Self::Single(b)) => Self::Single(gcd_euclidean(*a, *b)),
            _ => todo!(),
        }
    }

    fn lcm(&self, rhs: &Self) -> Self {
        match (self, rhs) {
            (Self::Single(a), Self::Single(b)) => {
                let gcd = gcd_euclidean(*a, *b);
                let (p, o) = b.carrying_mul(a / gcd, 0);
                if o == 0 {
                    Self::Single(p)
                } else {
                    todo!("handle precision overflow")
                }
            }
            _ => todo!(),
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

    fn isqrt(&self) -> Option<Self> {
        match self {
            Self::Single(u) => {
                let r = u.isqrt();
                if r.pow(2) == *u {
                    Some(Self::Single(r))
                } else {
                    None
                }
            }
            Self::Multiple(_) => todo!(),
        }
    }
}

impl Add for Precision {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Single(a), Self::Single(b)) => {
                let (s, c) = a.overflowing_add(b);
                if c {
                    todo!("handle precision overflow")
                } else {
                    Self::Single(s)
                }
            }
            _ => todo!(),
        }
    }
}

// Naive sub implementation, relying on Integer to avoid subtraction overflow
impl Sub for Precision {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Single(a), Self::Single(b)) => Self::Single(a - b),
            _ => todo!(),
        }
    }
}

impl Mul for Precision {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Single(a), Self::Single(b)) => {
                let (p, o) = a.carrying_mul(b, 0);
                if o == 0 {
                    Self::Single(p)
                } else {
                    todo!("handle precision overflow")
                }
            }
            _ => todo!(),
        }
    }
}

impl Display for Precision {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            // avoid direct Display impl for u64 to control expression of sign
            Self::Single(u) => write!(f, "{u}"),
            Self::Multiple(_) => todo!(),
        }
    }
}

impl From<u64> for Precision {
    fn from(value: u64) -> Self {
        Self::Single(value)
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
        match self.0 {
            Real::Integer(n) if n.is_zero() => Ok(()),
            r => r.fmt(f),
        }
    }
}

struct ComplexImagDatum<'a>(&'a Real);

impl Display for ComplexImagDatum<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self.0 {
            Real::Integer(n) if n.is_zero() => Ok(()),
            Real::Integer(n) if n.is_magnitude_one() => write!(f, "{:+}i", n.sign),
            r => write!(f, "{r:+}i"),
        }
    }
}

trait RadixPrivate {
    fn parse_inexact<R: Radix>(spec: IntSpec<R>, input: &str) -> RealResult {
        // always parse exact magnitude first to account for radix
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

// https://en.wikipedia.org/wiki/Euclidean_algorithm
fn gcd_euclidean(mut a: u64, mut b: u64) -> u64 {
    while b != 0 {
        let t = b;
        b = a % b;
        a = t;
    }
    a
}

// helper to keep the sign consistent across √ in order to apply imaginary root later
fn sign_preserving_sqrt(f: f64) -> f64 {
    f.abs().sqrt().copysign(f)
}

// helper to calculate the intermediate real value for complex sqrt
fn scaled_re(r: Real, x: Real, op: impl FnOnce(Real, Real) -> Real) -> Real {
    let t = (Real::two() * op(r, x)).sqrt();
    debug_assert!(!t.is_zero());
    t
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
