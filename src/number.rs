macro_rules! int_convert {
    ($($name:ident, $type: ty, $err:expr, $conv:expr);+ $(;)?) => {
        $(fn $name(&self) -> Result<$type, NumericError> {
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
        })+
    };
}

macro_rules! uint_convert {
    ($($name:ident, $type:ty, $err:expr);+ $(;)?) => {
        $(fn $name(&self) -> Result<$type, NumericError> {
            if self.is_negative() {
                return Err($err);
            }
            let Precision::Single(u) = self.precision else {
                return Err($err);
            };
            u.try_into().map_err(|_| $err)
        })+
    };
}

macro_rules! impl_val_delegate {
    (Add, $type:ty) => { impl_val_delegate!(@imp Add, $type, $type, add, +); };
    (Sub, $type:ty) => { impl_val_delegate!(@imp Sub, $type, $type, sub, -); };
    (Mul, $type:ty) => { impl_val_delegate!(@imp Mul, $type, $type, mul, *); };
    (Div, $type:ty) => { impl_val_delegate!(@imp Div, $type, $type, div, /); };
    (Rem, $type:ty) => { impl_val_delegate!(@imp Rem, $type, $type, rem, %); };
    (Add, $type:ty, $out:ty) => { impl_val_delegate!(@imp Add, $type, $out, add, +); };
    (Sub, $type:ty, $out:ty) => { impl_val_delegate!(@imp Sub, $type, $out, sub, -); };
    (Mul, $type:ty, $out:ty) => { impl_val_delegate!(@imp Mul, $type, $out, mul, *); };
    (Rem, $type:ty, $out:ty) => { impl_val_delegate!(@imp Rem, $type, $out, rem, %); };

    (@imp $imp:ident, $this:ty, $out:ty, $func:ident, $op:tt) => {
        impl<Rhs: Borrow<$this>> $imp<Rhs> for $this {
            type Output = $out;

            fn $func(self, rhs: Rhs) -> Self::Output {
                &self $op rhs
            }
        }
    };
}

macro_rules! inexact_cmp_exact {
    ($inexact:expr, $flt:expr, $cmp:ident, $exact:expr) => {
        $inexact
            .try_to_exact()
            .map_or_else(|_| f64::$cmp($flt, &$exact.to_float()), |n| n.$cmp($exact))
    };
}

macro_rules! exact_cmp_inexact {
    ($exact:expr, $cmp:ident, $inexact:expr, $flt:expr) => {
        $inexact
            .try_to_exact()
            .map_or_else(|_| f64::$cmp(&$exact.to_float(), $flt), |n| $exact.$cmp(&n))
    };
}

macro_rules! assume_safe_div {
    ($div:expr) => {
        $div.expect("denominator cannot be zero")
    };
}

mod precision;
mod spec;
#[cfg(test)]
mod tests;

use self::precision::Precision;
pub(crate) use self::spec::{Binary, Decimal, FloatSpec, Hexadecimal, IntSpec, Octal, Radix};
use std::{
    borrow::Borrow,
    cmp::Ordering,
    convert, f64,
    fmt::{self, Display, Formatter, Write},
    num::ParseFloatError,
    ops::{Add, Div, Mul, Neg, Rem, Sub},
    result::Result,
};

pub(crate) const INF_STR: &str = "inf.0";
pub(crate) const NAN_STR: &str = "nan.0";
// 2^53 - 1; maximum safe integer in f64 format
// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/MAX_SAFE_INTEGER
// TODO: https://doc.rust-lang.org/std/primitive.f64.html#associatedconstant.MAX_EXACT_INTEGER
const FMAX_INT: f64 = 9_007_199_254_740_991.0;
// The size of the mantissa in bits is one less than digits due to the
// implicit leading one.
const MANTISSA_SIZE: u32 = f64::MANTISSA_DIGITS - 1;

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

    pub(crate) fn to_inexact(&self) -> Self {
        match self {
            Self::Complex(Complex(z)) => Self::complex(z.0.to_inexact(), z.1.to_inexact()),
            Self::Real(r) => Self::real(r.to_inexact()),
        }
    }

    pub(crate) fn to_real(&self) -> Real {
        match self {
            Self::Complex(z) => z.to_real(),
            Self::Real(r) => r.clone(),
        }
    }

    pub(crate) fn to_imag(&self) -> Real {
        match self {
            Self::Complex(z) => z.to_imag(),
            Self::Real(_) => Real::zero(),
        }
    }

    pub(crate) fn try_to_exact(&self) -> NumResult {
        Ok(match self {
            Self::Complex(Complex(z)) => Self::complex(z.0.try_to_exact()?, z.1.try_to_exact()?),
            Self::Real(r) => Self::real(r.try_to_exact()?),
        })
    }

    pub(crate) fn magnitude(&self) -> Real {
        match self {
            Self::Complex(z) => z.magnitude(),
            // complex magnitude of a real is just √r² = |r|
            Self::Real(r) => r.abs(),
        }
    }

    pub(crate) fn angle(&self) -> RealResult {
        match self {
            Self::Complex(z) => Ok(z.angle()),
            Self::Real(r) => r.angle(),
        }
    }

    pub(crate) fn reciprocal(&self) -> NumResult {
        match self {
            Self::Complex(z) => z.reciprocal(),
            Self::Real(r) => Ok(Self::real(r.reciprocal()?)),
        }
    }

    pub(crate) fn sqrt(&self) -> Self {
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

    pub(crate) fn exp(&self) -> Self {
        match self {
            Self::Complex(z) => z.exp(),
            Self::Real(r) => Self::real(r.exp()),
        }
    }

    pub(crate) fn ln(&self) -> NumResult {
        match self {
            Self::Complex(z) => Ok(z.ln()),
            Self::Real(r) => r.ln(),
        }
    }

    pub(crate) fn log(&self, base: &Self) -> NumResult {
        // logₓy = ln y / ln x
        self.ln()? / base.ln()?
    }

    pub(crate) fn sin(&self) -> Self {
        match self {
            Self::Complex(z) => z.sin(),
            Self::Real(r) => Self::real(r.sin()),
        }
    }

    pub(crate) fn cos(&self) -> Self {
        match self {
            Self::Complex(z) => z.cos(),
            Self::Real(r) => Self::real(r.cos()),
        }
    }

    pub(crate) fn tan(&self) -> Self {
        match self {
            Self::Complex(z) => z.tan(),
            Self::Real(r) => Self::real(r.tan()),
        }
    }

    pub(crate) fn asin(&self) -> Self {
        match self {
            Self::Complex(z) => z.asin(),
            Self::Real(r) => r.asin(),
        }
    }

    pub(crate) fn acos(&self) -> Self {
        match self {
            Self::Complex(z) => z.acos(),
            Self::Real(r) => r.acos(),
        }
    }

    pub(crate) fn atan(&self) -> Self {
        match self {
            Self::Complex(z) => z.atan(),
            Self::Real(r) => Self::real(r.atan()),
        }
    }

    pub(crate) fn complex_conjugate(self) -> Self {
        match self {
            Self::Complex(z) => z.conjugate(),
            Self::Real(_) => self,
        }
    }

    // convenience wrappers for passing Op impls as closures
    pub(crate) fn add(self, rhs: &Self) -> Self {
        self + rhs
    }
    pub(crate) fn mul(self, rhs: &Self) -> Self {
        self * rhs
    }
    pub(crate) fn div(self, rhs: &Self) -> NumResult {
        self / rhs
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

impl Neg for &Number {
    type Output = Number;

    fn neg(self) -> Self::Output {
        -self.clone()
    }
}

macro_rules! impl_num_commutative {
    ($imp:ident, $func:ident, $op:tt; $($this:ty, $that:ty);+ $(;)?) => {
        $(impl $imp<$that> for $this {
            type Output = Number;

            fn $func(self, rhs: $that) -> Self::Output {
                match (self, rhs) {
                    (Number::Complex(z), x) => z $op x,
                    (x, Number::Complex(z)) => z $op x,
                    (Number::Real(a), Number::Real(b)) => Number::real(a $op b),
                }
            }
        })+
    };
}
impl_num_commutative! {
    Add, add, +;
    Number, Number;
    Number, &Number;
    &Number, Number;
    &Number, &Number;
}
impl_num_commutative! {
    Mul, mul, *;
    Number, Number;
    Number, &Number;
    &Number, Number;
    &Number, &Number;
}

macro_rules! impl_num_inverse {
    ($imp:ident, $func:ident, $op:tt, $out:ty; $($this:ty, $that:ty, $conv:expr, $rfn:expr);+ $(;)?) => {
        $(impl $imp<$that> for $this {
            type Output = $out;

            fn $func(self, rhs: $that) -> Self::Output {
                match (self, rhs) {
                    (Number::Complex(z), n) => z $op n,
                    (Number::Real(r), Number::Complex(z)) => $conv(r).into_complex() $op z,
                    (Number::Real(a), Number::Real(b)) => $rfn(a, b),
                }
            }
        })+
    };
}
impl_num_inverse! {
    Sub, sub, -, Number;
    Number, Number, convert::identity, |a, b| Number::real(a - b);
    Number, &Number, convert::identity, |a, b| Number::real(a - b);
    &Number, Number, Real::clone, |a, b| Number::real(a - b);
    &Number, &Number, Real::clone, |a, b| Number::real(a - b);
}
impl_num_inverse! {
    Div, div, /, NumResult;
    Number, Number, convert::identity, |a, b| Ok(Number::real((a / b)?));
    Number, &Number, convert::identity, |a, b| Ok(Number::real((a / b)?));
    &Number, Number, Real::clone, |a, b| Ok(Number::real((a / b)?));
    &Number, &Number, Real::clone, |a, b| Ok(Number::real((a / b)?));
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

macro_rules! try_int_conversion {
    ($($type:ty, $convert:ident);+ $(;)?) => {
        $(impl TryFrom<Number> for $type {
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
        })+
    };
}
try_int_conversion! {
    u8, try_to_u8;
    i32, try_to_i32;
    u32, try_to_u32;
    i64, try_to_i64;
    usize, try_to_usize;
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct Complex(Box<(Real, Real)>);

impl Complex {
    fn is_zero(&self) -> bool {
        self.0.0.is_zero() && self.0.1.is_zero()
    }

    fn to_real(&self) -> Real {
        self.0.0.clone()
    }

    fn to_imag(&self) -> Real {
        self.0.1.clone()
    }

    fn magnitude(&self) -> Real {
        let (x, y) = self.get_parts();
        if x.is_inexact() || y.is_inexact() {
            Real::Float(x.to_float().hypot(y.to_float()))
        } else {
            ((x * x) + (y * y)).sqrt()
        }
    }

    fn angle(&self) -> Real {
        let (x, y) = self.get_parts();
        Real::Float(y.to_float().atan2(x.to_float()))
    }

    fn reciprocal(&self) -> NumResult {
        Real::one().into_complex() / self
    }

    fn get_parts(&self) -> (&Real, &Real) {
        (&self.0.0, &self.0.1)
    }

    /*
     * Square root of Complex is defined as:
     * given x+yi and r = √(x² + y²)
     * √x+yi = √((r+x)/2) + sign(y)i√((r-x)/2)
     * split on whether x < 0 to avoid cancellation issues for (r-x)/2; x > 0, |y| << x
     * used by C99's csqrt, which also includes some special casing for signed zeros and infs.
     */
    #[allow(clippy::many_single_char_names)]
    fn sqrt(&self) -> Number {
        // Square root of zero always sets x to + and keeps sign of y; if we calculated
        // this with the below algorithm instead, the zero signs go wonky due to IEEE rules.
        // Complex is_zero implies float values (exact zeros would have reduced to Integer),
        // so we can safely hardcode the answer without losing exactness.
        if self.is_zero() {
            return Number::complex(0.0, 0.0f64.copysign(self.0.1.signum()));
        }
        let (x, y) = self.get_parts();
        // According to C99-Annex-G inf y always sets x to +inf and keeps y
        // because IEEE infinities are not limits but actual values, which means
        // the math doesn't work out without special-casing it.
        if y.is_infinite() {
            return Number::complex(f64::INFINITY, f64::INFINITY.copysign(self.0.1.signum()));
        }
        let r = self.magnitude();
        let (u, v) = if x.is_negative() {
            let t = scaled_re(&r, x, Real::sub);
            (
                assume_safe_div!(y.abs() / &t),
                assume_safe_div!(t / Real::two()).copysign(y),
            )
        } else {
            let t = scaled_re(&r, x, Real::add);
            (assume_safe_div!(&t / Real::two()), assume_safe_div!(y / t))
        };
        Number::complex(u, v)
    }

    // e^z = e^(x+yi) = e^x * e^yi = (e^x * cos y) + (ie^x * sin y)
    fn exp(&self) -> Number {
        todo!();
    }

    // ln z = ln r + iθ = ln (mag z) + i(angle z)
    fn ln(&self) -> Number {
        todo!();
    }

    // sin z = sin (x+yi) = (sin x)(cosh y) + i(cos x)(sinh y)
    fn sin(&self) -> Number {
        todo!();
    }

    // cos z = cos (x+yi) = (cos x)(cosh y) - i(sin x)(sinh y)
    fn cos(&self) -> Number {
        todo!();
    }

    // tan z = sin z / cos z = (sin 2x + i(sinh 2y)) / (cos 2x + cosh 2y)
    // The real-form identity is used to avoid rounding errors with dividing
    // complex trig functions.
    fn tan(&self) -> Number {
        todo!();
    }

    fn asin(&self) -> Number {
        todo!();
    }

    fn acos(&self) -> Number {
        todo!();
    }

    fn atan(&self) -> Number {
        todo!();
    }

    fn into_parts(self) -> (Real, Real) {
        (self.0.0, self.0.1)
    }

    fn conjugate(self) -> Number {
        let (x, y) = self.into_parts();
        Number::complex(x, -y)
    }
}

macro_rules! impl_cpx_sub {
    ($($this:ty, $that:ty, $lp:expr, $rp:expr);+ $(;)?) => {
        $(impl Sub<$that> for $this {
            type Output = Number;

            fn sub(self, rhs: $that) -> Self::Output {
                let (x, y) = $lp(self);
                let (u, v) = $rp(rhs);
                Number::complex(x - u, y - v)
            }
        })+
    };
}
impl_cpx_sub! {
    Complex, Complex, Complex::into_parts, Complex::into_parts;
    Complex, &Complex, Complex::into_parts, Complex::get_parts;
    &Complex, Complex, Complex::get_parts, Complex::into_parts;
    &Complex, &Complex, Complex::get_parts, Complex::get_parts;
}
macro_rules! impl_cpx_div {
    ($($this:ty, $that:ty, $conv:expr);+ $(;)?) => {
        $(impl Div<$that> for $this {
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
            fn div(self, rhs: $that) -> Self::Output {
                let ((a, b), (mut c, mut d)) = (self.get_parts(), $conv(rhs).into_parts());
                // Don't mix exact/inexact in the ratio/denominator elements as exact values
                // can interact strangely with signed zeros, nan, and inf; apply float-taint
                // to quotient calculations to avoid these issues.
                if c.is_inexact() || d.is_inexact() {
                    (c, d) = (c.to_inexact(), d.to_inexact());
                }
                let (re, im) = if c.abs() < d.abs() {
                    let r = (&c / &d)?;
                    let denom = (c * &r) + d;
                    ((((a * &r) + b) / &denom)?, (((b * r) - a) / denom)?)
                } else {
                    let r = (&d / &c)?;
                    let denom = c + (d * &r);
                    (((a + (b * &r)) / &denom)?, ((b - (a * r)) / denom)?)
                };
                Ok(Number::complex(re, im))
            }
        })+
    };
}
impl_cpx_div! {
    Complex, Complex, convert::identity;
    Complex, &Complex, Complex::clone;
    &Complex, Complex, convert::identity;
    &Complex, &Complex, Complex::clone;
}

macro_rules! impl_cpx_num_add {
    ($($this:ty, $that:ty, $lp:expr, $rp:expr, $ic:expr);+ $(;)?) => {
        $(impl Add<$that> for $this {
            type Output = Number;

            fn add(self, rhs: $that) -> Self::Output {
                let (x, y) = $lp(self);
                match rhs {
                    Number::Complex(w) => {
                        let (u, v) = $rp(w);
                        Number::complex(x + u, y + v)
                    }
                    Number::Real(r) => Number::complex(x + r, $ic(y)),
                }
            }
        })+
    };
}
impl_cpx_num_add! {
    Complex, Number, Complex::into_parts, Complex::into_parts, convert::identity;
    Complex, &Number, Complex::into_parts, Complex::get_parts, convert::identity;
    &Complex, Number, Complex::get_parts, Complex::into_parts, Real::clone;
    &Complex, &Number, Complex::get_parts, Complex::get_parts, Real::clone;
}
macro_rules! impl_cpx_num_mul {
    ($($this:ty, $that:ty, $rc:expr);+ $(;)?) => {
        $(impl Mul<$that> for $this {
            type Output = Number;

            // Complex multiplication: (a + bi) * (c + di) = (ac - bd) + (ad + bc)i
            fn mul(self, rhs: $that) -> Self::Output {
                let (a, b) = self.get_parts();
                match rhs {
                    Number::Complex(z) => {
                        let (c, d) = z.get_parts();
                        cpx_product(a, b, c, d)
                    },
                    Number::Real(r) => {
                        let (c, d) = $rc(r).into_complex().into_parts();
                        cpx_product(a, b, &c, &d)
                    },
                }
            }
        })+
    };
}
impl_cpx_num_mul! {
    Complex, Number, convert::identity;
    Complex, &Number, Real::clone;
    &Complex, Number, convert::identity;
    &Complex, &Number, Real::clone;
}

macro_rules! impl_cpx_num_sub {
    ($($this:ty, $that:ty, $conv:expr);+ $(;)?) => {
        $(impl Sub<$that> for $this {
            type Output = Number;

            fn sub(self, rhs: $that) -> Self::Output {
                match rhs {
                    Number::Complex(z) => self - z,
                    Number::Real(r) => self - $conv(r).into_complex(),
                }
            }
        })+
    };
}
impl_cpx_num_sub! {
    Complex, Number, convert::identity;
    Complex, &Number, Real::clone;
    &Complex, Number, convert::identity;
    &Complex, &Number, Real::clone;
}
macro_rules! impl_cpx_num_div {
    ($($this:ty, $that:ty, $parts:expr);+ $(;)?) => {
        $(impl Div<$that> for $this {
            type Output = NumResult;

            fn div(self, rhs: $that) -> Self::Output {
                match rhs {
                    Number::Complex(z) => self / z,
                    Number::Real(r) => {
                        let (x, y) = $parts(self);
                        Ok(Number::complex((x / r.borrow())?, (y / r)?))
                    }
                }
            }
        })+
    };
}
impl_cpx_num_div! {
    Complex, Number, Complex::into_parts;
    Complex, &Number, Complex::into_parts;
    &Complex, Number, Complex::get_parts;
    &Complex, &Number, Complex::get_parts;
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
        // TODO: https://doc.rust-lang.org/std/primitive.f64.html#associatedconstant.MIN_EXACT_INTEGER
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

    pub(crate) fn is_zero(&self) -> bool {
        match self {
            Self::Float(f) => *f == 0.0,
            Self::Integer(n) => n.is_zero(),
            Self::Rational(q) => q.is_zero(),
        }
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

    pub(crate) fn to_inexact(&self) -> Self {
        match self {
            Self::Float(_) => self.clone(),
            Self::Integer(n) => n.to_inexact(),
            Self::Rational(q) => q.to_inexact(),
        }
    }

    pub(crate) fn try_to_exact(&self) -> RealResult {
        if let Self::Float(f) = self {
            try_float_to_exact(*f)
        } else {
            Ok(self.clone())
        }
    }

    pub(crate) fn try_to_exact_integer(&self) -> IntResult {
        match self {
            Self::Float(f) if f.fract() == 0.0 => Ok(Integer::from_exact_float(*f)),
            Self::Integer(n) => Ok(n.clone()),
            _ => Err(NumericError::NotExactInteger(self.to_string())),
        }
    }

    pub(crate) fn try_to_numerator(&self) -> RealResult {
        Ok(match self {
            Self::Float(_) => self.try_to_exact()?.try_to_numerator()?.to_inexact(),
            Self::Integer(_) => self.clone(),
            Self::Rational(q) => q.to_numerator().into(),
        })
    }

    pub(crate) fn try_to_denominator(&self) -> RealResult {
        Ok(match self {
            Self::Float(_) => self.try_to_exact()?.try_to_denominator()?.to_inexact(),
            Self::Integer(_) => Integer::one().into(),
            Self::Rational(q) => q.to_denominator().into(),
        })
    }

    pub(crate) fn signum(&self) -> f64 {
        match self {
            Self::Float(f) => f.signum(),
            Self::Integer(n) => n.signum(),
            Self::Rational(q) => q.signum(),
        }
    }

    pub(crate) fn abs(&self) -> Self {
        match self {
            Self::Float(f) => f.abs().into(),
            Self::Integer(n) => n.clone().abs().into(),
            Self::Rational(q) => Self::Rational(q.clone().abs()),
        }
    }

    pub(crate) fn floor(&self) -> Self {
        match self {
            Self::Float(f) => f.floor().into(),
            Self::Integer(_) => self.clone(),
            Self::Rational(q) => q.floor().into(),
        }
    }

    pub(crate) fn ceiling(&self) -> Self {
        match self {
            Self::Float(f) => f.ceil().into(),
            Self::Integer(_) => self.clone(),
            Self::Rational(q) => q.ceiling().into(),
        }
    }

    pub(crate) fn truncate(&self) -> Self {
        match self {
            Self::Float(f) => f.trunc().into(),
            Self::Integer(_) => self.clone(),
            Self::Rational(q) => q.truncate().into(),
        }
    }

    pub(crate) fn round(&self) -> Self {
        match self {
            Self::Float(f) => f.round_ties_even().into(),
            Self::Integer(_) => self.clone(),
            Self::Rational(q) => q.round().into(),
        }
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

    fn is_exact_zero(&self) -> bool {
        if let Self::Integer(n) = self {
            n.is_zero()
        } else {
            false
        }
    }

    fn is_exact_one(&self) -> bool {
        if let Self::Integer(n) = self {
            n.is_one()
        } else {
            false
        }
    }

    fn is_infinite(&self) -> bool {
        if let Self::Float(f) = self {
            f.is_infinite()
        } else {
            false
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

    fn reciprocal(&self) -> RealResult {
        match self {
            Self::Float(f) => Ok(f.recip().into()),
            Self::Integer(n) => n.clone().reciprocal(),
            Self::Rational(q) => q.clone().reciprocal(),
        }
    }

    fn angle(&self) -> RealResult {
        if self.is_exact_zero() {
            Err(NumericError::UndefinedAtZero)
        } else {
            Ok(real_angle(self.signum()))
        }
    }

    fn copysign(&self, sign: &Real) -> Self {
        let s = sign.signum();
        match self {
            Self::Float(f) => f.copysign(s).into(),
            Self::Integer(n) => n.clone().copysign(s).into(),
            Self::Rational(q) => Self::Rational(q.clone().copysign(s)),
        }
    }

    // Number handles negative sign so this function technically returns the
    // wrong value for negative roots (e.g. √-4 = -2 instead of +2i)
    fn sqrt(&self) -> Self {
        match self {
            Self::Float(0.0) => self.clone(),
            Self::Float(f) => sign_preserving_sqrt(*f).into(),
            Self::Integer(n) => n.sqrt(),
            Self::Rational(q) => q.sqrt(),
        }
    }

    fn exp(&self) -> Self {
        if self.is_exact_zero() {
            Self::one()
        } else {
            self.to_float().exp().into()
        }
    }

    fn ln(&self) -> NumResult {
        if self.is_exact_zero() {
            Err(NumericError::UndefinedAtZero)
        } else if self.is_exact_one() {
            Ok(Number::zero())
        } else {
            // ln(-x) = ln(x) + iπ
            // ln(x) = ln(x) + i0 -> ln(x)
            // use signum so it doesn't differentiate between ± NaN
            let f = self.to_float();
            Ok(Number::complex(f.abs().ln(), real_angle(f.signum())))
        }
    }

    fn sin(&self) -> Self {
        if self.is_exact_zero() {
            Self::zero()
        } else {
            self.to_float().sin().into()
        }
    }

    fn cos(&self) -> Self {
        if self.is_exact_zero() {
            Self::one()
        } else {
            self.to_float().cos().into()
        }
    }

    fn tan(&self) -> Self {
        if self.is_exact_zero() {
            Self::zero()
        } else {
            self.to_float().tan().into()
        }
    }

    fn asin(&self) -> Number {
        if self.is_exact_zero() {
            Number::zero()
        } else {
            self.arc_sincos(Number::asin, f64::asin)
        }
    }

    fn acos(&self) -> Number {
        if self.is_exact_one() {
            Number::zero()
        } else {
            self.arc_sincos(Number::acos, f64::acos)
        }
    }

    fn atan(&self) -> Self {
        if self.is_exact_zero() {
            Self::zero()
        } else {
            self.to_float().atan().into()
        }
    }

    // Inverse cos/sin have complex-number cuts above 1 and below -1; according
    // to Kahan's rule the imaginary part's sign is the opposite of the real.
    fn arc_sincos(
        &self,
        zfn: impl FnOnce(&Number) -> Number,
        ffn: impl FnOnce(f64) -> f64,
    ) -> Number {
        let f = self.to_float();
        if f > 1.0 {
            zfn(&Number::complex(f, -0.0))
        } else if f < -1.0 {
            zfn(&Number::complex(f, 0.0))
        } else {
            Number::real(ffn(f))
        }
    }

    // convenience wrappers for passing Op impls as closures
    fn add(&self, rhs: &Self) -> Self {
        self + rhs
    }
    fn sub(&self, rhs: &Self) -> Self {
        self - rhs
    }

    fn into_complex(self) -> Complex {
        Complex((self, Self::zero()).into())
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

impl Neg for &Real {
    type Output = Real;

    fn neg(self) -> Self::Output {
        -self.clone()
    }
}

macro_rules! impl_real_commutative {
    ($imp:ident, $func:ident, $op:tt; $($this:ty, $that:ty, $zfn:expr);+ $(;)?) => {
        $(impl $imp<$that> for $this {
            type Output = Real;

            fn $func(self, rhs: $that) -> Self::Output {
                match self {
                    // Addition: integral additive identity should not affect a float;
                    // if we convert to float we get -0.0 + 0.0 = 0.0 which is wrong!
                    // exact zero shouldn't affect the sign of inexact zero.
                    // Multiplication: exact zero overrides float-taint.
                    Real::Float(_) if rhs.is_exact_zero() => $zfn(self, rhs),
                    Real::Float(f) => (f $op rhs.to_float()).into(),
                    Real::Integer(n) => n $op rhs,
                    Real::Rational(q) => q $op rhs,
                }
            }
        })+
    };
}
impl_real_commutative! {
    Add, add, +;
    Real, Real, |s, _| s;
    Real, &Real, |s, _| s;
    &Real, Real, |s: &Real, _| s.clone();
    &Real, &Real, |s: &Real, _| s.clone();
}
impl_real_commutative! {
    Mul, mul, *;
    Real, Real, |_, r| r;
    Real, &Real, |_, r: &Real| r.clone();
    &Real, Real, |_, r| r;
    &Real, &Real, |_, r: &Real| r.clone();
}

macro_rules! impl_real_sub {
    ($($this:ty, $that:ty);+ $(;)?) => {
        $(impl Sub<$that> for $this {
            type Output = Real;

            fn sub(self, rhs: $that) -> Self::Output {
                match self {
                    Real::Float(f) => (f - rhs.to_float()).into(),
                    Real::Integer(n) => n - rhs,
                    Real::Rational(q) => q - rhs,
                }
            }
        })+
    };
}
impl_real_sub! {
    Real, Real;
    Real, &Real;
    &Real, Real;
    &Real, &Real;
}
macro_rules! impl_real_div {
    ($($this:ty, $that:ty);+ $(;)?) => {
        $(impl Div<$that> for $this {
            type Output = RealResult;

            fn div(self, rhs: $that) -> Self::Output {
                match self {
                    // exact zero overrides float-taint
                    Real::Float(_) if rhs.is_exact_zero() => Err(NumericError::DivideByZero),
                    Real::Float(f) => Ok((f / rhs.to_float()).into()),
                    Real::Integer(n) => n / rhs,
                    Real::Rational(q) => q / rhs,
                }
            }
        })+
    };
}
impl_real_div! {
    Real, Real;
    Real, &Real;
    &Real, Real;
    &Real, &Real;
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

    fn to_inexact(&self) -> Real {
        self.to_float().into()
    }

    fn to_numerator(&self) -> Integer {
        self.0.0.clone()
    }

    fn to_denominator(&self) -> Integer {
        self.0.1.clone()
    }

    fn get_parts(&self) -> (&Integer, &Integer) {
        (&self.0.0, &self.0.1)
    }

    fn floor(&self) -> Integer {
        let (n, d) = self.get_parts();
        n.div_floor(d)
    }

    fn ceiling(&self) -> Integer {
        let (n, d) = self.get_parts();
        n.div_ceiling(d)
    }

    fn truncate(&self) -> Integer {
        let (n, d) = self.get_parts();
        n.div_truncate(d)
    }

    fn round(&self) -> Integer {
        let (n, d) = self.get_parts();
        n.div_round(d)
    }

    // a/b ± c/d = (ad ± cb)/bd except cross-reduce with gcd and lcm to lessen
    // likelihood of intermediate result overflow.
    // div/0 should be a programmer error here, hence the panics. if everything
    // is wired up correctly this will only be called with canonical rationals
    // or integer reciprocals.
    #[allow(clippy::many_single_char_names)]
    fn additive_op(&self, rhs: &Self, op: impl FnOnce(Integer, Integer) -> Integer) -> Real {
        let (a, b) = self.get_parts();
        let (c, d) = rhs.get_parts();
        let g = b.gcd(d);
        let m = b.lcm(d);
        debug_assert!(!g.is_zero());
        debug_assert!(!m.is_zero());
        let ad = a * Real::exact_quotient(d.clone(), g.clone());
        let cb = c * Real::exact_quotient(b.clone(), g);
        assume_safe_div!(op(ad, cb) / m)
    }

    fn sqrt(&self) -> Real {
        let (n, d) = self.get_parts();
        match (n.sqrt(), d.sqrt()) {
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

    fn reciprocal(self) -> RealResult {
        Real::reduce(self.0.1, self.0.0)
    }

    fn abs(mut self) -> Self {
        self.0.0 = self.0.0.abs();
        self
    }

    fn copysign(mut self, sign: f64) -> Self {
        self.0.0 = self.0.0.copysign(sign);
        self
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
        let (a, b) = self.get_parts();
        let (c, d) = other.get_parts();
        (a * d).cmp(&(c * b))
    }
}

impl Neg for Rational {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Self((-self.0.0, self.0.1).into())
    }
}

impl Neg for &Rational {
    type Output = Rational;

    fn neg(self) -> Self::Output {
        -self.clone()
    }
}

macro_rules! impl_rat_additive {
    ($imp:ident, $func:ident) => {
        impl<Rhs: Borrow<Rational>> $imp<Rhs> for &Rational {
            type Output = Real;

            fn $func(self, rhs: Rhs) -> Self::Output {
                self.additive_op(rhs.borrow(), Integer::$func)
            }
        }
    };
}
impl_rat_additive!(Add, add);
impl_val_delegate!(Add, Rational, Real);
impl_rat_additive!(Sub, sub);
impl_val_delegate!(Sub, Rational, Real);

impl<Rhs: Borrow<Rational>> Mul<Rhs> for &Rational {
    type Output = Real;

    // a/b * c/d = ac/bd except cross-reduce with gcds first to lessen
    // likelihood of intermediate result overflow.
    // div/0 should be a programmer error here, hence the panics. if everything
    // is wired up correctly this will only be called with canonical rationals
    // or integer reciprocals.
    fn mul(self, rhs: Rhs) -> Self::Output {
        let (a, b) = self.get_parts();
        let (c, d) = rhs.borrow().get_parts();
        let (g1, g2) = (a.gcd(d), c.gcd(b));
        debug_assert!(!g1.is_zero());
        debug_assert!(!g2.is_zero());
        assume_safe_div!(Real::reduce(
            Real::exact_quotient(a.clone(), g1.clone())
                * Real::exact_quotient(c.clone(), g2.clone()),
            Real::exact_quotient(b.clone(), g2) * Real::exact_quotient(d.clone(), g1),
        ))
    }
}
impl_val_delegate!(Mul, Rational, Real);

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

macro_rules! impl_rat_real_arithmetic_ring {
    ($imp:ident, $func:ident, $op:tt; $($this:ty, $that:ty, $int_conv:expr);+ $(;)?) => {
        $(impl $imp<$that> for $this {
            type Output = Real;

            fn $func(self, rhs: $that) -> Self::Output {
                match rhs {
                    Real::Float(f) => (self.to_float() $op f).into(),
                    Real::Integer(n) => self $op $int_conv(n).into_rational(),
                    Real::Rational(q) => self $op q,
                }
            }
        })+
    };
}
impl_rat_real_arithmetic_ring! {
    Add, add, +;
    Rational, Real, convert::identity;
    Rational, &Real, Integer::clone;
    &Rational, Real, convert::identity;
    &Rational, &Real, Integer::clone;
}
impl_rat_real_arithmetic_ring! {
    Sub, sub, -;
    Rational, Real, convert::identity;
    Rational, &Real, Integer::clone;
    &Rational, Real, convert::identity;
    &Rational, &Real, Integer::clone;
}
impl_rat_real_arithmetic_ring! {
    Mul, mul, *;
    Rational, Real, convert::identity;
    Rational, &Real, Integer::clone;
    &Rational, Real, convert::identity;
    &Rational, &Real, Integer::clone;
}

macro_rules! impl_rat_real_div {
    ($($this:ty, $that:ty, $nc:expr, $qc:expr);+ $(;)?) => {
        $(impl Div<$that> for $this {
            type Output = RealResult;

            fn div(self, rhs: $that) -> Self::Output {
                Ok(match rhs {
                    // need this because (q * f.recip()) ends up losing precision
                    Real::Float(f) => (self.to_float() / f).into(),
                    Real::Integer(n) => self * $nc(n).reciprocal()?,
                    Real::Rational(q) => self * $qc(q).reciprocal()?,
                })
            }
        })+
    }
}
impl_rat_real_div! {
    Rational, Real, convert::identity, convert::identity;
    Rational, &Real, Integer::clone, Rational::clone;
    &Rational, Real, convert::identity, convert::identity;
    &Rational, &Real, Integer::clone, Rational::clone;
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

    pub(crate) fn to_inexact(&self) -> Real {
        Real::Float(self.to_float())
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

    pub(crate) fn truncate_quotient(&self, rhs: &Self) -> IntResult {
        if rhs.is_zero() {
            Err(NumericError::DivideByZero)
        } else {
            Ok(self.div_truncate(rhs))
        }
    }

    pub(crate) fn truncate_rem(&self, rhs: &Self) -> IntResult {
        self % rhs
    }

    pub(crate) fn truncate_quotrem(&self, rhs: &Self) -> Result<(Self, Self), NumericError> {
        Ok((self.truncate_quotient(rhs)?, (self % rhs)?))
    }

    pub(crate) fn floor_quotient(&self, rhs: &Self) -> IntResult {
        if rhs.is_zero() {
            Err(NumericError::DivideByZero)
        } else {
            Ok(self.div_floor(rhs))
        }
    }

    pub(crate) fn floor_rem(&self, rhs: &Self) -> IntResult {
        let (_, r) = self.floor_quotrem(rhs)?;
        Ok(r)
    }

    pub(crate) fn floor_quotrem(&self, rhs: &Self) -> Result<(Self, Self), NumericError> {
        if rhs.is_zero() {
            return Err(NumericError::DivideByZero);
        }
        let q = self.div_floor(rhs);
        let r = self - (&q * rhs);
        Ok((q, r))
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

    fn is_one(&self) -> bool {
        self.is_positive() && self.is_magnitude_one()
    }

    fn is_magnitude_one(&self) -> bool {
        match &self.precision {
            Precision::Single(u) => *u == 1,
            Precision::Multiple(_) => todo!(),
        }
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

    int_convert! {
        try_to_i32,
        i32,
        NumericError::Int32ConversionInvalidRange,
        |u| (-(u as i64)).try_into();

        try_to_i64,
        i64,
        NumericError::Int64ConversionInvalidRange,
        |u| {
            Ok(if u == i64::MIN.unsigned_abs() {
                i64::MIN
            } else {
                -(u as i64)
            })
        };
    }
    uint_convert! {
        try_to_u8, u8, NumericError::ByteConversionInvalidRange;
        try_to_u32, u32, NumericError::Uint32ConversionInvalidRange;
        try_to_usize, usize, NumericError::UsizeConversionInvalidRange;
    }

    fn signum(&self) -> f64 {
        self.sign.into()
    }

    fn cmp_magnitude(&self, other: &Self) -> Ordering {
        self.precision.cmp(&other.precision)
    }

    fn safe_sum(&self, rhs: &Self) -> Self {
        let sum = &self.precision + &rhs.precision;
        Self::new(sum, self.sign)
    }

    fn add_opposite_signs(&self, rhs: &Self) -> Self {
        self.overflowing_sum(rhs, rhs.sign)
    }

    fn sub_same_signs(&self, rhs: &Self) -> Self {
        self.overflowing_sum(&rhs, -self.sign)
    }

    fn overflowing_sum(&self, rhs: &Self, ovf_sign: Sign) -> Self {
        let (sign, sum) = if self.precision < rhs.precision {
            (ovf_sign, &rhs.precision - &self.precision)
        } else {
            (self.sign, &self.precision - &rhs.precision)
        };
        Self::new(sum, sign)
    }

    fn sqrt(&self) -> Real {
        if self.is_zero() {
            self.clone().into()
        } else if let Some(p) = self.precision.isqrt() {
            Self::new(p, self.sign).into()
        } else {
            sign_preserving_sqrt(self.to_float()).into()
        }
    }

    // All of the following exact-division operations assume arguments come
    // from well-formed arguments, i.e. no zero divisor.
    fn div_floor(&self, rhs: &Self) -> Self {
        self.div_exact(rhs, Precision::div, Precision::div_ceil)
    }

    fn div_ceiling(&self, rhs: &Self) -> Self {
        self.div_exact(rhs, Precision::div_ceil, Precision::div)
    }

    fn div_truncate(&self, rhs: &Self) -> Self {
        self.div_exact(rhs, Precision::div, Precision::div)
    }

    fn div_round(&self, rhs: &Self) -> Self {
        self.div_exact(rhs, Precision::div_round, Precision::div_round)
    }

    fn div_exact(
        &self,
        rhs: &Self,
        pos: impl FnOnce(&Precision, &Precision) -> Precision,
        neg: impl FnOnce(&Precision, &Precision) -> Precision,
    ) -> Self {
        debug_assert!(!rhs.is_zero());
        let s = self.sign * rhs.sign;
        match s {
            Sign::Negative => Self::new(neg(&self.precision, &rhs.precision), s),
            Sign::Positive => Self::new(pos(&self.precision, &rhs.precision), s),
            Sign::Zero => Self::zero(),
        }
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

    fn into_rational(self) -> Rational {
        Rational((self, Self::one()).into())
    }

    fn abs(mut self) -> Self {
        self.make_positive();
        self
    }

    fn reciprocal(self) -> RealResult {
        Self::one() / self
    }

    fn copysign(mut self, sign: f64) -> Self {
        self.sign = sign.into();
        self
    }

    // convenience wrappers for passing Op impls as closures
    fn add(self, rhs: Self) -> Self {
        self + rhs
    }
    fn sub(self, rhs: Self) -> Self {
        self - rhs
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

impl Neg for &Integer {
    type Output = Integer;

    fn neg(self) -> Self::Output {
        -self.clone()
    }
}

macro_rules! impl_int_additive {
    ($imp:ident, $func:ident; $($this:ty, $that:ty, $zrhs:expr, $zlhs:expr, $same:expr, $diff:expr);+ $(;)?) => {
        $(impl $imp<$that> for $this {
            type Output = Integer;

            fn $func(self, rhs: $that) -> Self::Output {
                match (&self.sign, &rhs.sign) {
                    (_, Sign::Zero) => $zrhs(self),
                    (Sign::Zero, _) => $zlhs(rhs),
                    (Sign::Positive, Sign::Positive) | (Sign::Negative, Sign::Negative) => {
                        $same(&self, &rhs)
                    }
                    (Sign::Positive, Sign::Negative) | (Sign::Negative, Sign::Positive) => {
                        $diff(&self, &rhs)
                    }
                }
            }
        })+
    };
}
impl_int_additive! {
    Add, add;
    Integer, Integer, convert::identity, convert::identity, Integer::safe_sum, Integer::add_opposite_signs;
    Integer, &Integer, convert::identity, Integer::clone, Integer::safe_sum, Integer::add_opposite_signs;
    &Integer, Integer, Integer::clone, convert::identity, Integer::safe_sum, Integer::add_opposite_signs;
    &Integer, &Integer, Integer::clone, Integer::clone, Integer::safe_sum, Integer::add_opposite_signs;
}
impl_int_additive! {
    Sub, sub;
    Integer, Integer, convert::identity, Integer::neg, Integer::sub_same_signs, Integer::safe_sum;
    Integer, &Integer, convert::identity, <&Integer>::neg, Integer::sub_same_signs, Integer::safe_sum;
    &Integer, Integer, Integer::clone, Integer::neg, Integer::sub_same_signs, Integer::safe_sum;
    &Integer, &Integer, Integer::clone, <&Integer>::neg, Integer::sub_same_signs, Integer::safe_sum;
}

impl<Rhs: Borrow<Integer>> Mul<Rhs> for &Integer {
    type Output = Integer;

    fn mul(self, rhs: Rhs) -> Self::Output {
        match self.sign * rhs.borrow().sign {
            Sign::Zero => Integer::zero(),
            s => Integer::new(&self.precision * &rhs.borrow().precision, s),
        }
    }
}
impl_val_delegate!(Mul, Integer);

macro_rules! impl_int_div {
    ($($this:ty, $that:ty, $op:expr);+ $(;)?) => {
        $(impl Div<$that> for $this {
            type Output = RealResult;

            fn div(self, rhs: $that) -> Self::Output {
                $op(self, rhs)
            }
        })+
    }
}
impl_int_div! {
    Integer, Integer, Real::reduce;
    Integer, &Integer, |a, b: &Integer| a / b.clone();
    &Integer, Integer, |a: &Integer, b| a.clone() / b;
    &Integer, &Integer, |a: &Integer, b: &Integer| a.clone() / b.clone();
}

impl<Rhs: Borrow<Integer>> Rem<Rhs> for &Integer {
    type Output = IntResult;

    fn rem(self, rhs: Rhs) -> Self::Output {
        if rhs.borrow().is_zero() {
            return Err(NumericError::DivideByZero);
        }
        Ok(Integer::new(
            &self.precision % &rhs.borrow().precision,
            self.sign,
        ))
    }
}
impl_val_delegate!(Rem, Integer, IntResult);

impl Display for Integer {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        self.sign.fmt(f)?;
        self.precision.fmt(f)
    }
}

// TODO: handle multi-precision later
impl From<i64> for Integer {
    fn from(value: i64) -> Self {
        Self::new(value.unsigned_abs(), value)
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

macro_rules! impl_int_real_arithmetic_ring {
    ($imp:ident, $func:ident, $op:tt; $($this:ty, $that:ty, $zfn:expr, $rat_conv:expr);+ $(;)?) => {
        $(impl $imp<$that> for $this {
            type Output = Real;

            fn $func(self, rhs: $that) -> Self::Output {
                match rhs {
                    // Addition: integral additive identity should not affect a float;
                    // if we convert to float we get 0.0 + -0.0 = 0.0 which is wrong!
                    // Exact zero shouldn't affect the sign of inexact zero.
                    // Subtraction: integral additive reciprical should flip the sign of a float;
                    // this ends up being relevant for 0 - 0.0 = -0.0
                    Real::Float(_) if self.is_zero() => $zfn(self, rhs).into(),
                    Real::Float(f) => (self.to_float() $op f).into(),
                    Real::Integer(n) => (self $op n).into(),
                    Real::Rational(q) => $rat_conv(self).into_rational() $op q,
                }
            }
        })+
    }
}
impl_int_real_arithmetic_ring! {
    Add, add, +;
    Integer, Real, |_, r| r, convert::identity;
    Integer, &Real, |_, r: &Real| r.clone(), convert::identity;
    &Integer, Real, |_, r| r, Integer::clone;
    &Integer, &Real, |_, r: &Real| r.clone(), Integer::clone;
}
impl_int_real_arithmetic_ring! {
    Sub, sub, -;
    Integer, Real, |_, r: Real| -r, convert::identity;
    Integer, &Real, |_, r: &Real| -r, convert::identity;
    &Integer, Real, |_, r: Real| -r, Integer::clone;
    &Integer, &Real, |_, r: &Real| -r, Integer::clone;
}
impl_int_real_arithmetic_ring! {
    Mul, mul, *;
    Integer, Real, |l, _| l, convert::identity;
    Integer, &Real, |l, _| l, convert::identity;
    &Integer, Real, |l: &Integer, _| l.clone(), Integer::clone;
    &Integer, &Real, |l: &Integer, _| l.clone(), Integer::clone;
}

macro_rules! impl_int_real_div {
    ($($this:ty, $that:ty, $lhc:expr, $rhc:expr, $qc:expr);+ $(;)?) => {
        $(impl Div<$that> for $this {
            type Output = RealResult;

            fn div(self, rhs: $that) -> Self::Output {
                match rhs {
                    // nan overrides exact zero which overrides float-taint
                    Real::Float(f) if f.is_nan() => Ok($rhc(rhs)),
                    Real::Float(_) if self.is_zero() => Ok($lhc(self).into()),
                    Real::Float(f) => Ok((self.to_float() / f).into()),
                    Real::Integer(n) => self / n,
                    Real::Rational(q) => Ok(self * $qc(q).reciprocal()?),
                }
            }
        })+
    }
}
impl_int_real_div! {
    Integer, Real, convert::identity, convert::identity, convert::identity;
    Integer, &Real, convert::identity, Real::clone, Rational::clone;
    &Integer, Real, Integer::clone, convert::identity, convert::identity;
    &Integer, &Real, Integer::clone, Real::clone, Rational::clone;
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

macro_rules! sign_from {
    ($($type:ty);+ $(;)?) => {
        $(impl From<$type> for Sign {
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
        })+
    };
}
sign_from! {
    i64;
    f64;
}

impl From<Sign> for f64 {
    fn from(value: Sign) -> Self {
        match value {
            Sign::Negative => -1.0,
            Sign::Zero => 0.0,
            Sign::Positive => 1.0,
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
    UndefinedAtZero,
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
            Self::UndefinedAtZero => f.write_str("undefined at zero"),
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

// helper to keep the sign consistent across √ in order to apply imaginary root later
fn sign_preserving_sqrt(f: f64) -> f64 {
    f.abs().sqrt().copysign(f)
}

// helper to calculate the intermediate real value for complex sqrt
fn scaled_re(r: &Real, x: &Real, op: impl FnOnce(&Real, &Real) -> Real) -> Real {
    let t = (Real::two() * op(r, x)).sqrt();
    debug_assert!(!t.is_zero());
    t
}

fn cpx_product(a: &Real, b: &Real, c: &Real, d: &Real) -> Number {
    Number::complex((a * c) - (b * d), (a * d) + (b * c))
}

fn real_angle(sign: f64) -> Real {
    if sign > 0.0 {
        Real::zero()
    } else {
        f64::consts::PI.into()
    }
}

fn try_float_to_exact(flt: f64) -> RealResult {
    if flt == 0.0 {
        // both +/- zero converts to integer zero
        Ok(Real::zero())
    } else if flt.is_finite() {
        try_to_dyadic_rational(flt)
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
    let exp_bits = ((bits & 0x7ff0_0000_0000_0000) >> MANTISSA_SIZE) as i32;
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
            mantissa_bits.cast_signed() | (1 << MANTISSA_SIZE),
            exp_bits - bias - MANTISSA_SIZE.cast_signed(),
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

fn write_intconversion_range_error(
    min: impl Display,
    max: impl Display,
    f: &mut Formatter,
) -> fmt::Result {
    write!(f, "integer literal out of range: [{min}, {max}]")
}
