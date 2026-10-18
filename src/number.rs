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
// float exponent bias is one < max exponent, due to the implicit leading one
const EXP_BIAS: i32 = f64::MAX_EXP - 1;

pub(crate) type NumResult = Result<Number, NumericError>;
pub(crate) type RealResult = Result<Real, NumericError>;
pub(crate) type IntResult = Result<Integer, NumericError>;
pub(crate) type IntPairResult = Result<(Integer, Integer), NumericError>;

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
            Self::Complex(z) => z.is_infinite(),
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

    pub(crate) fn isqrt_rem(&self) -> IntPairResult {
        if let Self::Real(Real::Integer(n)) = self {
            n.isqrt_rem()
        } else {
            Err(NumericError::NotNaturalNumber(self.to_string()))
        }
    }

    pub(crate) fn pow(&self, expt: &Self) -> NumResult {
        if let Number::Real(r) = expt
            && r.is_exact_zero()
        {
            Ok(Number::one())
        } else {
            match self {
                Self::Complex(z) => z.pow(expt),
                Self::Real(r) => r.pow(expt),
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
            Self::Complex(z) => z.ln(),
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

    fn zerof() -> Self {
        Self::real(0.0)
    }

    fn onef() -> Self {
        Self::real(1.0)
    }

    fn to_cpx_floats(&self) -> (f64, f64) {
        match self {
            Self::Complex(z) => z.to_floats(),
            Self::Real(r) => (r.to_float(), 0.0),
        }
    }

    fn powsq(&self) -> Self {
        match self {
            Self::Complex(z) => z.powsq(),
            Self::Real(r) => Number::real(r * r),
        }
    }

    fn into_real(self) -> Real {
        match self {
            Self::Complex(z) => z.into_real(),
            Self::Real(r) => r,
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
    // large number threshold for asin/acos (same as CPython)
    const ATRIG_SCALE_T: f64 = f64::MAX / 4.0;

    fn is_zero(&self) -> bool {
        self.0.0.is_zero() && self.0.1.is_zero()
    }

    fn is_infinite(&self) -> bool {
        self.0.0.is_infinite() || self.0.1.is_infinite()
    }

    fn to_real(&self) -> Real {
        self.0.0.clone()
    }

    fn to_imag(&self) -> Real {
        self.0.1.clone()
    }

    fn to_floats(&self) -> (f64, f64) {
        let (x, y) = self.get_parts();
        (x.to_float(), y.to_float())
    }

    fn to_number(&self) -> Number {
        Number::Complex(self.clone())
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
        // according to C99 Annex G if z has any infinite part,
        // even with nan, the reciprocal is 0-0i.
        if self.is_infinite() {
            Ok(Number::complex(0.0, -0.0))
        } else {
            Real::one().into_complex() / self
        }
    }

    fn get_real(&self) -> &Real {
        &self.0.0
    }

    fn get_parts(&self) -> (&Real, &Real) {
        (self.get_real(), &self.0.1)
    }

    /*
     * Square root of Complex is defined as:
     * given x+yi and r = √(x² + y²)
     * √x+yi = √(½(r+x)) + sign(y)i√(½(r-x))
     * split on whether x < 0 to avoid cancellation issues for ½(r-x); x > 0, |y| << x
     * used by C99's csqrt, which also includes some special casing for signed zeros and infs.
     */
    #[allow(clippy::many_single_char_names)]
    fn sqrt(&self) -> Number {
        // Float threshold for complex √ overflow-avoidant scaling; avoids numbers large
        // enough that either magnitude (r) or scaling 2(r+x) overflows.
        const SCALE_T: f64 = f64::MAX / 8.0;

        // Square root of zero always sets x to + and keeps sign of y; if we calculated
        // this with the below algorithm instead, the zero signs go wonky due to IEEE rules.
        // Complex is_zero implies non-exact imaginary, so only check exactness of the real part.
        if self.is_zero() {
            return Number::complex(
                if self.get_real().is_exact_zero() {
                    Real::zero()
                } else {
                    0.0.into()
                },
                0.0_f64.copysign(self.0.1.signum()),
            );
        }
        let (x, y) = self.get_parts();
        // According to C99-Annex-G inf y always sets x to +inf and keeps y
        // because IEEE infinities are not limits but actual values, which means
        // the math doesn't work out without special-casing it.
        if y.is_infinite() {
            return Number::complex(f64::INFINITY, f64::INFINITY.copysign(self.0.1.signum()));
        }
        // if x or y are very large floats, then scaling r with x will overflow;
        // taking scaled r = magnitude z/16, then multipling re/im by 4 avoids the issue
        if x.is_inexact() || y.is_inexact() {
            let (xf, yf) = (x.to_float(), y.to_float());
            if xf.abs() > SCALE_T || yf.abs() > SCALE_T {
                return float_scaled_cpx_sqrt(xf, yf);
            }
        }
        scaled_cpx_sqrt(&self.magnitude(), x, y)
    }

    // General form: z^w = e^(w·ln z); for exact int exponent square-and-multiply
    // works for both exact and inexact; for rational exponent q=n/d if d=2 then
    // (√z)^n keeps exactness better than general form.
    fn pow(&self, expt: &Number) -> NumResult {
        if let Number::Real(Real::Integer(n)) = expt {
            return square_and_multiply(self.to_number(), n);
        }
        if let Number::Real(Real::Rational(q)) = expt {
            let (n, d) = q.get_parts();
            if d.is_two() {
                return self.sqrt().pow(&n.to_number());
            }
        }
        Ok((expt * self.ln()?).exp())
    }

    // safe complex square for square-and-multiply to avoid cancellation and
    // rounding of floats: z * z as (x-y)(x+y) + i2xy; this is not needed for
    // general Mult but does come up for this specific algorithm.
    fn powsq(&self) -> Number {
        let (x, y) = self.get_parts();
        let re = (x - y) * (x + y);
        // if re = nan that means x+y overflowed and x²-y²=0
        Number::complex(
            if re.is_nan() { 0.0.into() } else { re },
            Real::two() * x * y,
        )
    }

    /*
     * Complex Exponential
     * e^z = e^(x+yi) = e^x·e^yi = (e^x·cos y) + (ie^x·sin y)
     *
     * EXCEPT e^x can overflow if it's too large, so mitigate this with the following:
     * split e^x in two and distribute it across the multiplication, so cos/sin can scale
     * half the exponent down before accounting for the other half. Overflow can still
     * happen but at a much higher threshold.
     */
    fn exp(&self) -> Number {
        let (x, y) = self.get_parts();
        // arbitrary cut, exp overflow hits around 709.78
        if x > &Real::from(700.0) {
            // split in half: e^x = e^(x/2 + x/2) = e^(x/2)·e^(x/2)
            let hx = assume_safe_div!(x / Real::two());
            let exp_hx = hx.exp();
            Number::complex((&exp_hx * y.cos()) * &exp_hx, (&exp_hx * y.sin()) * exp_hx)
        } else {
            let expx = x.exp();
            Number::complex(&expx * y.cos(), expx * y.sin())
        }
    }

    /*
     * Complex Natural Logarithm
     * z = re^(iθ) => ln z = ln r + iθ = ln(mag z) + i(angle z)
     *
     * However when z is near the unit circle (on either side, i.e. r around 1) you get
     * cancellation of ln r that throws the answer off. Start with the formulation:
     *
     * ln|z| = ln(r) = ½ln(r²) = ½ln(1 + (x²+y²−1)) = ½ln_1p(x²+y²−1)
     *
     * which means we need to calculate x²+y²-1 accurately when its natural log will
     * be very small. The solution is to use an alternate calculation called
     * double-double evaluation via TwoSum/TwoProduct (from Knuth/Dekker) which
     * factors out the rounding errors and combines them in the final result.
     * This is the same technique used in FreeBSD's clog implementation.
     */
    fn ln(&self) -> NumResult {
        Ok(Number::complex(self.real_for_ln()?, self.angle()))
    }

    // sin z = sin (x+yi) = (sin x)(cosh y) + i(cos x)(sinh y)
    fn sin(&self) -> Number {
        let (re, im) = cpx_trig(self.get_parts(), Real::sin, Real::cos);
        Number::complex(re, im)
    }

    // cos z = cos (x+yi) = (cos x)(cosh y) - i(sin x)(sinh y)
    fn cos(&self) -> Number {
        let (re, im) = cpx_trig(self.get_parts(), Real::cos, Real::sin);
        Number::complex(re, -im)
    }

    /*
     * Complex Tangent
     * tan z = sin z / cos z = (sin 2x + i(sinh 2y)) / (cos 2x + cosh 2y)
     *
     * However as per-usual there are floating-point rounding errors if done
     * naively. For |2y| ≳ 710 sinh/cosh both overflow, so you end up with ∞/∞ = NaN,
     * but complex tan is supposed to saturate around ±i. Instead use the following
     * approximations:
     *  sinh 2y ≈ sign(y)·½e^(2|y|)
     *  cosh 2y ≈ ½e^(2|y|)
     * Then:
     * let t = e^(−2|y|) which is in (0, 1], so never overflows
     * D = (1−t)² + 4t·cos²x  <= this is 1 + t² + 2t·cos 2x rearranged to
     *                           avoid cancellation with t ≈ 1 and cos 2x ≈ -1
     * real part = 2t * sin 2x / D
     * imag part = sign(y)·(1−t²) / D
     *
     * Optimization: exact real 0 passes through as exact real 0 since sin 0 = 0
     */
    fn tan(&self) -> Number {
        let (x, y) = self.get_parts();
        // an exact y has no impact on this algorithm, so convert to float
        let yf = y.to_float();
        // with u = -2|y| then
        // t = e^u and
        // 1 − t = −(e^u − 1) which avoids rounding when t ≈ 1
        let u = -2.0 * yf.abs();
        let t = u.exp();
        let t_1m = -u.exp_m1();
        let xcos = x.cos().to_float();
        let t2 = 2.0 * t;
        let d = (t_1m * t_1m) + (2.0 * t2 * (xcos * xcos));
        let re = if x.is_exact_zero() {
            x.clone()
        } else {
            let sin2x = (Real::two() * x).sin().to_float();
            ((t2 * sin2x) / d).into()
        };
        // (1-t²) = (1-t)(1+t)
        let im = ((t_1m * (1.0 + t)) / d).copysign(yf);
        Number::complex(re, im)
    }

    /*
     * Complex Inverse Trig Formulations
     * once again the classic mathematical formulations as defined in the R7RS
     * spec aren't useful for avoiding floating-point nonsense.
     *
     * Instead use Kahan's formulations as specified in C99 Annex G based around
     * the shared roots of z: s1 = √(1-z), s2 = √(1+z). In addition we have a
     * large number threshold similar to sqrt where we need an alternate approach
     * to avoid rounding errors (Kahan 1987, "Branch Cuts for Complex Elementary Functions").
     */
    fn asin(&self) -> Number {
        let (x, y) = self.get_parts();
        if x.is_zero() {
            return Number::complex(x.clone(), y.to_float().asinh());
        }
        let (xf, yf) = (x.to_float(), y.to_float());
        let yfa = yf.abs();
        let (re, im) = if yfa > Self::ATRIG_SCALE_T || xf.abs() > Self::ATRIG_SCALE_T {
            let im_mag = atrig_mag_im(xf, yf);
            (xf.atan2(yfa), im_mag.copysign(yf))
        } else {
            let (s1, s2) = cpx_atrig_roots(xf, yf);
            let ((s1r, s1i), (s2r, s2i)) = (s1.to_cpx_floats(), s2.to_cpx_floats());
            (
                xf.atan2((s1 * s2).into_real().to_float()),
                ((s1r * s2i) - (s1i * s2r)).asinh(),
            )
        };
        Number::complex(re, im)
    }

    fn acos(&self) -> Number {
        let (x, y) = self.to_floats();
        if x == 0.0 && y.is_nan() {
            return Number::complex(f64::consts::FRAC_PI_2.copysign(x), y);
        }
        let ya = y.abs();
        let (re, im) = if ya > Self::ATRIG_SCALE_T || x.abs() > Self::ATRIG_SCALE_T {
            let im_mag = atrig_mag_im(x, y);
            (ya.atan2(x), im_mag.copysign(-y))
        } else {
            let (s1, s2) = cpx_atrig_roots(x, y);
            let ((s1r, s1i), (s2r, s2i)) = (s1.to_cpx_floats(), s2.to_cpx_floats());
            (2.0 * s1r.atan2(s2r), ((s2r * s1i) - (s2i * s1r)).asinh())
        };
        Number::complex(re, im)
    }

    fn atan(&self) -> Number {
        let (x, y) = self.get_parts();
        if x.is_nan() && y.is_zero() {
            return self.to_number();
        }
        let yf = y.to_float();
        if x.is_exact_zero() && yf.abs() <= 1.0 {
            return Number::complex(x.clone(), yf.atanh());
        }
        let xf = x.to_float();
        if xf.is_infinite() || yf.is_infinite() {
            Number::complex(
                if xf.is_nan() {
                    xf
                } else {
                    f64::consts::FRAC_PI_2.copysign(xf)
                },
                0.0_f64.copysign(yf),
            )
        } else {
            /*
             * atan z is calculated through atanh,
             * we transform z to the atanh form and then back;
             * atan z = −i · atanh(iz), so rotate z → iz;
             * let z = x+iy, then iz = i(x + iy) = ix − y → (−y, x);
             * now calculate atanh on that which gives some new w = u + vi;
             * rotate back to the original input as:
             * -iw → −i(u + vi) = −ui − vi² = v − ui → (v, −u);
             * at the end of it all atan z = v-ui
             * the too-big threshold is √(asin/acos threshold)
             */
            let (u, v) = cpx_atanh(-yf, xf, Self::ATRIG_SCALE_T.sqrt());
            Number::complex(v, -u)
        }
    }

    fn real_for_ln(&self) -> RealResult {
        Ok(match near_unit_ln_re(self.get_parts()) {
            // magnitude is always positive, so ln(mag z) is always real
            None => (self.magnitude().ln()?).into_real(),
            Some(r) => r,
        })
    }

    fn into_real(self) -> Real {
        self.0.0
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
             * Smith's Algorithm for (a+bi) / (c+di)
             * https://dl.acm.org/doi/abs/10.1145/368637.368661
             *  if |c| < |d|:
             *      r = c / d
             *      denom = cr + d
             *      real = (ar + b) / denom
             *      imag = (br - a) / denom
             *  else:
             *      r = d / c
             *      denom = c + dr
             *      real = (a + br) / denom
             *      imag = (b - ar) / denom
             * The textbook formula for complex division is:
             * (a+bi) / (c+di) = (ac+bd) / (c²+d²) + (bc-ad) / (c²+d²)i
             * which causes issues with IEEE-754 where the squares could overflow to inf
             * even if the final answer would be within range, as well as losing sign-of-zero
             * in the event the numerators sum over opposite-sign zeros.
             * Smith's algorithm fixes this by replacing squares with ratios of the
             * denominator, as well as avoiding addition/subtraction of two products.
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
            Self::Rational(_) => false,
        }
    }

    pub(crate) fn is_exact_zero(&self) -> bool {
        if let Self::Integer(n) = self {
            n.is_zero()
        } else {
            false
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

    pub(crate) fn try_to_integer(&self) -> IntResult {
        match self {
            Self::Float(f) if f.fract() == 0.0 => Ok(Integer::from_exact_float(*f)),
            Self::Integer(n) => Ok(n.clone()),
            _ => Err(NumericError::NotInteger(self.to_string())),
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

    fn to_number(&self) -> Number {
        Number::real(self.clone())
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

    // Number handles negative sign, so this function technically returns the
    // wrong value for negative roots (e.g. √-4 = -2 instead of +2i)
    fn sqrt(&self) -> Self {
        match self {
            Self::Float(0.0) => self.clone(),
            Self::Float(f) => sign_preserving_sqrt(*f).into(),
            Self::Integer(n) => n.sqrt(),
            Self::Rational(q) => q.sqrt(),
        }
    }

    fn pow(&self, expt: &Number) -> NumResult {
        if self.is_exact_one() {
            Ok(Number::one())
        } else {
            match expt {
                Number::Complex(w) => self.powc(w),
                Number::Real(r) => self.powr(r),
            }
        }
    }

    fn powc(&self, expt: &Complex) -> NumResult {
        if self.is_zero() {
            self.powc_zero(expt)
        } else {
            Ok((expt * self.ln()?).exp())
        }
    }

    fn powc_zero(&self, expt: &Complex) -> NumResult {
        let re = expt.get_real();
        if self.is_exact_zero() {
            if expt.is_zero() {
                Ok(Number::onef())
            } else if re.is_zero() || re.is_nan() {
                Err(NumericError::UndefinedAtZero)
            } else if re.is_positive() {
                Ok(Number::zero())
            } else {
                Err(NumericError::DivideByZero)
            }
        } else {
            Ok(if expt.is_zero() {
                Number::complex(1.0, 0.0)
            } else if re.is_positive() {
                Number::zerof()
            } else {
                Number::complex(Real::nan(), Real::nan())
            })
        }
    }

    fn powr(&self, expt: &Real) -> NumResult {
        if self.is_exact_zero() {
            if expt.is_zero() {
                return Ok(Number::onef());
            } else if expt.is_nan() {
                return Ok(Number::nan());
            } else if expt.is_positive() {
                return Ok(Number::zero());
            } else {
                return Err(NumericError::DivideByZero);
            }
        }
        match expt {
            // if b < 0 and e is non-integral => polar(|b|^e, πe),
            // otherwise normal inexact exponential
            Self::Float(f) if self.is_negative() && !expt.is_integer() => Ok(Number::polar(
                self.to_float().abs().powf(*f),
                f64::consts::PI * f,
            )),
            Self::Float(f) => Ok(Number::real(self.to_float().powf(*f))),
            Self::Integer(n) => match self {
                Self::Float(f) => {
                    let r = f.abs().powf(n.to_float());
                    Ok(if f.is_sign_negative() && !n.is_even() {
                        Number::real(-r)
                    } else {
                        Number::real(r)
                    })
                }
                // -1 shortcut: result is 1 with sign set by n's parity
                Self::Integer(b) if b.is_negative_one() => Ok(if n.is_even() {
                    Number::one()
                } else {
                    Number::real(-1)
                }),
                Self::Integer(_) | Self::Rational(_) => square_and_multiply(self.to_number(), n),
            },
            Self::Rational(q) => match self {
                Self::Float(f) if self.is_negative() => {
                    let (n, d) = q.get_parts();
                    if d.is_two() {
                        // TODO: Number::real(*f).sqrt().powr(&n.clone().into())
                        Ok(Number::imaginary(f.abs().sqrt().powf(n.to_float())))
                    } else {
                        let qf = q.to_float();
                        Ok(Number::polar(f.abs().powf(qf), f64::consts::PI * qf))
                    }
                }
                Self::Float(f) => Ok(Number::real(f.powf(q.to_float()))),
                _ => todo!(),
            },
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
            // use signum, so it doesn't differentiate between ± NaN
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

    fn sinh(&self) -> Self {
        if self.is_exact_zero() {
            Self::zero()
        } else {
            self.to_float().sinh().into()
        }
    }

    fn cosh(&self) -> Self {
        if self.is_exact_zero() {
            Self::one()
        } else {
            self.to_float().cosh().into()
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

    // WARNING: only used for reciprocal as it's non-simplified and may act
    // strangely for other complex operations.
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

// Assume Rationals are always reduced (with one exception for cmp), sign is
// always on the numerator, and neither numerator nor denonominator can be zero.
#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct Rational(Box<(Integer, Integer)>);

impl Rational {
    fn is_positive(&self) -> bool {
        self.0.0.is_positive()
    }

    fn is_negative(&self) -> bool {
        self.0.0.is_negative()
    }

    fn signum(&self) -> f64 {
        self.0.0.signum()
    }

    fn to_float(&self) -> f64 {
        let (n, d) = self.get_parts();
        n.to_rat_float(d)
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

    pub(crate) fn truncate_quotrem(&self, rhs: &Self) -> IntPairResult {
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

    pub(crate) fn floor_quotrem(&self, rhs: &Self) -> IntPairResult {
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

    fn is_negative_one(&self) -> bool {
        self.is_negative() && self.is_magnitude_one()
    }

    fn is_two(&self) -> bool {
        self.is_positive() && self.is_magnitude_two()
    }

    fn is_magnitude_one(&self) -> bool {
        self.precision.is_magnitude_one()
    }

    fn is_magnitude_two(&self) -> bool {
        self.precision.is_magnitude_two()
    }

    fn to_float(&self) -> f64 {
        self.precision.to_float().copysign(self.signum())
    }

    fn to_rat_float(&self, d: &Self) -> f64 {
        self.precision
            .to_rat_float(&d.precision)
            .copysign(self.signum())
    }

    fn to_number(&self) -> Number {
        Number::real(self.clone())
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
        self.overflowing_sum(rhs, -self.sign)
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

    fn isqrt_rem(&self) -> IntPairResult {
        if self.is_negative() {
            Err(NumericError::NotNaturalNumber(self.to_string()))
        } else {
            let (s, r) = self.precision.isqrt_rem();
            Ok((Self::new(s, Sign::Positive), Self::new(r, Sign::Positive)))
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

    // WARNING: only used for cmp as it's non-reduced
    // and will act strangely for mathematical operations.
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
    Integer, Real, |_, b| b, convert::identity;
    Integer, &Real, |_, b: &Real| b.clone(), convert::identity;
    &Integer, Real, |_, b| b, Integer::clone;
    &Integer, &Real, |_, b: &Real| b.clone(), Integer::clone;
}
impl_int_real_arithmetic_ring! {
    Sub, sub, -;
    Integer, Real, |_, b: Real| -b, convert::identity;
    Integer, &Real, |_, b: &Real| -b, convert::identity;
    &Integer, Real, |_, b: Real| -b, Integer::clone;
    &Integer, &Real, |_, b: &Real| -b, Integer::clone;
}
impl_int_real_arithmetic_ring! {
    Mul, mul, *;
    Integer, Real, |a, _| a, convert::identity;
    Integer, &Real, |a, _| a, convert::identity;
    &Integer, Real, |a: &Integer, _| a.clone(), Integer::clone;
    &Integer, &Real, |a: &Integer, _| a.clone(), Integer::clone;
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
    NotInteger(String),
    NotNaturalNumber(String),
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
            Self::NotInteger(s) => write!(f, "expected integer, got: {s}"),
            Self::NotNaturalNumber(s) => write!(f, "expected positive exact integer, got: {s}"),
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

// square-and-multiply: compute b^n in O(log₂ n);
// each set bit p in the exponent contributes b^p to the result,
// where p ∈ [1, n]; if n < 0 take the reciprical of b^|n|.
fn square_and_multiply(mut b: Number, n: &Integer) -> NumResult {
    let mut r = Number::one();
    let mut e = n.precision.clone();
    while !e.is_zero() {
        if !e.is_even() {
            r = r * &b;
        }
        e = e >> 1;
        // TODO: remove when MP is implemented as overflow panic goes away
        // SP only overflows on last iteration when b² is not used, so safe to skip
        if !e.is_zero() {
            b = b.powsq();
        }
    }
    if n.is_negative() {
        r.reciprocal()
    } else {
        Ok(r)
    }
}

// helper to keep the sign consistent across √ in order to apply imaginary root later
fn sign_preserving_sqrt(f: f64) -> f64 {
    f.abs().sqrt().copysign(f)
}

// Ultimately from Kahan 1987; Numerical Recipes §5.4, scale the real part
// based on x's sign to avoid r±x canceling for ∓x when x >> y and dominates r.
fn scaled_cpx_sqrt(r: &Real, x: &Real, y: &Real) -> Number {
    let (u, v) = if x.is_negative() {
        let t = scaled_re(r, x, Real::sub);
        (
            assume_safe_div!(y.abs() / &t),
            assume_safe_div!(t / Real::two()).copysign(y),
        )
    } else {
        let t = scaled_re(r, x, Real::add);
        (assume_safe_div!(&t / Real::two()), assume_safe_div!(y / t))
    };
    Number::complex(u, v)
}

// same algorithm as above except optimized for large floats
fn float_scaled_cpx_sqrt(x: f64, y: f64) -> Number {
    // scale-down, scale-up
    const SD: f64 = 16.0;
    const SU: f64 = 4.0;

    let (xs, ys) = (x / SD, y / SD);
    // magnitude is hypotenuse if both args are inexact
    let r = xs.hypot(ys);
    let (u, v) = if xs.is_sign_negative() {
        let t = scaledf_re(r, xs, f64::sub);
        (ys.abs() / t, (t / 2.0).copysign(ys))
    } else {
        let t = scaledf_re(r, xs, f64::add);
        (t / 2.0, ys / t)
    };
    Number::complex(u * SU, v * SU)
}

// helper to calculate the intermediate real value for complex sqrt
fn scaled_re(r: &Real, x: &Real, op: impl FnOnce(&Real, &Real) -> Real) -> Real {
    let t = (Real::two() * op(r, x)).sqrt();
    debug_assert!(!t.is_zero());
    t
}

fn scaledf_re(r: f64, x: f64, op: impl FnOnce(f64, f64) -> f64) -> f64 {
    (2.0 * op(r, x)).sqrt()
}

// calculate the real part of ln if z is near the unit circle, otherwise None
fn near_unit_ln_re((x, y): (&Real, &Real)) -> Option<Real> {
    // if x is zero the naive calculation will work regardless of y
    if x.is_zero() {
        return None;
    }
    let total = if x.is_inexact() || y.is_inexact() {
        dd_2sum_2product(x.to_float(), y.to_float())
    } else {
        // If x and y are exact keep everything exact until we need to take ln_p;
        // calculate x²+y²-1 directly
        let t = (x * x) + (y * y) - Real::one();
        // exact zero magnitude -> exact zero real part
        if t.is_exact_zero() {
            return Some(t);
        }
        Some(t.to_float())
    };
    total.map(|t| (t.ln_1p() / 2.0).into())
}

/*
 * For large |z|, asin z ≈ −i·ln(2iz), this calculates the ln(2|z|) factor of that
 * approximation, with halving and then scaling by 4 (2·ln2) to avoid overflow;
 * this is the asymptotic magnitude of the imaginary part of asin/acos, defined
 * in the formulation as:
 * L = ln(hypot(x/2, y/2)) + 2·ln 2
 */
fn atrig_mag_im(x: f64, y: f64) -> f64 {
    (x / 2.0).hypot(y / 2.0).ln() + (2.0 * 2.0_f64.ln())
}

// double-double TwoSum/TwoProduct
// https://en.wikipedia.org/wiki/2Sum
// https://en.wikipedia.org/wiki/Quadruple-precision_floating-point_format#Double-double_arithmetic
// also related to: https://en.wikipedia.org/wiki/Kahan_summation_algorithm
// https://en.wikipedia.org/wiki/William_Kahan
fn dd_2sum_2product(x: f64, y: f64) -> Option<f64> {
    // calculate s = x²+y²
    let p = x * x;
    let q = y * y;
    let s = p + q;
    // 0.5 <= s <= 2.0 is considered near the unit circle (r in about [0.71, 1.41])
    if s < 0.5 || s > 2.0 {
        return None;
    }
    // error of x² and y²
    let e = x.mul_add(x, -p);
    let f = y.mul_add(y, -q);
    // how much of q made it into s after rounding errors
    let bb = s - p;
    // (error from p) + (error from q) => now p+q = s+t exactly
    let t = (p - (s - bb)) + (q - bb);
    // add in the small error factors and the -1,
    // order matters here to avoid blowing up intermediate results
    // => now we have x²+y²-1 with rounding errors handled
    Some(t + e + f + (s - 1.0))
}

/*
 * More floating-point woes: if sinh/cosh y overflow you lose precision on
 * both of these trig functions. So if they do overflow use the approximation:
 * sinh/cosh y ≈ ½e^|y|
 * to keep this approximation managable:
 * let h = e^(|y|/2) <=> e^|y| = h·h (cuts magnitude in half to reduce overflow)
 * trig·½e^|y| = trig·½(h·h) = (trig·½h)·h
 */
fn scale_hyperbolic_ovf(mut trig: Real, y: f64, odd: bool) -> Real {
    // sinh is odd, so the sign of y propagates through the result (including zero)
    if odd && y.is_sign_negative() {
        trig = -trig;
    }
    // handle zero as a special case to avoid NaN and loss of sign
    if trig.is_zero() {
        trig
    } else {
        let h = (y.abs() / 2.0).exp();
        // order matters here to calculate from smallest-to-largest values
        (((h / 2.0) * trig.to_float()) * h).into()
    }
}

fn cpx_product(a: &Real, b: &Real, c: &Real, d: &Real) -> Number {
    Number::complex((a * c) - (b * d), (a * d) + (b * c))
}

fn cpx_trig(
    (x, y): (&Real, &Real),
    re_trig: impl FnOnce(&Real) -> Real,
    im_trig: impl FnOnce(&Real) -> Real,
) -> (Real, Real) {
    let (sinhy, coshy) = (y.sinh(), y.cosh());
    let re = if coshy.is_infinite() {
        scale_hyperbolic_ovf(re_trig(x), y.to_float(), false)
    } else {
        re_trig(x) * coshy
    };
    let im = if sinhy.is_infinite() {
        scale_hyperbolic_ovf(im_trig(x), y.to_float(), true)
    } else {
        im_trig(x) * sinhy
    };
    (re, im)
}

// calculate √(1-z), √(1+z) as √(1-x, -y), √(1+x, y)
fn cpx_atrig_roots(x: f64, y: f64) -> (Number, Number) {
    let (z1, z2) = (Number::complex(1.0 - x, -y), Number::complex(1.0 + x, y));
    (z1.sqrt(), z2.sqrt())
}

// The atanh core of complex atan, due to the rotation of z = x+iy it takes -y, x;
// denoted here flip-y and x.
fn cpx_atanh(fy: f64, x: f64, scale_th: f64) -> (f64, f64) {
    // if flip-y is negative, calculate mirror atanh to avoid cancellation of ln_1p
    if fy.is_sign_negative() {
        let (re, im) = cpx_atanh(-fy, -x, scale_th);
        return (-re, -im);
    }
    let (fya, xa) = (fy.abs(), x.abs());
    if fya > scale_th || xa > scale_th {
        let h = (fy / 2.0).hypot(x / 2.0);
        let re = ((fy / 4.0) / h) / h;
        let im = f64::consts::FRAC_PI_2.copysign(x);
        (re, im)
    } else if fy == 1.0 && xa != 0.0 && xa < f64::MIN_POSITIVE.sqrt() {
        // Another special case near the complex pole, except x=0 is a singularity,
        // so exclude that special case from this special case.
        let re = (2.0_f64.ln() - xa.ln()) / 2.0;
        let im = (2.0_f64.atan2(-xa) / 2.0).copysign(x);
        (re, im)
    } else {
        let xsq = x * x;
        let one_minus_fy = 1.0 - fy;
        let re = ((4.0 * fy) / ((one_minus_fy * one_minus_fy) + xsq)).ln_1p() / 4.0;
        let im = (2.0 * x).atan2((one_minus_fy * (1.0 + fy)) - xsq) / 2.0;
        (re, im)
    }
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
        // both ± zero converts to integer zero
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
        (
            mantissa_bits.cast_signed() | (1 << MANTISSA_SIZE),
            exp_bits - EXP_BIAS - MANTISSA_SIZE.cast_signed(),
        )
    };

    if exp < 0 {
        // negative exponent: sign * mantissa * 2^exponent = (sign * mantissa) / 2^-exponent
        let numerator = sign * mantissa;
        let denom = 2_i64.pow((-exp).cast_unsigned());
        Real::reduce(numerator, denom)
    } else {
        // positive exponent: sign * mantissa * 2^exponent
        Ok((sign * mantissa * 2_i64.pow(exp.cast_unsigned())).into())
    }
}

fn write_intconversion_range_error(
    min: impl Display,
    max: impl Display,
    f: &mut Formatter,
) -> fmt::Result {
    write!(f, "integer literal out of range: [{min}, {max}]")
}
