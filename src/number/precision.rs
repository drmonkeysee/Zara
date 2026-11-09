use super::{EXP_BIAS, FMAX_INT, MANTISSA_SIZE, NumericError, Radix, RadixResult};
use std::{
    borrow::Borrow,
    cmp::Ordering,
    fmt::{self, Binary, Display, Formatter, LowerHex, Octal},
    ops::{Add, Div, Mul, Rem, Shr, Sub},
    rc::Rc,
};

#[cfg(test)]
mod tests;

#[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
const MAX_FLT_INT: u64 = FMAX_INT as u64;

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub(super) enum Precision {
    Single(u64),
    #[allow(dead_code, reason = "not yet implemented")]
    Multiple(Rc<[u64]>),
}

impl Precision {
    pub(super) fn is_zero(&self) -> bool {
        match self {
            Self::Single(u) => *u == 0,
            Self::Multiple(_) => false,
        }
    }

    pub(super) fn is_even(&self) -> bool {
        match self {
            Self::Single(u) => u % 2 == 0,
            Self::Multiple(_) => todo!("handle mp"),
        }
    }

    pub(super) fn is_magnitude_one(&self) -> bool {
        match self {
            Self::Single(u) => *u == 1,
            Self::Multiple(_) => false,
        }
    }

    pub(super) fn is_magnitude_two(&self) -> bool {
        match self {
            Self::Single(u) => *u == 2,
            Self::Multiple(_) => false,
        }
    }

    pub(super) fn to_float(&self) -> f64 {
        match self {
            #[allow(clippy::cast_precision_loss)]
            Self::Single(u) => *u as f64,
            Self::Multiple(_) => todo!("handle mp"),
        }
    }

    pub(super) fn to_rat_float(&self, denom: &Self) -> f64 {
        debug_assert!(!denom.is_zero());

        // if n,d are both ≤ 2^53, then direct conversion is exact
        if self.in_float_range() && denom.in_float_range() {
            self.to_float() / denom.to_float()
        } else {
            // otherwise scale and divide exactly, round, then rescale
            match (self, denom) {
                (Self::Single(n), Self::Single(d)) => u64_float_div(*n, *d),
                _ => todo!("handle sp+mp"),
            }
        }
    }

    pub(super) fn try_to_radix(&self) -> RadixResult {
        match self {
            Self::Single(u) => match u {
                2 => Ok(Radix::Binary),
                8 => Ok(Radix::Octal),
                10 => Ok(Radix::Decimal),
                16 => Ok(Radix::Hexadecimal),
                _ => Err(NumericError::RadixInvalid),
            },
            Self::Multiple(_) => Err(NumericError::RadixInvalid),
        }
    }

    pub(super) fn gcd(&self, rhs: &Self) -> Self {
        match (self, rhs) {
            (Self::Single(a), Self::Single(b)) => gcd_euclidean(*a, *b).into(),
            _ => todo!("handle sp+mp"),
        }
    }

    pub(super) fn lcm(&self, rhs: &Self) -> Self {
        match (self, rhs) {
            (Self::Single(a), Self::Single(b)) => {
                let gcd = gcd_euclidean(*a, *b);
                let (p, o) = b.carrying_mul(a / gcd, 0);
                if o == 0 {
                    p.into()
                } else {
                    todo!("handle precision overflow")
                }
            }
            _ => todo!("handle sp+mp"),
        }
    }

    // return sqrt of self if self is a perfect square
    pub(super) fn isqrt(&self) -> Option<Self> {
        match self {
            Self::Single(_) => {
                let (s, r) = self.isqrt_rem();
                if r.is_zero() { Some(s) } else { None }
            }
            Self::Multiple(_) => todo!("handle mp"),
        }
    }

    pub(super) fn isqrt_rem(&self) -> (Self, Self) {
        match self {
            Self::Single(u) => {
                let s = u.isqrt();
                let r = u - (s * s);
                (s.into(), r.into())
            }
            Self::Multiple(_) => todo!("handle mp"),
        }
    }

    // return root r if r^n = self, meaning self is a perfect nth power;
    // assumes n > 0 and does not handle the undefined 0th root case.
    pub(super) fn nth_root(&self, n: &Self) -> Option<Self> {
        match (self, n) {
            (Self::Single(u), Self::Single(q)) => {
                debug_assert_ne!(*q, 0);
                if *q == 1 || *u < 2 {
                    // 0 and 1 are perfect roots of any n
                    // and any u is a perfect power of 1.
                    return Some(self.clone());
                }
                let bitlen = u64::BITS - u.leading_zeros();
                // if q ≥ bitlen(u) then only 0 and 1 can be roots;
                // r ≥ 2 only exists if u ≥ 2^q, i.e. u has > q bits
                if u64::from(bitlen) > *q {
                    debug_assert!(*q < u64::BITS.into());
                    #[allow(
                        clippy::cast_possible_truncation,
                        reason = "q must be < 64; downcast safe"
                    )]
                    let q = *q as u32;
                    Some(root_bsearch(0, 2 << bitlen.div_ceil(q), q, *u)?.into())
                } else {
                    None
                }
            }
            _ => todo!("handle sp+mp"),
        }
    }

    pub(super) fn div_ceil(&self, rhs: &Self) -> Self {
        match (self, rhs) {
            (Self::Single(a), Self::Single(b)) => a.div_ceil(*b).into(),
            _ => todo!("handle sp+mp"),
        }
    }

    pub(super) fn div_round(&self, rhs: &Self) -> Self {
        match (self, rhs) {
            (Self::Single(a), Self::Single(b)) => {
                /*
                 * For rational n/d:
                 *   q = floor(n/d)
                 *   r = remainder (n % d)
                 *   compare 2r to d:
                 *     2r < d  →  q
                 *     2r > d  →  q + 1
                 *     2r == d →  q is even → q
                 *                else      → q + 1
                 */
                let q = a / b;
                let r = a % b;
                let r2 = 2 * r;
                match r2.cmp(b) {
                    Ordering::Equal if q % 2 == 0 => q.into(),
                    Ordering::Less => q.into(),
                    _ => (q + 1).into(),
                }
            }
            _ => todo!("handle sp+mp"),
        }
    }

    // convenience wrappers for passing Op impls as closures
    pub(super) fn div(&self, rhs: &Self) -> Self {
        self / rhs
    }

    pub(super) fn reduce(&mut self, other: &mut Self) {
        match (&self, &other) {
            (Self::Single(a), Self::Single(b)) => {
                let gcd = gcd_euclidean(*a, *b);
                *self = (*a / gcd).into();
                *other = (*b / gcd).into();
            }
            _ => todo!("handle sp+mp"),
        }
    }

    fn in_float_range(&self) -> bool {
        match self {
            Self::Single(u) => *u <= MAX_FLT_INT,
            Self::Multiple(_) => false,
        }
    }
}

impl<Rhs: Borrow<Precision>> Add<Rhs> for &Precision {
    type Output = Precision;

    fn add(self, rhs: Rhs) -> Self::Output {
        match (self, rhs.borrow()) {
            (Precision::Single(a), Precision::Single(b)) => {
                let (s, c) = a.overflowing_add(*b);
                if c {
                    todo!("handle precision overflow")
                } else {
                    s.into()
                }
            }
            _ => todo!("handle sp+mp"),
        }
    }
}
impl_val_delegate!(Add, Precision);

// Naive sub implementation, relying on Integer to avoid subtraction overflow
impl<Rhs: Borrow<Precision>> Sub<Rhs> for &Precision {
    type Output = Precision;

    fn sub(self, rhs: Rhs) -> Self::Output {
        match (self, rhs.borrow()) {
            (Precision::Single(a), Precision::Single(b)) => (a - b).into(),
            _ => todo!("handle sp+mp"),
        }
    }
}
impl_val_delegate!(Sub, Precision);

impl<Rhs: Borrow<Precision>> Mul<Rhs> for &Precision {
    type Output = Precision;

    fn mul(self, rhs: Rhs) -> Self::Output {
        match (self, rhs.borrow()) {
            (Precision::Single(a), Precision::Single(b)) => {
                let (p, o) = a.carrying_mul(*b, 0);
                if o == 0 {
                    p.into()
                } else {
                    todo!("handle precision overflow")
                }
            }
            _ => todo!("handle sp+mp"),
        }
    }
}
impl_val_delegate!(Mul, Precision);

// Integer division (e.g. div_floor); caller ensures divisor is not zero
impl<Rhs: Borrow<Precision>> Div<Rhs> for &Precision {
    type Output = Precision;

    fn div(self, rhs: Rhs) -> Self::Output {
        match (self, rhs.borrow()) {
            (Precision::Single(a), Precision::Single(b)) => {
                debug_assert_ne!(*b, 0);
                (a / b).into()
            }
            _ => todo!("handle sp+mp"),
        }
    }
}
impl_val_delegate!(Div, Precision);

// Unsigned remainder or modulo; caller ensures the modulus is not zero
impl<Rhs: Borrow<Precision>> Rem<Rhs> for &Precision {
    type Output = Precision;

    fn rem(self, rhs: Rhs) -> Self::Output {
        match (self, rhs.borrow()) {
            (Precision::Single(a), Precision::Single(b)) => {
                debug_assert_ne!(*b, 0);
                (a % b).into()
            }
            _ => todo!("handle sp+mp"),
        }
    }
}
impl_val_delegate!(Rem, Precision);

impl Shr<u64> for Precision {
    type Output = Self;

    fn shr(self, rhs: u64) -> Self::Output {
        match self {
            Self::Single(u) => (u >> rhs).into(),
            Self::Multiple(_) => todo!("handle mp"),
        }
    }
}

macro_rules! display_impl {
    ($($trait:path, $fmt:expr);+ $(;)?) => {
        $(impl $trait for Precision {
            fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
                match self {
                    // avoid direct impl::fmt for u64 to suppress sign
                    Self::Single(u) => write!(f, $fmt, u),
                    Self::Multiple(_) => todo!("handle mp"),
                }
            }
        })+
    };
}
display_impl! {
    Display, "{}";
    Binary, "{:b}";
    Octal, "{:o}";
    LowerHex, "{:x}";
}

impl From<u64> for Precision {
    fn from(value: u64) -> Self {
        Self::Single(value)
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

/*
 * Scale both u64s up to u128 in order to produce a quotient of at least 55 bits;
 * the specific identity is: n/d = (n·2^s / d) · 2^-s;
 * the remainder "sticky bit" is used to resolve ties when converting the quotient
 * to f64, since the sticky bit is below the round bit (because q-width > 53), the
 * rounding breaks in the correct direction based on what was lost in the division.
 */
#[allow(
    clippy::many_single_char_names,
    clippy::cast_precision_loss,
    clippy::cast_sign_loss,
    reason = "algorithm ensures safe casts"
)]
fn u64_float_div(n: u64, d: u64) -> f64 {
    // width of the quotient = mantissa-width + rounding bit + sticky (remainder) bit
    const QBITS: i32 = (f64::MANTISSA_DIGITS + 2).cast_signed();

    // ratio of the scale of n,d:
    // n ∈ [2^(bn-1), 2^bn), d ∈ [2^(bd-1), 2^bd) => n/d ∈ (2^(bn-bd-1), 2^(bn-bd+1))
    let (bn, bd) = (
        (u64::BITS - n.leading_zeros()).cast_signed(),
        (u64::BITS - d.leading_zeros()).cast_signed(),
    );
    let s = QBITS + bd - bn;
    let (ns, ds) = if s < 0 {
        (u128::from(n), u128::from(d) << -s)
    } else {
        (u128::from(n) << s, u128::from(d))
    };

    // divide, get remainder, and combine to quotient + sticky bit
    let (q, r) = (ns / ds, ns % ds);
    let qs = q | u128::from(r != 0);

    // round to float, then scale back up with the operand ratio:
    // result = f * 2^-s where -s ∈ [-118, 8] and the result is in [2^-64, 2^64]
    // build the scale factor 2^-s exactly using bits rather than mathematical
    // operators to keep the power of 2 exact.
    let fs = f64::from_bits(((EXP_BIAS - s) as u64) << MANTISSA_SIZE);
    (qs as f64) * fs
}

// binary search for root r ∈ (0, 2^max], where r^n = needle; None if not found
fn root_bsearch(mut min: u64, mut max: u64, n: u32, needle: u64) -> Option<u64> {
    while min <= max {
        let mid = min + ((max - min) / 2);
        match mid
            .checked_pow(n)
            .map_or(Ordering::Greater, |r| r.cmp(&needle))
        {
            Ordering::Equal => return Some(mid),
            Ordering::Greater => max = mid - 1,
            Ordering::Less => min = mid + 1,
        }
    }
    None
}
