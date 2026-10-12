use super::FMAX_INT;
use std::{
    borrow::Borrow,
    cmp::Ordering,
    fmt::{self, Display, Formatter},
    ops::{Add, Div, Mul, Rem, Sub},
    rc::Rc,
};

#[cfg(test)]
mod tests;

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
            Self::Multiple(_) => todo!(),
        }
    }

    pub(super) fn is_magnitude_one(&self) -> bool {
        match self {
            Self::Single(u) => *u == 1,
            Self::Multiple(_) => false,
        }
    }

    pub(super) fn to_float(&self) -> f64 {
        match self {
            #[allow(clippy::cast_precision_loss)]
            Self::Single(u) => *u as f64,
            Self::Multiple(_) => todo!(),
        }
    }

    pub(super) fn to_rat_float(&self, d: &Self) -> f64 {
        if self.in_float_range() && d.in_float_range() {
            self.to_float() / d.to_float()
        } else {
            todo!();
        }
    }

    pub(super) fn gcd(&self, rhs: &Self) -> Self {
        match (self, rhs) {
            (Self::Single(a), Self::Single(b)) => gcd_euclidean(*a, *b).into(),
            _ => todo!(),
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
            _ => todo!(),
        }
    }

    // return sqrt of self if self is a perfect square
    pub(super) fn isqrt(&self) -> Option<Self> {
        match self {
            Self::Single(_) => {
                let (s, r) = self.isqrt_rem();
                if r.is_zero() { Some(s) } else { None }
            }
            Self::Multiple(_) => todo!(),
        }
    }

    pub(super) fn isqrt_rem(&self) -> (Self, Self) {
        match self {
            Self::Single(u) => {
                let s = u.isqrt();
                let r = u - (s * s);
                (s.into(), r.into())
            }
            Self::Multiple(_) => todo!(),
        }
    }

    pub(super) fn div_ceil(&self, rhs: &Self) -> Self {
        match (self, rhs) {
            (Self::Single(a), Self::Single(b)) => a.div_ceil(*b).into(),
            _ => todo!(),
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
            _ => todo!(),
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
            _ => todo!(),
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
            _ => todo!(),
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
            _ => todo!(),
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
            _ => todo!(),
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
            _ => todo!(),
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
            _ => todo!(),
        }
    }
}
impl_val_delegate!(Rem, Precision);

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

// https://en.wikipedia.org/wiki/Euclidean_algorithm
fn gcd_euclidean(mut a: u64, mut b: u64) -> u64 {
    while b != 0 {
        let t = b;
        b = a % b;
        a = t;
    }
    a
}
