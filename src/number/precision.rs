use std::{
    borrow::Borrow,
    cmp::Ordering,
    fmt::{self, Display, Formatter},
    ops::{Add, Div, Mul, Rem, Sub},
    rc::Rc,
};

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

    pub(super) fn isqrt(&self) -> Option<Self> {
        match self {
            Self::Single(u) => {
                let r = u.isqrt();
                if r.pow(2) == *u { Some(r.into()) } else { None }
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

#[cfg(test)]
mod tests {
    use super::*;

    mod div {
        use super::*;

        #[test]
        fn exact_division() {
            let q = Precision::Single(12) / Precision::Single(4);

            assert_eq!(q, Precision::Single(3));
        }

        #[test]
        fn truncates_toward_zero_on_remainder() {
            let cases = [(67, 6, 11), (7, 2, 3), (1, 3, 0), (2, 3, 0)];
            for (a, b, expected) in cases {
                let q = Precision::Single(a) / Precision::Single(b);

                assert_eq!(q, Precision::Single(expected));
            }
        }

        #[test]
        fn divisor_larger_than_dividend_is_zero() {
            let q = Precision::Single(3) / Precision::Single(7);

            assert_eq!(q, Precision::Single(0));
        }

        #[test]
        fn equal_operands_is_one() {
            let q = Precision::Single(7) / Precision::Single(7);

            assert_eq!(q, Precision::Single(1));
        }

        #[test]
        fn unit_divisor_is_identity() {
            let q = Precision::Single(9) / Precision::Single(1);

            assert_eq!(q, Precision::Single(9));
        }

        #[test]
        fn zero_dividend_is_zero() {
            let q = Precision::Single(0) / Precision::Single(5);

            assert_eq!(q, Precision::Single(0));
        }

        #[test]
        fn large_magnitude_divides_exactly() {
            let q = Precision::Single(u64::MAX) / Precision::Single(3);

            assert_eq!(q, Precision::Single(u64::MAX / 3));
        }

        #[test]
        #[ignore = "multi-precision division not yet implemented"]
        fn multi_precision() {
            let a = Precision::Multiple([4, 6].into());
            let b = Precision::Single(4);

            let q = a / b;

            assert_eq!(q, Precision::Single(4));
        }
    }

    mod rem {
        use super::*;
        use crate::testutil::extract_or_fail;

        #[test]
        fn nonzero_remainder() {
            let r = Precision::Single(67) % Precision::Single(6);

            assert_eq!(r, Precision::Single(1));
        }

        #[test]
        fn exact_division_leaves_zero() {
            let r = Precision::Single(12) % Precision::Single(4);

            assert_eq!(r, Precision::Single(0));
        }

        #[test]
        fn divisor_larger_than_dividend_returns_dividend() {
            let r = Precision::Single(3) % Precision::Single(7);

            assert_eq!(r, Precision::Single(3));
        }

        #[test]
        fn unit_divisor_is_always_zero() {
            let r = Precision::Single(9) % Precision::Single(1);

            assert_eq!(r, Precision::Single(0));
        }

        #[test]
        fn zero_dividend_is_zero() {
            let r = Precision::Single(0) % Precision::Single(5);

            assert_eq!(r, Precision::Single(0));
        }

        #[test]
        fn remainder_is_always_less_than_divisor() {
            let cases = [(67, 6), (7, 2), (1, 3), (2, 3), (100, 7), (u64::MAX, 3)];
            for (a, b) in cases {
                let r = Precision::Single(a) % Precision::Single(b);

                let r = extract_or_fail!(r, Precision::Single);
                assert!(r < b, "{a} % {b} = {r} should be < {b}");
            }
        }

        #[test]
        #[ignore = "multi-precision division not yet implemented"]
        fn multi_precision() {
            let a = Precision::Multiple([4, 6].into());
            let b = Precision::Single(4);

            let r = a % b;

            assert_eq!(r, Precision::Single(0));
        }
    }

    mod div_ceil {
        use super::*;

        #[test]
        fn rounds_up_on_remainder() {
            let q = Precision::Single(67).div_ceil(&Precision::Single(6));

            assert_eq!(q, Precision::Single(12));
        }

        #[test]
        fn exact_division_does_not_round_up() {
            let q = Precision::Single(12).div_ceil(&Precision::Single(4));

            assert_eq!(q, Precision::Single(3));
        }

        #[test]
        fn dividend_smaller_than_divisor_rounds_up_to_one() {
            let q = Precision::Single(1).div_ceil(&Precision::Single(7));

            assert_eq!(q, Precision::Single(1));
        }

        #[test]
        fn zero_dividend_is_zero() {
            let q = Precision::Single(0).div_ceil(&Precision::Single(5));

            assert_eq!(q, Precision::Single(0));
        }

        #[test]
        fn agrees_with_div_plus_one_exactly_when_there_is_a_remainder() {
            let cases = [(67, 6), (7, 2), (1, 3), (2, 3), (12, 4), (9, 3)];
            for (a, b) in cases {
                let div = Precision::Single(a) / Precision::Single(b);
                let rem = Precision::Single(a) % Precision::Single(b);
                let div_ceil = Precision::Single(a).div_ceil(&Precision::Single(b));

                let expected = if rem == Precision::Single(0) {
                    div
                } else {
                    div + Precision::Single(1)
                };
                assert_eq!(div_ceil, expected);
            }
        }

        #[test]
        #[ignore = "multi-precision division not yet implemented"]
        fn multi_precision() {
            let a = Precision::Multiple([4, 6].into());
            let b = Precision::Single(4);

            let q = a.div_ceil(&b);

            assert_eq!(q, Precision::Single(2));
        }
    }

    #[test]
    fn division_identity_holds() {
        // a == (a / b) * b + (a % b)
        let cases = [
            (67, 6),
            (7, 2),
            (1, 3),
            (2, 3),
            (100, 7),
            (9, 3),
            (u64::MAX, 3),
        ];
        for (a, b) in cases {
            let div = Precision::Single(a) / Precision::Single(b);
            let rem = Precision::Single(a) % Precision::Single(b);

            let reconstructed = div * Precision::Single(b) + rem;

            assert_eq!(reconstructed, Precision::Single(a));
        }
    }
}
