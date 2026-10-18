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

// shared by the isqrt_rem and nth_root suites
fn sample_inputs() -> Vec<u64> {
    let mut ks: Vec<u64> = (0..=1000).collect();
    let roots = [
        2,
        3,
        10,
        1 << 16,
        (1 << 26) + 1,
        1 << 31,
        u64::from(u32::MAX) - 1,
        u64::from(u32::MAX),
    ];
    for s in roots {
        ks.extend([s * s - 1, s * s, s * s + 1]);
    }
    ks.push(u64::MAX);
    ks
}

// R7RS exact-integer-sqrt: for natural k returns s, r where
// k = s^2 + r and k < (s+1)^2, i.e. s = floor(sqrt(k)) and 0 <= r <= 2s.
mod isqrt_rem {
    use super::*;
    use crate::testutil::extract_or_fail;

    const MAX_SQUARE: u64 = (u32::MAX as u64) * (u32::MAX as u64);

    fn check(k: u64, s: u64, r: u64) {
        let (rs, rr) = Precision::Single(k).isqrt_rem();

        assert_eq!(rs, Precision::Single(s), "root of {k}");
        assert_eq!(rr, Precision::Single(r), "remainder of {k}");
    }

    #[test]
    fn zero() {
        check(0, 0, 0);
    }

    #[test]
    fn one() {
        check(1, 1, 0);
    }

    #[test]
    fn small_non_squares() {
        check(2, 1, 1);
        check(3, 1, 2);
    }

    #[test]
    fn r7rs_examples() {
        check(4, 2, 0);
        check(5, 2, 1);
    }

    #[test]
    fn remainder_is_maximal_just_below_next_square() {
        check(8, 2, 4);
        check(15, 3, 6);
    }

    #[test]
    fn perfect_squares_have_zero_remainder() {
        check(9, 3, 0);
        check(144, 12, 0);
        check(1_000_000_000_000_000_000, 1_000_000_000, 0);
    }

    #[test]
    fn largest_perfect_square() {
        check(MAX_SQUARE, u64::from(u32::MAX), 0);
    }

    #[test]
    fn one_below_largest_perfect_square() {
        let s = u64::from(u32::MAX) - 1;

        check(MAX_SQUARE - 1, s, 2 * s);
    }

    #[test]
    fn max_u64() {
        let s = u64::from(u32::MAX);

        check(u64::MAX, s, 2 * s);
    }

    #[test]
    fn satisfies_definition() {
        for k in sample_inputs() {
            let (s, r) = Precision::Single(k).isqrt_rem();

            let s = u128::from(extract_or_fail!(s, Precision::Single));
            let r = u128::from(extract_or_fail!(r, Precision::Single));
            assert_eq!(s * s + r, u128::from(k), "k = s^2 + r for {k}");
            assert!(u128::from(k) < (s + 1) * (s + 1), "k < (s+1)^2 for {k}");
            assert!(r <= 2 * s, "r <= 2s for {k}");
        }
    }

    #[test]
    fn zero_remainder_exactly_when_perfect_square() {
        for k in sample_inputs() {
            let p = Precision::Single(k);

            let (s, r) = p.isqrt_rem();

            assert_eq!(
                r.is_zero(),
                p.isqrt().is_some(),
                "perfect square test for {k}"
            );
            if r.is_zero() {
                assert_eq!(p.isqrt(), Some(s));
            }
        }
    }

    #[test]
    #[ignore = "multi-precision sqrt not yet implemented"]
    fn multi_precision() {
        // 2^64, assuming little-endian limbs
        let p = Precision::Multiple([0, 1].into());

        let (s, r) = p.isqrt_rem();

        assert_eq!(s, Precision::Single(1 << 32));
        assert_eq!(r, Precision::Single(0));
    }
}

// nth_root(x, q) is Some(r) exactly when r^q == x, i.e. x is a perfect q-th power
mod nth_root {
    use super::*;

    fn check(x: u64, q: u64, expected: Option<u64>) {
        let r = Precision::Single(x).nth_root(&Precision::Single(q));

        assert_eq!(r, expected.map(Precision::Single), "root {q} of {x}");
    }

    #[test]
    fn zero_and_one_are_roots_for_any_q() {
        for x in [0, 1] {
            for q in [1, 2, 3, 63, 64, 1000, u64::MAX] {
                check(x, q, Some(x));
            }
        }
    }

    #[test]
    fn small_perfect_powers() {
        let cases = [
            (4, 2, 2),
            (8, 3, 2),
            (27, 3, 3),
            (16, 4, 2),
            (81, 4, 3),
            (32, 5, 2),
            (1024, 10, 2),
            (1000, 3, 10),
        ];
        for (x, q, r) in cases {
            check(x, q, Some(r));
        }
    }

    #[test]
    fn small_non_powers() {
        let cases = [
            (2, 2),
            (3, 2),
            (8, 2),
            (7, 3),
            (9, 3),
            (26, 3),
            (28, 3),
            (15, 4),
            (17, 4),
        ];
        for (x, q) in cases {
            check(x, q, None);
        }
    }

    // every number is a perfect first power
    #[test]
    fn first_root_returns_self() {
        for x in [2, 12345, 1 << 63, u64::MAX] {
            check(x, 1, Some(x));
        }
    }

    // a root r >= 2 needs x >= 2^q, i.e. more than q bits
    #[test]
    fn only_trivial_roots_when_q_at_least_bitlen() {
        for (x, q) in [
            ((1 << 63) - 1, 63),
            (1 << 63, 64),
            (u64::MAX, 64),
            (7, 3),
            (3, 2),
        ] {
            check(x, q, None);
        }
        // one bit past the boundary
        check(1 << 63, 63, Some(2));
        check(8, 3, Some(2));
    }

    #[test]
    fn powers_of_ten() {
        let x = 1_000_000_000_000_000_000;
        let cases = [
            (2, Some(1_000_000_000)),
            (3, Some(1_000_000)),
            (6, Some(1000)),
            (9, Some(100)),
            (18, Some(10)),
            (4, None),
            (5, None),
        ];
        for (q, expected) in cases {
            check(x, q, expected);
        }
    }

    #[test]
    fn largest_perfect_powers_in_u64() {
        check(18_446_744_065_119_617_025, 2, Some(4_294_967_295));
        check(18_446_724_184_312_856_125, 3, Some(2_642_245));
        check(18_445_618_199_572_250_625, 4, Some(65535));
        check(18_439_629_140_666_724_651, 5, Some(7131));
        for q in [2, 3, 4] {
            check(u64::MAX, q, None);
        }
    }

    // 3^40 and its neighbours are indistinguishable once rounded to f64, so a
    // float estimate alone would call the neighbours perfect powers
    #[test]
    fn near_misses_of_large_powers() {
        let x = 12_157_665_459_056_928_801;

        check(x, 40, Some(3));
        check(x, 20, Some(9));
        check(x, 8, Some(243));
        check(x - 1, 40, None);
        check(x + 1, 40, None);
    }

    // consecutive q-th powers of r >= 2 are more than 2 apart, so the
    // neighbours of a perfect power are never perfect q-th powers themselves
    #[test]
    fn exhaustive_perfect_powers_and_neighbours() {
        for q in 2..=63_u32 {
            for (r, x) in (2..=2000_u64).map_while(|r| r.checked_pow(q).map(|x| (r, x))) {
                check(x, u64::from(q), Some(r));
                check(x - 1, u64::from(q), None);
                if let Some(next) = x.checked_add(1) {
                    check(next, u64::from(q), None);
                }
            }
        }
    }

    #[test]
    fn agrees_with_isqrt() {
        for k in sample_inputs() {
            let p = Precision::Single(k);

            assert_eq!(p.nth_root(&Precision::Single(2)), p.isqrt(), "sqrt of {k}");
        }
    }

    #[test]
    #[ignore = "multi-precision nth_root not yet implemented"]
    fn multi_precision() {
        // 2^64, assuming little-endian limbs
        let p = Precision::Multiple([0, 1].into());

        assert_eq!(
            p.nth_root(&Precision::Single(64)),
            Some(Precision::Single(2))
        );
        assert_eq!(
            p.nth_root(&Precision::Single(2)),
            Some(Precision::Single(1 << 32))
        );
    }
}

mod euclid {
    use super::*;

    #[test]
    fn zeros() {
        assert_eq!(gcd_euclidean(0, 0), 0);
    }

    #[test]
    fn numerator_zero() {
        assert_eq!(gcd_euclidean(0, 5), 5);
    }

    #[test]
    fn denominator_zero() {
        assert_eq!(gcd_euclidean(5, 0), 5);
    }

    #[test]
    fn reduce_below_zero() {
        assert_eq!(gcd_euclidean(6, 10), 2);
    }

    #[test]
    fn reduce_above_zero() {
        assert_eq!(gcd_euclidean(15, 10), 5);
    }

    #[test]
    fn reduce_equal() {
        assert_eq!(gcd_euclidean(7, 7), 7);
    }
}
