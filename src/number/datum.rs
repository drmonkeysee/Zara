use super::Radix;
use crate::{
    NumReader,
    eval::{EvalResult, Frame, Namespace},
    src::StringSource,
    syntax::ParserOutput,
    value::Value,
};

pub(crate) fn read(
    s: impl Into<String>,
    rx: Radix,
    env: &Frame,
    label: impl Into<String>,
) -> EvalResult {
    let mut s = s.into();
    // If no radix in current string prepend the requested radix; if s is a
    // valid numeric datum, prepending prefix keeps it valid, if it's invalid
    // prepending the radix won't make it worse. Note that prepending any radix
    // other than #d on an inexact literal is an invalid numeric datum.
    if !Radix::in_string(&s) {
        s.insert_str(0, rx.into_prefix());
    }
    let mut src = StringSource::new(s, label);
    let mut r = NumReader::default();
    match r.read(&mut src, Namespace(env.new_child())) {
        Err(_) | Ok(ParserOutput::Continuation) => Ok(Value::Boolean(false)),
        // NumReader guarantees if we got this far, eval will yield a number
        Ok(ParserOutput::Complete(seq)) => seq.eval(env),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        number::{Number, Real, Sign},
        testutil::{TestEnv, ok_or_fail},
    };
    use std::assert_matches;

    const ALL_RADIXES: [Radix; 4] = [
        Radix::Binary,
        Radix::Octal,
        Radix::Decimal,
        Radix::Hexadecimal,
    ];

    fn read_str(s: &str, rx: Radix) -> Value {
        let env = TestEnv::default();
        ok_or_fail!(read(s, rx, &env.new_frame(), "<test>"))
    }

    fn assert_reads(s: &str, rx: Radix, expected: &Number) {
        let v = read_str(s, rx);
        let Value::Number(n) = &v else {
            panic!(
                "{s:?} (radix {}) should read as {expected}, got {v:?}",
                rx.into_prefix()
            );
        };
        assert!(
            expected.is_eqv(n),
            "{s:?} (radix {}) should read as {expected}, got {n}",
            rx.into_prefix()
        );
    }

    fn assert_false(s: &str, rx: Radix) {
        let v = read_str(s, rx);
        assert!(
            matches!(v, Value::Boolean(false)),
            "{s:?} (radix {}) should be #f, got {v:?}",
            rx.into_prefix()
        );
    }

    fn assert_roundtrip(x: &Number, rx: Radix) {
        let s = rx.num_to_string(x);
        assert_reads(&s, rx, x);
    }

    fn int(n: i64) -> Number {
        Number::real(n)
    }

    fn float(f: f64) -> Number {
        Number::real(f)
    }

    fn rat(n: i64, d: i64) -> Number {
        Number::real(ok_or_fail!(Real::reduce(n, d)))
    }

    //
    // Invalid input
    //

    #[test]
    fn empty_and_blank_are_false() {
        for rx in ALL_RADIXES {
            assert_false("", rx);
            assert_false(" ", rx);
            assert_false("\t", rx);
        }
    }

    #[test]
    fn non_numbers_are_false() {
        let inputs = [
            "xyz", "#t", "#f", "#\\a", "\"1\"", "'1", "(1)", "+", "-", "#x", "#e", "#i", "1/",
            "/2", "1/0", "--1", "++1", "1-", "1.2.3", "1e+2e3", "i", "+inf", "nan",
        ];
        for rx in ALL_RADIXES {
            for s in inputs {
                assert_false(s, rx);
            }
        }
    }

    #[test]
    fn dangling_exponent_marker_is_false() {
        assert_false("1e", Radix::Decimal);
        assert_false("1e+", Radix::Decimal);
        assert_false("1.5e", Radix::Decimal);
        assert_false("e5", Radix::Decimal);
    }

    #[test]
    fn lone_dot_is_false() {
        for rx in ALL_RADIXES {
            assert_false(".", rx);
        }
    }

    #[test]
    fn extra_whitespace_is_false() {
        for rx in ALL_RADIXES {
            for s in [" 1", "1 ", " 1 ", "\t1", "\n1", "#x ff", "# x1"] {
                assert_false(s, rx);
            }
        }
    }

    #[test]
    fn trailing_newline_is_false() {
        for rx in ALL_RADIXES {
            assert_false("1\n", rx);
            assert_false("\n1\n", rx);
        }
    }

    #[test]
    fn multiple_data_are_false() {
        for rx in ALL_RADIXES {
            for s in [
                "1 2",
                "1\n2",
                "1 a",
                "#t 1",
                "1 (2)",
                "1;comment",
                "#|c|#1",
                "1 #;2",
            ] {
                assert_false(s, rx);
            }
        }
    }

    #[test]
    fn digits_outside_radix_are_false() {
        assert_false("2", Radix::Binary);
        assert_false("102", Radix::Binary);
        assert_false("8", Radix::Octal);
        assert_false("19", Radix::Octal);
        assert_false("a", Radix::Decimal);
        assert_false("1f", Radix::Decimal);
        assert_false("g", Radix::Hexadecimal);
        assert_false("fg", Radix::Hexadecimal);
        for rx in ALL_RADIXES {
            assert_false("#b102", rx);
            assert_false("#o9", rx);
            assert_false("#d1f", rx);
            assert_false("#xg", rx);
        }
    }

    #[test]
    fn duplicate_or_conflicting_prefixes_are_false() {
        for rx in ALL_RADIXES {
            for s in [
                "#x#x1", "#x#b1", "#e#i1", "#i#e1", "#e#e1", "#x#e#i1", "#x#e#x1",
            ] {
                assert_false(s, rx);
            }
        }
    }

    //
    // Radix as a parameter
    //

    #[test]
    fn radix_param_selects_base() {
        assert_reads("101", Radix::Binary, &int(5));
        assert_reads("101", Radix::Octal, &int(65));
        assert_reads("101", Radix::Decimal, &int(101));
        assert_reads("101", Radix::Hexadecimal, &int(257));
        assert_reads("17", Radix::Octal, &int(15));
        assert_reads("0", Radix::Binary, &int(0));
    }

    #[test]
    fn hex_param_digits_are_case_insensitive() {
        assert_reads("ff", Radix::Hexadecimal, &int(255));
        assert_reads("FF", Radix::Hexadecimal, &int(255));
        assert_reads("fF", Radix::Hexadecimal, &int(255));
        assert_reads("DeadBeef", Radix::Hexadecimal, &int(0xdead_beef));
    }

    #[test]
    fn radix_param_signs() {
        assert_reads("-ff", Radix::Hexadecimal, &int(-255));
        assert_reads("+ff", Radix::Hexadecimal, &int(255));
        assert_reads("+101", Radix::Binary, &int(5));
        assert_reads("-101", Radix::Binary, &int(-5));
        assert_reads("-17", Radix::Octal, &int(-15));
    }

    #[test]
    fn radix_param_rationals() {
        assert_reads("1/10", Radix::Binary, &rat(1, 2));
        assert_reads("-a/c", Radix::Hexadecimal, &rat(-5, 6));
        assert_reads("7/10", Radix::Octal, &rat(7, 8));
        assert_reads("2/4", Radix::Decimal, &rat(1, 2));
        assert_reads("4/2", Radix::Decimal, &int(2));
        assert_reads("0/5", Radix::Decimal, &int(0));
    }

    #[test]
    fn hex_param_treats_e_as_digit_not_exponent() {
        assert_reads("1e2", Radix::Hexadecimal, &int(0x1e2));
    }

    #[test]
    fn exactness_prefix_respects_radix_param() {
        assert_reads("#e10", Radix::Hexadecimal, &int(16));
        assert_reads("#i10", Radix::Hexadecimal, &float(16.0));
        assert_reads("#i1/10", Radix::Binary, &float(0.5));
        assert_reads("#e101", Radix::Binary, &int(5));
        assert_reads("#i101", Radix::Binary, &float(5.0));
    }

    #[test]
    fn float_syntax_with_non_decimal_param_is_false() {
        for rx in [Radix::Binary, Radix::Octal, Radix::Hexadecimal] {
            for s in ["1.5", ".5", "1.", "-1.5", "1.5e2"] {
                assert_false(s, rx);
            }
        }
        assert_false("1e2", Radix::Binary);
        assert_false("1e2", Radix::Octal);
    }

    //
    // Radix inside the string
    //

    #[test]
    fn string_prefix_overrides_radix_param() {
        for rx in ALL_RADIXES {
            assert_reads("#xff", rx, &int(255));
            assert_reads("#b101", rx, &int(5));
            assert_reads("#o17", rx, &int(15));
            assert_reads("#d10", rx, &int(10));
            assert_reads("#x-ff", rx, &int(-255));
            assert_reads("#b1/10", rx, &rat(1, 2));
        }
    }

    #[test]
    fn string_prefix_is_case_insensitive() {
        for rx in ALL_RADIXES {
            assert_reads("#XFF", rx, &int(255));
            assert_reads("#xFf", rx, &int(255));
            assert_reads("#B101", rx, &int(5));
            assert_reads("#O17", rx, &int(15));
            assert_reads("#D10", rx, &int(10));
        }
    }

    #[test]
    fn uppercase_exactness_prefix_respects_radix_param() {
        // #E/#I are exactness prefixes, not radix prefixes, so the radix
        // argument still decides how "10" is read
        for (rx, n) in [
            (Radix::Binary, 2),
            (Radix::Octal, 8),
            (Radix::Decimal, 10),
            (Radix::Hexadecimal, 16),
        ] {
            assert_reads("#E10", rx, &int(n.into()));
            assert_reads("#I10", rx, &float(n.into()));
        }
    }

    #[test]
    fn exactness_and_radix_prefixes_in_either_order() {
        for rx in ALL_RADIXES {
            assert_reads("#e#x10", rx, &int(16));
            assert_reads("#x#e10", rx, &int(16));
            assert_reads("#i#xff", rx, &float(255.0));
            assert_reads("#x#iff", rx, &float(255.0));
            assert_reads("#e#b101", rx, &int(5));
            assert_reads("#b#e101", rx, &int(5));
        }
    }

    #[test]
    fn infinities_and_nan_are_valid_in_every_radix() {
        for rx in ALL_RADIXES {
            assert_reads("+inf.0", rx, &float(f64::INFINITY));
            assert_reads("-inf.0", rx, &float(f64::NEG_INFINITY));
            let v = read_str("+nan.0", rx);
            assert_matches!(v, Value::Number(Number::Real(Real::Float(f))) if f.is_nan());
        }
    }

    #[test]
    fn explicit_decimal_prefix_allows_float_syntax() {
        for rx in ALL_RADIXES {
            assert_reads("#d1.5", rx, &float(1.5));
            assert_reads("#d1e2", rx, &float(100.0));
            assert_reads("#e#d1.5", rx, &rat(3, 2));
            assert_reads("#d#e1.5", rx, &rat(3, 2));
        }
    }

    #[test]
    fn explicit_non_decimal_prefix_with_float_syntax_is_false() {
        for rx in ALL_RADIXES {
            for s in ["#x1.5", "#b1.1", "#o1.5", "#b1e1", "#o1e1", "#x.8", "#b1."] {
                assert_false(s, rx);
            }
        }
    }

    #[test]
    fn decimal_floats() {
        assert_reads("1.5", Radix::Decimal, &float(1.5));
        assert_reads("-1.5", Radix::Decimal, &float(-1.5));
        assert_reads("+1.5", Radix::Decimal, &float(1.5));
        assert_reads(".5", Radix::Decimal, &float(0.5));
        assert_reads("-.5", Radix::Decimal, &float(-0.5));
        assert_reads("1.", Radix::Decimal, &float(1.0));
        assert_reads("1e3", Radix::Decimal, &float(1000.0));
        assert_reads("1E3", Radix::Decimal, &float(1000.0));
        assert_reads("1e-3", Radix::Decimal, &float(0.001));
        assert_reads("1.5e+2", Radix::Decimal, &float(150.0));
        assert_reads("0.0", Radix::Decimal, &float(0.0));
    }

    #[test]
    fn negative_zero_float_keeps_its_sign() {
        assert_reads("-0.0", Radix::Decimal, &float(-0.0));
        // eqv? distinguishes inexact zeros by sign
        let pos = float(0.0);
        let neg = float(-0.0);
        assert!(!pos.is_eqv(&neg));
        assert_reads("0.0", Radix::Decimal, &pos);
    }

    #[test]
    fn exact_and_inexact_prefixes_on_decimals() {
        assert_reads("#e1.5", Radix::Decimal, &rat(3, 2));
        assert_reads("#e1e3", Radix::Decimal, &int(1000));
        assert_reads("#e.25", Radix::Decimal, &rat(1, 4));
        assert_reads("#e-1.5", Radix::Decimal, &rat(-3, 2));
        assert_reads("#e1.5e2", Radix::Decimal, &int(150));
        assert_reads("#i1/2", Radix::Decimal, &float(0.5));
        assert_reads("#i3", Radix::Decimal, &float(3.0));
    }

    #[test]
    fn exactness_of_unprefixed_numbers() {
        assert_reads("1", Radix::Decimal, &int(1));
        assert_reads("1/2", Radix::Decimal, &rat(1, 2));
        assert!(!int(1).is_eqv(&float(1.0)));
        assert_reads("1.0", Radix::Decimal, &float(1.0));
    }

    #[test]
    fn infinities_and_nan() {
        assert_reads("+inf.0", Radix::Decimal, &float(f64::INFINITY));
        assert_reads("-inf.0", Radix::Decimal, &float(f64::NEG_INFINITY));
        assert_reads("+INF.0", Radix::Decimal, &float(f64::INFINITY));

        for s in ["+nan.0", "-nan.0"] {
            let v = read_str(s, Radix::Decimal);
            let Value::Number(Number::Real(Real::Float(f))) = v else {
                panic!("{s:?} should read as a float nan, got {v:?}");
            };
            assert!(f.is_nan(), "{s:?} should be nan, got {f}");
        }
    }

    #[test]
    fn inf_and_nan_need_a_sign() {
        for s in ["inf.0", "nan.0", "2i", "1+2"] {
            assert_false(s, Radix::Decimal);
        }
    }

    #[test]
    fn complex_literals() {
        assert_reads("1+2i", Radix::Decimal, &Number::complex(1, 2));
        assert_reads("1-2i", Radix::Decimal, &Number::complex(1, -2));
        assert_reads("+i", Radix::Decimal, &Number::complex(0, 1));
        assert_reads("-i", Radix::Decimal, &Number::complex(0, -1));
        assert_reads("+2i", Radix::Decimal, &Number::complex(0, 2));
        assert_reads("1.5-2.5i", Radix::Decimal, &Number::complex(1.5, -2.5));
        assert_reads("#b1+10i", Radix::Decimal, &Number::complex(1, 2));
        assert_reads("1+10i", Radix::Binary, &Number::complex(1, 2));
        assert_reads("ff+ffi", Radix::Hexadecimal, &Number::complex(255, 255));
    }

    //
    // Round trips: (eqv? n (string->number (number->string n rx) rx))
    //

    fn exact_integers() -> Vec<Number> {
        vec![
            int(0),
            int(1),
            int(-1),
            int(7),
            int(8),
            int(10),
            int(15),
            int(16),
            int(255),
            int(-255),
            int(i64::MAX),
            int(i64::MIN),
            int(i64::MIN + 1),
            Number::real((Sign::Positive, u64::MAX)),
        ]
    }

    fn exact_rationals() -> Vec<Number> {
        vec![
            rat(1, 2),
            rat(-1, 2),
            rat(-7, 3),
            rat(255, 256),
            rat(1, 1024),
            rat(i64::MAX, 2),
            rat(1, i64::MAX),
        ]
    }

    #[test]
    fn exact_integers_roundtrip_in_every_radix() {
        for rx in ALL_RADIXES {
            for n in exact_integers() {
                assert_roundtrip(&n, rx);
            }
        }
    }

    #[test]
    fn exact_rationals_roundtrip_in_every_radix() {
        for rx in ALL_RADIXES {
            for q in exact_rationals() {
                assert_roundtrip(&q, rx);
            }
        }
    }

    #[test]
    fn exact_complex_roundtrip_in_every_radix() {
        let zs = [
            Number::complex(1, 2),
            Number::complex(0, 1),
            Number::complex(0, -1),
            Number::complex(-3, -5),
            Number::complex(255, -256),
            Number::complex(ok_or_fail!(Real::reduce(-3, 4)), -5),
            Number::complex(7, ok_or_fail!(Real::reduce(-1, 2))),
        ];
        for rx in ALL_RADIXES {
            for z in &zs {
                assert_roundtrip(z, rx);
            }
        }
    }

    #[test]
    fn inexact_reals_roundtrip_in_radix_10() {
        let xs = [
            0.0,
            -0.0,
            1.0,
            -1.0,
            1.5,
            -2.25,
            0.1,
            1.0 / 3.0,
            std::f64::consts::PI,
            1e21,
            1e-7,
            123_456_789.012_345_68,
            1e300,
            1e-300,
            f64::MAX,
            f64::MIN,
            f64::MIN_POSITIVE,
            f64::EPSILON,
            5e-324,
            f64::INFINITY,
            f64::NEG_INFINITY,
        ];
        for x in xs {
            assert_roundtrip(&float(x), Radix::Decimal);
        }
    }

    #[test]
    fn nan_roundtrips_as_nan_in_radix_10() {
        let s = Radix::Decimal.num_to_string(&float(f64::NAN));
        let v = read_str(&s, Radix::Decimal);
        let Value::Number(Number::Real(Real::Float(f))) = v else {
            panic!("{s:?} should read back as a float nan, got {v:?}");
        };
        assert!(f.is_nan());
    }

    #[test]
    fn inexact_complex_roundtrip_in_radix_10() {
        let zs = [
            Number::complex(1.5, 2.5),
            Number::complex(-0.0, 1.0),
            Number::complex(0.1, -0.2),
            Number::complex(f64::INFINITY, f64::NEG_INFINITY),
            Number::complex(1e300, 1e-300),
        ];
        for z in &zs {
            assert_roundtrip(z, Radix::Decimal);
        }
    }

    #[test]
    fn inexact_numbers_print_in_decimal_regardless_of_radix() {
        for rx in ALL_RADIXES {
            assert_eq!(rx.num_to_string(&float(1.5)), "1.5");
            assert_eq!(rx.num_to_string(&float(-0.25)), "-0.25");
        }
    }

    #[test]
    fn inexact_output_cannot_be_read_back_with_non_decimal_radix() {
        for rx in [Radix::Binary, Radix::Octal, Radix::Hexadecimal] {
            let s = rx.num_to_string(&float(1.5));
            assert_false(&s, rx);
        }
    }

    #[test]
    fn inexact_integers_print_with_a_decimal_point() {
        // R7RS: an inexact result must be distinguishable from an exact one
        let s = Radix::Decimal.num_to_string(&float(2.0));
        assert!(s.contains('.'), "expected a decimal point in {s:?}");
        assert_reads(&s, Radix::Decimal, &float(2.0));
    }

    #[test]
    fn number_to_string_known_values() {
        assert_eq!(Radix::Binary.num_to_string(&int(5)), "101");
        assert_eq!(Radix::Octal.num_to_string(&int(64)), "100");
        assert_eq!(Radix::Decimal.num_to_string(&int(-42)), "-42");
        assert_eq!(Radix::Hexadecimal.num_to_string(&int(255)), "ff");
        assert_eq!(Radix::Binary.num_to_string(&rat(1, 2)), "1/10");
        assert_eq!(Radix::Hexadecimal.num_to_string(&rat(-10, 12)), "-5/6");
        assert_eq!(Radix::Decimal.num_to_string(&Number::complex(1, 2)), "1+2i");
        assert_eq!(Radix::Decimal.num_to_string(&Number::complex(0, 1)), "+i");
        assert_eq!(
            Radix::Decimal.num_to_string(&float(f64::INFINITY)),
            "+inf.0"
        );
        assert_eq!(
            Radix::Decimal.num_to_string(&float(f64::NEG_INFINITY)),
            "-inf.0"
        );
        assert_eq!(Radix::Decimal.num_to_string(&float(f64::NAN)), "+nan.0");
    }

    #[test]
    #[ignore = "multi-precision integer parsing is unimplemented"]
    fn integers_beyond_64_bits_read_as_exact() {
        let v = read_str("18446744073709551616", Radix::Decimal);
        assert_matches!(v, Value::Number(Number::Real(Real::Integer(_))));
        let v = read_str("#x10000000000000000", Radix::Decimal);
        assert_matches!(v, Value::Number(Number::Real(Real::Integer(_))));
    }
}
