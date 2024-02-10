use super::*;
use crate::{
    number::{Number, NumericError, Real},
    testutil::extract_or_fail,
};

macro_rules! extract_number {
    ($exp:expr) => {{
        let tok = $exp;
        let lit = extract_or_fail!(tok, TokenKind::Literal);
        extract_or_fail!(lit, Literal::Number)
    }};
    ($exp:expr, $real:path) => {{
        let tok = $exp;
        let lit = extract_or_fail!(tok, TokenKind::Literal);
        let num = extract_or_fail!(lit, Literal::Number);
        extract_or_fail!(num, $real)
    }};
    ($exp:expr, $real:path, $kind:path) => {{
        let tok = $exp;
        let lit = extract_or_fail!(tok, TokenKind::Literal);
        let num = extract_or_fail!(lit, Literal::Number);
        let r = extract_or_fail!(num, $real);
        extract_or_fail!(r, $kind)
    }};
}

mod integer {
    use super::*;

    #[test]
    fn positive() {
        let mut s = Scanner::new("4");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(Literal::Number(_)),
                span: Range { start: 0, end: 1 },
            }
        ));
        let int = extract_number!(tok.kind, Number::Real, Real::Integer);
        assert_eq!(int.to_string(), "4");
    }

    #[test]
    fn explicit_positive() {
        let mut s = Scanner::new("+4");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(Literal::Number(_)),
                span: Range { start: 0, end: 2 },
            }
        ));
        let int = extract_number!(tok.kind, Number::Real, Real::Integer);
        assert_eq!(int.to_string(), "4");
    }

    #[test]
    fn negative() {
        let mut s = Scanner::new("-5");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(Literal::Number(_)),
                span: Range { start: 0, end: 2 },
            }
        ));
        let int = extract_number!(tok.kind, Number::Real, Real::Integer);
        assert_eq!(int.to_string(), "-5");
    }

    #[test]
    fn zero() {
        let mut s = Scanner::new("0");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(Literal::Number(_)),
                span: Range { start: 0, end: 1 },
            }
        ));
        let int = extract_number!(tok.kind, Number::Real, Real::Integer);
        assert_eq!(int.to_string(), "0");
    }

    #[test]
    fn explicit_zero() {
        let mut s = Scanner::new("+0");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(Literal::Number(_)),
                span: Range { start: 0, end: 2 },
            }
        ));
        let int = extract_number!(tok.kind, Number::Real, Real::Integer);
        assert_eq!(int.to_string(), "0");
    }

    #[test]
    fn signed_max() {
        let input = i64::MAX.to_string();
        let mut s = Scanner::new(&input);
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(Literal::Number(_)),
                span: Range { start: 0, end: 19 },
            }
        ));
        let int = extract_number!(tok.kind, Number::Real, Real::Integer);
        assert_eq!(int.to_string(), "9223372036854775807");
    }

    #[test]
    fn signed_min() {
        let input = i64::MIN.to_string();
        let mut s = Scanner::new(&input);
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(Literal::Number(_)),
                span: Range { start: 0, end: 20 },
            }
        ));
        let int = extract_number!(tok.kind, Number::Real, Real::Integer);
        assert_eq!(int.to_string(), "-9223372036854775808");
    }

    #[test]
    fn unsigned_max() {
        let input = u64::MAX.to_string();
        let mut s = Scanner::new(&input);
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(Literal::Number(_)),
                span: Range { start: 0, end: 20 },
            }
        ));
        let int = extract_number!(tok.kind, Number::Real, Real::Integer);
        assert_eq!(int.to_string(), "18446744073709551615");
    }

    #[test]
    fn explicit_unsigned_max() {
        let input = format!("+{}", u64::MAX);
        let mut s = Scanner::new(&input);
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(Literal::Number(_)),
                span: Range { start: 0, end: 21 },
            }
        ));
        let int = extract_number!(tok.kind, Number::Real, Real::Integer);
        assert_eq!(int.to_string(), "18446744073709551615");
    }

    #[test]
    fn unsigned_negative_max() {
        let input = format!("-{}", u64::MAX);
        let mut s = Scanner::new(&input);
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(Literal::Number(_)),
                span: Range { start: 0, end: 21 },
            }
        ));
        let int = extract_number!(tok.kind, Number::Real, Real::Integer);
        assert_eq!(int.to_string(), "-18446744073709551615");
    }

    #[test]
    fn radix_and_sign() {
        let radix = [("#b", "101010"), ("#o", "52"), ("#d", "42"), ("#x", "2a")];
        let prefix = ["", "+", "-", "0", "+0", "-0"];
        let combos = prefix
            .into_iter()
            .flat_map(|p| radix.map(|r| format!("{}{p}{}", r.0, r.1)));
        for case in combos {
            let mut s = Scanner::new(&case);
            let start = some_or_fail!(s.next_token());
            let t = Tokenizer {
                scan: &mut s,
                start,
            };

            let (r, c) = t.extract();

            assert!(c.is_none());
            let tok = ok_or_fail!(r);
            assert!(matches!(
                tok,
                Token {
                    kind: TokenKind::Literal(Literal::Number(_)),
                    span: Range { start: 0, end },
                } if end == case.len()
            ));
            let int = extract_number!(tok.kind, Number::Real, Real::Integer);
            let expected = if case.contains('-') { "-42" } else { "42" };
            assert_eq!(int.to_string(), expected);
        }
    }

    #[test]
    fn radix_uppercase() {
        let cases = ["#B101010", "#O52", "#D42", "#X2a"];
        for case in cases {
            let mut s = Scanner::new(&case);
            let start = some_or_fail!(s.next_token());
            let t = Tokenizer {
                scan: &mut s,
                start,
            };

            let (r, c) = t.extract();

            assert!(c.is_none());
            let tok = ok_or_fail!(r);
            assert!(matches!(
                tok,
                Token {
                    kind: TokenKind::Literal(Literal::Number(_)),
                    span: Range { start: 0, end },
                } if end == case.len()
            ));
            let int = extract_number!(tok.kind, Number::Real, Real::Integer);
            assert_eq!(int.to_string(), "42");
        }
    }

    #[test]
    fn exact_has_no_effect() {
        let mut s = Scanner::new("#e4");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(Literal::Number(_)),
                span: Range { start: 0, end: 3 },
            }
        ));
        let int = extract_number!(tok.kind, Number::Real, Real::Integer);
        assert_eq!(int.to_string(), "4");
    }

    #[test]
    fn exact_and_radix() {
        let cases = ["#e#xa", "#E#XA"];
        for case in cases {
            let mut s = Scanner::new(case);
            let start = some_or_fail!(s.next_token());
            let t = Tokenizer {
                scan: &mut s,
                start,
            };

            let (r, c) = t.extract();

            assert!(c.is_none());
            let tok = ok_or_fail!(r);
            assert!(matches!(
                tok,
                Token {
                    kind: TokenKind::Literal(Literal::Number(_)),
                    span: Range { start: 0, end: 5 },
                }
            ));
            let int = extract_number!(tok.kind, Number::Real, Real::Integer);
            assert_eq!(int.to_string(), "10");
        }
    }

    #[test]
    fn radix_and_exact() {
        let cases = ["#x#ea", "#X#EA"];
        for case in cases {
            let mut s = Scanner::new(case);
            let start = some_or_fail!(s.next_token());
            let t = Tokenizer {
                scan: &mut s,
                start,
            };

            let (r, c) = t.extract();

            assert!(c.is_none());
            let tok = ok_or_fail!(r);
            assert!(matches!(
                tok,
                Token {
                    kind: TokenKind::Literal(Literal::Number(_)),
                    span: Range { start: 0, end: 5 },
                }
            ));
            let int = extract_number!(tok.kind, Number::Real, Real::Integer);
            assert_eq!(int.to_string(), "10");
        }
    }

    #[test]
    fn inexact() {
        let cases = ["#i4", "#i+4"];
        for case in cases {
            let mut s = Scanner::new(case);
            let start = some_or_fail!(s.next_token());
            let t = Tokenizer {
                scan: &mut s,
                start,
            };

            let (r, c) = t.extract();

            assert!(c.is_none());
            let tok = ok_or_fail!(r);
            assert!(matches!(
                tok,
                Token {
                    kind: TokenKind::Literal(Literal::Number(_)),
                    span: Range { start: 0, end },
                } if end == case.len()
            ));
            let r = extract_number!(tok.kind, Number::Real);
            assert_eq!(r.to_string(), "4.0");
        }
    }

    #[test]
    fn inexact_zero() {
        let mut s = Scanner::new("#i0");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(Literal::Number(_)),
                span: Range { start: 0, end: 3 },
            }
        ));
        let r = extract_number!(tok.kind, Number::Real);
        assert_eq!(r.to_string(), "0.0");
    }

    #[test]
    fn inexact_negative() {
        let mut s = Scanner::new("#i-4");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(Literal::Number(_)),
                span: Range { start: 0, end: 4 },
            }
        ));
        let r = extract_number!(tok.kind, Number::Real);
        assert_eq!(r.to_string(), "-4.0");
    }

    #[test]
    fn inexact_radix() {
        let mut s = Scanner::new("#i#b100");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(Literal::Number(_)),
                span: Range { start: 0, end: 7 },
            }
        ));
        let r = extract_number!(tok.kind, Number::Real);
        assert_eq!(r.to_string(), "4.0");
    }

    #[test]
    fn inexact_umax() {
        let input = format!("#i{}", u64::MAX.to_string());
        let mut s = Scanner::new(&input);
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(Literal::Number(_)),
                span: Range { start: 0, end },
            } if end == input.len()
        ));
        let r = extract_number!(tok.kind, Number::Real);
        assert_eq!(r.to_string(), "1.8446744073709552e19");
    }

    #[test]
    fn invalid_radix_digits() {
        let cases = ["#b102010", "#o28", "#d42af", "#x2ag"];
        for case in cases {
            let mut s = Scanner::new(case);
            let start = some_or_fail!(s.next_token());
            let t = Tokenizer {
                scan: &mut s,
                start,
            };

            let (r, c) = t.extract();

            assert!(c.is_none());
            let err = err_or_fail!(r);
            assert!(matches!(
                err,
                TokenError {
                    kind: TokenErrorKind::NumberInvalid,
                    span: Range { start: 0, end },
                } if end == case.len()
            ));
        }
    }
}

mod rational {
    use super::*;

    #[test]
    fn simple() {
        let mut s = Scanner::new("4/5");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(Literal::Number(_)),
                span: Range { start: 0, end: 3 },
            }
        ));
        let rat = extract_number!(tok.kind, Number::Real, Real::Rational);
        assert_eq!(rat.to_string(), "4/5");
    }

    #[test]
    fn explicit_positive() {
        let mut s = Scanner::new("+4/5");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(Literal::Number(_)),
                span: Range { start: 0, end: 4 },
            }
        ));
        let rat = extract_number!(tok.kind, Number::Real, Real::Rational);
        assert_eq!(rat.to_string(), "4/5");
    }

    #[test]
    fn zero_numerator() {
        let mut s = Scanner::new("0/5");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(Literal::Number(_)),
                span: Range { start: 0, end: 3 },
            }
        ));
        let rat = extract_number!(tok.kind, Number::Real, Real::Integer);
        assert_eq!(rat.to_string(), "0");
    }

    #[test]
    fn zero_denominator() {
        let mut s = Scanner::new("4/0");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let err = err_or_fail!(r);
        assert!(matches!(
            err,
            TokenError {
                kind: TokenErrorKind::NumericError(NumericError::DivideByZero),
                span: Range { start: 0, end: 3 },
            }
        ));
    }

    #[test]
    fn reduce() {
        let mut s = Scanner::new("4/10");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(Literal::Number(_)),
                span: Range { start: 0, end: 4 },
            }
        ));
        let rat = extract_number!(tok.kind, Number::Real, Real::Rational);
        assert_eq!(rat.to_string(), "2/5");
    }

    #[test]
    fn reduce_to_int() {
        let mut s = Scanner::new("10/5");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(Literal::Number(_)),
                span: Range { start: 0, end: 4 },
            }
        ));
        let rat = extract_number!(tok.kind, Number::Real, Real::Integer);
        assert_eq!(rat.to_string(), "2");
    }

    #[test]
    fn reduce_to_unity() {
        let mut s = Scanner::new("10/10");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(Literal::Number(_)),
                span: Range { start: 0, end: 5 },
            }
        ));
        let rat = extract_number!(tok.kind, Number::Real, Real::Integer);
        assert_eq!(rat.to_string(), "1");
    }

    #[test]
    fn unity_denominator() {
        let mut s = Scanner::new("10/1");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(Literal::Number(_)),
                span: Range { start: 0, end: 4 },
            }
        ));
        let rat = extract_number!(tok.kind, Number::Real, Real::Integer);
        assert_eq!(rat.to_string(), "10");
    }

    #[test]
    fn max_uint() {
        let input = format!("+1/{}", u64::MAX);
        let mut s = Scanner::new(&input);
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(Literal::Number(_)),
                span: Range { start: 0, end: 23 },
            }
        ));
        let rat = extract_number!(tok.kind, Number::Real, Real::Rational);
        assert_eq!(rat.to_string(), "1/18446744073709551615");
    }

    #[test]
    fn negative_numerator() {
        let mut s = Scanner::new("-4/5");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(Literal::Number(_)),
                span: Range { start: 0, end: 4 },
            }
        ));
        let rat = extract_number!(tok.kind, Number::Real, Real::Rational);
        assert_eq!(rat.to_string(), "-4/5");
    }

    #[test]
    fn radix_and_sign() {
        let radix = [
            ("#b", "101010/101011"),
            ("#o", "52/53"),
            ("#d", "42/43"),
            ("#x", "2a/2b"),
        ];
        let sign = ["", "+", "-"];
        let combos = sign
            .into_iter()
            .flat_map(|s| radix.map(|r| format!("{}{s}{}", r.0, r.1)));
        for case in combos {
            let mut s = Scanner::new(&case);
            let start = some_or_fail!(s.next_token());
            let t = Tokenizer {
                scan: &mut s,
                start,
            };

            let (r, c) = t.extract();

            assert!(c.is_none());
            let tok = ok_or_fail!(r);
            assert!(matches!(
                tok,
                Token {
                    kind: TokenKind::Literal(Literal::Number(_)),
                    span: Range { start: 0, end },
                } if end == case.len()
            ));
            let rat = extract_number!(tok.kind, Number::Real, Real::Rational);
            let expected = if case.contains('-') {
                "-42/43"
            } else {
                "42/43"
            };
            assert_eq!(rat.to_string(), expected);
        }
    }

    #[test]
    fn exact_has_no_effect() {
        let mut s = Scanner::new("#e4/5");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(Literal::Number(_)),
                span: Range { start: 0, end: 5 },
            }
        ));
        let rat = extract_number!(tok.kind, Number::Real, Real::Rational);
        assert_eq!(rat.to_string(), "4/5");
    }

    #[test]
    fn exact_and_radix() {
        let cases = ["#e#xa/b", "#E#XA/B"];
        for case in cases {
            let mut s = Scanner::new(case);
            let start = some_or_fail!(s.next_token());
            let t = Tokenizer {
                scan: &mut s,
                start,
            };

            let (r, c) = t.extract();

            assert!(c.is_none());
            let tok = ok_or_fail!(r);
            assert!(matches!(
                tok,
                Token {
                    kind: TokenKind::Literal(Literal::Number(_)),
                    span: Range { start: 0, end: 7 },
                }
            ));
            let rat = extract_number!(tok.kind, Number::Real, Real::Rational);
            assert_eq!(rat.to_string(), "10/11");
        }
    }

    #[test]
    fn radix_and_exact() {
        let cases = ["#x#ea/b", "#X#EA/B"];
        for case in cases {
            let mut s = Scanner::new(case);
            let start = some_or_fail!(s.next_token());
            let t = Tokenizer {
                scan: &mut s,
                start,
            };

            let (r, c) = t.extract();

            assert!(c.is_none());
            let tok = ok_or_fail!(r);
            assert!(matches!(
                tok,
                Token {
                    kind: TokenKind::Literal(Literal::Number(_)),
                    span: Range { start: 0, end: 7 },
                }
            ));
            let rat = extract_number!(tok.kind, Number::Real, Real::Rational);
            assert_eq!(rat.to_string(), "10/11");
        }
    }

    #[test]
    fn inexact() {
        let cases = ["#i4/5", "#i+4/5"];
        for case in cases {
            let mut s = Scanner::new(case);
            let start = some_or_fail!(s.next_token());
            let t = Tokenizer {
                scan: &mut s,
                start,
            };

            let (r, c) = t.extract();

            assert!(c.is_none());
            let tok = ok_or_fail!(r);
            assert!(matches!(
                tok,
                Token {
                    kind: TokenKind::Literal(Literal::Number(_)),
                    span: Range { start: 0, end },
                } if end == case.len()
            ));
            let flt = extract_number!(tok.kind, Number::Real);
            assert_eq!(flt.to_string(), "0.8");
        }
    }

    #[test]
    fn inexact_negative() {
        let mut s = Scanner::new("#i-4/5");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(Literal::Number(_)),
                span: Range { start: 0, end: 6 },
            }
        ));
        let flt = extract_number!(tok.kind, Number::Real);
        assert_eq!(flt.to_string(), "-0.8");
    }

    #[test]
    fn inexact_radix() {
        let mut s = Scanner::new("#i#xa/b");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(Literal::Number(_)),
                span: Range { start: 0, end: 7 },
            }
        ));
        let flt = extract_number!(tok.kind, Number::Real);
        assert_eq!(flt.to_string(), "0.9090909090909091");
    }

    #[test]
    fn inexact_reduce_to_int() {
        let mut s = Scanner::new("#i10/2");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(Literal::Number(_)),
                span: Range { start: 0, end: 6 },
            }
        ));
        let flt = extract_number!(tok.kind, Number::Real);
        assert_eq!(flt.to_string(), "5.0");
    }

    #[test]
    fn inexact_max_uint() {
        let input = format!("#i1/{}", u64::MAX);
        let mut s = Scanner::new(&input);
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(Literal::Number(_)),
                span: Range { start: 0, end },
            } if end == input.len()
        ));
        let flt = extract_number!(tok.kind, Number::Real);
        assert_eq!(flt.to_string(), "5.421010862427522e-20");
    }

    #[test]
    fn negative_denominator() {
        let mut s = Scanner::new("4/-5");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let err = err_or_fail!(r);
        assert!(matches!(
            err,
            TokenError {
                kind: TokenErrorKind::RationalInvalid,
                span: Range { start: 0, end: 4 },
            }
        ));
    }

    #[test]
    fn explicit_positive_denominator() {
        let mut s = Scanner::new("4/+5");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let err = err_or_fail!(r);
        assert!(matches!(
            err,
            TokenError {
                kind: TokenErrorKind::RationalInvalid,
                span: Range { start: 0, end: 4 },
            }
        ));
    }

    #[test]
    fn negative_both() {
        let mut s = Scanner::new("-4/-5");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let err = err_or_fail!(r);
        assert!(matches!(
            err,
            TokenError {
                kind: TokenErrorKind::RationalInvalid,
                span: Range { start: 0, end: 5 },
            }
        ));
    }

    #[test]
    fn missing_denominator() {
        let mut s = Scanner::new("4/");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let err = err_or_fail!(r);
        assert!(matches!(
            err,
            TokenError {
                kind: TokenErrorKind::RationalInvalid,
                span: Range { start: 0, end: 2 },
            }
        ));
    }

    #[test]
    fn float_numerator() {
        let mut s = Scanner::new("4.2/5");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let err = err_or_fail!(r);
        assert!(matches!(
            err,
            TokenError {
                kind: TokenErrorKind::RationalInvalid,
                span: Range { start: 0, end: 5 },
            }
        ));
    }

    #[test]
    fn float_denominator() {
        let mut s = Scanner::new("4/5.3");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let err = err_or_fail!(r);
        assert!(matches!(
            err,
            TokenError {
                kind: TokenErrorKind::RationalInvalid,
                span: Range { start: 0, end: 5 },
            }
        ));
    }

    #[test]
    fn too_many_slashes() {
        let mut s = Scanner::new("4/5/6");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let err = err_or_fail!(r);
        assert!(matches!(
            err,
            TokenError {
                kind: TokenErrorKind::RationalInvalid,
                span: Range { start: 0, end: 5 },
            }
        ));
    }

    #[test]
    fn radix_double_sign() {
        let mut s = Scanner::new("#x++");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let err = err_or_fail!(r);
        assert!(matches!(
            err,
            TokenError {
                kind: TokenErrorKind::NumberInvalid,
                span: Range { start: 0, end: 4 },
            }
        ));
    }

    #[test]
    fn invalid_radix_placement() {
        let mut s = Scanner::new("4/#b101");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let err = err_or_fail!(r);
        assert!(matches!(
            err,
            TokenError {
                kind: TokenErrorKind::RationalInvalid,
                span: Range { start: 0, end: 2 },
            }
        ));
    }

    #[test]
    fn invalid_radix_denom() {
        let mut s = Scanner::new("#b100/5");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let err = err_or_fail!(r);
        assert!(matches!(
            err,
            TokenError {
                kind: TokenErrorKind::RationalInvalid,
                span: Range { start: 0, end: 7 },
            }
        ));
    }
}

mod float {
    use super::*;

    #[test]
    fn simple() {
        let mut s = Scanner::new("42.34");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(Literal::Number(_)),
                span: Range { start: 0, end: 5 },
            }
        ));
        let flt = extract_number!(tok.kind, Number::Real);
        assert_eq!(flt.to_string(), "42.34");
    }

    #[test]
    fn positive_float() {
        let mut s = Scanner::new("+42.34");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(Literal::Number(_)),
                span: Range { start: 0, end: 6 },
            }
        ));
        let flt = extract_number!(tok.kind, Number::Real);
        assert_eq!(flt.to_string(), "42.34");
    }

    #[test]
    fn negative_float() {
        let mut s = Scanner::new("-42.34");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(Literal::Number(_)),
                span: Range { start: 0, end: 6 },
            }
        ));
        let flt = extract_number!(tok.kind, Number::Real);
        assert_eq!(flt.to_string(), "-42.34");
    }

    #[test]
    fn leading_decimal() {
        let mut s = Scanner::new(".5");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(Literal::Number(_)),
                span: Range { start: 0, end: 2 },
            }
        ));
        let flt = extract_number!(tok.kind, Number::Real);
        assert_eq!(flt.to_string(), "0.5");
    }

    #[test]
    fn positive_leading_decimal() {
        let mut s = Scanner::new("+.5");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(Literal::Number(_)),
                span: Range { start: 0, end: 3 },
            }
        ));
        let flt = extract_number!(tok.kind, Number::Real);
        assert_eq!(flt.to_string(), "0.5");
    }

    #[test]
    fn negative_leading_decimal() {
        let mut s = Scanner::new("-.5");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(Literal::Number(_)),
                span: Range { start: 0, end: 3 },
            }
        ));
        let flt = extract_number!(tok.kind, Number::Real);
        assert_eq!(flt.to_string(), "-0.5");
    }

    #[test]
    fn trailing_decimal() {
        let mut s = Scanner::new("5.");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(Literal::Number(_)),
                span: Range { start: 0, end: 2 },
            }
        ));
        let flt = extract_number!(tok.kind, Number::Real);
        assert_eq!(flt.to_string(), "5.0");
    }

    #[test]
    fn positive_exponent() {
        let mut s = Scanner::new("34e4");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(Literal::Number(_)),
                span: Range { start: 0, end: 4 },
            }
        ));
        let flt = extract_number!(tok.kind, Number::Real);
        assert_eq!(flt.to_string(), "340000.0");
    }

    #[test]
    fn explicit_positive_exponent() {
        let mut s = Scanner::new("34e+4");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(Literal::Number(_)),
                span: Range { start: 0, end: 5 },
            }
        ));
        let flt = extract_number!(tok.kind, Number::Real);
        assert_eq!(flt.to_string(), "340000.0");
    }

    #[test]
    fn zero_exponent() {
        let mut s = Scanner::new("34e0");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(Literal::Number(_)),
                span: Range { start: 0, end: 4 },
            }
        ));
        let flt = extract_number!(tok.kind, Number::Real);
        assert_eq!(flt.to_string(), "34.0");
    }

    #[test]
    fn upper_exponent() {
        let mut s = Scanner::new("34E4");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(Literal::Number(_)),
                span: Range { start: 0, end: 4 },
            }
        ));
        let flt = extract_number!(tok.kind, Number::Real);
        assert_eq!(flt.to_string(), "340000.0");
    }

    #[test]
    fn negative_exponent() {
        let mut s = Scanner::new("34e-4");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(Literal::Number(_)),
                span: Range { start: 0, end: 5 },
            }
        ));
        let flt = extract_number!(tok.kind, Number::Real);
        assert_eq!(flt.to_string(), "0.0034");
    }

    #[test]
    fn decimal_with_exponent() {
        let mut s = Scanner::new("12.45e4");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(Literal::Number(_)),
                span: Range { start: 0, end: 7 },
            }
        ));
        let flt = extract_number!(tok.kind, Number::Real);
        assert_eq!(flt.to_string(), "124500.0");
    }

    #[test]
    fn large_exponent() {
        let mut s = Scanner::new("12.45e50");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(Literal::Number(_)),
                span: Range { start: 0, end: 8 },
            }
        ));
        let flt = extract_number!(tok.kind, Number::Real);
        assert_eq!(flt.to_string(), "1.245e51");
    }

    #[test]
    fn too_large_exponent() {
        let mut s = Scanner::new("12.45e500");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(Literal::Number(_)),
                span: Range { start: 0, end: 9 },
            }
        ));
        let flt = extract_number!(tok.kind, Number::Real);
        assert_eq!(flt.to_string(), "+inf.0");
    }

    #[test]
    fn too_small_exponent() {
        let mut s = Scanner::new("12.45e-500");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(Literal::Number(_)),
                span: Range { start: 0, end: 10 },
            }
        ));
        let flt = extract_number!(tok.kind, Number::Real);
        assert_eq!(flt.to_string(), "0.0");
    }

    #[test]
    fn positive_infinity() {
        let mut s = Scanner::new("+inf.0");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(Literal::Number(_)),
                span: Range { start: 0, end: 6 },
            }
        ));
        let flt = extract_number!(tok.kind, Number::Real);
        assert_eq!(flt.to_string(), "+inf.0");
    }

    #[test]
    fn infinity_upper() {
        let mut s = Scanner::new("+INF.0");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(Literal::Number(_)),
                span: Range { start: 0, end: 6 },
            }
        ));
        let flt = extract_number!(tok.kind, Number::Real);
        assert_eq!(flt.to_string(), "+inf.0");
    }

    #[test]
    fn negative_infinity() {
        let mut s = Scanner::new("-inf.0");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(Literal::Number(_)),
                span: Range { start: 0, end: 6 },
            }
        ));
        let flt = extract_number!(tok.kind, Number::Real);
        assert_eq!(flt.to_string(), "-inf.0");
    }

    #[test]
    fn inexact_has_no_effect() {
        let mut s = Scanner::new("#i42.34");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(Literal::Number(_)),
                span: Range { start: 0, end: 7 },
            }
        ));
        let flt = extract_number!(tok.kind, Number::Real);
        assert_eq!(flt.to_string(), "42.34");
    }

    #[test]
    fn inexact_inf_nan() {
        let cases = ["#i+inf.0", "#i-inf.0", "#i+nan.0", "#i-nan.0"];
        for case in cases {
            let mut s = Scanner::new(case);
            let start = some_or_fail!(s.next_token());
            let t = Tokenizer {
                scan: &mut s,
                start,
            };

            let (r, c) = t.extract();

            assert!(c.is_none());
            let tok = ok_or_fail!(r);
            assert!(matches!(
                tok,
                Token {
                    kind: TokenKind::Literal(Literal::Number(_)),
                    span: Range { start: 0, end },
                } if end == case.len()
            ));
            let flt = extract_number!(tok.kind, Number::Real);
            let expected = if case.contains("-nan") {
                case[2..].replace("-nan", "+nan")
            } else {
                case[2..].to_owned()
            };
            assert_eq!(flt.to_string(), expected);
        }
    }

    #[test]
    fn inexact_and_radix() {
        let cases = ["#i#d42.34", "#I#D42.34"];
        for case in cases {
            let mut s = Scanner::new(case);
            let start = some_or_fail!(s.next_token());
            let t = Tokenizer {
                scan: &mut s,
                start,
            };

            let (r, c) = t.extract();

            assert!(c.is_none());
            let tok = ok_or_fail!(r);
            assert!(matches!(
                tok,
                Token {
                    kind: TokenKind::Literal(Literal::Number(_)),
                    span: Range { start: 0, end: 9 },
                }
            ));
            let flt = extract_number!(tok.kind, Number::Real);
            assert_eq!(flt.to_string(), "42.34");
        }
    }

    #[test]
    fn radix_and_inexact() {
        let cases = ["#d#i42.34", "#D#I42.34"];
        for case in cases {
            let mut s = Scanner::new(case);
            let start = some_or_fail!(s.next_token());
            let t = Tokenizer {
                scan: &mut s,
                start,
            };

            let (r, c) = t.extract();

            assert!(c.is_none());
            let tok = ok_or_fail!(r);
            assert!(matches!(
                tok,
                Token {
                    kind: TokenKind::Literal(Literal::Number(_)),
                    span: Range { start: 0, end: 9 },
                }
            ));
            let flt = extract_number!(tok.kind, Number::Real);
            assert_eq!(flt.to_string(), "42.34");
        }
    }

    #[test]
    fn exact_integer() {
        let cases = ["#e4.", "#e4.0", "#e+4.0"];
        for case in cases {
            let mut s = Scanner::new(case);
            let start = some_or_fail!(s.next_token());
            let t = Tokenizer {
                scan: &mut s,
                start,
            };

            let (r, c) = t.extract();

            assert!(c.is_none());
            let tok = ok_or_fail!(r);
            assert!(matches!(
                tok,
                Token {
                    kind: TokenKind::Literal(Literal::Number(_)),
                    span: Range { start: 0, end },
                } if end == case.len()
            ));
            let int = extract_number!(tok.kind, Number::Real, Real::Integer);
            assert_eq!(int.to_string(), "4");
        }
    }

    #[test]
    fn exact_zero() {
        let mut s = Scanner::new("#e0.0");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(Literal::Number(_)),
                span: Range { start: 0, end: 5 },
            }
        ));
        let int = extract_number!(tok.kind, Number::Real, Real::Integer);
        assert_eq!(int.to_string(), "0");
    }

    #[test]
    fn exact_negative_integer() {
        let mut s = Scanner::new("#e-4.0");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(Literal::Number(_)),
                span: Range { start: 0, end: 6 },
            }
        ));
        let int = extract_number!(tok.kind, Number::Real, Real::Integer);
        assert_eq!(int.to_string(), "-4");
    }

    #[test]
    fn exact_decimal() {
        let mut s = Scanner::new("#e12.45");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(Literal::Number(_)),
                span: Range { start: 0, end: 7 },
            }
        ));
        let rat = extract_number!(tok.kind, Number::Real, Real::Rational);
        assert_eq!(rat.to_string(), "249/20");
    }

    #[test]
    fn exact_smaller_than_unity() {
        let cases = ["#e.45", "#e0.45", "#e+.45", "#e+0.45"];
        for case in cases {
            let mut s = Scanner::new(case);
            let start = some_or_fail!(s.next_token());
            let t = Tokenizer {
                scan: &mut s,
                start,
            };

            let (r, c) = t.extract();

            assert!(c.is_none());
            let tok = ok_or_fail!(r);
            assert!(matches!(
                tok,
                Token {
                    kind: TokenKind::Literal(Literal::Number(_)),
                    span: Range { start: 0, end },
                } if end == case.len()
            ));
            let rat = extract_number!(tok.kind, Number::Real, Real::Rational);
            assert_eq!(rat.to_string(), "9/20");
        }
    }

    #[test]
    fn exact_negative_smaller_than_unity() {
        let cases = ["#e-.45", "#e-0.45"];
        for case in cases {
            let mut s = Scanner::new(case);
            let start = some_or_fail!(s.next_token());
            let t = Tokenizer {
                scan: &mut s,
                start,
            };

            let (r, c) = t.extract();

            assert!(c.is_none());
            let tok = ok_or_fail!(r);
            assert!(matches!(
                tok,
                Token {
                    kind: TokenKind::Literal(Literal::Number(_)),
                    span: Range { start: 0, end },
                } if end == case.len()
            ));
            let rat = extract_number!(tok.kind, Number::Real, Real::Rational);
            assert_eq!(rat.to_string(), "-9/20");
        }
    }

    #[test]
    fn exact_exponent() {
        let cases = ["#e4e4", "#e4e+4"];
        for case in cases {
            let mut s = Scanner::new(case);
            let start = some_or_fail!(s.next_token());
            let t = Tokenizer {
                scan: &mut s,
                start,
            };

            let (r, c) = t.extract();

            assert!(c.is_none());
            let tok = ok_or_fail!(r);
            assert!(matches!(
                tok,
                Token {
                    kind: TokenKind::Literal(Literal::Number(_)),
                    span: Range { start: 0, end },
                } if end == case.len()
            ));
            let int = extract_number!(tok.kind, Number::Real, Real::Integer);
            assert_eq!(int.to_string(), "40000");
        }
    }

    #[test]
    fn exact_negative_exponent() {
        let mut s = Scanner::new("#e4e-4");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(Literal::Number(_)),
                span: Range { start: 0, end: 6 },
            }
        ));
        let rat = extract_number!(tok.kind, Number::Real, Real::Rational);
        assert_eq!(rat.to_string(), "1/2500");
    }

    #[test]
    fn exact_decimal_and_exponent_integer() {
        let mut s = Scanner::new("#e4.52e4");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(Literal::Number(_)),
                span: Range { start: 0, end: 8 },
            }
        ));
        let int = extract_number!(tok.kind, Number::Real, Real::Integer);
        assert_eq!(int.to_string(), "45200");
    }

    #[test]
    fn exact_decimal_no_fraction_and_exponent_integer() {
        let mut s = Scanner::new("#e4.e4");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(Literal::Number(_)),
                span: Range { start: 0, end: 6 },
            }
        ));
        let int = extract_number!(tok.kind, Number::Real, Real::Integer);
        assert_eq!(int.to_string(), "40000");
    }

    #[test]
    fn exact_decimal_and_exponent_rational() {
        let mut s = Scanner::new("#e4.524242e3");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(Literal::Number(_)),
                span: Range { start: 0, end: 12 },
            }
        ));
        let rat = extract_number!(tok.kind, Number::Real, Real::Rational);
        assert_eq!(rat.to_string(), "2262121/500");
    }

    #[test]
    fn exact_decimal_and_exponent_cancel_out() {
        let cases = ["42", "#e4.2e1"];
        for case in cases {
            let mut s = Scanner::new(case);
            let start = some_or_fail!(s.next_token());
            let t = Tokenizer {
                scan: &mut s,
                start,
            };

            let (r, c) = t.extract();

            assert!(c.is_none());
            let tok = ok_or_fail!(r);
            assert!(matches!(
                tok,
                Token {
                    kind: TokenKind::Literal(Literal::Number(_)),
                    span: Range { start: 0, end },
                } if end == case.len()
            ));
            let int = extract_number!(tok.kind, Number::Real, Real::Integer);
            assert_eq!(int.to_string(), "42");
        }
    }

    #[test]
    fn exact_inf_nan() {
        let cases = ["#e+inf.0", "#e-inf.0", "#e+nan.0", "#e-nan.0"];
        for case in cases {
            let mut s = Scanner::new(case);
            let start = some_or_fail!(s.next_token());
            let t = Tokenizer {
                scan: &mut s,
                start,
            };

            let (r, c) = t.extract();

            assert!(c.is_none());
            let tok = ok_or_fail!(r);
            assert!(matches!(
                tok,
                Token {
                    kind: TokenKind::Literal(Literal::Number(_)),
                    span: Range { start: 0, end },
                } if end == case.len()
            ));
            let flt = extract_number!(tok.kind, Number::Real);
            let expected = if case.contains("-nan") {
                case[2..].replace("-nan", "+nan")
            } else {
                case[2..].to_owned()
            };
            assert_eq!(flt.to_string(), expected);
        }
    }

    #[test]
    fn invalid_leading_digits_infinity() {
        let mut s = Scanner::new("+0inf.0");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let err = err_or_fail!(r);
        assert!(matches!(
            err,
            TokenError {
                kind: TokenErrorKind::NumberInvalid,
                span: Range { start: 0, end: 7 },
            }
        ));
    }

    #[test]
    fn positive_nan() {
        let mut s = Scanner::new("+nan.0");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(Literal::Number(_)),
                span: Range { start: 0, end: 6 },
            }
        ));
        let flt = extract_number!(tok.kind, Number::Real);
        assert_eq!(flt.to_string(), "+nan.0");
    }

    #[test]
    fn nan_upper() {
        let mut s = Scanner::new("+NAN.0");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(Literal::Number(_)),
                span: Range { start: 0, end: 6 },
            }
        ));
        let flt = extract_number!(tok.kind, Number::Real);
        assert_eq!(flt.to_string(), "+nan.0");
    }

    #[test]
    fn invalid_leading_digits_nan() {
        let mut s = Scanner::new("+0nan.0");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let err = err_or_fail!(r);
        assert!(matches!(
            err,
            TokenError {
                kind: TokenErrorKind::NumberInvalid,
                span: Range { start: 0, end: 7 },
            }
        ));
    }

    #[test]
    fn negative_nan() {
        let mut s = Scanner::new("-nan.0");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(Literal::Number(_)),
                span: Range { start: 0, end: 6 },
            }
        ));
        let flt = extract_number!(tok.kind, Number::Real);
        assert_eq!(flt.to_string(), "+nan.0");
    }

    #[test]
    fn valid_decimal_radix() {
        let mut s = Scanner::new("#d42.34");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(Literal::Number(_)),
                span: Range { start: 0, end: 7 },
            }
        ));
        let flt = extract_number!(tok.kind, Number::Real);
        assert_eq!(flt.to_string(), "42.34");
    }

    #[test]
    fn invalid_decimal_radix() {
        let cases = [
            ("#b1.01", "binary"),
            ("#o3.73", "octal"),
            ("#xf.a2", "hexadecimal"),
        ];
        for (case, label) in cases {
            let mut s = Scanner::new(case);
            let start = some_or_fail!(s.next_token());
            let t = Tokenizer {
                scan: &mut s,
                start,
            };

            let (r, c) = t.extract();

            assert!(c.is_none());
            let err = err_or_fail!(r);
            assert!(matches!(
                err,
                TokenError {
                    kind: TokenErrorKind::NumberInvalidDecimalPoint { at: 3, radix },
                    span: Range { start: 3, end },
                } if radix == label && end == case.len()
            ));
        }
    }

    #[test]
    fn valid_exponent_radix() {
        let mut s = Scanner::new("#d34e4");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(Literal::Number(_)),
                span: Range { start: 0, end: 6 },
            }
        ));
        let flt = extract_number!(tok.kind, Number::Real);
        assert_eq!(flt.to_string(), "340000.0");
    }

    #[test]
    fn invalid_exponent_radix() {
        let cases = [("#b11e10", "binary"), ("#o34e72", "octal")];
        for (case, label) in cases {
            let mut s = Scanner::new(case);
            let start = some_or_fail!(s.next_token());
            let t = Tokenizer {
                scan: &mut s,
                start,
            };

            let (r, c) = t.extract();

            assert!(c.is_none());
            let err = err_or_fail!(r);
            assert!(matches!(
                err,
                TokenError {
                    kind: TokenErrorKind::NumberInvalidExponent { at: 4, radix },
                    span: Range { start: 4, end },
                } if radix == label && end == case.len()
            ));
        }
    }

    #[test]
    fn hex_digit_is_not_exponent() {
        let mut s = Scanner::new("#x4fea2");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(Literal::Number(_)),
                span: Range { start: 0, end: 7 },
            }
        ));
        let num = extract_number!(tok.kind, Number::Real);
        assert_eq!(num.to_string(), "327330");
    }

    #[test]
    fn unexpected_decimal_point() {
        let mut s = Scanner::new("3.456.23");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let err = err_or_fail!(r);
        assert!(matches!(
            err,
            TokenError {
                kind: TokenErrorKind::NumberUnexpectedDecimalPoint { at: 5 },
                span: Range { start: 5, end: 8 },
            }
        ));
    }

    #[test]
    fn empty_exponent() {
        let mut s = Scanner::new("3.456e");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let err = err_or_fail!(r);
        assert!(matches!(
            err,
            TokenError {
                kind: TokenErrorKind::NumericErrorAt {
                    at: 5,
                    err: NumericError::ParseExponentFailure
                },
                span: Range { start: 5, end: 6 },
            }
        ));
    }

    #[test]
    fn empty_exponent_no_fraction() {
        let mut s = Scanner::new("3e");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let err = err_or_fail!(r);
        assert!(matches!(
            err,
            TokenError {
                kind: TokenErrorKind::NumericErrorAt {
                    at: 1,
                    err: NumericError::ParseExponentFailure
                },
                span: Range { start: 1, end: 2 },
            }
        ));
    }

    #[test]
    fn sign_only_exponent() {
        let mut s = Scanner::new("3.456e-");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let err = err_or_fail!(r);
        assert!(matches!(
            err,
            TokenError {
                kind: TokenErrorKind::NumericErrorAt {
                    at: 5,
                    err: NumericError::ParseExponentFailure
                },
                span: Range { start: 5, end: 7 },
            }
        ));
    }

    #[test]
    fn sign_only_exponent_no_fraction() {
        let mut s = Scanner::new("3e-");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let err = err_or_fail!(r);
        assert!(matches!(
            err,
            TokenError {
                kind: TokenErrorKind::NumericErrorAt {
                    at: 1,
                    err: NumericError::ParseExponentFailure
                },
                span: Range { start: 1, end: 3 },
            }
        ));
    }

    #[test]
    fn malformed_exponent() {
        let mut s = Scanner::new("3.456e4.5");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let err = err_or_fail!(r);
        assert!(matches!(
            err,
            TokenError {
                kind: TokenErrorKind::NumericErrorAt {
                    at: 5,
                    err: NumericError::ParseExponentFailure
                },
                span: Range { start: 5, end: 9 },
            }
        ));
    }

    #[test]
    fn exponent_too_many_signs() {
        let mut s = Scanner::new("3.456e+-");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let err = err_or_fail!(r);
        assert!(matches!(
            err,
            TokenError {
                kind: TokenErrorKind::NumericErrorAt {
                    at: 5,
                    err: NumericError::ParseExponentFailure
                },
                span: Range { start: 5, end: 8 },
            }
        ));
    }

    #[test]
    fn exact_exponent_empty_and_only_sign() {
        let cases = ["3.456e", "3.456e-"];
        for case in cases {
            let mut s = Scanner::new(case);
            let start = some_or_fail!(s.next_token());
            let t = Tokenizer {
                scan: &mut s,
                start,
            };

            let (r, c) = t.extract();

            assert!(c.is_none());
            let err = err_or_fail!(r);
            assert!(matches!(
                err,
                TokenError {
                    kind: TokenErrorKind::NumericErrorAt {
                        at: 5,
                        err: NumericError::ParseExponentFailure
                    },
                    span: Range { start: 5, end },
                } if end == case.len()
            ));
        }
    }

    #[test]
    fn exact_exponent_overflow() {
        let cases = [(i32::MAX as i64) + 1, (i32::MIN as i64) - 1];
        for case in cases {
            let input = format!("#e4.0e{case}");
            let mut s = Scanner::new(&input);
            let start = some_or_fail!(s.next_token());
            let t = Tokenizer {
                scan: &mut s,
                start,
            };

            let (r, c) = t.extract();

            assert!(c.is_none());
            let err = err_or_fail!(r);
            assert!(matches!(
                err,
                TokenError {
                    kind: TokenErrorKind::NumericErrorAt {
                        at: 5,
                        err: NumericError::ParseExponentOutOfRange
                    },
                    span: Range { start: 5, end },
                } if end == input.len()
            ));
        }
    }
}

mod imaginary {
    use super::*;

    #[test]
    fn positive_imaginary() {
        let mut s = Scanner::new("+i");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Imaginary(_),
                span: Range { start: 0, end: 2 },
            }
        ));
        let r = extract_or_fail!(tok.kind, TokenKind::Imaginary);
        assert_eq!(r.to_string(), "1");
    }

    #[test]
    fn imaginary_upper() {
        let mut s = Scanner::new("+I");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Imaginary(_),
                span: Range { start: 0, end: 2 },
            }
        ));
        let r = extract_or_fail!(tok.kind, TokenKind::Imaginary);
        assert_eq!(r.to_string(), "1");
    }

    #[test]
    fn zero_imaginary() {
        let mut s = Scanner::new("0i");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let err = err_or_fail!(r);
        assert!(matches!(
            err,
            TokenError {
                kind: TokenErrorKind::ImaginaryMissingSign,
                span: Range { start: 0, end: 2 },
            }
        ));
    }

    #[test]
    fn anything_after_i() {
        let mut s = Scanner::new("+4i5");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let err = err_or_fail!(r);
        assert!(matches!(
            err,
            TokenError {
                kind: TokenErrorKind::ImaginaryInvalid,
                span: Range { start: 0, end: 4 },
            }
        ));
    }

    #[test]
    fn sign_after_i() {
        let mut s = Scanner::new("+4i+");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let err = err_or_fail!(r);
        assert!(matches!(
            err,
            TokenError {
                kind: TokenErrorKind::ImaginaryInvalid,
                span: Range { start: 0, end: 4 },
            }
        ));
    }

    #[test]
    fn explicit_zero_imaginary() {
        let mut s = Scanner::new("+0i");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Imaginary(_),
                span: Range { start: 0, end: 3 },
            }
        ));
        let r = extract_or_fail!(tok.kind, TokenKind::Imaginary);
        assert_eq!(r.to_string(), "0");
    }

    #[test]
    fn integer_imaginary() {
        let mut s = Scanner::new("+5i");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Imaginary(_),
                span: Range { start: 0, end: 3 },
            }
        ));
        let r = extract_or_fail!(tok.kind, TokenKind::Imaginary);
        assert_eq!(r.to_string(), "5");
    }

    #[test]
    fn rational_imaginary() {
        let mut s = Scanner::new("+4/5i");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Imaginary(_),
                span: Range { start: 0, end: 5 },
            }
        ));
        let r = extract_or_fail!(tok.kind, TokenKind::Imaginary);
        assert_eq!(r.to_string(), "4/5");
    }

    #[test]
    fn rational_imaginary_unity_denominator() {
        let mut s = Scanner::new("+4/1i");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Imaginary(_),
                span: Range { start: 0, end: 5 },
            }
        ));
        let r = extract_or_fail!(tok.kind, TokenKind::Imaginary);
        assert_eq!(r.to_string(), "4");
    }

    #[test]
    fn radix_and_sign() {
        let radix = [("#b", "101010"), ("#o", "52"), ("#d", "42"), ("#x", "2a")];
        let prefix = ["+", "-", "+0", "-0"];
        let combos = prefix
            .into_iter()
            .flat_map(|p| radix.map(|r| format!("{}{p}{}i", r.0, r.1)));
        for case in combos {
            let mut s = Scanner::new(&case);
            let start = some_or_fail!(s.next_token());
            let t = Tokenizer {
                scan: &mut s,
                start,
            };

            let (r, c) = t.extract();

            assert!(c.is_none());
            let tok = ok_or_fail!(r);
            assert!(matches!(
                tok,
                Token {
                    kind: TokenKind::Imaginary(_),
                    span: Range { start: 0, end },
                } if end == case.len()
            ));
            let r = extract_or_fail!(tok.kind, TokenKind::Imaginary);
            let expected = if case.contains('-') { "-42" } else { "42" };
            assert_eq!(r.to_string(), expected);
        }
    }

    #[test]
    fn unity_radix() {
        let radix = ["#b", "#o", "#d", "#x"];
        let sign = ["+", "-"];
        let combos = sign
            .into_iter()
            .flat_map(|s| radix.map(|r| format!("{r}{s}i")));
        for case in combos {
            let mut s = Scanner::new(&case);
            let start = some_or_fail!(s.next_token());
            let t = Tokenizer {
                scan: &mut s,
                start,
            };

            let (r, c) = t.extract();

            assert!(c.is_none());
            let tok = ok_or_fail!(r);
            assert!(matches!(
                tok,
                Token {
                    kind: TokenKind::Imaginary(_),
                    span: Range { start: 0, end },
                } if end == case.len()
            ));
            let r = extract_or_fail!(tok.kind, TokenKind::Imaginary);
            let expected = if case.contains('-') { "-1" } else { "1" };
            assert_eq!(r.to_string(), expected);
        }
    }

    #[test]
    fn exact_and_radix() {
        let cases = ["#e#x+ai", "#E#X+Ai"];
        for case in cases {
            let mut s = Scanner::new(case);
            let start = some_or_fail!(s.next_token());
            let t = Tokenizer {
                scan: &mut s,
                start,
            };

            let (r, c) = t.extract();

            assert!(c.is_none());
            let tok = ok_or_fail!(r);
            assert!(matches!(
                tok,
                Token {
                    kind: TokenKind::Imaginary(_),
                    span: Range { start: 0, end: 7 },
                }
            ));
            let r = extract_or_fail!(tok.kind, TokenKind::Imaginary);
            assert_eq!(r.to_string(), "10");
        }
    }

    #[test]
    fn radix_and_exact() {
        let cases = ["#x#e+ai", "#X#E+Ai"];
        for case in cases {
            let mut s = Scanner::new(case);
            let start = some_or_fail!(s.next_token());
            let t = Tokenizer {
                scan: &mut s,
                start,
            };

            let (r, c) = t.extract();

            assert!(c.is_none());
            let tok = ok_or_fail!(r);
            assert!(matches!(
                tok,
                Token {
                    kind: TokenKind::Imaginary(_),
                    span: Range { start: 0, end: 7 },
                }
            ));
            let r = extract_or_fail!(tok.kind, TokenKind::Imaginary);
            assert_eq!(r.to_string(), "10");
        }
    }

    #[test]
    fn inexact_and_radix() {
        let cases = ["#i#d+42.34i", "#I#D+42.34i"];
        for case in cases {
            let mut s = Scanner::new(case);
            let start = some_or_fail!(s.next_token());
            let t = Tokenizer {
                scan: &mut s,
                start,
            };

            let (r, c) = t.extract();

            assert!(c.is_none());
            let tok = ok_or_fail!(r);
            assert!(matches!(
                tok,
                Token {
                    kind: TokenKind::Imaginary(_),
                    span: Range { start: 0, end: 11 },
                }
            ));
            let r = extract_or_fail!(tok.kind, TokenKind::Imaginary);
            assert_eq!(r.to_string(), "42.34");
        }
    }

    #[test]
    fn radix_and_inexact() {
        let cases = ["#d#i+42.34i", "#D#I+42.34i"];
        for case in cases {
            let mut s = Scanner::new(case);
            let start = some_or_fail!(s.next_token());
            let t = Tokenizer {
                scan: &mut s,
                start,
            };

            let (r, c) = t.extract();

            assert!(c.is_none());
            let tok = ok_or_fail!(r);
            assert!(matches!(
                tok,
                Token {
                    kind: TokenKind::Imaginary(_),
                    span: Range { start: 0, end: 11 },
                }
            ));
            let r = extract_or_fail!(tok.kind, TokenKind::Imaginary);
            assert_eq!(r.to_string(), "42.34");
        }
    }

    #[test]
    fn applied_inexact_unity() {
        let mut s = Scanner::new("#i+i");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Imaginary(_),
                span: Range { start: 0, end: 4 },
            }
        ));
        let r = extract_or_fail!(tok.kind, TokenKind::Imaginary);
        assert_eq!(r.to_string(), "1.0");
    }

    #[test]
    fn applied_inexact() {
        let mut s = Scanner::new("#i+5i");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Imaginary(_),
                span: Range { start: 0, end: 5 },
            }
        ));
        let r = extract_or_fail!(tok.kind, TokenKind::Imaginary);
        assert_eq!(r.to_string(), "5.0");
    }

    #[test]
    fn applied_exact() {
        let mut s = Scanner::new("#e+5.2i");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Imaginary(_),
                span: Range { start: 0, end: 7 },
            }
        ));
        let r = extract_or_fail!(tok.kind, TokenKind::Imaginary);
        assert_eq!(r.to_string(), "26/5");
    }

    #[test]
    fn rational_imaginary_missing_sign() {
        let mut s = Scanner::new("4/5i");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let err = err_or_fail!(r);
        assert!(matches!(
            err,
            TokenError {
                kind: TokenErrorKind::ImaginaryMissingSign,
                span: Range { start: 0, end: 4 },
            }
        ));
    }

    #[test]
    fn imaginary_numerator() {
        let mut s = Scanner::new("+4i/5");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let err = err_or_fail!(r);
        assert!(matches!(
            err,
            TokenError {
                kind: TokenErrorKind::ImaginaryInvalid,
                span: Range { start: 0, end: 5 },
            }
        ));
    }

    #[test]
    fn rational_imaginary_missing_denominator_digit() {
        let mut s = Scanner::new("+4/i");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let err = err_or_fail!(r);
        assert!(matches!(
            err,
            TokenError {
                kind: TokenErrorKind::RationalInvalid,
                span: Range { start: 0, end: 4 },
            }
        ));
    }

    #[test]
    fn float_imaginary() {
        let mut s = Scanner::new("+4.5i");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Imaginary(_),
                span: Range { start: 0, end: 5 },
            }
        ));
        let r = extract_or_fail!(tok.kind, TokenKind::Imaginary);
        assert_eq!(r.to_string(), "4.5");
    }

    #[test]
    fn exponent_imaginary() {
        let mut s = Scanner::new("+4.5e4i");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Imaginary(_),
                span: Range { start: 0, end: 7 },
            }
        ));
        let r = extract_or_fail!(tok.kind, TokenKind::Imaginary);
        assert_eq!(r.to_string(), "45000.0");
    }

    #[test]
    fn positive_exponent_imaginary() {
        let mut s = Scanner::new("+4.5e+4i");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Imaginary(_),
                span: Range { start: 0, end: 8 },
            }
        ));
        let r = extract_or_fail!(tok.kind, TokenKind::Imaginary);
        assert_eq!(r.to_string(), "45000.0");
    }

    #[test]
    fn negative_exponent_imaginary() {
        let mut s = Scanner::new("+4.5e-4i");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Imaginary(_),
                span: Range { start: 0, end: 8 },
            }
        ));
        let r = extract_or_fail!(tok.kind, TokenKind::Imaginary);
        assert_eq!(r.to_string(), "0.00045");
    }

    #[test]
    fn invalid_exponent_imaginary() {
        let mut s = Scanner::new("+4.5ei");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let err = err_or_fail!(r);
        assert!(matches!(
            err,
            TokenError {
                kind: TokenErrorKind::NumericErrorAt {
                    at: 4,
                    err: NumericError::ParseExponentFailure
                },
                span: Range { start: 4, end: 6 },
            }
        ));
    }

    #[test]
    fn negative_imaginary() {
        let mut s = Scanner::new("-i");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Imaginary(_),
                span: Range { start: 0, end: 2 },
            }
        ));
        let r = extract_or_fail!(tok.kind, TokenKind::Imaginary);
        assert_eq!(r.to_string(), "-1");
    }

    #[test]
    fn negative_integer_imaginary() {
        let mut s = Scanner::new("-5i");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Imaginary(_),
                span: Range { start: 0, end: 3 },
            }
        ));
        let r = extract_or_fail!(tok.kind, TokenKind::Imaginary);
        assert_eq!(r.to_string(), "-5");
    }

    #[test]
    fn positive_infinite_imaginary() {
        let mut s = Scanner::new("+inf.0i");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Imaginary(_),
                span: Range { start: 0, end: 7 },
            }
        ));
        let r = extract_or_fail!(tok.kind, TokenKind::Imaginary);
        assert_eq!(r.to_string(), "+inf.0");
    }

    #[test]
    fn upper_positive_infinite_imaginary() {
        let mut s = Scanner::new("+INF.0I");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Imaginary(_),
                span: Range { start: 0, end: 7 },
            }
        ));
        let r = extract_or_fail!(tok.kind, TokenKind::Imaginary);
        assert_eq!(r.to_string(), "+inf.0");
    }

    #[test]
    fn negative_infinite_imaginary() {
        let mut s = Scanner::new("-inf.0i");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Imaginary(_),
                span: Range { start: 0, end: 7 },
            }
        ));
        let r = extract_or_fail!(tok.kind, TokenKind::Imaginary);
        assert_eq!(r.to_string(), "-inf.0");
    }

    #[test]
    fn positive_nan_imaginary() {
        let mut s = Scanner::new("+nan.0i");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Imaginary(_),
                span: Range { start: 0, end: 7 },
            }
        ));
        let r = extract_or_fail!(tok.kind, TokenKind::Imaginary);
        assert_eq!(r.to_string(), "+nan.0");
    }

    #[test]
    fn upper_positive_nan_imaginary() {
        let mut s = Scanner::new("+NAN.0I");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Imaginary(_),
                span: Range { start: 0, end: 7 },
            }
        ));
        let r = extract_or_fail!(tok.kind, TokenKind::Imaginary);
        assert_eq!(r.to_string(), "+nan.0");
    }

    #[test]
    fn negative_nan_imaginary() {
        let mut s = Scanner::new("-nan.0i");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Imaginary(_),
                span: Range { start: 0, end: 7 },
            }
        ));
        let r = extract_or_fail!(tok.kind, TokenKind::Imaginary);
        assert_eq!(r.to_string(), "+nan.0");
    }

    #[test]
    fn imaginary_in_real_position() {
        let mut s = Scanner::new("4i+3");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let err = err_or_fail!(r);
        assert!(matches!(
            err,
            TokenError {
                kind: TokenErrorKind::ImaginaryInvalid,
                span: Range { start: 0, end: 4 },
            }
        ));
    }
}

mod cartesian {
    use super::*;

    #[test]
    fn simple() {
        let mut s = Scanner::new("4+3i");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(_),
                span: Range { start: 0, end: 4 },
            }
        ));
        let num = extract_number!(tok.kind);
        assert_eq!(num.as_datum().to_string(), "4+3i");
    }

    #[test]
    fn unity_imaginary_part() {
        let mut s = Scanner::new("4+i");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(_),
                span: Range { start: 0, end: 3 },
            }
        ));
        let num = extract_number!(tok.kind);
        assert_eq!(num.as_datum().to_string(), "4+i");
    }

    #[test]
    fn explicit_unity_imaginary_part() {
        let mut s = Scanner::new("4+1i");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(_),
                span: Range { start: 0, end: 4 },
            }
        ));
        let num = extract_number!(tok.kind);
        assert_eq!(num.as_datum().to_string(), "4+i");
    }

    #[test]
    fn zero_imaginary_part() {
        let mut s = Scanner::new("4+0i");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(_),
                span: Range { start: 0, end: 4 },
            }
        ));
        let num = extract_number!(tok.kind);
        assert_eq!(num.as_datum().to_string(), "4");
    }

    #[test]
    fn negative_zero_imaginary_part() {
        let mut s = Scanner::new("4-0i");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(_),
                span: Range { start: 0, end: 4 },
            }
        ));
        let num = extract_number!(tok.kind);
        assert_eq!(num.as_datum().to_string(), "4");
    }

    #[test]
    fn zero_real_part() {
        let mut s = Scanner::new("0+3i");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(_),
                span: Range { start: 0, end: 4 },
            }
        ));
        let num = extract_number!(tok.kind);
        assert_eq!(num.as_datum().to_string(), "+3i");
    }

    #[test]
    fn negative_zero_real_part() {
        let mut s = Scanner::new("-0+3i");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(_),
                span: Range { start: 0, end: 5 },
            }
        ));
        let num = extract_number!(tok.kind);
        assert_eq!(num.as_datum().to_string(), "+3i");
    }

    #[test]
    fn explicit_positive() {
        let mut s = Scanner::new("+4+3i");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(_),
                span: Range { start: 0, end: 5 },
            }
        ));
        let num = extract_number!(tok.kind);
        assert_eq!(num.as_datum().to_string(), "4+3i");
    }

    #[test]
    fn negative_real() {
        let mut s = Scanner::new("-4+3i");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(_),
                span: Range { start: 0, end: 5 },
            }
        ));
        let num = extract_number!(tok.kind);
        assert_eq!(num.as_datum().to_string(), "-4+3i");
    }

    #[test]
    fn negative_imaginary() {
        let mut s = Scanner::new("4-3i");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(_),
                span: Range { start: 0, end: 4 },
            }
        ));
        let num = extract_number!(tok.kind);
        assert_eq!(num.as_datum().to_string(), "4-3i");
    }

    #[test]
    fn negative_complex() {
        let mut s = Scanner::new("-4-3i");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(_),
                span: Range { start: 0, end: 5 },
            }
        ));
        let num = extract_number!(tok.kind);
        assert_eq!(num.as_datum().to_string(), "-4-3i");
    }

    #[test]
    fn combos() {
        let reals = ["4", "3.5", "4.2e40", "1/6"];
        let imags = ["5", "12.2", "5.6e-20", "7/3"];
        let combos = reals
            .into_iter()
            .flat_map(|r| imags.into_iter().map(move |i| format!("{r}+{i}i")));
        for cpx in combos {
            let mut s = Scanner::new(&cpx);
            let start = some_or_fail!(s.next_token());
            let t = Tokenizer {
                scan: &mut s,
                start,
            };

            let (r, c) = t.extract();

            assert!(c.is_none());
            let tok = ok_or_fail!(r);
            assert!(matches!(
                tok,
                Token {
                    kind: TokenKind::Literal(_),
                    span: Range { start: 0, end },
                } if end == cpx.len()
            ));
            let num = extract_number!(tok.kind);
            assert_eq!(num.as_datum().to_string(), cpx);
        }
    }

    #[test]
    fn unity_inf_nan_combos() {
        let combos = [
            "4+i",
            "4-i",
            "4+inf.0i",
            "4-inf.0i",
            "4+nan.0i",
            "4-nan.0i",
            "+inf.0+4i",
            "-inf.0+4i",
            "+nan.0+4i",
            "-nan.0+4i",
            "+inf.0+inf.0i",
            "-inf.0-inf.0i",
            "+nan.0+nan.0i",
            "-nan.0-nan.0i",
            "+inf.0+nan.0i",
            "+nan.0+inf.0i",
            "-inf.0-nan.0i",
            "-nan.0-inf.0i",
            "+inf.0+i",
            "-inf.0-i",
            "+nan.0+i",
            "-nan.0-i",
        ];
        for cpx in combos {
            let mut s = Scanner::new(cpx);
            let start = some_or_fail!(s.next_token());
            let t = Tokenizer {
                scan: &mut s,
                start,
            };

            let (r, c) = t.extract();

            assert!(c.is_none());
            let tok = ok_or_fail!(r);
            assert!(matches!(
                tok,
                Token {
                    kind: TokenKind::Literal(_),
                    span: Range { start: 0, end },
                } if end == cpx.len()
            ));
            let num = extract_number!(tok.kind);
            let expected = if cpx.contains("-nan") {
                cpx.replace("-nan", "+nan")
            } else {
                cpx.to_owned()
            };
            assert_eq!(num.as_datum().to_string(), expected);
        }
    }

    #[test]
    fn radix_and_sign() {
        let radix = [
            ("#b", "101010", "101011"),
            ("#o", "52", "53"),
            ("#d", "42", "43"),
            ("#x", "2a", "2b"),
        ];
        let sign = ["", "+", "-"];
        let combos = sign.into_iter().flat_map(|s| {
            radix.map(|r| {
                format!(
                    "{}{s}{}{}{}i",
                    r.0,
                    r.1,
                    if s.is_empty() { "+" } else { s },
                    r.2
                )
            })
        });
        for case in combos {
            let mut s = Scanner::new(&case);
            let start = some_or_fail!(s.next_token());
            let t = Tokenizer {
                scan: &mut s,
                start,
            };

            let (r, c) = t.extract();

            assert!(c.is_none());
            let tok = ok_or_fail!(r);
            assert!(matches!(
                tok,
                Token {
                    kind: TokenKind::Literal(Literal::Number(_)),
                    span: Range { start: 0, end },
                } if end == case.len()
            ));
            let num = extract_number!(tok.kind);
            let expected = if case.contains('-') {
                "-42-43i"
            } else {
                "42+43i"
            };
            assert_eq!(num.as_datum().to_string(), expected);
        }
    }

    #[test]
    fn rational_radix() {
        let mut s = Scanner::new("#b100/101+10/11i");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(_),
                span: Range { start: 0, end: 16 },
            }
        ));
        let num = extract_number!(tok.kind);
        assert_eq!(num.as_datum().to_string(), "4/5+2/3i");
    }

    #[test]
    fn exact_and_radix() {
        let cases = ["#e#xb+ai", "#E#XB+Ai"];
        for case in cases {
            let mut s = Scanner::new(case);
            let start = some_or_fail!(s.next_token());
            let t = Tokenizer {
                scan: &mut s,
                start,
            };

            let (r, c) = t.extract();

            assert!(c.is_none());
            let tok = ok_or_fail!(r);
            assert!(matches!(
                tok,
                Token {
                    kind: TokenKind::Literal(_),
                    span: Range { start: 0, end: 8 },
                }
            ));
            let num = extract_number!(tok.kind);
            assert_eq!(num.as_datum().to_string(), "11+10i");
        }
    }

    #[test]
    fn radix_and_exact() {
        let cases = ["#x#eb+ai", "#X#EB+Ai"];
        for case in cases {
            let mut s = Scanner::new(case);
            let start = some_or_fail!(s.next_token());
            let t = Tokenizer {
                scan: &mut s,
                start,
            };

            let (r, c) = t.extract();

            assert!(c.is_none());
            let tok = ok_or_fail!(r);
            assert!(matches!(
                tok,
                Token {
                    kind: TokenKind::Literal(_),
                    span: Range { start: 0, end: 8 },
                }
            ));
            let num = extract_number!(tok.kind);
            assert_eq!(num.as_datum().to_string(), "11+10i");
        }
    }

    #[test]
    fn inexact_and_radix() {
        let cases = ["#i#d3.5+12.2i", "#I#D3.5+12.2i"];
        for case in cases {
            let mut s = Scanner::new(case);
            let start = some_or_fail!(s.next_token());
            let t = Tokenizer {
                scan: &mut s,
                start,
            };

            let (r, c) = t.extract();

            assert!(c.is_none());
            let tok = ok_or_fail!(r);
            assert!(matches!(
                tok,
                Token {
                    kind: TokenKind::Literal(_),
                    span: Range { start: 0, end: 13 },
                }
            ));
            let num = extract_number!(tok.kind);
            assert_eq!(num.as_datum().to_string(), "3.5+12.2i");
        }
    }

    #[test]
    fn radix_and_inexact() {
        let cases = ["#d#i3.5+12.2i", "#D#I3.5+12.2i"];
        for case in cases {
            let mut s = Scanner::new(case);
            let start = some_or_fail!(s.next_token());
            let t = Tokenizer {
                scan: &mut s,
                start,
            };

            let (r, c) = t.extract();

            assert!(c.is_none());
            let tok = ok_or_fail!(r);
            assert!(matches!(
                tok,
                Token {
                    kind: TokenKind::Literal(_),
                    span: Range { start: 0, end: 13 },
                }
            ));
            let num = extract_number!(tok.kind);
            assert_eq!(num.as_datum().to_string(), "3.5+12.2i");
        }
    }

    #[test]
    fn applied_inexact() {
        let mut s = Scanner::new("#i4+3/2i");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(_),
                span: Range { start: 0, end: 8 },
            }
        ));
        let num = extract_number!(tok.kind);
        assert_eq!(num.as_datum().to_string(), "4.0+1.5i");
    }

    #[test]
    fn applied_exact() {
        let mut s = Scanner::new("#e4.2+3.2e3i");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(_),
                span: Range { start: 0, end: 12 },
            }
        ));
        let num = extract_number!(tok.kind);
        assert_eq!(num.as_datum().to_string(), "21/5+3200i");
    }

    #[test]
    fn applied_inexact_radix() {
        let mut s = Scanner::new("#i#xc+a/bi");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(_),
                span: Range { start: 0, end: 10 },
            }
        ));
        let num = extract_number!(tok.kind);
        assert_eq!(num.as_datum().to_string(), "12.0+0.9090909090909091i");
    }

    #[test]
    fn missing_imaginary() {
        let mut s = Scanner::new("4+");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let err = err_or_fail!(r);
        assert!(matches!(
            err,
            TokenError {
                kind: TokenErrorKind::ComplexInvalid,
                span: Range { start: 0, end: 2 },
            }
        ));
    }

    #[test]
    fn missing_imaginary_with_exponent() {
        let mut s = Scanner::new("4e2+");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let err = err_or_fail!(r);
        assert!(matches!(
            err,
            TokenError {
                kind: TokenErrorKind::ComplexInvalid,
                span: Range { start: 0, end: 4 },
            }
        ));
    }

    #[test]
    fn exponent_real_invalid_imaginary() {
        let mut s = Scanner::new("3.456e+4-5");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let err = err_or_fail!(r);
        assert!(matches!(
            err,
            TokenError {
                kind: TokenErrorKind::ComplexInvalid,
                span: Range { start: 0, end: 10 },
            }
        ));
    }

    #[test]
    fn recursive_complex() {
        let mut s = Scanner::new("4+3+2i");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let err = err_or_fail!(r);
        assert!(matches!(
            err,
            TokenError {
                kind: TokenErrorKind::ComplexInvalid,
                span: Range { start: 0, end: 6 },
            }
        ));
    }

    #[test]
    fn invalid_radix_placement() {
        let mut s = Scanner::new("4+#b101i");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let err = err_or_fail!(r);
        assert!(matches!(
            err,
            TokenError {
                kind: TokenErrorKind::ComplexInvalid,
                span: Range { start: 0, end: 2 },
            }
        ));
    }

    #[test]
    fn invalid_radix_imaginary() {
        let mut s = Scanner::new("#b100+5i");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let err = err_or_fail!(r);
        assert!(matches!(
            err,
            TokenError {
                kind: TokenErrorKind::ComplexInvalid,
                span: Range { start: 0, end: 8 },
            }
        ));
    }

    #[test]
    fn invalid_exactness_placement() {
        let mut s = Scanner::new("4+#e3i");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let err = err_or_fail!(r);
        assert!(matches!(
            err,
            TokenError {
                kind: TokenErrorKind::ComplexInvalid,
                span: Range { start: 0, end: 2 },
            }
        ));
    }
}

mod polar {
    use super::*;

    #[test]
    fn simple() {
        let mut s = Scanner::new("4@3");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(_),
                span: Range { start: 0, end: 3 },
            }
        ));
        let num = extract_number!(tok.kind);
        assert_eq!(
            num.as_datum().to_string(),
            "-3.9599699864017817+0.5644800322394689i"
        );
    }

    #[test]
    fn negative_mag() {
        let mut s = Scanner::new("-4@3");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(_),
                span: Range { start: 0, end: 4 },
            }
        ));
        let num = extract_number!(tok.kind);
        assert_eq!(
            num.as_datum().to_string(),
            "3.9599699864017817-0.5644800322394689i"
        );
    }

    #[test]
    fn negative_rads() {
        let mut s = Scanner::new("4@-3");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(_),
                span: Range { start: 0, end: 4 },
            }
        ));
        let num = extract_number!(tok.kind);
        assert_eq!(
            num.as_datum().to_string(),
            "-3.9599699864017817-0.5644800322394689i"
        );
    }

    #[test]
    fn negative() {
        let mut s = Scanner::new("-4@-3");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(_),
                span: Range { start: 0, end: 5 },
            }
        ));
        let num = extract_number!(tok.kind);
        assert_eq!(
            num.as_datum().to_string(),
            "3.9599699864017817+0.5644800322394689i"
        );
    }

    #[test]
    fn zero_magnitude() {
        let mut s = Scanner::new("0@3");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(_),
                span: Range { start: 0, end: 3 },
            }
        ));
        let num = extract_number!(tok.kind);
        assert_eq!(num.as_datum().to_string(), "0");
    }

    #[test]
    fn decimal_zero_magnitude() {
        let mut s = Scanner::new("0.0@3.0");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(_),
                span: Range { start: 0, end: 7 },
            }
        ));
        let num = extract_number!(tok.kind);
        assert_eq!(num.as_datum().to_string(), "0.0");
    }

    #[test]
    fn negative_zero_magnitude() {
        let mut s = Scanner::new("-0@3");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(_),
                span: Range { start: 0, end: 4 },
            }
        ));
        let num = extract_number!(tok.kind);
        assert_eq!(num.as_datum().to_string(), "0");
    }

    #[test]
    fn zero_angle() {
        let mut s = Scanner::new("4@0");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(_),
                span: Range { start: 0, end: 3 },
            }
        ));
        let num = extract_number!(tok.kind);
        assert_eq!(num.as_datum().to_string(), "4");
    }

    #[test]
    fn decimal_zero_angle() {
        let mut s = Scanner::new("4.0@0.0");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(_),
                span: Range { start: 0, end: 7 },
            }
        ));
        let num = extract_number!(tok.kind);
        assert_eq!(num.as_datum().to_string(), "4.0");
    }

    #[test]
    fn negative_zero_angle() {
        let mut s = Scanner::new("4@-0");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(_),
                span: Range { start: 0, end: 4 },
            }
        ));
        let num = extract_number!(tok.kind);
        assert_eq!(num.as_datum().to_string(), "4");
    }

    #[test]
    fn all_zeros() {
        let mut s = Scanner::new("0@0");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(_),
                span: Range { start: 0, end: 3 },
            }
        ));
        let num = extract_number!(tok.kind);
        assert_eq!(num.as_datum().to_string(), "0");
    }

    #[test]
    fn unit_45degrees() {
        let input = format!("1@{}", std::f64::consts::FRAC_PI_4);
        let mut s = Scanner::new(&input);
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(_),
                span: Range { start: 0, end: 20 },
            }
        ));
        let num = extract_number!(tok.kind);
        assert_eq!(
            num.as_datum().to_string(),
            "0.7071067811865476+0.7071067811865475i"
        );
    }

    #[test]
    fn combos() {
        let reals = ["4", "3.5", "4.2e4", "1/6"];
        let imags = ["5", "12.2", "5.6e-2", "7/3"];
        let expected = [
            "1.134648741852905-3.835697098652554i",
            "3.7345345762985493-1.4329171289473148i",
            "3.9937296389113377+0.2238829410230219i",
            "-2.7630325589995057+2.892343526953298i",
            "0.9928176491212919-3.3562349613209848i",
            "3.2677177542612306-1.2538024878289005i",
            "3.4945134340474207+0.19589757339514416i",
            "-2.4176534891245676+2.530800586084136i",
            "11913.811789455502-40274.81953585181i",
            "39212.613051134766-15045.629853946806i",
            "41934.16120856904+2350.77088074173i",
            "-29011.84186949481+30369.60703300963i",
            "0.047277030910537705-0.1598207124438564i",
            "0.1556056073457729-0.05970488037280478i",
            "0.16640540162130574+0.009328455875959245i",
            "-0.1151263566249794+0.12051431362305409i",
        ];
        let combos = reals
            .into_iter()
            .flat_map(|r| imags.into_iter().map(move |i| format!("{r}@{i}")));
        assert_eq!(expected.len(), reals.len() * imags.len());
        for (idx, cpx) in combos.enumerate() {
            let mut s = Scanner::new(&cpx);
            let start = some_or_fail!(s.next_token());
            let t = Tokenizer {
                scan: &mut s,
                start,
            };

            let (r, c) = t.extract();

            assert!(c.is_none());
            let tok = ok_or_fail!(r);
            assert!(matches!(
                tok,
                Token {
                    kind: TokenKind::Literal(_),
                    span: Range { start: 0, end },
                } if end == cpx.len()
            ));
            let num = extract_number!(tok.kind);
            assert_eq!(num.as_datum().to_string(), expected[idx]);
        }
    }

    #[test]
    fn inf_nan_combos() {
        let combos = [
            ("4@+inf.0", "+nan.0+nan.0i"),
            ("4@-inf.0", "+nan.0+nan.0i"),
            ("4@+nan.0", "+nan.0+nan.0i"),
            ("4@-nan.0", "+nan.0+nan.0i"),
            ("+inf.0@4", "-inf.0-inf.0i"),
            ("-inf.0@4", "+inf.0+inf.0i"),
            ("+nan.0@4", "+nan.0+nan.0i"),
            ("-nan.0@4", "+nan.0+nan.0i"),
            ("+inf.0@+inf.0", "+nan.0+nan.0i"),
            ("-inf.0@-inf.0", "+nan.0+nan.0i"),
            ("+nan.0@+nan.0", "+nan.0+nan.0i"),
            ("-nan.0@-nan.0", "+nan.0+nan.0i"),
            ("+inf.0@+nan.0", "+nan.0+nan.0i"),
            ("+nan.0@+inf.0", "+nan.0+nan.0i"),
            ("-inf.0@-nan.0", "+nan.0+nan.0i"),
            ("-nan.0@-inf.0", "+nan.0+nan.0i"),
        ];
        for (cpx, exp) in combos {
            let mut s = Scanner::new(cpx);
            let start = some_or_fail!(s.next_token());
            let t = Tokenizer {
                scan: &mut s,
                start,
            };

            let (r, c) = t.extract();

            assert!(c.is_none());
            let tok = ok_or_fail!(r);
            assert!(matches!(
                tok,
                Token {
                    kind: TokenKind::Literal(_),
                    span: Range { start: 0, end },
                } if end == cpx.len()
            ));
            let num = extract_number!(tok.kind);
            assert_eq!(num.as_datum().to_string(), exp);
        }
    }

    #[test]
    fn radix_and_sign() {
        let radix = [
            ("#b", "101010", "101011"),
            ("#o", "52", "53"),
            ("#d", "42", "43"),
            ("#x", "2a", "2b"),
        ];
        let sign = ["", "+", "-"];
        let combos = sign
            .into_iter()
            .flat_map(|s| radix.map(|r| format!("{}{s}{}@{s}{}", r.0, r.1, r.2)));
        for case in combos {
            let mut s = Scanner::new(&case);
            let start = some_or_fail!(s.next_token());
            let t = Tokenizer {
                scan: &mut s,
                start,
            };

            let (r, c) = t.extract();

            assert!(c.is_none());
            let tok = ok_or_fail!(r);
            assert!(matches!(
                tok,
                Token {
                    kind: TokenKind::Literal(Literal::Number(_)),
                    span: Range { start: 0, end },
                } if end == case.len()
            ));
            let num = extract_number!(tok.kind);
            let expected = if case.contains('-') {
                "-23.31475866386628-34.934539190401125i"
            } else {
                "23.31475866386628-34.934539190401125i"
            };
            assert_eq!(num.as_datum().to_string(), expected);
        }
    }

    #[test]
    fn rational_radix() {
        let mut s = Scanner::new("#b100/101@10/11");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(_),
                span: Range { start: 0, end: 15 },
            }
        ));
        let num = extract_number!(tok.kind);
        assert_eq!(
            num.as_datum().to_string(),
            "0.6287098086215585+0.4946958424557896i"
        );
    }

    #[test]
    fn inexact_and_radix() {
        let cases = ["#i#d3.5@12.2", "#I#D3.5@12.2"];
        for case in cases {
            let mut s = Scanner::new(case);
            let start = some_or_fail!(s.next_token());
            let t = Tokenizer {
                scan: &mut s,
                start,
            };

            let (r, c) = t.extract();

            assert!(c.is_none());
            let tok = ok_or_fail!(r);
            assert!(matches!(
                tok,
                Token {
                    kind: TokenKind::Literal(_),
                    span: Range { start: 0, end: 12 },
                }
            ));
            let num = extract_number!(tok.kind);
            assert_eq!(
                num.as_datum().to_string(),
                "3.2677177542612306-1.2538024878289005i"
            );
        }
    }

    #[test]
    fn radix_and_inexact() {
        let cases = ["#d#i3.5@12.2", "#D#I3.5@12.2"];
        for case in cases {
            let mut s = Scanner::new(case);
            let start = some_or_fail!(s.next_token());
            let t = Tokenizer {
                scan: &mut s,
                start,
            };

            let (r, c) = t.extract();

            assert!(c.is_none());
            let tok = ok_or_fail!(r);
            assert!(matches!(
                tok,
                Token {
                    kind: TokenKind::Literal(_),
                    span: Range { start: 0, end: 12 },
                }
            ));
            let num = extract_number!(tok.kind);
            assert_eq!(
                num.as_datum().to_string(),
                "3.2677177542612306-1.2538024878289005i",
            );
        }
    }

    #[test]
    fn applied_inexact() {
        let cases = ["#i3@4/5", "#i3.0@0.8", "3.0@0.8"];
        for case in cases {
            let mut s = Scanner::new(case);
            let start = some_or_fail!(s.next_token());
            let t = Tokenizer {
                scan: &mut s,
                start,
            };

            let (r, c) = t.extract();

            assert!(c.is_none());
            let tok = ok_or_fail!(r);
            assert!(matches!(
                tok,
                Token {
                    kind: TokenKind::Literal(_),
                    span: Range { start: 0, end },
                } if end == case.len()
            ));
            let num = extract_number!(tok.kind);
            assert_eq!(
                num.as_datum().to_string(),
                "2.0901201280414963+2.1520682726985685i"
            );
        }
    }

    #[test]
    fn applied_inexact_negative_output() {
        let cases = ["#i-3@4/5", "#i-3.0@0.8", "-3.0@0.8"];
        for case in cases {
            let mut s = Scanner::new(case);
            let start = some_or_fail!(s.next_token());
            let t = Tokenizer {
                scan: &mut s,
                start,
            };

            let (r, c) = t.extract();

            assert!(c.is_none());
            let tok = ok_or_fail!(r);
            assert!(matches!(
                tok,
                Token {
                    kind: TokenKind::Literal(_),
                    span: Range { start: 0, end },
                } if end == case.len()
            ));
            let num = extract_number!(tok.kind);
            assert_eq!(
                num.as_datum().to_string(),
                "-2.0901201280414963-2.1520682726985685i"
            );
        }
    }

    #[test]
    fn applied_exact() {
        let cases = ["#e3@4/5", "#e3.0@0.8"];
        for case in cases {
            let mut s = Scanner::new(case);
            let start = some_or_fail!(s.next_token());
            let t = Tokenizer {
                scan: &mut s,
                start,
            };

            let (r, c) = t.extract();

            assert!(c.is_none());
            let tok = ok_or_fail!(r);
            assert!(matches!(
                tok,
                Token {
                    kind: TokenKind::Literal(_),
                    span: Range { start: 0, end },
                } if end == case.len()
            ));
            let num = extract_number!(tok.kind);
            assert_eq!(
                num.as_datum().to_string(),
                "20901201280414963/10000000000000000+4304136545397137/2000000000000000i"
            );
        }
    }

    #[test]
    fn applied_exact_negative_output() {
        let cases = ["#e-3@4/5", "#e-3.0@0.8"];
        for case in cases {
            let mut s = Scanner::new(case);
            let start = some_or_fail!(s.next_token());
            let t = Tokenizer {
                scan: &mut s,
                start,
            };

            let (r, c) = t.extract();

            assert!(c.is_none());
            let tok = ok_or_fail!(r);
            assert!(matches!(
                tok,
                Token {
                    kind: TokenKind::Literal(_),
                    span: Range { start: 0, end },
                } if end == case.len()
            ));
            let num = extract_number!(tok.kind);
            assert_eq!(
                num.as_datum().to_string(),
                "-20901201280414963/10000000000000000-4304136545397137/2000000000000000i"
            );
        }
    }

    #[test]
    fn applied_exact_decimal_zero_magnitude() {
        let mut s = Scanner::new("#e0.0@3.0");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(_),
                span: Range { start: 0, end: 9 },
            }
        ));
        let num = extract_number!(tok.kind);
        assert_eq!(num.as_datum().to_string(), "0");
    }

    #[test]
    fn applied_exact_decimal_zero_angle() {
        let mut s = Scanner::new("#e4.0@0.0");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(_),
                span: Range { start: 0, end: 9 },
            }
        ));
        let num = extract_number!(tok.kind);
        assert_eq!(num.as_datum().to_string(), "4");
    }

    #[test]
    fn applied_inexact_decimal_zero_magnitude() {
        let mut s = Scanner::new("#i0@3");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(_),
                span: Range { start: 0, end: 5 },
            }
        ));
        let num = extract_number!(tok.kind);
        assert_eq!(num.as_datum().to_string(), "0.0");
    }

    #[test]
    fn applied_inexact_decimal_zero_angle() {
        let mut s = Scanner::new("#i4@0");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(_),
                span: Range { start: 0, end: 5 },
            }
        ));
        let num = extract_number!(tok.kind);
        assert_eq!(num.as_datum().to_string(), "4.0");
    }

    #[test]
    fn applied_inexact_radix() {
        let mut s = Scanner::new("#i#xc@a/b");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Literal(_),
                span: Range { start: 0, end: 9 },
            }
        ));
        let num = extract_number!(tok.kind);
        assert_eq!(
            num.as_datum().to_string(),
            "7.373558717381609+9.46734555413109i"
        );
    }

    #[test]
    fn do_not_allow_i() {
        let mut s = Scanner::new("4@3i");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let err = err_or_fail!(r);
        assert!(matches!(
            err,
            TokenError {
                kind: TokenErrorKind::PolarInvalid,
                span: Range { start: 0, end: 4 },
            }
        ));
    }

    #[test]
    fn missing_angle() {
        let mut s = Scanner::new("4@");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let err = err_or_fail!(r);
        assert!(matches!(
            err,
            TokenError {
                kind: TokenErrorKind::PolarInvalid,
                span: Range { start: 0, end: 2 },
            }
        ));
    }

    #[test]
    fn recursive_polar() {
        let mut s = Scanner::new("4@3@2");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let err = err_or_fail!(r);
        assert!(matches!(
            err,
            TokenError {
                kind: TokenErrorKind::PolarInvalid,
                span: Range { start: 0, end: 5 },
            }
        ));
    }

    #[test]
    fn invalid_radix_placement() {
        let mut s = Scanner::new("4@#b101");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let err = err_or_fail!(r);
        assert!(matches!(
            err,
            TokenError {
                kind: TokenErrorKind::PolarInvalid,
                span: Range { start: 0, end: 2 },
            }
        ));
    }

    #[test]
    fn invalid_radix_rads() {
        let mut s = Scanner::new("#b100@5");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let err = err_or_fail!(r);
        assert!(matches!(
            err,
            TokenError {
                kind: TokenErrorKind::PolarInvalid,
                span: Range { start: 0, end: 7 },
            }
        ));
    }

    #[test]
    fn invalid_exactness_placement() {
        let mut s = Scanner::new("4@#e3");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let err = err_or_fail!(r);
        assert!(matches!(
            err,
            TokenError {
                kind: TokenErrorKind::PolarInvalid,
                span: Range { start: 0, end: 2 },
            }
        ));
    }
}

mod identifiers {
    use super::*;

    #[test]
    fn double_sign_number() {
        let mut s = Scanner::new("+-4");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Identifier(txt),
                span: Range { start: 0, end: 3 },
            } if txt == "+-4"
        ));
    }

    #[test]
    fn sign_at_number() {
        let mut s = Scanner::new("+@4");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Identifier(txt),
                span: Range { start: 0, end: 3 },
            } if txt == "+@4"
        ));
    }

    #[test]
    fn sign_double_period_number() {
        let mut s = Scanner::new("+..4");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Identifier(txt),
                span: Range { start: 0, end: 4 },
            } if txt == "+..4"
        ));
    }

    #[test]
    fn sign_period_sign_number() {
        let mut s = Scanner::new("+.-4");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Identifier(txt),
                span: Range { start: 0, end: 4 },
            } if txt == "+.-4"
        ));
    }

    #[test]
    fn double_period_number() {
        let mut s = Scanner::new("..4");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Identifier(txt),
                span: Range { start: 0, end: 3 },
            } if txt == "..4"
        ));
    }

    #[test]
    fn period_sign_is_identifier() {
        let mut s = Scanner::new(".-4");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Identifier(txt),
                span: Range { start: 0, end: 3 },
            } if txt == ".-4"
        ));
    }

    #[test]
    fn period_at_number() {
        let mut s = Scanner::new(".@4");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Identifier(txt),
                span: Range { start: 0, end: 3 },
            } if txt == ".@4"
        ));
    }

    #[test]
    fn at_number() {
        let mut s = Scanner::new("@4");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Identifier(txt),
                span: Range { start: 0, end: 2 },
            } if txt == "@4"
        ));
    }

    #[test]
    fn nosign_infinity() {
        let mut s = Scanner::new("inf.0");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Identifier(txt),
                span: Range { start: 0, end: 5 },
            } if txt == "inf.0"
        ));
    }

    #[test]
    fn inf_followed_by_signed_identifier() {
        let mut s = Scanner::new("+inf.0+foo");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Identifier(txt),
                span: Range { start: 0, end: 10 },
            } if txt == "+inf.0+foo"
        ));
    }

    #[test]
    fn inf_followed_by_signed_invalid_number() {
        let mut s = Scanner::new("+inf.0+4.2.2i");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Identifier(txt),
                span: Range { start: 0, end: 13 },
            } if txt == "+inf.0+4.2.2i"
        ));
    }

    #[test]
    fn inf_followed_by_sign() {
        let mut s = Scanner::new("+inf.0+");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Identifier(txt),
                span: Range { start: 0, end: 7 },
            } if txt == "+inf.0+"
        ));
    }

    #[test]
    fn inf_followed_by_polar_identifier() {
        let mut s = Scanner::new("+inf.0@foo");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Identifier(txt),
                span: Range { start: 0, end: 10 },
            } if txt == "+inf.0@foo"
        ));
    }

    #[test]
    fn inf_end_in_polar_symbol() {
        let mut s = Scanner::new("+inf.0@");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Identifier(txt),
                span: Range { start: 0, end: 7 },
            } if txt == "+inf.0@"
        ));
    }

    #[test]
    fn inf_followed_by_polar_imaginary() {
        let mut s = Scanner::new("+inf.0@4i");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Identifier(txt),
                span: Range { start: 0, end: 9 },
            } if txt == "+inf.0@4i"
        ));
    }

    #[test]
    fn inf_followed_by_polar_invalid_number() {
        let mut s = Scanner::new("+inf.0@4.2.2");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Identifier(txt),
                span: Range { start: 0, end: 12 },
            } if txt == "+inf.0@4.2.2"
        ));
    }

    #[test]
    fn nosign_nan() {
        let mut s = Scanner::new("nan.0");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Identifier(txt),
                span: Range { start: 0, end: 5 },
            } if txt == "nan.0"
        ));
    }

    #[test]
    fn nan_followed_by_signed_identifier() {
        let mut s = Scanner::new("+nan.0-bar");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Identifier(txt),
                span: Range { start: 0, end: 10 },
            } if txt == "+nan.0-bar"
        ));
    }

    #[test]
    fn nan_followed_by_polar_identifier() {
        let mut s = Scanner::new("+nan.0@bar");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Identifier(txt),
                span: Range { start: 0, end: 10 },
            } if txt == "+nan.0@bar"
        ));
    }

    #[test]
    fn nan_followed_by_polar_imaginary() {
        let mut s = Scanner::new("+nan.0@4i");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Identifier(txt),
                span: Range { start: 0, end: 9 },
            } if txt == "+nan.0@4i"
        ));
    }

    #[test]
    fn imag_sign_number() {
        let mut s = Scanner::new("+i+4");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Identifier(txt),
                span: Range { start: 0, end: 4 },
            } if txt == "+i+4"
        ));
    }

    #[test]
    fn imag_at_number() {
        let mut s = Scanner::new("+i@4");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Identifier(txt),
                span: Range { start: 0, end: 4 },
            } if txt == "+i@4"
        ));
    }

    #[test]
    fn just_i() {
        let mut s = Scanner::new("i");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Identifier(txt),
                span: Range { start: 0, end: 1 },
            } if txt == "i"
        ));
    }

    #[test]
    fn sign_period_i() {
        let mut s = Scanner::new("+.i");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Identifier(txt),
                span: Range { start: 0, end: 3 },
            } if txt == "+.i"
        ));
    }

    #[test]
    fn nosign_imaginary() {
        let mut s = Scanner::new("i");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Identifier(txt),
                span: Range { start: 0, end: 1 },
            } if txt == "i"
        ));
    }

    #[test]
    fn infinite_imaginary_extra_letters() {
        let mut s = Scanner::new("+inf.0ifni");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Identifier(txt),
                span: Range { start: 0, end: 10 },
            } if txt == "+inf.0ifni"
        ));
    }

    #[test]
    fn nan_imaginary_extra_letters() {
        let mut s = Scanner::new("+nan.0inon");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::Identifier(txt),
                span: Range { start: 0, end: 10 },
            } if txt == "+nan.0inon"
        ));
    }

    #[test]
    fn inf_nan_invalid_identifier() {
        let cases = [
            "+inf.0+foo{bar",
            "+nan.0-bar{foo",
            "+inf.0@foo{bar",
            "+nan.0@bar{foo",
        ];
        for case in cases {
            let mut s = Scanner::new(case);
            let start = some_or_fail!(s.next_token());
            let t = Tokenizer {
                scan: &mut s,
                start,
            };

            let (r, c) = t.extract();

            assert!(c.is_none());
            let err = err_or_fail!(r);
            assert!(matches!(
                err,
                TokenError {
                    kind: TokenErrorKind::IdentifierInvalid('{'),
                    span: Range { start: 0, end },
                } if end == case.len()
            ));
        }
    }
}
