use super::*;
use crate::{
    number::{Number, Real},
    testutil::{extract_or_fail, ok_or_fail},
};

macro_rules! extract_number {
    ($exp:expr, $complex:path) => {{
        let result = $exp;
        let tok = ok_or_fail!(result);
        let lit = extract_or_fail!(tok, TokenKind::Literal);
        let num = extract_or_fail!(lit, Literal::Number);
        extract_or_fail!(num, $complex)
    }};
    ($exp:expr, $real:path, $kind:path) => {{
        let result = $exp;
        let tok = ok_or_fail!(result);
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 1,
                result: Ok(TokenKind::Literal(Literal::Number(_))),
            }
        ));
        let int = extract_number!(r.result, Number::Real, Real::Integer);
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 2,
                result: Ok(TokenKind::Literal(Literal::Number(_))),
            }
        ));
        let int = extract_number!(r.result, Number::Real, Real::Integer);
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 2,
                result: Ok(TokenKind::Literal(Literal::Number(_))),
            }
        ));
        let int = extract_number!(r.result, Number::Real, Real::Integer);
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 1,
                result: Ok(TokenKind::Literal(Literal::Number(_))),
            }
        ));
        let int = extract_number!(r.result, Number::Real, Real::Integer);
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 2,
                result: Ok(TokenKind::Literal(Literal::Number(_))),
            }
        ));
        let int = extract_number!(r.result, Number::Real, Real::Integer);
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 19,
                result: Ok(TokenKind::Literal(Literal::Number(_))),
            }
        ));
        let int = extract_number!(r.result, Number::Real, Real::Integer);
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 20,
                result: Ok(TokenKind::Literal(Literal::Number(_))),
            }
        ));
        let int = extract_number!(r.result, Number::Real, Real::Integer);
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 20,
                result: Ok(TokenKind::Literal(Literal::Number(_))),
            }
        ));
        let int = extract_number!(r.result, Number::Real, Real::Integer);
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 21,
                result: Ok(TokenKind::Literal(Literal::Number(_))),
            }
        ));
        let int = extract_number!(r.result, Number::Real, Real::Integer);
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 21,
                result: Ok(TokenKind::Literal(Literal::Number(_))),
            }
        ));
        let int = extract_number!(r.result, Number::Real, Real::Integer);
        assert_eq!(int.to_string(), "-18446744073709551615");
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 3,
                result: Ok(TokenKind::Literal(Literal::Number(_))),
            }
        ));
        let rat = extract_number!(r.result, Number::Real, Real::Rational);
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 4,
                result: Ok(TokenKind::Literal(Literal::Number(_))),
            }
        ));
        let rat = extract_number!(r.result, Number::Real, Real::Rational);
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 3,
                result: Ok(TokenKind::Literal(Literal::Number(_))),
            }
        ));
        let rat = extract_number!(r.result, Number::Real, Real::Integer);
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 3,
                result: Err(TokenErrorKind::Unimplemented(_)),
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 4,
                result: Ok(TokenKind::Literal(Literal::Number(_))),
            }
        ));
        let rat = extract_number!(r.result, Number::Real, Real::Rational);
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 4,
                result: Ok(TokenKind::Literal(Literal::Number(_))),
            }
        ));
        let rat = extract_number!(r.result, Number::Real, Real::Integer);
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 5,
                result: Ok(TokenKind::Literal(Literal::Number(_))),
            }
        ));
        let rat = extract_number!(r.result, Number::Real, Real::Integer);
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 4,
                result: Ok(TokenKind::Literal(Literal::Number(_))),
            }
        ));
        let rat = extract_number!(r.result, Number::Real, Real::Integer);
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 22,
                result: Ok(TokenKind::Literal(Literal::Number(_))),
            }
        ));
        let rat = extract_number!(r.result, Number::Real, Real::Rational);
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 4,
                result: Ok(TokenKind::Literal(Literal::Number(_))),
            }
        ));
        let rat = extract_number!(r.result, Number::Real, Real::Rational);
        assert_eq!(rat.to_string(), "-4/5");
    }

    #[test]
    fn negative_denominator() {
        let mut s = Scanner::new("4/-5");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 4,
                result: Err(TokenErrorKind::Unimplemented(_)),
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 4,
                result: Err(TokenErrorKind::Unimplemented(_)),
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 5,
                result: Err(TokenErrorKind::Unimplemented(_)),
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 5,
                result: Err(TokenErrorKind::Unimplemented(_)),
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 5,
                result: Err(TokenErrorKind::Unimplemented(_)),
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 5,
                result: Err(TokenErrorKind::Unimplemented(_)),
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 5,
                result: Ok(TokenKind::Literal(Literal::Number(_))),
            }
        ));
        let flt = extract_number!(r.result, Number::Real);
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 6,
                result: Ok(TokenKind::Literal(Literal::Number(_))),
            }
        ));
        let flt = extract_number!(r.result, Number::Real);
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 6,
                result: Ok(TokenKind::Literal(Literal::Number(_))),
            }
        ));
        let flt = extract_number!(r.result, Number::Real);
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 2,
                result: Ok(TokenKind::Literal(Literal::Number(_))),
            }
        ));
        let flt = extract_number!(r.result, Number::Real);
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 3,
                result: Ok(TokenKind::Literal(Literal::Number(_))),
            }
        ));
        let flt = extract_number!(r.result, Number::Real);
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 3,
                result: Ok(TokenKind::Literal(Literal::Number(_))),
            }
        ));
        let flt = extract_number!(r.result, Number::Real);
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 2,
                result: Ok(TokenKind::Literal(Literal::Number(_))),
            }
        ));
        let flt = extract_number!(r.result, Number::Real);
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 4,
                result: Ok(TokenKind::Literal(Literal::Number(_))),
            }
        ));
        let flt = extract_number!(r.result, Number::Real);
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 5,
                result: Ok(TokenKind::Literal(Literal::Number(_))),
            }
        ));
        let flt = extract_number!(r.result, Number::Real);
        assert_eq!(flt.to_string(), "340000.0");
    }

    #[test]
    fn upper_exponent() {
        let mut s = Scanner::new("34E4");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 4,
                result: Ok(TokenKind::Literal(Literal::Number(_))),
            }
        ));
        let flt = extract_number!(r.result, Number::Real);
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 5,
                result: Ok(TokenKind::Literal(Literal::Number(_))),
            }
        ));
        let flt = extract_number!(r.result, Number::Real);
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 7,
                result: Ok(TokenKind::Literal(Literal::Number(_))),
            }
        ));
        let flt = extract_number!(r.result, Number::Real);
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 8,
                result: Ok(TokenKind::Literal(Literal::Number(_))),
            }
        ));
        let flt = extract_number!(r.result, Number::Real);
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 9,
                result: Ok(TokenKind::Literal(Literal::Number(_))),
            }
        ));
        let flt = extract_number!(r.result, Number::Real);
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 10,
                result: Ok(TokenKind::Literal(Literal::Number(_))),
            }
        ));
        let flt = extract_number!(r.result, Number::Real);
        assert_eq!(flt.to_string(), "0.0");
    }

    #[test]
    fn inexact_integer() {
        let mut s = Scanner::new("#i5");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 2,
                result: Ok(TokenKind::Literal(Literal::Number(_))),
            }
        ));
        let flt = extract_number!(r.result, Number::Real);
        assert_eq!(flt.to_string(), "5.0");
    }

    #[test]
    fn positive_inexact_integer() {
        let mut s = Scanner::new("#i+5");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 2,
                result: Ok(TokenKind::Literal(Literal::Number(_))),
            }
        ));
        let flt = extract_number!(r.result, Number::Real);
        assert_eq!(flt.to_string(), "5.0");
    }

    #[test]
    fn negative_inexact_integer() {
        let mut s = Scanner::new("#i-5");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 2,
                result: Ok(TokenKind::Literal(Literal::Number(_))),
            }
        ));
        let flt = extract_number!(r.result, Number::Real);
        assert_eq!(flt.to_string(), "-5.0");
    }

    #[test]
    fn inexact_rational() {
        let mut s = Scanner::new("#i4/5");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 5,
                result: Ok(TokenKind::Literal(Literal::Number(_))),
            }
        ));
        let flt = extract_number!(r.result, Number::Real);
        assert_eq!(flt.to_string(), "0.8");
    }

    #[test]
    fn positive_infinity() {
        let mut s = Scanner::new("+inf.0");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 6,
                result: Ok(TokenKind::Literal(Literal::Number(_))),
            }
        ));
        let flt = extract_number!(r.result, Number::Real);
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 6,
                result: Ok(TokenKind::Literal(Literal::Number(_))),
            }
        ));
        let flt = extract_number!(r.result, Number::Real);
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 6,
                result: Ok(TokenKind::Literal(Literal::Number(_))),
            }
        ));
        let flt = extract_number!(r.result, Number::Real);
        assert_eq!(flt.to_string(), "-inf.0");
    }

    #[test]
    fn invalid_leading_digits_infinity() {
        let mut s = Scanner::new("+0inf.0");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 7,
                result: Err(TokenErrorKind::NumberInvalid),
            }
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 5,
                result: Ok(TokenKind::Identifier(s)),
            } if s == "inf.0"
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 6,
                result: Ok(TokenKind::Literal(Literal::Number(_))),
            }
        ));
        let flt = extract_number!(r.result, Number::Real);
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 6,
                result: Ok(TokenKind::Literal(Literal::Number(_))),
            }
        ));
        let flt = extract_number!(r.result, Number::Real);
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 7,
                result: Err(TokenErrorKind::NumberInvalid),
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 6,
                result: Ok(TokenKind::Literal(Literal::Number(_))),
            }
        ));
        let flt = extract_number!(r.result, Number::Real);
        assert_eq!(flt.to_string(), "+nan.0");
    }

    #[test]
    fn nosign_nan() {
        let mut s = Scanner::new("nan.0");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 5,
                result: Ok(TokenKind::Identifier(s)),
            } if s == "nan.0"
        ));
    }

    #[test]
    fn invalid_decimal_radix() {
        let mut s = Scanner::new("#b1.01");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 6,
                result: Err(TokenErrorKind::NumberInvalidDecimalPoint { at: 3, radix }),
            } if radix == "binary"
        ));
    }

    #[test]
    fn invalid_exponent_radix() {
        let mut s = Scanner::new("#x101e11");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 6,
                result: Err(TokenErrorKind::NumberInvalidExponent { at: 3, radix }),
            } if radix == "binary"
        ));
    }

    #[test]
    fn unexpected_decimal_point() {
        let mut s = Scanner::new("3.456.23");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 5,
                end: 8,
                result: Err(TokenErrorKind::NumberUnexpectedDecimalPoint { at: 5 }),
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 5,
                end: 6,
                result: Err(TokenErrorKind::NumberMalformedExponent { at: 5 }),
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 1,
                end: 2,
                result: Err(TokenErrorKind::NumberMalformedExponent { at: 1 }),
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 5,
                end: 7,
                result: Err(TokenErrorKind::NumberMalformedExponent { at: 5 }),
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 1,
                end: 3,
                result: Err(TokenErrorKind::NumberMalformedExponent { at: 1 }),
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 7,
                end: 9,
                result: Err(TokenErrorKind::NumberMalformedExponent { at: 7 }),
            }
        ));
    }

    #[test]
    fn exponent_too_many_signs() {
        let mut s = Scanner::new("3.456e+4-5");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 8,
                end: 10,
                result: Err(TokenErrorKind::NumberMalformedExponent { at: 8 }),
            }
        ));
    }
}

mod complex {
    use super::*;

    #[test]
    fn positive_imaginary() {
        let mut s = Scanner::new("+i");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 2,
                result: Ok(TokenKind::Imaginary(_)),
            }
        ));
        let tok = ok_or_fail!(r.result);
        let r = extract_or_fail!(tok, TokenKind::Imaginary);
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 2,
                result: Ok(TokenKind::Imaginary(_)),
            }
        ));
        let tok = ok_or_fail!(r.result);
        let r = extract_or_fail!(tok, TokenKind::Imaginary);
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 2,
                result: Err(TokenErrorKind::ImaginaryMissingSign),
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 4,
                result: Err(TokenErrorKind::NumberInvalid),
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 4,
                result: Err(TokenErrorKind::NumberInvalid),
            }
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 1,
                result: Ok(TokenKind::Identifier(s)),
            } if s == "i"
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 3,
                result: Ok(TokenKind::Identifier(s)),
            } if s == "+.i"
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 3,
                result: Ok(TokenKind::Imaginary(_)),
            }
        ));
        let tok = ok_or_fail!(r.result);
        let r = extract_or_fail!(tok, TokenKind::Imaginary);
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 3,
                result: Ok(TokenKind::Imaginary(_)),
            }
        ));
        let tok = ok_or_fail!(r.result);
        let r = extract_or_fail!(tok, TokenKind::Imaginary);
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 5,
                result: Ok(TokenKind::Imaginary(_)),
            }
        ));
        let tok = ok_or_fail!(r.result);
        let r = extract_or_fail!(tok, TokenKind::Imaginary);
        assert_eq!(r.to_string(), "4/5");
    }

    #[test]
    fn imaginary_numerator() {
        let mut s = Scanner::new("+4i/5");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 5,
                result: Err(TokenErrorKind::Unimplemented(_)),
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 5,
                result: Ok(TokenKind::Imaginary(_)),
            }
        ));
        let tok = ok_or_fail!(r.result);
        let r = extract_or_fail!(tok, TokenKind::Imaginary);
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 7,
                result: Ok(TokenKind::Imaginary(_)),
            }
        ));
        let tok = ok_or_fail!(r.result);
        let r = extract_or_fail!(tok, TokenKind::Imaginary);
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 8,
                result: Ok(TokenKind::Imaginary(_)),
            }
        ));
        let tok = ok_or_fail!(r.result);
        let r = extract_or_fail!(tok, TokenKind::Imaginary);
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 8,
                result: Ok(TokenKind::Imaginary(_)),
            }
        ));
        let tok = ok_or_fail!(r.result);
        let r = extract_or_fail!(tok, TokenKind::Imaginary);
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 4,
                end: 6,
                result: Err(TokenErrorKind::NumberMalformedExponent { at: 4 }),
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 2,
                result: Ok(TokenKind::Imaginary(_)),
            }
        ));
        let tok = ok_or_fail!(r.result);
        let r = extract_or_fail!(tok, TokenKind::Imaginary);
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 3,
                result: Ok(TokenKind::Imaginary(_)),
            }
        ));
        let tok = ok_or_fail!(r.result);
        let r = extract_or_fail!(tok, TokenKind::Imaginary);
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 7,
                result: Ok(TokenKind::Imaginary(_)),
            }
        ));
        let tok = ok_or_fail!(r.result);
        let r = extract_or_fail!(tok, TokenKind::Imaginary);
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 7,
                result: Ok(TokenKind::Imaginary(_)),
            }
        ));
        let tok = ok_or_fail!(r.result);
        let r = extract_or_fail!(tok, TokenKind::Imaginary);
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 7,
                result: Ok(TokenKind::Imaginary(_)),
            }
        ));
        let tok = ok_or_fail!(r.result);
        let r = extract_or_fail!(tok, TokenKind::Imaginary);
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 7,
                result: Ok(TokenKind::Imaginary(_)),
            }
        ));
        let tok = ok_or_fail!(r.result);
        let r = extract_or_fail!(tok, TokenKind::Imaginary);
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 7,
                result: Ok(TokenKind::Imaginary(_)),
            }
        ));
        let tok = ok_or_fail!(r.result);
        let r = extract_or_fail!(tok, TokenKind::Imaginary);
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 7,
                result: Ok(TokenKind::Imaginary(_)),
            }
        ));
        let tok = ok_or_fail!(r.result);
        let r = extract_or_fail!(tok, TokenKind::Imaginary);
        assert_eq!(r.to_string(), "+nan.0");
    }

    #[test]
    fn nosign_imaginary() {
        let mut s = Scanner::new("i");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 1,
                result: Ok(TokenKind::Identifier(s)),
            } if s == "i"
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 10,
                result: Ok(TokenKind::Identifier(s)),
            } if s == "+inf.0ifni"
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 10,
                result: Ok(TokenKind::Identifier(s)),
            } if s == "+nan.0inon"
        ));
    }
}
