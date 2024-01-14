use super::*;
use crate::{
    number::{Number, Real},
    testutil::{extract_or_fail, ok_or_fail},
};

macro_rules! extract_number {
    ($exp:expr) => {{
        let result = $exp;
        let tok = ok_or_fail!(result);
        let lit = extract_or_fail!(tok, TokenKind::Literal);
        extract_or_fail!(lit, Literal::Number)
    }};
    ($exp:expr, $real:path) => {{
        let result = $exp;
        let tok = ok_or_fail!(result);
        let lit = extract_or_fail!(tok, TokenKind::Literal);
        let num = extract_or_fail!(lit, Literal::Number);
        extract_or_fail!(num, $real)
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
    use crate::number::NumericError;

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
                result: Err(TokenErrorKind::NumericError(NumericError::DivideByZero)),
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
                end: 23,
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
                result: Err(TokenErrorKind::RationalInvalid),
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
                result: Err(TokenErrorKind::RationalInvalid),
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
                result: Err(TokenErrorKind::RationalInvalid),
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 2,
                result: Err(TokenErrorKind::RationalInvalid),
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
                result: Err(TokenErrorKind::RationalInvalid),
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
                result: Err(TokenErrorKind::RationalInvalid),
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
                result: Err(TokenErrorKind::RationalInvalid),
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
                start: 5,
                end: 9,
                result: Err(TokenErrorKind::NumberMalformedExponent { at: 5 }),
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 5,
                end: 8,
                result: Err(TokenErrorKind::NumberMalformedExponent { at: 5 }),
            }
        ));
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
                result: Err(TokenErrorKind::ImaginaryInvalid),
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
                result: Err(TokenErrorKind::ImaginaryInvalid),
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
    fn rational_imaginary_unity_denominator() {
        let mut s = Scanner::new("+4/1i");
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
        assert_eq!(r.to_string(), "4");
    }

    #[test]
    fn rational_imaginary_missing_sign() {
        let mut s = Scanner::new("4/5i");
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
                result: Err(TokenErrorKind::ImaginaryMissingSign),
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 5,
                result: Err(TokenErrorKind::ImaginaryInvalid),
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 4,
                result: Err(TokenErrorKind::RationalInvalid),
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
    fn imaginary_in_real_position() {
        let mut s = Scanner::new("4i+3");
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
                result: Err(TokenErrorKind::ImaginaryInvalid),
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 4,
                result: Ok(TokenKind::Literal(_)),
            }
        ));
        let num = extract_number!(r.result);
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 3,
                result: Ok(TokenKind::Literal(_)),
            }
        ));
        let num = extract_number!(r.result);
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 4,
                result: Ok(TokenKind::Literal(_)),
            }
        ));
        let num = extract_number!(r.result);
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 4,
                result: Ok(TokenKind::Literal(_)),
            }
        ));
        let num = extract_number!(r.result);
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 4,
                result: Ok(TokenKind::Literal(_)),
            }
        ));
        let num = extract_number!(r.result);
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 4,
                result: Ok(TokenKind::Literal(_)),
            }
        ));
        let num = extract_number!(r.result);
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 5,
                result: Ok(TokenKind::Literal(_)),
            }
        ));
        let num = extract_number!(r.result);
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 5,
                result: Ok(TokenKind::Literal(_)),
            }
        ));
        let num = extract_number!(r.result);
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 5,
                result: Ok(TokenKind::Literal(_)),
            }
        ));
        let num = extract_number!(r.result);
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 4,
                result: Ok(TokenKind::Literal(_)),
            }
        ));
        let num = extract_number!(r.result);
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 5,
                result: Ok(TokenKind::Literal(_)),
            }
        ));
        let num = extract_number!(r.result);
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

            let r = t.extract();
            dbg!(&r);

            assert!(matches!(
                r,
                TokenExtract {
                    start: 0,
                    end,
                    result: Ok(TokenKind::Literal(_)),
                } if end == cpx.len()
            ));
            let num = extract_number!(r.result);
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

            let r = t.extract();
            dbg!(&r);

            assert!(matches!(
                r,
                TokenExtract {
                    start: 0,
                    end,
                    result: Ok(TokenKind::Literal(_)),
                } if end == cpx.len()
            ));
            let num = extract_number!(r.result);
            let expected = if cpx.contains("-nan") {
                cpx.replace("-nan", "+nan")
            } else {
                cpx.to_owned()
            };
            assert_eq!(num.as_datum().to_string(), expected);
        }
    }

    #[test]
    fn missing_imaginary() {
        let mut s = Scanner::new("4+");
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
                result: Err(TokenErrorKind::ComplexInvalid),
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 4,
                result: Err(TokenErrorKind::ComplexInvalid),
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 10,
                result: Err(TokenErrorKind::ComplexInvalid),
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 6,
                result: Err(TokenErrorKind::ComplexInvalid),
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 3,
                result: Ok(TokenKind::Literal(_)),
            }
        ));
        let num = extract_number!(r.result);
        assert_eq!(num.as_datum().to_string(), "???");
    }

    #[test]
    fn negative_mag() {
        let mut s = Scanner::new("-4@3");
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
                result: Ok(TokenKind::Literal(_)),
            }
        ));
        let num = extract_number!(r.result);
        assert_eq!(num.as_datum().to_string(), "???");
    }

    #[test]
    fn negative_rads() {
        let mut s = Scanner::new("4@-3");
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
                result: Ok(TokenKind::Literal(_)),
            }
        ));
        let num = extract_number!(r.result);
        assert_eq!(num.as_datum().to_string(), "???");
    }

    #[test]
    fn negative() {
        let mut s = Scanner::new("-4@-3");
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
                result: Ok(TokenKind::Literal(_)),
            }
        ));
        let num = extract_number!(r.result);
        assert_eq!(num.as_datum().to_string(), "???");
    }

    #[test]
    fn zero_magnitude() {
        let mut s = Scanner::new("0@3");
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
                result: Ok(TokenKind::Literal(_)),
            }
        ));
        let num = extract_number!(r.result);
        assert_eq!(num.as_datum().to_string(), "???");
    }

    #[test]
    fn negative_zero_magnitude() {
        let mut s = Scanner::new("-0@3");
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
                result: Ok(TokenKind::Literal(_)),
            }
        ));
        let num = extract_number!(r.result);
        assert_eq!(num.as_datum().to_string(), "???");
    }

    #[test]
    fn zero_angle() {
        let mut s = Scanner::new("4@0");
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
                result: Ok(TokenKind::Literal(_)),
            }
        ));
        let num = extract_number!(r.result);
        assert_eq!(num.as_datum().to_string(), "???");
    }

    #[test]
    fn negative_zero_angle() {
        let mut s = Scanner::new("4@-0");
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
                result: Ok(TokenKind::Literal(_)),
            }
        ));
        let num = extract_number!(r.result);
        assert_eq!(num.as_datum().to_string(), "???");
    }

    #[test]
    fn all_zeros() {
        let mut s = Scanner::new("0@0");
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
                result: Ok(TokenKind::Literal(_)),
            }
        ));
        let num = extract_number!(r.result);
        assert_eq!(num.as_datum().to_string(), "???");
    }

    #[test]
    fn unit_45degrees() {
        let input = format!("0@{}", std::f64::consts::FRAC_PI_4);
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
                end: 3,
                result: Ok(TokenKind::Literal(_)),
            }
        ));
        let num = extract_number!(r.result);
        assert_eq!(num.as_datum().to_string(), "???");
    }

    #[test]
    fn combos() {
        let reals = ["4", "3.5", "4.2e4", "1/6"];
        let imags = ["5", "12.2", "5.6e-2", "7/3"];
        let expected = ["???"];
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

            let r = t.extract();
            dbg!(&r);

            assert!(matches!(
                r,
                TokenExtract {
                    start: 0,
                    end,
                    result: Ok(TokenKind::Literal(_)),
                } if end == cpx.len()
            ));
            let num = extract_number!(r.result);
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

            let r = t.extract();
            dbg!(&r);

            assert!(matches!(
                r,
                TokenExtract {
                    start: 0,
                    end,
                    result: Ok(TokenKind::Literal(_)),
                } if end == cpx.len()
            ));
            let num = extract_number!(r.result);
            assert_eq!(num.as_datum().to_string(), exp);
        }
    }

    #[test]
    fn do_not_allow_i() {
        let mut s = Scanner::new("4@3i");
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
    fn missing_angle() {
        let mut s = Scanner::new("4@");
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
    fn recursive_polar() {
        let mut s = Scanner::new("4@3@2");
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

        let r = t.extract();

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 3,
                result: Ok(TokenKind::Identifier(s)),
            } if s == "+-4"
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

        let r = t.extract();

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 3,
                result: Ok(TokenKind::Identifier(s)),
            } if s == "+@4"
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

        let r = t.extract();

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 4,
                result: Ok(TokenKind::Identifier(s)),
            } if s == "+..4"
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

        let r = t.extract();

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 4,
                result: Ok(TokenKind::Identifier(s)),
            } if s == "+.-4"
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

        let r = t.extract();

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 3,
                result: Ok(TokenKind::Identifier(s)),
            } if s == "..4"
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

        let r = t.extract();

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 3,
                result: Ok(TokenKind::Identifier(s)),
            } if s == ".-4"
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

        let r = t.extract();

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 3,
                result: Ok(TokenKind::Identifier(s)),
            } if s == ".@4"
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

        let r = t.extract();

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 2,
                result: Ok(TokenKind::Identifier(s)),
            } if s == "@4"
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
    fn inf_followed_by_signed_identifier() {
        let mut s = Scanner::new("+inf.0+foo");
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
            } if s == "+inf.0+foo"
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 13,
                result: Ok(TokenKind::Identifier(s)),
            } if s == "+inf.0+4.2.2i"
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 7,
                result: Ok(TokenKind::Identifier(s)),
            } if s == "+inf.0+"
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 10,
                result: Ok(TokenKind::Identifier(s)),
            } if s == "+inf.0@foo"
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 7,
                result: Ok(TokenKind::Identifier(s)),
            } if s == "+inf.0@"
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 9,
                result: Ok(TokenKind::Identifier(s)),
            } if s == "+inf.0@4i"
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 12,
                result: Ok(TokenKind::Identifier(s)),
            } if s == "+inf.0@4.2.2"
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
    fn nan_followed_by_signed_identifier() {
        let mut s = Scanner::new("+nan.0-bar");
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
            } if s == "+nan.0-bar"
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 10,
                result: Ok(TokenKind::Identifier(s)),
            } if s == "+nan.0@bar"
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

        let r = t.extract();
        dbg!(&r);

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 9,
                result: Ok(TokenKind::Identifier(s)),
            } if s == "+nan.0@4i"
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

        let r = t.extract();

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 4,
                result: Ok(TokenKind::Identifier(s)),
            } if s == "+i+4"
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

        let r = t.extract();

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 4,
                result: Ok(TokenKind::Identifier(s)),
            } if s == "+i@4"
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

            let r = t.extract();
            dbg!(&r);

            assert!(matches!(
                r,
                TokenExtract {
                    start: 0,
                    end,
                    result: Err(TokenErrorKind::IdentifierInvalid('{')),
                } if end == case.len()
            ));
        }
    }
}
