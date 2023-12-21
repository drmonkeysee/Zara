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
                end: 1,
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
                end: 1,
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
                end: 1,
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
                end: 1,
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
                end: 1,
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
                end: 1,
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
                end: 1,
                result: Ok(TokenKind::Literal(Literal::Number(_))),
            }
        ));
        let int = extract_number!(r.result, Number::Real, Real::Integer);
        assert_eq!(int.to_string(), "-18446744073709551615");
    }
}

mod float {
    use super::*;

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
                start: 0,
                end: 8,
                result: Err(TokenErrorKind::NumberUnexpectedDecimalPoint { at: 5 }),
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
                result: Ok(TokenKind::Literal(Literal::Number(_))),
            }
        ));
        let tok = ok_or_fail!(r.result);
        let lit = extract_or_fail!(tok, TokenKind::Literal);
        let cpx = extract_or_fail!(lit, Literal::Number);
        assert_eq!(cpx.as_datum().to_string(), "+i");
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
                result: Ok(TokenKind::Literal(Literal::Number(_))),
            }
        ));
        let tok = ok_or_fail!(r.result);
        let lit = extract_or_fail!(tok, TokenKind::Literal);
        let cpx = extract_or_fail!(lit, Literal::Number);
        assert_eq!(cpx.as_datum().to_string(), "+i");
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
                result: Ok(TokenKind::Literal(Literal::Number(_))),
            }
        ));
        let tok = ok_or_fail!(r.result);
        let lit = extract_or_fail!(tok, TokenKind::Literal);
        let cpx = extract_or_fail!(lit, Literal::Number);
        assert_eq!(cpx.as_datum().to_string(), "-i");
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
}
