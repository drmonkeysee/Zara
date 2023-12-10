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
