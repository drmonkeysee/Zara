use super::*;

#[test]
fn true_short() {
    let mut s = Scanner::new("#t");
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
            result: Ok(TokenKind::Literal(Literal::Boolean(true))),
        }
    ));
}

#[test]
fn true_long() {
    let mut s = Scanner::new("#true");
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
            end: 5,
            result: Ok(TokenKind::Literal(Literal::Boolean(true))),
        }
    ));
}

#[test]
fn true_uppercase() {
    let mut s = Scanner::new("#TRUE");
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
            end: 5,
            result: Ok(TokenKind::Literal(Literal::Boolean(true))),
        }
    ));
}

#[test]
fn true_malformed() {
    let mut s = Scanner::new("#trueasd");
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
            end: 8,
            result: Err(TokenErrorKind::BooleanExpected(true)),
        }
    ));
}

#[test]
fn false_short() {
    let mut s = Scanner::new("#f");
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
            result: Ok(TokenKind::Literal(Literal::Boolean(false))),
        }
    ));
}

#[test]
fn false_long() {
    let mut s = Scanner::new("#false");
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
            end: 6,
            result: Ok(TokenKind::Literal(Literal::Boolean(false))),
        }
    ));
}

#[test]
fn false_uppercase() {
    let mut s = Scanner::new("#FALSE");
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
            end: 6,
            result: Ok(TokenKind::Literal(Literal::Boolean(false))),
        }
    ));
}

#[test]
fn false_malformed() {
    let mut s = Scanner::new("#fals");
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
            end: 5,
            result: Err(TokenErrorKind::BooleanExpected(false)),
        }
    ));
}
