use super::*;

#[test]
fn true_short() {
    let mut s = Scanner::new("#t");
    let start = some_or_fail!(s.next_token());
    let t = Tokenizer {
        scanner: &mut s,
        start,
    };

    let (r, c) = t.extract();

    assert!(c.is_none());
    let tok = ok_or_fail!(r);
    assert!(matches!(
        tok,
        Token {
            kind: TokenKind::Literal(Literal::Boolean(true)),
            span: Range { start: 0, end: 2 },
        }
    ));
}

#[test]
fn true_long() {
    let mut s = Scanner::new("#true");
    let start = some_or_fail!(s.next_token());
    let t = Tokenizer {
        scanner: &mut s,
        start,
    };

    let (r, c) = t.extract();

    assert!(c.is_none());
    let tok = ok_or_fail!(r);
    assert!(matches!(
        tok,
        Token {
            kind: TokenKind::Literal(Literal::Boolean(true)),
            span: Range { start: 0, end: 5 },
        }
    ));
}

#[test]
fn true_uppercase() {
    let mut s = Scanner::new("#TRUE");
    let start = some_or_fail!(s.next_token());
    let t = Tokenizer {
        scanner: &mut s,
        start,
    };

    let (r, c) = t.extract();

    assert!(c.is_none());
    let tok = ok_or_fail!(r);
    assert!(matches!(
        tok,
        Token {
            kind: TokenKind::Literal(Literal::Boolean(true)),
            span: Range { start: 0, end: 5 },
        }
    ));
}

#[test]
fn true_malformed() {
    let mut s = Scanner::new("#trueasd");
    let start = some_or_fail!(s.next_token());
    let t = Tokenizer {
        scanner: &mut s,
        start,
    };

    let (r, c) = t.extract();

    assert!(c.is_none());
    let err = err_or_fail!(r);
    assert!(matches!(
        err,
        TokenError {
            kind: TokenErrorKind::BooleanExpected(true),
            span: Range { start: 0, end: 8 },
        }
    ));
}

#[test]
fn false_short() {
    let mut s = Scanner::new("#f");
    let start = some_or_fail!(s.next_token());
    let t = Tokenizer {
        scanner: &mut s,
        start,
    };

    let (r, c) = t.extract();

    assert!(c.is_none());
    let tok = ok_or_fail!(r);
    assert!(matches!(
        tok,
        Token {
            kind: TokenKind::Literal(Literal::Boolean(false)),
            span: Range { start: 0, end: 2 },
        }
    ));
}

#[test]
fn false_long() {
    let mut s = Scanner::new("#false");
    let start = some_or_fail!(s.next_token());
    let t = Tokenizer {
        scanner: &mut s,
        start,
    };

    let (r, c) = t.extract();

    assert!(c.is_none());
    let tok = ok_or_fail!(r);
    assert!(matches!(
        tok,
        Token {
            kind: TokenKind::Literal(Literal::Boolean(false)),
            span: Range { start: 0, end: 6 },
        }
    ));
}

#[test]
fn false_uppercase() {
    let mut s = Scanner::new("#FALSE");
    let start = some_or_fail!(s.next_token());
    let t = Tokenizer {
        scanner: &mut s,
        start,
    };

    let (r, c) = t.extract();

    assert!(c.is_none());
    let tok = ok_or_fail!(r);
    assert!(matches!(
        tok,
        Token {
            kind: TokenKind::Literal(Literal::Boolean(false)),
            span: Range { start: 0, end: 6 },
        }
    ));
}

#[test]
fn false_malformed() {
    let mut s = Scanner::new("#fals");
    let start = some_or_fail!(s.next_token());
    let t = Tokenizer {
        scanner: &mut s,
        start,
    };

    let (r, c) = t.extract();

    assert!(c.is_none());
    let err = err_or_fail!(r);
    assert!(matches!(
        err,
        TokenError {
            kind: TokenErrorKind::BooleanExpected(false),
            span: Range { start: 0, end: 5 },
        }
    ));
}
