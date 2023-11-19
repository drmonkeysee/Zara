use super::*;

#[test]
fn bytevector() {
    let mut s = Scanner::new("#u8(");
    let start = s.next_token().unwrap();
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
            result: Ok(TokenKind::ByteVector),
        }
    ));
}

#[test]
fn bytevector_uppercase() {
    let mut s = Scanner::new("#U8(");
    let start = s.next_token().unwrap();
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
            result: Ok(TokenKind::ByteVector),
        }
    ));
}

#[test]
fn bytevector_ends_at_paren() {
    let mut s = Scanner::new("#u8(sdf");
    let start = s.next_token().unwrap();
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
            result: Ok(TokenKind::ByteVector),
        }
    ));
}

#[test]
fn bytevector_unterminated() {
    let mut s = Scanner::new("#u");
    let start = s.next_token().unwrap();
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
            result: Err(TokenErrorKind::ByteVectorExpected),
        }
    ));
}

#[test]
fn bytevector_wrong_number() {
    let mut s = Scanner::new("#u9(");
    let start = s.next_token().unwrap();
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
            result: Err(TokenErrorKind::ByteVectorExpected),
        }
    ));
}

#[test]
fn bytevector_extra_number() {
    let mut s = Scanner::new("#u81(");
    let start = s.next_token().unwrap();
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
            result: Err(TokenErrorKind::ByteVectorExpected),
        }
    ));
}

#[test]
fn bytevector_no_paren() {
    let mut s = Scanner::new("#u8");
    let start = s.next_token().unwrap();
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
            result: Err(TokenErrorKind::ByteVectorExpected),
        }
    ));
}

#[test]
fn bytevector_no_paren_whitespace() {
    let mut s = Scanner::new("#u8  ");
    let start = s.next_token().unwrap();
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
            result: Err(TokenErrorKind::ByteVectorExpected),
        }
    ));
}
