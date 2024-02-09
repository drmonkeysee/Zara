use super::*;

#[test]
fn basic_token() {
    let mut s = Scanner::new("#u8(");
    let start = some_or_fail!(s.next_token());
    let t = Tokenizer {
        scan: &mut s,
        start,
    };

    let (r, c) = t.extract();

    assert!(c.is_none());
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
fn uppercase() {
    let mut s = Scanner::new("#U8(");
    let start = some_or_fail!(s.next_token());
    let t = Tokenizer {
        scan: &mut s,
        start,
    };

    let (r, c) = t.extract();

    assert!(c.is_none());
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
fn ends_at_paren() {
    let mut s = Scanner::new("#u8(sdf");
    let start = some_or_fail!(s.next_token());
    let t = Tokenizer {
        scan: &mut s,
        start,
    };

    let (r, c) = t.extract();
    dbg!(&r);

    assert!(c.is_none());
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
fn unterminated() {
    let mut s = Scanner::new("#u");
    let start = some_or_fail!(s.next_token());
    let t = Tokenizer {
        scan: &mut s,
        start,
    };

    let (r, c) = t.extract();

    assert!(c.is_none());
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
fn wrong_number() {
    let mut s = Scanner::new("#u9(");
    let start = some_or_fail!(s.next_token());
    let t = Tokenizer {
        scan: &mut s,
        start,
    };

    let (r, c) = t.extract();

    assert!(c.is_none());
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
fn extra_number() {
    let mut s = Scanner::new("#u81(");
    let start = some_or_fail!(s.next_token());
    let t = Tokenizer {
        scan: &mut s,
        start,
    };

    let (r, c) = t.extract();

    assert!(c.is_none());
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
fn no_paren() {
    let mut s = Scanner::new("#u8");
    let start = some_or_fail!(s.next_token());
    let t = Tokenizer {
        scan: &mut s,
        start,
    };

    let (r, c) = t.extract();

    assert!(c.is_none());
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
fn no_paren_whitespace() {
    let mut s = Scanner::new("#u8  ");
    let start = some_or_fail!(s.next_token());
    let t = Tokenizer {
        scan: &mut s,
        start,
    };

    let (r, c) = t.extract();

    assert!(c.is_none());
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
fn no_paren_extra_chars() {
    let mut s = Scanner::new("#u8abc");
    let start = some_or_fail!(s.next_token());
    let t = Tokenizer {
        scan: &mut s,
        start,
    };

    let (r, c) = t.extract();

    assert!(c.is_none());
    assert!(matches!(
        r,
        TokenExtract {
            start: 0,
            end: 6,
            result: Err(TokenErrorKind::ByteVectorExpected),
        }
    ));
}
