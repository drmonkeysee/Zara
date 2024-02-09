use super::*;

#[test]
fn quote() {
    let mut s = Scanner::new("'");
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
            end: 1,
            result: Ok(TokenKind::Quote),
        }
    ));
}

#[test]
fn quasiquote() {
    let mut s = Scanner::new("`");
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
            end: 1,
            result: Ok(TokenKind::Quasiquote),
        }
    ));
}

#[test]
fn unquote() {
    let mut s = Scanner::new(",");
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
            end: 1,
            result: Ok(TokenKind::Unquote),
        }
    ));
}

#[test]
fn unquote_followed_by_non_splice() {
    let mut s = Scanner::new(",a");
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
            end: 1,
            result: Ok(TokenKind::Unquote),
        }
    ));
}

#[test]
fn unquote_splicing() {
    let mut s = Scanner::new(",@");
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
            result: Ok(TokenKind::UnquoteSplice),
        }
    ));
}

#[test]
fn unquote_whitespace_between_splice() {
    let mut s = Scanner::new(", @");
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
            end: 1,
            result: Ok(TokenKind::Unquote),
        }
    ));
}
