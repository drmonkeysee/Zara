use super::*;

#[test]
fn quote() {
    let mut s = Scanner::new("'");
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
            end: 1,
            result: Ok(TokenKind::Quote),
        }
    ));
}

#[test]
fn quasiquote() {
    let mut s = Scanner::new("`");
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
            end: 1,
            result: Ok(TokenKind::Quasiquote),
        }
    ));
}

#[test]
fn unquote() {
    let mut s = Scanner::new(",");
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
            end: 1,
            result: Ok(TokenKind::Unquote),
        }
    ));
}

#[test]
fn unquote_followed_by_non_splice() {
    let mut s = Scanner::new(",a");
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
            end: 1,
            result: Ok(TokenKind::Unquote),
        }
    ));
}

#[test]
fn unquote_splicing() {
    let mut s = Scanner::new(",@");
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
            result: Ok(TokenKind::UnquoteSplice),
        }
    ));
}

#[test]
fn unquote_whitespace_between_splice() {
    let mut s = Scanner::new(", @");
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
            end: 1,
            result: Ok(TokenKind::Unquote),
        }
    ));
}
