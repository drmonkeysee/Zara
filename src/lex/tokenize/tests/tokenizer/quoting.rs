use super::*;

#[test]
fn quote() {
    let mut s = Scanner::new("'");
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
            kind: TokenKind::Quote,
            span: Range { start: 0, end: 1 },
        }
    ));
}

#[test]
fn quasiquote() {
    let mut s = Scanner::new("`");
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
            kind: TokenKind::Quasiquote,
            span: Range { start: 0, end: 1 },
        }
    ));
}

#[test]
fn unquote() {
    let mut s = Scanner::new(",");
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
            kind: TokenKind::Unquote,
            span: Range { start: 0, end: 1 },
        }
    ));
}

#[test]
fn unquote_followed_by_non_splice() {
    let mut s = Scanner::new(",a");
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
            kind: TokenKind::Unquote,
            span: Range { start: 0, end: 1 },
        }
    ));
}

#[test]
fn unquote_splicing() {
    let mut s = Scanner::new(",@");
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
            kind: TokenKind::UnquoteSplice,
            span: Range { start: 0, end: 2 },
        }
    ));
}

#[test]
fn unquote_whitespace_between_splice() {
    let mut s = Scanner::new(", @");
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
            kind: TokenKind::Unquote,
            span: Range { start: 0, end: 1 },
        }
    ));
}
