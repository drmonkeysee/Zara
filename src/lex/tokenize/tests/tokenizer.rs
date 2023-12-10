mod boolean;
mod bytevector;
mod character;
mod hashtag;
mod identifier;
mod numeric;
mod quoting;
mod string;

use super::*;
use crate::testutil::some_or_fail;

#[test]
fn left_paren() {
    let mut s = Scanner::new("(");
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
            end: 1,
            result: Ok(TokenKind::ParenLeft),
        }
    ));
}

#[test]
fn right_paren() {
    let mut s = Scanner::new(")");
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
            end: 1,
            result: Ok(TokenKind::ParenRight),
        }
    ));
}

#[test]
fn pair_joiner() {
    let mut s = Scanner::new(".");
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
            end: 1,
            result: Ok(TokenKind::PairJoiner),
        }
    ));
}

#[test]
fn pair_joiner_with_whitespace() {
    let mut s = Scanner::new(" . ");
    let start = some_or_fail!(s.next_token());
    let t = Tokenizer {
        scan: &mut s,
        start,
    };

    let r = t.extract();

    assert!(matches!(
        r,
        TokenExtract {
            start: 1,
            end: 2,
            result: Ok(TokenKind::PairJoiner),
        }
    ));
}

#[test]
fn pair_joiner_prefixed_is_identifier() {
    let mut s = Scanner::new("a.");
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
            result: Ok(TokenKind::Identifier(txt)),
        } if txt == "a."
    ));
}

#[test]
fn pair_joiner_postfixed_is_identifier() {
    let mut s = Scanner::new(".a");
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
            result: Ok(TokenKind::Identifier(txt)),
        } if txt == ".a"
    ));
}

#[test]
fn pair_joiner_followed_by_delimiter() {
    let mut s = Scanner::new(".)");
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
            end: 1,
            result: Ok(TokenKind::PairJoiner),
        }
    ));
}

#[test]
fn token_ends_at_whitespace() {
    let mut s = Scanner::new("(  ");
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
            end: 1,
            result: Ok(TokenKind::ParenLeft),
        }
    ));
}

#[test]
fn token_ends_at_delimiter() {
    let mut s = Scanner::new("()");
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
            end: 1,
            result: Ok(TokenKind::ParenLeft),
        }
    ));
}

#[test]
fn comment() {
    let mut s = Scanner::new(";");
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
            end: 1,
            result: Ok(TokenKind::Comment),
        }
    ));
}

#[test]
fn comment_with_text() {
    let mut s = Scanner::new("; scanner input is always one line");
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
            end: 34,
            result: Ok(TokenKind::Comment),
        }
    ));
}
