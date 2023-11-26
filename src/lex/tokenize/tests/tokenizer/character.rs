use super::*;

#[test]
fn ascii_literal() {
    let mut s = Scanner::new("#\\a");
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
            end: 3,
            result: Ok(TokenKind::Literal(Literal::Character('a'))),
        }
    ));
}

#[test]
fn ascii_uppercase_literal() {
    let mut s = Scanner::new("#\\A");
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
            end: 3,
            result: Ok(TokenKind::Literal(Literal::Character('A'))),
        }
    ));
}

#[test]
fn extended_literal() {
    let mut s = Scanner::new("#\\λ");
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
            end: 4,
            result: Ok(TokenKind::Literal(Literal::Character('λ'))),
        }
    ));
}

#[test]
fn emoji_literal() {
    let mut s = Scanner::new("#\\🦀");
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
            result: Ok(TokenKind::Literal(Literal::Character('🦀'))),
        }
    ));
}

#[test]
fn string_escape_literals() {
    check_character_list(&[("\"", '"'), ("'", '\''), ("\\", '\\'), ("", '\n')]);
}

#[test]
fn space_literal() {
    let mut s = Scanner::new("#\\ ");
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
            end: 3,
            result: Ok(TokenKind::Literal(Literal::Character(' '))),
        }
    ));
}

#[test]
fn tab_literal() {
    let mut s = Scanner::new("#\\\t");
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
            end: 3,
            result: Ok(TokenKind::Literal(Literal::Character('\t'))),
        }
    ));
}

#[test]
fn name() {
    check_character_list(&[
        ("alarm", '\x07'),
        ("backspace", '\x08'),
        ("delete", '\x7f'),
        ("escape", '\x1b'),
        ("newline", '\n'),
        ("null", '\0'),
        ("return", '\r'),
        ("space", ' '),
        ("tab", '\t'),
    ]);
}

#[test]
fn name_does_not_match_uppercase() {
    let mut s = Scanner::new("#\\ALARM");
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
            end: 7,
            result: Err(TokenErrorKind::CharacterExpected),
        }
    ));
}

#[test]
fn space_followed_by_alpha() {
    let mut s = Scanner::new("#\\ b");
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
            end: 3,
            result: Ok(TokenKind::Literal(Literal::Character(' '))),
        }
    ));
}

#[test]
fn alpha_followed_by_alpha() {
    let mut s = Scanner::new("#\\ab");
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
            end: 4,
            result: Err(TokenErrorKind::CharacterExpected),
        }
    ));
}

#[test]
fn emoji_followed_by_alpha() {
    let mut s = Scanner::new("#\\🦀b");
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
            end: 7,
            result: Err(TokenErrorKind::CharacterExpected),
        }
    ));
}

#[test]
fn alpha_followed_by_delimiter() {
    let mut s = Scanner::new("#\\a(");
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
            end: 3,
            result: Ok(TokenKind::Literal(Literal::Character('a'))),
        }
    ));
}

#[test]
fn letter_x_is_not_hex() {
    let mut s = Scanner::new("#\\x");
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
            end: 3,
            result: Ok(TokenKind::Literal(Literal::Character('x'))),
        }
    ));
}

#[test]
fn hex_zero() {
    let mut s = Scanner::new("#\\x0");
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
            end: 4,
            result: Ok(TokenKind::Literal(Literal::Character('\0'))),
        }
    ));
}

#[test]
fn hex_lowercase() {
    let mut s = Scanner::new("#\\xa");
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
            end: 4,
            result: Ok(TokenKind::Literal(Literal::Character('\n'))),
        }
    ));
}

#[test]
fn hex_uppercase() {
    let mut s = Scanner::new("#\\xA");
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
            end: 4,
            result: Ok(TokenKind::Literal(Literal::Character('\n'))),
        }
    ));
}

#[test]
fn hex_uppercase_indicator() {
    let mut s = Scanner::new("#\\Xa");
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
            end: 4,
            result: Ok(TokenKind::Literal(Literal::Character('\n'))),
        }
    ));
}

#[test]
fn hex_multi_digits() {
    check_character_list(&[
        ("x45", 'E'),
        ("x39b", 'Λ'),
        ("x16A1", 'ᚡ'),
        ("x1F64A", '🙊'),
    ]);
}

#[test]
fn hex_sign_invalid() {
    let mut s = Scanner::new("#\\x+A");
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
            result: Err(TokenErrorKind::CharacterExpectedHex),
        }
    ));
}

#[test]
fn hex_too_large() {
    let mut s = Scanner::new("#\\xdeadbeef");
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
            end: 11,
            result: Err(TokenErrorKind::CharacterInvalidHex),
        }
    ));
}

#[test]
fn hex_malformed() {
    let mut s = Scanner::new("#\\x124nope");
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
            end: 10,
            result: Err(TokenErrorKind::CharacterExpectedHex),
        }
    ));
}

fn check_character_list(cases: &[(&str, char)]) {
    for &(inp, exp) in cases {
        let input = format!("#\\{inp}");
        let mut s = Scanner::new(&input);
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let r = t.extract();

        assert!(
            matches!(
                r,
                TokenExtract {
                    start: 0,
                    end,
                    result: Ok(TokenKind::Literal(Literal::Character(ch))),
                } if ch == exp && end == input.len()
            ),
            "Unexpected match for character input ({inp}, {exp})"
        );
    }
}
