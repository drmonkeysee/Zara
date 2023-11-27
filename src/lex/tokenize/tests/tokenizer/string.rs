use super::*;

#[test]
fn empty() {
    let mut s = Scanner::new("\"\"");
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
            result: Ok(TokenKind::Literal(Literal::String(s))),
        } if s == ""
    ));
}

#[test]
fn alphanumeric() {
    let mut s = Scanner::new("\"abc123!@#\"");
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
            result: Ok(TokenKind::Literal(Literal::String(s))),
        } if s == "abc123!@#"
    ));
}

#[test]
fn raw_extended_and_higher_char() {
    let mut s = Scanner::new("\"λ 🦀 \u{2401} \u{fffd}\"");
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
            end: 17,
            result: Ok(TokenKind::Literal(Literal::String(s))),
        } if s == "λ 🦀 ␁ �"
    ));
}

#[test]
fn contains_verbatim_identifier() {
    let mut s = Scanner::new("\"foo |verbatim| bar\"");
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
            end: 20,
            result: Ok(TokenKind::Literal(Literal::String(s))),
        } if s == "foo |verbatim| bar"
    ));
}

#[test]
fn raw_escape_sequences() {
    let mut s = Scanner::new("\"a:\x07, b:\x08, d:\x7f, e:\x1b, n:\n, 0:\0, r:\r, t:\t, v:\x7c\"");
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
            end: 45,
            result: Ok(TokenKind::Literal(Literal::String(s))),
        } if s == "a:\x07, b:\x08, d:\x7f, e:\x1b, n:\n, 0:\0, r:\r, t:\t, v:|"
    ));
}

#[test]
fn escape_sequences() {
    let mut s = Scanner::new("\"a:\\a, b:\\b, n:\\n, r:\r, t:\t, q:\\\", s:\\\\, v:\\|\"");
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
            end: 46,
            result: Ok(TokenKind::Literal(Literal::String(s))),
        } if s == "a:\x07, b:\x08, n:\n, r:\r, t:\t, q:\", s:\\, v:|"
    ));
}

#[test]
fn whitespace_escape() {
    let mut s = Scanner::new("\"foo\\   bar\"");
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
            end: 12,
            result: Ok(TokenKind::Literal(Literal::String(s))),
        } if s == "foo   bar"
    ));
}

#[test]
fn hex_escape_sequences() {
    let mut s = Scanner::new(
            "\"a:\\x7;, b:\\x8;, d:\\x7f;, e:\\x1b;, n:\\xa;, 0:\\x0;, r:\\xd;, t:\\x9;, q:\\x22;, s:\\x5c;, v:\\x7c;\"",
        );
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
            end: 93,
            result: Ok(TokenKind::Literal(Literal::String(s))),
        } if s == "a:\x07, b:\x08, d:\x7f, e:\x1b, n:\n, 0:\0, r:\r, t:\t, q:\", s:\\, v:|"
    ));
}

#[test]
fn hex_case_insensitive() {
    let mut s = Scanner::new("\"\\x4a; \\X4A;\"");
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
            end: 13,
            result: Ok(TokenKind::Literal(Literal::String(s))),
        } if s == "J J"
    ));
}

#[test]
fn higher_plane_raw() {
    let mut s = Scanner::new("\"\u{fff9} \u{e0001} \u{100001}\"");
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
            end: 15,
            result: Ok(TokenKind::Literal(Literal::String(s))),
        } if s == "\u{fff9} \u{e0001} \u{100001}"
    ));
}

#[test]
fn higher_plane_hex() {
    let mut s = Scanner::new("\"\\xfff9; \\xe0001; \\x100001;\"");
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
            end: 28,
            result: Ok(TokenKind::Literal(Literal::String(s))),
        } if s == "\u{fff9} \u{e0001} \u{100001}"
    ));
}

#[test]
fn invalid_escape() {
    let mut s = Scanner::new("\"\\B\"");
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
            end: 3,
            result: Err(TokenErrorKind::StringEscapeInvalid { idx: 1, ch: 'B' }),
        }
    ));
}

#[test]
fn hex_sign_invalid() {
    let mut s = Scanner::new("\"\\x+A;\"");
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
            end: 6,
            result: Err(TokenErrorKind::StringExpectedHex(1)),
        }
    ));
}

#[test]
fn hex_too_large() {
    let mut s = Scanner::new("\"\\xdeadbeef;\"");
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
            end: 12,
            result: Err(TokenErrorKind::StringInvalidHex(1)),
        }
    ));
}

#[test]
fn hex_malformed() {
    let mut s = Scanner::new("\"\\x124nope;\"");
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
            end: 11,
            result: Err(TokenErrorKind::StringExpectedHex(1)),
        }
    ));
}

#[test]
fn hex_unterminated() {
    let mut s = Scanner::new("\"\\x123\"");
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
            end: 6,
            result: Err(TokenErrorKind::StringUnterminatedHex(1)),
        }
    ));
}

#[test]
fn discard() {
    let mut s = Scanner::new("\\xbadstuff; discard this");
    s.find_any_char(&[';']);
    let start = s.pos();
    let t = Continuation {
        cont: TokenContinuation::SubstringError,
        scan: &mut s,
        start,
    };

    let r = t.extract();

    assert!(matches!(
        r,
        TokenExtract {
            start: 11,
            end: 24,
            result: Ok(TokenKind::StringDiscard),
        }
    ));
}

#[test]
fn begin() {
    let mut s = Scanner::new("\"beginning string");
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
            end: 17,
            result: Ok(TokenKind::StringBegin { s, line_cont: false }),
        } if s == "beginning string"
    ));
}

#[test]
fn begin_with_line_continuation() {
    let mut s = Scanner::new("\"beginning string\\");
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
            end: 18,
            result: Ok(TokenKind::StringBegin { s, line_cont: true }),
        } if s == "beginning string"
    ));
}

#[test]
fn begin_with_line_continuation_includes_leading_whitespace() {
    let mut s = Scanner::new("\"beginning string    \\");
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
            end: 22,
            result: Ok(TokenKind::StringBegin { s, line_cont: true }),
        } if s == "beginning string    "
    ));
}

#[test]
fn begin_with_line_continuation_excludes_trailing_whitespace() {
    let mut s = Scanner::new("\"beginning string\\    ");
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
            end: 22,
            result: Ok(TokenKind::StringBegin { s, line_cont: true }),
        } if s == "beginning string"
    ));
}

#[test]
fn begin_only_counts_final_slash_as_line_continuation() {
    let mut s = Scanner::new("\"beginning string\\  \\  \\  ");
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
            end: 26,
            result: Ok(TokenKind::StringBegin { s, line_cont: true }),
        } if s == "beginning string    "
    ));
}

#[test]
fn fragment() {
    let mut s = Scanner::new("continued string");
    let c = Continuation {
        cont: TokenContinuation::StringLiteral { line_cont: false },
        scan: &mut s,
        start: 0,
    };

    let r = c.extract();

    assert!(matches!(
        r,
        TokenExtract {
            start: 0,
            end: 16,
            result: Ok(TokenKind::StringFragment { s, line_cont: false }),
        } if s == "continued string"
    ));
}

#[test]
fn fragment_includes_whitespace() {
    let mut s = Scanner::new("   continued string");
    let c = Continuation {
        cont: TokenContinuation::StringLiteral { line_cont: false },
        scan: &mut s,
        start: 0,
    };

    let r = c.extract();

    assert!(matches!(
        r,
        TokenExtract {
            start: 0,
            end: 19,
            result: Ok(TokenKind::StringFragment { s, line_cont: false }),
        } if s == "   continued string"
    ));
}

#[test]
fn fragment_with_line_continuation() {
    let mut s = Scanner::new("continued string  \\  \\  ");
    let c = Continuation {
        cont: TokenContinuation::StringLiteral { line_cont: false },
        scan: &mut s,
        start: 0,
    };

    let r = c.extract();

    assert!(matches!(
        r,
        TokenExtract {
            start: 0,
            end: 24,
            result: Ok(TokenKind::StringFragment { s, line_cont: true }),
        } if s == "continued string    "
    ));
}

#[test]
fn fragment_from_string_continuation() {
    let mut s = Scanner::new("continued string");
    let c = Continuation {
        cont: TokenContinuation::StringLiteral { line_cont: true },
        scan: &mut s,
        start: 0,
    };

    let r = c.extract();

    assert!(matches!(
        r,
        TokenExtract {
            start: 0,
            end: 16,
            result: Ok(TokenKind::StringFragment { s, line_cont: false }),
        } if s == "continued string"
    ));
}

#[test]
fn fragment_from_string_continuation_ignores_leading_whitespace() {
    let mut s = Scanner::new("   continued string   ");
    let c = Continuation {
        cont: TokenContinuation::StringLiteral { line_cont: true },
        scan: &mut s,
        start: 0,
    };

    let r = c.extract();

    assert!(matches!(
        r,
        TokenExtract {
            start: 0,
            end: 22,
            result: Ok(TokenKind::StringFragment { s, line_cont: false }),
        } if s == "continued string   "
    ));
}

#[test]
fn fragment_from_string_continuation_all_whitespace() {
    let mut s = Scanner::new("      ");
    let c = Continuation {
        cont: TokenContinuation::StringLiteral { line_cont: true },
        scan: &mut s,
        start: 0,
    };

    let r = c.extract();

    assert!(matches!(
        r,
        TokenExtract {
            start: 0,
            end: 6,
            result: Ok(TokenKind::StringFragment { s, line_cont: false }),
        } if s == ""
    ));
}

#[test]
fn fragment_from_string_continuation_to_string_continuation() {
    let mut s = Scanner::new("   continued string  \\  \\  ");
    let c = Continuation {
        cont: TokenContinuation::StringLiteral { line_cont: true },
        scan: &mut s,
        start: 0,
    };

    let r = c.extract();

    assert!(matches!(
        r,
        TokenExtract {
            start: 0,
            end: 27,
            result: Ok(TokenKind::StringFragment { s, line_cont: true }),
        } if s == "continued string    "
    ));
}

#[test]
fn end() {
    let mut s = Scanner::new("end string\"");
    let c = Continuation {
        cont: TokenContinuation::StringLiteral { line_cont: false },
        scan: &mut s,
        start: 0,
    };

    let r = c.extract();

    assert!(matches!(
        r,
        TokenExtract {
            start: 0,
            end: 11,
            result: Ok(TokenKind::StringEnd(s)),
        } if s == "end string"
    ));
}

#[test]
fn end_includes_whitespace() {
    let mut s = Scanner::new("   end string  \"");
    let c = Continuation {
        cont: TokenContinuation::StringLiteral { line_cont: false },
        scan: &mut s,
        start: 0,
    };

    let r = c.extract();

    assert!(matches!(
        r,
        TokenExtract {
            start: 0,
            end: 16,
            result: Ok(TokenKind::StringEnd(s)),
        } if s == "   end string  "
    ));
}

#[test]
fn end_with_escaped_whitespace() {
    let mut s = Scanner::new("end string  \\  \\  \"");
    let c = Continuation {
        cont: TokenContinuation::StringLiteral { line_cont: false },
        scan: &mut s,
        start: 0,
    };

    let r = c.extract();

    assert!(matches!(
        r,
        TokenExtract {
            start: 0,
            end: 19,
            result: Ok(TokenKind::StringEnd(s)),
        } if s == "end string      "
    ));
}

#[test]
fn end_from_string_continuation() {
    let mut s = Scanner::new("end string\"");
    let c = Continuation {
        cont: TokenContinuation::StringLiteral { line_cont: true },
        scan: &mut s,
        start: 0,
    };

    let r = c.extract();

    assert!(matches!(
        r,
        TokenExtract {
            start: 0,
            end: 11,
            result: Ok(TokenKind::StringEnd(s)),
        } if s == "end string"
    ));
}

#[test]
fn end_from_string_continuation_ignores_leading_whitespace() {
    let mut s = Scanner::new("   end string   \"");
    let c = Continuation {
        cont: TokenContinuation::StringLiteral { line_cont: true },
        scan: &mut s,
        start: 0,
    };

    let r = c.extract();

    assert!(matches!(
        r,
        TokenExtract {
            start: 0,
            end: 17,
            result: Ok(TokenKind::StringEnd(s)),
        } if s == "end string   "
    ));
}

#[test]
fn end_from_string_continuation_all_whitespace() {
    let mut s = Scanner::new("      \"");
    let c = Continuation {
        cont: TokenContinuation::StringLiteral { line_cont: true },
        scan: &mut s,
        start: 0,
    };

    let r = c.extract();

    assert!(matches!(
        r,
        TokenExtract {
            start: 0,
            end: 7,
            result: Ok(TokenKind::StringEnd(s)),
        } if s == ""
    ));
}
