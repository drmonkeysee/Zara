use super::*;

#[test]
fn standard_identifier() {
    let mut s = Scanner::new("foo");
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
            kind: TokenKind::Identifier(txt),
            span: Range { start: 0, end: 3 },
        } if txt == "foo"
    ));
}

#[test]
fn with_digits() {
    let mut s = Scanner::new("a24");
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
            kind: TokenKind::Identifier(txt),
            span: Range { start: 0, end: 3 },
        } if txt == "a24"
    ));
}

#[test]
fn with_special_chars() {
    let mut s = Scanner::new("foo?bar@baz.beef");
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
            kind: TokenKind::Identifier(txt),
            span: Range { start: 0, end: 16 },
        } if txt == "foo?bar@baz.beef"
    ));
}

#[test]
fn stops_at_delimiter() {
    let mut s = Scanner::new("abc;");
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
            kind: TokenKind::Identifier(txt),
            span: Range { start: 0, end: 3 },
        } if txt == "abc"
    ));
}

#[test]
fn starts_with_special_char() {
    let mut s = Scanner::new("!foo");
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
            kind: TokenKind::Identifier(txt),
            span: Range { start: 0, end: 4 },
        } if txt == "!foo"
    ));
}

#[test]
fn ends_with_peculiar_chars() {
    let mut s = Scanner::new("c++");
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
            kind: TokenKind::Identifier(txt),
            span: Range { start: 0, end: 3 },
        } if txt == "c++"
    ));
}

#[test]
fn is_only_special_char() {
    let mut s = Scanner::new("!");
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
            kind: TokenKind::Identifier(txt),
            span: Range { start: 0, end: 1 },
        } if txt == "!"
    ));
}

#[test]
fn with_extended_and_higher_chars() {
    let mut s = Scanner::new("λ🦀\u{2401}\u{fffd}");
    let start = some_or_fail!(s.next_token());
    let t = Tokenizer {
        scanner: &mut s,
        start,
    };

    let (r, c) = t.extract();

    assert!(c.is_none());
    let err = err_or_fail!(r);
    // TODO: support unicode
    /*assert!(matches!(
        r,
        TokenExtract {
            start: 0,
            end: 16,
            result: Ok(TokenKind::Identifier(s)),
        } if s == "λ🦀\u{2401}\u{fffd}"
    ));*/
    assert!(matches!(
        err,
        TokenError {
            kind: TokenErrorKind::IdentifierInvalid('λ'),
            span: Range { start: 0, end: 12 },
        }
    ));
}

#[test]
fn peculiar_identifier() {
    let mut s = Scanner::new("+foo");
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
            kind: TokenKind::Identifier(txt),
            span: Range { start: 0, end: 4 },
        } if txt == "+foo"
    ));
}

#[test]
fn sign_identifier() {
    let cases = ["+", "-"];
    for case in cases {
        let mut s = Scanner::new(case);
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
                kind: TokenKind::Identifier(txt),
                span: Range { start: 0, end: 1},
            } if txt == case
        ));
    }
}

#[test]
fn double_sign_identifier() {
    let mut s = Scanner::new("+-");
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
            kind: TokenKind::Identifier(txt),
            span: Range { start: 0, end: 2 },
        } if txt == "+-"
    ));
}

#[test]
fn sign_dot_identifier() {
    let mut s = Scanner::new("+.");
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
            kind: TokenKind::Identifier(txt),
            span: Range { start: 0, end: 2 },
        } if txt == "+."
    ));
}

#[test]
fn dot_sign_identifier() {
    let mut s = Scanner::new(".+");
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
            kind: TokenKind::Identifier(txt),
            span: Range { start: 0, end: 2 },
        } if txt == ".+"
    ));
}

#[test]
fn double_period_identifier() {
    let mut s = Scanner::new("..");
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
            kind: TokenKind::Identifier(txt),
            span: Range { start: 0, end: 2 },
        } if txt == ".."
    ));
}

#[test]
fn double_period_word_identifier() {
    let mut s = Scanner::new("..foo");
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
            kind: TokenKind::Identifier(txt),
            span: Range { start: 0, end: 5 },
        } if txt == "..foo"
    ));
}

#[test]
fn period_identifier() {
    let mut s = Scanner::new(".foo");
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
            kind: TokenKind::Identifier(txt),
            span: Range { start: 0, end: 4 },
        } if txt == ".foo"
    ));
}

#[test]
fn start_digit() {
    let mut s = Scanner::new("4foo");
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
            kind: TokenErrorKind::NumberInvalid,
            span: Range { start: 0, end: 4 },
        }
    ));
}

#[test]
fn start_reserved_char() {
    let mut s = Scanner::new("{foo");
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
            kind: TokenErrorKind::IdentifierInvalid('{'),
            span: Range { start: 0, end: 4 },
        }
    ));
}

#[test]
fn contains_reserved_char() {
    let mut s = Scanner::new("foo{bar");
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
            kind: TokenErrorKind::IdentifierInvalid('{'),
            span: Range { start: 0, end: 7 },
        }
    ));
}

#[test]
fn contains_numeric_label() {
    let mut s = Scanner::new("f+inf.0");
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
            kind: TokenKind::Identifier(txt),
            span: Range { start: 0, end: 7 },
        } if txt == "f+inf.0"
    ));
}

#[test]
fn infnan_numerator_is_identifier() {
    let cases = ["+inf.0/5", "-inf.0/5", "+nan.0/5", "-nan.0/5"];
    for case in cases {
        let mut s = Scanner::new(case);
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
                kind: TokenKind::Identifier(txt),
                span: Range { start: 0, end: 8 },
            } if txt == case
        ));
    }
}

mod verbatim {
    use super::*;

    #[test]
    fn empty() {
        let mut s = Scanner::new("||");
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
                kind: TokenKind::Identifier(txt),
                span: Range { start: 0, end: 2 },
            } if txt == ""
        ));
    }

    #[test]
    fn basic() {
        let mut s = Scanner::new("|foo|");
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
                kind: TokenKind::Identifier(txt),
                span: Range { start: 0, end: 5 },
            } if txt == "foo"
        ));
    }

    #[test]
    fn includes_whitespace() {
        let mut s = Scanner::new("| foo bar |");
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
                kind: TokenKind::Identifier(txt),
                span: Range { start: 0, end: 11 },
            } if txt == " foo bar "
        ));
    }

    #[test]
    fn escape_pipe() {
        let mut s = Scanner::new("| foo\\|bar |");
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
                kind: TokenKind::Identifier(txt),
                span: Range { start: 0, end: 12 },
            } if txt == " foo|bar "
        ));
    }

    #[test]
    fn contains_string() {
        let mut s = Scanner::new("|foo \"string\" bar|");
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
                kind: TokenKind::Identifier(txt),
                span: Range { start: 0, end: 18 },
            } if txt == "foo \"string\" bar"
        ));
    }

    #[test]
    fn alphanumeric() {
        let mut s = Scanner::new("|abc123!@#|");
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
                kind: TokenKind::Identifier(txt),
                span: Range { start: 0, end: 11 },
            } if txt == "abc123!@#"
        ));
    }

    #[test]
    fn all_numbers() {
        let mut s = Scanner::new("|1234|");
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
                kind: TokenKind::Identifier(txt),
                span: Range { start: 0, end: 6 },
            } if txt == "1234"
        ));
    }

    #[test]
    fn raw_extended_and_higher_char() {
        let mut s = Scanner::new("|λ 🦀 \u{2401} \u{fffd}|");
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
                kind: TokenKind::Identifier(txt),
                span: Range { start: 0, end: 17 },
            } if txt == "λ 🦀 ␁ �"
        ));
    }

    #[test]
    fn raw_escape_sequences() {
        let mut s = Scanner::new("|a:\x07, b:\x08, d:\x7f, e:\x1b, n:\n, 0:\0, r:\r, t:\t, q:\"|");
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
                kind: TokenKind::Identifier(txt),
                span: Range { start: 0, end: 45 },
            } if txt == "a:\x07, b:\x08, d:\x7f, e:\x1b, n:\n, 0:\0, r:\r, t:\t, q:\""
        ));
    }

    #[test]
    fn escape_sequences() {
        let mut s = Scanner::new("|a:\\a, b:\\b, n:\\n, r:\r, t:\t, q:\\\", s:\\\\, v:\\||");
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
                kind: TokenKind::Identifier(txt),
                span: Range { start: 0, end: 46 },
            } if txt == "a:\x07, b:\x08, n:\n, r:\r, t:\t, q:\", s:\\, v:|"
        ));
    }

    #[test]
    fn whitespace_escape() {
        let mut s = Scanner::new("|foo\\   bar|");
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
                kind: TokenKind::Identifier(txt),
                span: Range { start: 0, end: 12 },
            } if txt == "foo   bar"
        ));
    }

    #[test]
    fn hex_escape_sequences() {
        let mut s = Scanner::new(
            "|a:\\x7;, b:\\x8;, d:\\x7f;, e:\\x1b;, n:\\xa;, 0:\\x0;, r:\\xd;, t:\\x9;, q:\\x22;, s:\\x5c;, v:\\x7c;|",
        );
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
                kind: TokenKind::Identifier(txt),
                span: Range { start: 0, end: 93 },
            } if txt == "a:\x07, b:\x08, d:\x7f, e:\x1b, n:\n, 0:\0, r:\r, t:\t, q:\", s:\\, v:|"
        ));
    }

    #[test]
    fn hex_case_insensitive() {
        let mut s = Scanner::new("|\\x4a; \\X4A;|");
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
                kind: TokenKind::Identifier(txt),
                span: Range { start: 0, end: 13 },
            } if txt == "J J"
        ));
    }

    #[test]
    fn higher_plane_raw() {
        let mut s = Scanner::new("|\u{fff9} \u{e0001} \u{100001}|");
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
                kind: TokenKind::Identifier(txt),
                span: Range { start: 0, end: 15 },
            } if txt == "\u{fff9} \u{e0001} \u{100001}"
        ));
    }

    #[test]
    fn higher_plane_hex() {
        let mut s = Scanner::new("|\\xfff9; \\xe0001; \\x100001;|");
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
                kind: TokenKind::Identifier(txt),
                span: Range { start: 0, end: 28 },
            } if txt == "\u{fff9} \u{e0001} \u{100001}"
        ));
    }

    #[test]
    fn invalid_escape() {
        let mut s = Scanner::new("|\\B|");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scanner: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(matches!(c, Some(TokenContinuation::SubidentifierError)));
        let err = err_or_fail!(r);
        assert!(matches!(
            err,
            TokenError {
                kind: TokenErrorKind::IdentifierEscapeInvalid { at: 1, ch: 'B' },
                span: Range { start: 1, end: 3 },
            }
        ));
    }

    #[test]
    fn hex_sign_invalid() {
        let mut s = Scanner::new("|\\x+A;|");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scanner: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(matches!(c, Some(TokenContinuation::SubidentifierError)));
        let err = err_or_fail!(r);
        assert!(matches!(
            err,
            TokenError {
                kind: TokenErrorKind::IdentifierExpectedHex { at: 1 },
                span: Range { start: 1, end: 6 },
            }
        ));
    }

    #[test]
    fn hex_too_large() {
        let mut s = Scanner::new("|\\xdeadbeef;|");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scanner: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(matches!(c, Some(TokenContinuation::SubidentifierError)));
        let err = err_or_fail!(r);
        assert!(matches!(
            err,
            TokenError {
                kind: TokenErrorKind::IdentifierInvalidHex { at: 1 },
                span: Range { start: 1, end: 12 },
            }
        ));
    }

    #[test]
    fn hex_malformed() {
        let mut s = Scanner::new("|\\x124nope;|");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scanner: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(matches!(c, Some(TokenContinuation::SubidentifierError)));
        let err = err_or_fail!(r);
        assert!(matches!(
            err,
            TokenError {
                kind: TokenErrorKind::IdentifierExpectedHex { at: 1 },
                span: Range { start: 1, end: 11 },
            }
        ));
    }

    #[test]
    fn hex_unterminated() {
        let mut s = Scanner::new("|\\x123|");
        let start = some_or_fail!(s.next_token());
        let t = Tokenizer {
            scanner: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(matches!(c, Some(TokenContinuation::SubidentifierError)));
        let err = err_or_fail!(r);
        assert!(matches!(
            err,
            TokenError {
                kind: TokenErrorKind::IdentifierUnterminatedHex { at: 1 },
                span: Range { start: 1, end: 6 },
            }
        ));
    }

    #[test]
    fn discard() {
        let mut s = Scanner::new("\\xbadstuff; discard this");
        s.find_any_char(&[';']);
        let start = s.pos();
        let t = Continuation {
            cont: TokenContinuation::SubidentifierError,
            scanner: &mut s,
            start,
        };

        let (r, c) = t.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::IdentifierDiscard,
                span: Range { start: 11, end: 24 },
            }
        ));
    }

    #[test]
    fn begin() {
        let mut s = Scanner::new("|beginning verbatim");
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
                kind: TokenKind::IdentifierBegin(txt),
                span: Range { start: 0, end: 19 },
            } if txt == "beginning verbatim"
        ));
    }

    #[test]
    fn begin_ignores_line_continuation() {
        let mut s = Scanner::new("|beginning verbatim\\");
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
                kind: TokenKind::IdentifierBegin(txt),
                span: Range { start: 0, end: 20 },
            } if txt == "beginning verbatim"
        ));
    }

    #[test]
    fn begin_ignores_line_continuation_with_leading_whitespace() {
        let mut s = Scanner::new("|beginning verbatim    \\");
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
                kind: TokenKind::IdentifierBegin(txt),
                span: Range { start: 0, end: 24 },
            } if txt == "beginning verbatim    "
        ));
    }

    #[test]
    fn begin_ignores_line_continuation_with_trailing_whitespace() {
        let mut s = Scanner::new("|beginning verbatim\\    ");
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
                kind: TokenKind::IdentifierBegin(txt),
                span: Range { start: 0, end: 24 },
            } if txt == "beginning verbatim    "
        ));
    }

    #[test]
    fn fragment() {
        let mut s = Scanner::new("continued verbatim");
        let c = Continuation {
            cont: TokenContinuation::VerbatimIdentifier,
            scanner: &mut s,
            start: 0,
        };

        let (r, c) = c.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::IdentifierFragment(txt),
                span: Range { start: 0, end: 18 },
            } if txt == "continued verbatim"
        ));
    }

    #[test]
    fn fragment_includes_whitespace() {
        let mut s = Scanner::new("   continued verbatim");
        let c = Continuation {
            cont: TokenContinuation::VerbatimIdentifier,
            scanner: &mut s,
            start: 0,
        };

        let (r, c) = c.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::IdentifierFragment(txt),
                span: Range { start: 0, end: 21 },
            } if txt == "   continued verbatim"
        ));
    }

    #[test]
    fn fragment_ignores_line_continuation() {
        let mut s = Scanner::new("continued verbatim  \\  \\  ");
        let c = Continuation {
            cont: TokenContinuation::VerbatimIdentifier,
            scanner: &mut s,
            start: 0,
        };

        let (r, c) = c.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::IdentifierFragment(txt),
                span: Range { start: 0, end: 26 },
            } if txt == "continued verbatim      "
        ));
    }

    #[test]
    fn end() {
        let mut s = Scanner::new("end verbatim|");
        let c = Continuation {
            cont: TokenContinuation::VerbatimIdentifier,
            scanner: &mut s,
            start: 0,
        };

        let (r, c) = c.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::IdentifierEnd(txt),
                span: Range { start: 0, end: 13 },
            } if txt == "end verbatim"
        ));
    }

    #[test]
    fn end_includes_whitespace() {
        let mut s = Scanner::new("   end verbatim  |");
        let c = Continuation {
            cont: TokenContinuation::VerbatimIdentifier,
            scanner: &mut s,
            start: 0,
        };

        let (r, c) = c.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::IdentifierEnd(txt),
                span: Range { start: 0, end: 18 },
            } if txt == "   end verbatim  "
        ));
    }

    #[test]
    fn end_with_escaped_whitespace() {
        let mut s = Scanner::new("end verbatim  \\  \\  |");
        let c = Continuation {
            cont: TokenContinuation::VerbatimIdentifier,
            scanner: &mut s,
            start: 0,
        };

        let (r, c) = c.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::IdentifierEnd(txt),
                span: Range { start: 0, end: 21 },
            } if txt == "end verbatim      "
        ));
    }
}
