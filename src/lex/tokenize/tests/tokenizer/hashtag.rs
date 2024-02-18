use super::*;

#[test]
fn unterminated() {
    let mut s = Scanner::new("#");
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
            kind: TokenErrorKind::HashUnterminated,
            span: Range { start: 0, end: 1 },
        }
    ));
}

#[test]
fn unterminated_with_whitespace() {
    let mut s = Scanner::new("#  ");
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
            kind: TokenErrorKind::HashUnterminated,
            span: Range { start: 0, end: 1 },
        }
    ));
}

#[test]
fn unterminated_with_delimiter() {
    let mut s = Scanner::new("#)");
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
            kind: TokenErrorKind::HashUnterminated,
            span: Range { start: 0, end: 1 },
        }
    ));
}

#[test]
fn invalid() {
    let mut s = Scanner::new("#g");
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
            kind: TokenErrorKind::HashInvalid,
            span: Range { start: 0, end: 2 },
        }
    ));
}

#[test]
fn invalid_long() {
    let mut s = Scanner::new("#not_a_valid_hashtag");
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
            kind: TokenErrorKind::HashInvalid,
            span: Range { start: 0, end: 20 },
        }
    ));
}

#[test]
fn directive_fold_case() {
    let mut s = Scanner::new("#!fold-case");
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
            kind: TokenKind::DirectiveCase(true),
            span: Range { start: 0, end: 11 },
        }
    ));
}

#[test]
fn directive_no_fold_case() {
    let mut s = Scanner::new("#!no-fold-case");
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
            kind: TokenKind::DirectiveCase(false),
            span: Range { start: 0, end: 14 },
        }
    ));
}

#[test]
fn directive_case_insensitive() {
    let mut s = Scanner::new("#!FOLD-CasE");
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
            kind: TokenKind::DirectiveCase(true),
            span: Range { start: 0, end: 11 },
        }
    ));
}

#[test]
fn directive_fold_case_followed_by_token() {
    let mut s = Scanner::new("#!fold-case#t");
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
            kind: TokenKind::DirectiveCase(true),
            span: Range { start: 0, end: 11 },
        }
    ));
}

#[test]
fn directive_expected() {
    let mut s = Scanner::new("#!");
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
            kind: TokenErrorKind::DirectiveExpected,
            span: Range { start: 0, end: 2 },
        }
    ));
}

#[test]
fn directive_invalid() {
    let mut s = Scanner::new("#!foobar");
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
            kind: TokenErrorKind::DirectiveInvalid,
            span: Range { start: 0, end: 8 },
        }
    ));
}

#[test]
fn vector() {
    let mut s = Scanner::new("#(");
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
            kind: TokenKind::Vector,
            span: Range { start: 0, end: 2 },
        }
    ));
}

#[test]
fn radix_with_no_number() {
    let cases = ["#d", "#x"];
    for case in cases {
        let mut s = Scanner::new(case);
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
                kind: TokenErrorKind::NumberExpected,
                span: Range { start: 0, end: 2 },
            }
        ));
    }
}

#[test]
fn radix_with_invalid_number() {
    let cases = ["#dfoo", "#ofoo"];
    for case in cases {
        let mut s = Scanner::new(case);
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
                kind: TokenErrorKind::NumberExpected,
                span: Range { start: 0, end: 5 },
            }
        ));
    }
}

#[test]
fn decimal_radix_with_malformed_number() {
    let mut s = Scanner::new("#d4.2.2");
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
            kind: TokenErrorKind::NumberUnexpectedDecimalPoint { at: 5 },
            span: Range { start: 5, end: 7 },
        }
    ));
}

#[test]
fn radix_with_malformed_number() {
    let mut s = Scanner::new("#x456xyz");
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
            span: Range { start: 0, end: 8 },
        }
    ));
}

#[test]
fn exactness_with_no_number() {
    let cases = ["#e", "#i"];
    for case in cases {
        let mut s = Scanner::new(case);
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
                kind: TokenErrorKind::NumberExpected,
                span: Range { start: 0, end: 2 },
            }
        ));
    }
}

#[test]
fn exactness_with_invalid_number() {
    let cases = ["#efoo", "#ibar"];
    for case in cases {
        let mut s = Scanner::new(case);
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
                kind: TokenErrorKind::NumberExpected,
                span: Range { start: 0, end: 5 },
            }
        ));
    }
}

#[test]
fn exactness_with_malformed_number() {
    let cases = ["#e4foo", "#i5bar"];
    for case in cases {
        let mut s = Scanner::new(case);
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
                span: Range { start: 0, end: 6 },
            }
        ));
    }
}

#[test]
fn exactness_missing_radix() {
    let mut s = Scanner::new("#e#");
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
            kind: TokenErrorKind::RadixExpected { at: 2 },
            span: Range { start: 2, end: 3 },
        }
    ));
}

#[test]
fn exactness_malformed_radix() {
    let mut s = Scanner::new("#e#h2af");
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
            kind: TokenErrorKind::RadixExpected { at: 2 },
            span: Range { start: 2, end: 7 },
        }
    ));
}

#[test]
fn radix_missing_exactness() {
    let mut s = Scanner::new("#x#");
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
            kind: TokenErrorKind::ExactnessExpected { at: 2 },
            span: Range { start: 2, end: 3 },
        }
    ));
}

#[test]
fn radix_malformed_exactness() {
    let mut s = Scanner::new("#x#g34234");
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
            kind: TokenErrorKind::ExactnessExpected { at: 2 },
            span: Range { start: 2, end: 9 },
        }
    ));
}

mod comments {
    use super::*;

    #[test]
    fn block_comment_empty() {
        let mut s = Scanner::new("#||#");
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
                kind: TokenKind::Comment,
                span: Range { start: 0, end: 4 },
            }
        ));
    }

    #[test]
    fn block_comment_with_text() {
        let mut s = Scanner::new("#|i am a comment|#");
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
                kind: TokenKind::Comment,
                span: Range { start: 0, end: 18 },
            }
        ));
    }

    #[test]
    fn block_comment_with_hash() {
        let mut s = Scanner::new("#|hashtag #comment|#");
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
                kind: TokenKind::Comment,
                span: Range { start: 0, end: 20 },
            }
        ));
    }

    #[test]
    fn block_comment_with_pipe() {
        let mut s = Scanner::new("#|pipe |comment|#");
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
                kind: TokenKind::Comment,
                span: Range { start: 0, end: 17 },
            }
        ));
    }

    #[test]
    fn block_comment_nested() {
        let mut s = Scanner::new("#|outer #|inner comment|# comment|#");
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
                kind: TokenKind::Comment,
                span: Range { start: 0, end: 35 },
            }
        ));
    }

    #[test]
    fn block_comment_followed_by_other_text() {
        let mut s = Scanner::new("#| comment |# other stuff");
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
                kind: TokenKind::Comment,
                span: Range { start: 0, end: 13 },
            }
        ));
    }

    #[test]
    fn block_comment_begin() {
        let mut s = Scanner::new("#| comment that continues...");
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
                kind: TokenKind::CommentBlockBegin { depth: 0 },
                span: Range { start: 0, end: 28 },
            }
        ));
    }

    #[test]
    fn block_comment_begin_with_hash() {
        let mut s = Scanner::new("#| comment with #hash...");
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
                kind: TokenKind::CommentBlockBegin { depth: 0 },
                span: Range { start: 0, end: 24 },
            }
        ));
    }

    #[test]
    fn block_comment_begin_with_pipe() {
        let mut s = Scanner::new("#| comment with |pipe...");
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
                kind: TokenKind::CommentBlockBegin { depth: 0 },
                span: Range { start: 0, end: 24 },
            }
        ));
    }

    #[test]
    fn block_comment_begin_contained_nested_comment() {
        let mut s = Scanner::new("#| begin #| nested |# continue...");
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
                kind: TokenKind::CommentBlockBegin { depth: 0 },
                span: Range { start: 0, end: 33 },
            }
        ));
    }

    #[test]
    fn block_comment_begin_with_trailing_nested() {
        let mut s = Scanner::new("#| begin #| nested...");
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
                kind: TokenKind::CommentBlockBegin { depth: 1 },
                span: Range { start: 0, end: 21 },
            }
        ));
    }

    #[test]
    fn block_comment_begin_with_multiple_trailing_nested() {
        let mut s = Scanner::new("#| begin #| nested #| goofy...");
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
                kind: TokenKind::CommentBlockBegin { depth: 2 },
                span: Range { start: 0, end: 30 },
            }
        ));
    }

    #[test]
    fn block_comment_fragment_top_nesting() {
        let mut s = Scanner::new("continued comment");
        let c = Continuation {
            cont: TokenContinuation::BlockComment { depth: 0 },
            scanner: &mut s,
            start: 0,
        };

        let (r, c) = c.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::CommentBlockFragment { depth: 0 },
                span: Range { start: 0, end: 17 },
            }
        ));
    }

    #[test]
    fn block_comment_fragment_includes_whitespace() {
        let mut s = Scanner::new("  continued comment\t");
        let c = Continuation {
            cont: TokenContinuation::BlockComment { depth: 0 },
            scanner: &mut s,
            start: 0,
        };

        let (r, c) = c.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::CommentBlockFragment { depth: 0 },
                span: Range { start: 0, end: 20 },
            }
        ));
    }

    #[test]
    fn block_comment_fragment_contains_block_comment() {
        let mut s = Scanner::new("continue #| whole comment |# more...");
        let c = Continuation {
            cont: TokenContinuation::BlockComment { depth: 0 },
            scanner: &mut s,
            start: 0,
        };

        let (r, c) = c.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::CommentBlockFragment { depth: 0 },
                span: Range { start: 0, end: 36 },
            }
        ));
    }

    #[test]
    fn block_comment_fragment_with_nesting() {
        let mut s = Scanner::new("continue #| partial comment...");
        let c = Continuation {
            cont: TokenContinuation::BlockComment { depth: 0 },
            scanner: &mut s,
            start: 0,
        };

        let (r, c) = c.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::CommentBlockFragment { depth: 1 },
                span: Range { start: 0, end: 30 },
            }
        ));
    }

    #[test]
    fn block_comment_fragment_extends_nesting() {
        let mut s = Scanner::new("continue #| partial comment...");
        let c = Continuation {
            cont: TokenContinuation::BlockComment { depth: 2 },
            scanner: &mut s,
            start: 0,
        };

        let (r, c) = c.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::CommentBlockFragment { depth: 3 },
                span: Range { start: 0, end: 30 },
            }
        ));
    }

    #[test]
    fn block_comment_end() {
        let mut s = Scanner::new("end comment |#");
        let c = Continuation {
            cont: TokenContinuation::BlockComment { depth: 0 },
            scanner: &mut s,
            start: 0,
        };

        let (r, c) = c.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::CommentBlockEnd,
                span: Range { start: 0, end: 14 },
            }
        ));
    }

    #[test]
    fn block_comment_end_includes_leading_whitespace() {
        let mut s = Scanner::new("  end comment |#  ");
        let c = Continuation {
            cont: TokenContinuation::BlockComment { depth: 0 },
            scanner: &mut s,
            start: 0,
        };

        let (r, c) = c.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::CommentBlockEnd,
                span: Range { start: 0, end: 16 },
            }
        ));
    }

    #[test]
    fn block_comment_end_stops_at_next_token() {
        let mut s = Scanner::new("end comment |##t");
        let c = Continuation {
            cont: TokenContinuation::BlockComment { depth: 0 },
            scanner: &mut s,
            start: 0,
        };

        let (r, c) = c.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::CommentBlockEnd,
                span: Range { start: 0, end: 14 },
            }
        ));
    }

    #[test]
    fn block_comment_end_contains_whole_comment() {
        let mut s = Scanner::new("end comment #| nested |# |#");
        let c = Continuation {
            cont: TokenContinuation::BlockComment { depth: 0 },
            scanner: &mut s,
            start: 0,
        };

        let (r, c) = c.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::CommentBlockEnd,
                span: Range { start: 0, end: 27 },
            }
        ));
    }

    #[test]
    fn block_comment_ends_multiple_nesting() {
        let mut s = Scanner::new("end inner |# end outer |#");
        let c = Continuation {
            cont: TokenContinuation::BlockComment { depth: 1 },
            scanner: &mut s,
            start: 0,
        };

        let (r, c) = c.extract();

        assert!(c.is_none());
        let tok = ok_or_fail!(r);
        assert!(matches!(
            tok,
            Token {
                kind: TokenKind::CommentBlockEnd,
                span: Range { start: 0, end: 25 },
            }
        ));
    }

    #[test]
    fn datum_comment() {
        let mut s = Scanner::new("#;");
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
                kind: TokenKind::CommentDatum,
                span: Range { start: 0, end: 2 },
            }
        ));
    }

    #[test]
    fn datum_comment_followed_by_datum() {
        let mut s = Scanner::new("#;#\\a");
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
                kind: TokenKind::CommentDatum,
                span: Range { start: 0, end: 2 },
            }
        ));
    }

    #[test]
    fn datum_comment_followed_by_whitespace() {
        let mut s = Scanner::new("#; ");
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
                kind: TokenKind::CommentDatum,
                span: Range { start: 0, end: 2 },
            }
        ));
    }
}
