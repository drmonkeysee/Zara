use super::*;

#[test]
fn unterminated() {
    let mut s = Scanner::new("#");
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
            result: Err(TokenErrorKind::HashUnterminated),
        }
    ));
}

#[test]
fn unterminated_with_whitespace() {
    let mut s = Scanner::new("#  ");
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
            result: Err(TokenErrorKind::HashUnterminated),
        }
    ));
}

#[test]
fn unterminated_with_delimiter() {
    let mut s = Scanner::new("#)");
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
            result: Err(TokenErrorKind::HashUnterminated),
        }
    ));
}

#[test]
fn invalid() {
    let mut s = Scanner::new("#g");
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
            result: Err(TokenErrorKind::HashInvalid),
        }
    ));
}

#[test]
fn invalid_long() {
    let mut s = Scanner::new("#not_a_valid_hashtag");
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
            result: Err(TokenErrorKind::HashInvalid),
        }
    ));
}

#[test]
fn directive_fold_case() {
    let mut s = Scanner::new("#!fold-case");
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
            result: Ok(TokenKind::DirectiveCase(true)),
        }
    ));
}

#[test]
fn directive_no_fold_case() {
    let mut s = Scanner::new("#!no-fold-case");
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
            end: 14,
            result: Ok(TokenKind::DirectiveCase(false)),
        }
    ));
}

#[test]
fn directive_case_insensitive() {
    let mut s = Scanner::new("#!FOLD-CasE");
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
            result: Ok(TokenKind::DirectiveCase(true)),
        }
    ));
}

#[test]
fn directive_fold_case_followed_by_token() {
    let mut s = Scanner::new("#!fold-case#t");
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
            result: Ok(TokenKind::DirectiveCase(true)),
        }
    ));
}

#[test]
fn directive_expected() {
    let mut s = Scanner::new("#!");
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
            result: Err(TokenErrorKind::DirectiveExpected),
        }
    ));
}

#[test]
fn directive_invalid() {
    let mut s = Scanner::new("#!foobar");
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
            end: 8,
            result: Err(TokenErrorKind::DirectiveInvalid),
        }
    ));
}

#[test]
fn vector() {
    let mut s = Scanner::new("#(");
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
            result: Ok(TokenKind::Vector),
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
            scan: &mut s,
            start,
        };

        let r = t.extract();

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 4,
                result: Ok(TokenKind::Comment),
            }
        ));
    }

    #[test]
    fn block_comment_with_text() {
        let mut s = Scanner::new("#|i am a comment|#");
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
                result: Ok(TokenKind::Comment),
            }
        ));
    }

    #[test]
    fn block_comment_with_hash() {
        let mut s = Scanner::new("#|hashtag #comment|#");
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
                result: Ok(TokenKind::Comment),
            }
        ));
    }

    #[test]
    fn block_comment_with_pipe() {
        let mut s = Scanner::new("#|pipe |comment|#");
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
                result: Ok(TokenKind::Comment),
            }
        ));
    }

    #[test]
    fn block_comment_nested() {
        let mut s = Scanner::new("#|outer #|inner comment|# comment|#");
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
                end: 35,
                result: Ok(TokenKind::Comment),
            }
        ));
    }

    #[test]
    fn block_comment_followed_by_other_text() {
        let mut s = Scanner::new("#| comment |# other stuff");
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
                result: Ok(TokenKind::Comment),
            }
        ));
    }

    #[test]
    fn block_comment_begin() {
        let mut s = Scanner::new("#| comment that continues...");
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
                result: Ok(TokenKind::CommentBlockBegin { depth: 0 }),
            }
        ));
    }

    #[test]
    fn block_comment_begin_with_hash() {
        let mut s = Scanner::new("#| comment with #hash...");
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
                end: 24,
                result: Ok(TokenKind::CommentBlockBegin { depth: 0 }),
            }
        ));
    }

    #[test]
    fn block_comment_begin_with_pipe() {
        let mut s = Scanner::new("#| comment with |pipe...");
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
                end: 24,
                result: Ok(TokenKind::CommentBlockBegin { depth: 0 }),
            }
        ));
    }

    #[test]
    fn block_comment_begin_contained_nested_comment() {
        let mut s = Scanner::new("#| begin #| nested |# continue...");
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
                end: 33,
                result: Ok(TokenKind::CommentBlockBegin { depth: 0 }),
            }
        ));
    }

    #[test]
    fn block_comment_begin_with_trailing_nested() {
        let mut s = Scanner::new("#| begin #| nested...");
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
                end: 21,
                result: Ok(TokenKind::CommentBlockBegin { depth: 1 }),
            }
        ));
    }

    #[test]
    fn block_comment_begin_with_multiple_trailing_nested() {
        let mut s = Scanner::new("#| begin #| nested #| goofy...");
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
                end: 30,
                result: Ok(TokenKind::CommentBlockBegin { depth: 2 }),
            }
        ));
    }

    #[test]
    fn block_comment_fragment_top_nesting() {
        let mut s = Scanner::new("continued comment");
        let c = Continuation {
            cont: TokenContinuation::BlockComment { depth: 0 },
            scan: &mut s,
            start: 0,
        };

        let r = c.extract();

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 17,
                result: Ok(TokenKind::CommentBlockFragment { depth: 0 }),
            }
        ));
    }

    #[test]
    fn block_comment_fragment_includes_whitespace() {
        let mut s = Scanner::new("  continued comment\t");
        let c = Continuation {
            cont: TokenContinuation::BlockComment { depth: 0 },
            scan: &mut s,
            start: 0,
        };

        let r = c.extract();

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 20,
                result: Ok(TokenKind::CommentBlockFragment { depth: 0 }),
            }
        ));
    }

    #[test]
    fn block_comment_fragment_contains_block_comment() {
        let mut s = Scanner::new("continue #| whole comment |# more...");
        let c = Continuation {
            cont: TokenContinuation::BlockComment { depth: 0 },
            scan: &mut s,
            start: 0,
        };

        let r = c.extract();

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 36,
                result: Ok(TokenKind::CommentBlockFragment { depth: 0 }),
            }
        ));
    }

    #[test]
    fn block_comment_fragment_with_nesting() {
        let mut s = Scanner::new("continue #| partial comment...");
        let c = Continuation {
            cont: TokenContinuation::BlockComment { depth: 0 },
            scan: &mut s,
            start: 0,
        };

        let r = c.extract();

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 30,
                result: Ok(TokenKind::CommentBlockFragment { depth: 1 }),
            }
        ));
    }

    #[test]
    fn block_comment_fragment_extends_nesting() {
        let mut s = Scanner::new("continue #| partial comment...");
        let c = Continuation {
            cont: TokenContinuation::BlockComment { depth: 2 },
            scan: &mut s,
            start: 0,
        };

        let r = c.extract();

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 30,
                result: Ok(TokenKind::CommentBlockFragment { depth: 3 }),
            }
        ));
    }

    #[test]
    fn block_comment_end() {
        let mut s = Scanner::new("end comment |#");
        let c = Continuation {
            cont: TokenContinuation::BlockComment { depth: 0 },
            scan: &mut s,
            start: 0,
        };

        let r = c.extract();

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 14,
                result: Ok(TokenKind::CommentBlockEnd),
            }
        ));
    }

    #[test]
    fn block_comment_end_includes_leading_whitespace() {
        let mut s = Scanner::new("  end comment |#  ");
        let c = Continuation {
            cont: TokenContinuation::BlockComment { depth: 0 },
            scan: &mut s,
            start: 0,
        };

        let r = c.extract();

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 16,
                result: Ok(TokenKind::CommentBlockEnd),
            }
        ));
    }

    #[test]
    fn block_comment_end_stops_at_next_token() {
        let mut s = Scanner::new("end comment |##t");
        let c = Continuation {
            cont: TokenContinuation::BlockComment { depth: 0 },
            scan: &mut s,
            start: 0,
        };

        let r = c.extract();

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 14,
                result: Ok(TokenKind::CommentBlockEnd),
            }
        ));
    }

    #[test]
    fn block_comment_end_contains_whole_comment() {
        let mut s = Scanner::new("end comment #| nested |# |#");
        let c = Continuation {
            cont: TokenContinuation::BlockComment { depth: 0 },
            scan: &mut s,
            start: 0,
        };

        let r = c.extract();

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 27,
                result: Ok(TokenKind::CommentBlockEnd),
            }
        ));
    }

    #[test]
    fn block_comment_ends_multiple_nesting() {
        let mut s = Scanner::new("end inner |# end outer |#");
        let c = Continuation {
            cont: TokenContinuation::BlockComment { depth: 1 },
            scan: &mut s,
            start: 0,
        };

        let r = c.extract();

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 25,
                result: Ok(TokenKind::CommentBlockEnd),
            }
        ));
    }

    #[test]
    fn datum_comment() {
        let mut s = Scanner::new("#;");
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
                result: Ok(TokenKind::CommentDatum),
            }
        ));
    }

    #[test]
    fn datum_comment_followed_by_datum() {
        let mut s = Scanner::new("#;#\\a");
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
                result: Ok(TokenKind::CommentDatum),
            }
        ));
    }

    #[test]
    fn datum_comment_followed_by_whitespace() {
        let mut s = Scanner::new("#; ");
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
                result: Ok(TokenKind::CommentDatum),
            }
        ));
    }
}
