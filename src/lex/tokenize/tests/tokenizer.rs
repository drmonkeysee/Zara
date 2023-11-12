use super::*;

#[test]
fn left_paren() {
    let mut s = Scanner::new("(");
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
            result: Ok(TokenKind::ParenLeft),
        }
    ));
}

#[test]
fn right_paren() {
    let mut s = Scanner::new(")");
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
            result: Ok(TokenKind::ParenRight),
        }
    ));
}

#[test]
fn pair_joiner() {
    let mut s = Scanner::new(".");
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
            result: Ok(TokenKind::PairJoiner),
        }
    ));
}

#[test]
fn pair_joiner_with_whitespace() {
    let mut s = Scanner::new(" . ");
    let start = s.next_token().unwrap();
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
            result: Ok(TokenKind::Identifier(txt)),
        } if txt == "a."
    ));
}

#[test]
fn pair_joiner_postfixed_is_identifier() {
    let mut s = Scanner::new(".a");
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
            result: Ok(TokenKind::Identifier(txt)),
        } if txt == ".a"
    ));
}

#[test]
fn pair_joiner_followed_by_delimiter() {
    let mut s = Scanner::new(".)");
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
            result: Ok(TokenKind::PairJoiner),
        }
    ));
}

#[test]
fn token_ends_at_whitespace() {
    let mut s = Scanner::new("(  ");
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
            result: Ok(TokenKind::ParenLeft),
        }
    ));
}

#[test]
fn token_ends_at_delimiter() {
    let mut s = Scanner::new("()");
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
            result: Ok(TokenKind::ParenLeft),
        }
    ));
}

#[test]
fn comment() {
    let mut s = Scanner::new(";");
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
            result: Ok(TokenKind::Comment),
        }
    ));
}

#[test]
fn comment_with_text() {
    let mut s = Scanner::new("; scanner input is always one line");
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
            end: 34,
            result: Ok(TokenKind::Comment),
        }
    ));
}

mod hashtag {
    use super::*;

    #[test]
    fn unterminated() {
        let mut s = Scanner::new("#");
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
                result: Err(TokenErrorKind::HashUnterminated),
            }
        ));
    }

    #[test]
    fn unterminated_with_whitespace() {
        let mut s = Scanner::new("#  ");
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
                result: Err(TokenErrorKind::HashUnterminated),
            }
        ));
    }

    #[test]
    fn unterminated_with_delimiter() {
        let mut s = Scanner::new("#)");
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
                result: Err(TokenErrorKind::HashUnterminated),
            }
        ));
    }

    #[test]
    fn invalid() {
        let mut s = Scanner::new("#g");
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
                result: Err(TokenErrorKind::HashInvalid),
            }
        ));
    }

    #[test]
    fn invalid_long() {
        let mut s = Scanner::new("#not_a_valid_hashtag");
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
                end: 20,
                result: Err(TokenErrorKind::HashInvalid),
            }
        ));
    }

    #[test]
    fn directive_fold_case() {
        let mut s = Scanner::new("#!fold-case");
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
                end: 11,
                result: Ok(TokenKind::DirectiveCase(true)),
            }
        ));
    }

    #[test]
    fn directive_no_fold_case() {
        let mut s = Scanner::new("#!no-fold-case");
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
                end: 14,
                result: Ok(TokenKind::DirectiveCase(false)),
            }
        ));
    }

    #[test]
    fn directive_case_insensitive() {
        let mut s = Scanner::new("#!FOLD-CasE");
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
                end: 11,
                result: Ok(TokenKind::DirectiveCase(true)),
            }
        ));
    }

    #[test]
    fn directive_fold_case_followed_by_token() {
        let mut s = Scanner::new("#!fold-case#t");
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
                end: 11,
                result: Ok(TokenKind::DirectiveCase(true)),
            }
        ));
    }

    #[test]
    fn directive_expected() {
        let mut s = Scanner::new("#!");
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
                result: Err(TokenErrorKind::DirectiveExpected),
            }
        ));
    }

    #[test]
    fn directive_invalid() {
        let mut s = Scanner::new("#!foobar");
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
                end: 8,
                result: Err(TokenErrorKind::DirectiveInvalid),
            }
        ));
    }

    #[test]
    fn vector() {
        let mut s = Scanner::new("#(");
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
                result: Ok(TokenKind::Vector),
            }
        ));
    }

    mod comments {
        use super::*;

        #[test]
        fn block_comment_empty() {
            let mut s = Scanner::new("#||#");
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
                    end: 4,
                    result: Ok(TokenKind::CommentBlock),
                }
            ));
        }

        #[test]
        fn block_comment_with_text() {
            let mut s = Scanner::new("#|i am a comment|#");
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
                    end: 18,
                    result: Ok(TokenKind::CommentBlock),
                }
            ));
        }

        #[test]
        fn block_comment_with_hash() {
            let mut s = Scanner::new("#|hashtag #comment|#");
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
                    end: 20,
                    result: Ok(TokenKind::CommentBlock),
                }
            ));
        }

        #[test]
        fn block_comment_with_pipe() {
            let mut s = Scanner::new("#|pipe |comment|#");
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
                    end: 17,
                    result: Ok(TokenKind::CommentBlock),
                }
            ));
        }

        #[test]
        fn block_comment_nested() {
            let mut s = Scanner::new("#|outer #|inner comment|# comment|#");
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
                    end: 35,
                    result: Ok(TokenKind::CommentBlock),
                }
            ));
        }

        #[test]
        fn block_comment_followed_by_other_text() {
            let mut s = Scanner::new("#| comment |# other stuff");
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
                    end: 13,
                    result: Ok(TokenKind::CommentBlock),
                }
            ));
        }

        #[test]
        fn block_comment_begin() {
            let mut s = Scanner::new("#| comment that continues...");
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
                    end: 28,
                    result: Ok(TokenKind::CommentBlockBegin(0)),
                }
            ));
        }

        #[test]
        fn block_comment_begin_with_hash() {
            let mut s = Scanner::new("#| comment with #hash...");
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
                    end: 24,
                    result: Ok(TokenKind::CommentBlockBegin(0)),
                }
            ));
        }

        #[test]
        fn block_comment_begin_with_pipe() {
            let mut s = Scanner::new("#| comment with |pipe...");
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
                    end: 24,
                    result: Ok(TokenKind::CommentBlockBegin(0)),
                }
            ));
        }

        #[test]
        fn block_comment_begin_contained_nested_comment() {
            let mut s = Scanner::new("#| begin #| nested |# continue...");
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
                    end: 33,
                    result: Ok(TokenKind::CommentBlockBegin(0)),
                }
            ));
        }

        #[test]
        fn block_comment_begin_with_trailing_nested() {
            let mut s = Scanner::new("#| begin #| nested...");
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
                    end: 21,
                    result: Ok(TokenKind::CommentBlockBegin(1)),
                }
            ));
        }

        #[test]
        fn block_comment_begin_with_multiple_trailing_nested() {
            let mut s = Scanner::new("#| begin #| nested #| goofy...");
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
                    end: 30,
                    result: Ok(TokenKind::CommentBlockBegin(2)),
                }
            ));
        }

        #[test]
        fn block_comment_fragment_top_nesting() {
            let mut s = Scanner::new("continued comment");
            let c = Continuation {
                cont: TokenContinuation::BlockComment(0),
                scan: &mut s,
                start: 0,
            };

            let r = c.extract();

            assert!(matches!(
                r,
                TokenExtract {
                    start: 0,
                    end: 17,
                    result: Ok(TokenKind::CommentBlockFragment(0)),
                }
            ));
        }

        #[test]
        fn block_comment_fragment_includes_whitespace() {
            let mut s = Scanner::new("  continued comment\t");
            let c = Continuation {
                cont: TokenContinuation::BlockComment(0),
                scan: &mut s,
                start: 0,
            };

            let r = c.extract();

            assert!(matches!(
                r,
                TokenExtract {
                    start: 0,
                    end: 20,
                    result: Ok(TokenKind::CommentBlockFragment(0)),
                }
            ));
        }

        #[test]
        fn block_comment_fragment_contains_block_comment() {
            let mut s = Scanner::new("continue #| whole comment |# more...");
            let c = Continuation {
                cont: TokenContinuation::BlockComment(0),
                scan: &mut s,
                start: 0,
            };

            let r = c.extract();

            assert!(matches!(
                r,
                TokenExtract {
                    start: 0,
                    end: 36,
                    result: Ok(TokenKind::CommentBlockFragment(0)),
                }
            ));
        }

        #[test]
        fn block_comment_fragment_with_nesting() {
            let mut s = Scanner::new("continue #| partial comment...");
            let c = Continuation {
                cont: TokenContinuation::BlockComment(0),
                scan: &mut s,
                start: 0,
            };

            let r = c.extract();

            assert!(matches!(
                r,
                TokenExtract {
                    start: 0,
                    end: 30,
                    result: Ok(TokenKind::CommentBlockFragment(1)),
                }
            ));
        }

        #[test]
        fn block_comment_fragment_extends_nesting() {
            let mut s = Scanner::new("continue #| partial comment...");
            let c = Continuation {
                cont: TokenContinuation::BlockComment(2),
                scan: &mut s,
                start: 0,
            };

            let r = c.extract();

            assert!(matches!(
                r,
                TokenExtract {
                    start: 0,
                    end: 30,
                    result: Ok(TokenKind::CommentBlockFragment(3)),
                }
            ));
        }

        #[test]
        fn block_comment_end() {
            let mut s = Scanner::new("end comment |#");
            let c = Continuation {
                cont: TokenContinuation::BlockComment(0),
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
                cont: TokenContinuation::BlockComment(0),
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
                cont: TokenContinuation::BlockComment(0),
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
                cont: TokenContinuation::BlockComment(0),
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
                cont: TokenContinuation::BlockComment(1),
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
                    result: Ok(TokenKind::CommentDatum),
                }
            ));
        }

        #[test]
        fn datum_comment_followed_by_datum() {
            let mut s = Scanner::new("#;#\\a");
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
                    result: Ok(TokenKind::CommentDatum),
                }
            ));
        }

        #[test]
        fn datum_comment_followed_by_whitespace() {
            let mut s = Scanner::new("#; ");
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
                    result: Ok(TokenKind::CommentDatum),
                }
            ));
        }
    }
}

mod bytevector {
    use super::*;

    #[test]
    fn bytevector() {
        let mut s = Scanner::new("#u8(");
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
                end: 4,
                result: Ok(TokenKind::ByteVector),
            }
        ));
    }

    #[test]
    fn bytevector_uppercase() {
        let mut s = Scanner::new("#U8(");
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
                end: 4,
                result: Ok(TokenKind::ByteVector),
            }
        ));
    }

    #[test]
    fn bytevector_ends_at_paren() {
        let mut s = Scanner::new("#u8(sdf");
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
                end: 4,
                result: Ok(TokenKind::ByteVector),
            }
        ));
    }

    #[test]
    fn bytevector_unterminated() {
        let mut s = Scanner::new("#u");
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
                result: Err(TokenErrorKind::ByteVectorExpected),
            }
        ));
    }

    #[test]
    fn bytevector_wrong_number() {
        let mut s = Scanner::new("#u9(");
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
                end: 3,
                result: Err(TokenErrorKind::ByteVectorExpected),
            }
        ));
    }

    #[test]
    fn bytevector_extra_number() {
        let mut s = Scanner::new("#u81(");
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
                end: 4,
                result: Err(TokenErrorKind::ByteVectorExpected),
            }
        ));
    }

    #[test]
    fn bytevector_no_paren() {
        let mut s = Scanner::new("#u8");
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
                end: 3,
                result: Err(TokenErrorKind::ByteVectorExpected),
            }
        ));
    }

    #[test]
    fn bytevector_no_paren_whitespace() {
        let mut s = Scanner::new("#u8  ");
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
                end: 3,
                result: Err(TokenErrorKind::ByteVectorExpected),
            }
        ));
    }
}

mod boolean {
    use super::*;

    #[test]
    fn true_short() {
        let mut s = Scanner::new("#t");
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
                result: Ok(TokenKind::Literal(Literal::Boolean(true))),
            }
        ));
    }

    #[test]
    fn true_long() {
        let mut s = Scanner::new("#true");
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
                end: 5,
                result: Ok(TokenKind::Literal(Literal::Boolean(true))),
            }
        ));
    }

    #[test]
    fn true_uppercase() {
        let mut s = Scanner::new("#TRUE");
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
                end: 5,
                result: Ok(TokenKind::Literal(Literal::Boolean(true))),
            }
        ));
    }

    #[test]
    fn true_malformed() {
        let mut s = Scanner::new("#trueasd");
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
                end: 8,
                result: Err(TokenErrorKind::BooleanExpected(true)),
            }
        ));
    }

    #[test]
    fn false_short() {
        let mut s = Scanner::new("#f");
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
                result: Ok(TokenKind::Literal(Literal::Boolean(false))),
            }
        ));
    }

    #[test]
    fn false_long() {
        let mut s = Scanner::new("#false");
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
                end: 6,
                result: Ok(TokenKind::Literal(Literal::Boolean(false))),
            }
        ));
    }

    #[test]
    fn false_uppercase() {
        let mut s = Scanner::new("#FALSE");
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
                end: 6,
                result: Ok(TokenKind::Literal(Literal::Boolean(false))),
            }
        ));
    }

    #[test]
    fn false_malformed() {
        let mut s = Scanner::new("#fals");
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
                end: 5,
                result: Err(TokenErrorKind::BooleanExpected(false)),
            }
        ));
    }
}

mod character {
    use super::*;

    #[test]
    fn ascii_literal() {
        let mut s = Scanner::new("#\\a");
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
                end: 3,
                result: Ok(TokenKind::Literal(Literal::Character('a'))),
            }
        ));
    }

    #[test]
    fn ascii_uppercase_literal() {
        let mut s = Scanner::new("#\\A");
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
                end: 3,
                result: Ok(TokenKind::Literal(Literal::Character('A'))),
            }
        ));
    }

    #[test]
    fn extended_literal() {
        let mut s = Scanner::new("#\\λ");
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
                end: 4,
                result: Ok(TokenKind::Literal(Literal::Character('λ'))),
            }
        ));
    }

    #[test]
    fn emoji_literal() {
        let mut s = Scanner::new("#\\🦀");
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
                end: 3,
                result: Ok(TokenKind::Literal(Literal::Character(' '))),
            }
        ));
    }

    #[test]
    fn tab_literal() {
        let mut s = Scanner::new("#\\\t");
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
                end: 7,
                result: Err(TokenErrorKind::CharacterExpected),
            }
        ));
    }

    #[test]
    fn space_followed_by_alpha() {
        let mut s = Scanner::new("#\\ b");
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
                end: 3,
                result: Ok(TokenKind::Literal(Literal::Character(' '))),
            }
        ));
    }

    #[test]
    fn alpha_followed_by_alpha() {
        let mut s = Scanner::new("#\\ab");
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
                end: 4,
                result: Err(TokenErrorKind::CharacterExpected),
            }
        ));
    }

    #[test]
    fn emoji_followed_by_alpha() {
        let mut s = Scanner::new("#\\🦀b");
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
                end: 7,
                result: Err(TokenErrorKind::CharacterExpected),
            }
        ));
    }

    #[test]
    fn alpha_followed_by_delimiter() {
        let mut s = Scanner::new("#\\a(");
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
                end: 3,
                result: Ok(TokenKind::Literal(Literal::Character('a'))),
            }
        ));
    }

    #[test]
    fn letter_x_is_not_hex() {
        let mut s = Scanner::new("#\\x");
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
                end: 3,
                result: Ok(TokenKind::Literal(Literal::Character('x'))),
            }
        ));
    }

    #[test]
    fn hex_zero() {
        let mut s = Scanner::new("#\\x0");
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
                end: 4,
                result: Ok(TokenKind::Literal(Literal::Character('\0'))),
            }
        ));
    }

    #[test]
    fn hex_lowercase() {
        let mut s = Scanner::new("#\\xa");
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
                end: 4,
                result: Ok(TokenKind::Literal(Literal::Character('\n'))),
            }
        ));
    }

    #[test]
    fn hex_uppercase() {
        let mut s = Scanner::new("#\\xA");
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
                end: 4,
                result: Ok(TokenKind::Literal(Literal::Character('\n'))),
            }
        ));
    }

    #[test]
    fn hex_uppercase_indicator() {
        let mut s = Scanner::new("#\\Xa");
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
                end: 5,
                result: Err(TokenErrorKind::CharacterExpectedHex),
            }
        ));
    }

    #[test]
    fn hex_too_large() {
        let mut s = Scanner::new("#\\xdeadbeef");
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
                end: 11,
                result: Err(TokenErrorKind::CharacterInvalidHex),
            }
        ));
    }

    #[test]
    fn hex_malformed() {
        let mut s = Scanner::new("#\\x124nope");
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
                end: 10,
                result: Err(TokenErrorKind::CharacterExpectedHex),
            }
        ));
    }

    fn check_character_list(cases: &[(&str, char)]) {
        for &(inp, exp) in cases {
            let input = format!("#\\{inp}");
            let mut s = Scanner::new(&input);
            let start = s.next_token().unwrap();
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
}

mod quoting {
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
}

mod string {
    use super::*;

    #[test]
    fn empty() {
        let mut s = Scanner::new("\"\"");
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
                result: Ok(TokenKind::Literal(Literal::String(s))),
            } if s == ""
        ));
    }

    #[test]
    fn alphanumeric() {
        let mut s = Scanner::new("\"abc123!@#\"");
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
                end: 11,
                result: Ok(TokenKind::Literal(Literal::String(s))),
            } if s == "abc123!@#"
        ));
    }

    #[test]
    fn raw_extended_and_higher_char() {
        let mut s = Scanner::new("\"λ 🦀 \u{2401} \u{fffd}\"");
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
                end: 17,
                result: Ok(TokenKind::Literal(Literal::String(s))),
            } if s == "λ 🦀 ␁ �"
        ));
    }

    #[test]
    fn contains_verbatim_identifier() {
        let mut s = Scanner::new("\"foo |verbatim| bar\"");
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
                end: 20,
                result: Ok(TokenKind::Literal(Literal::String(s))),
            } if s == "foo |verbatim| bar"
        ));
    }

    #[test]
    fn raw_escape_sequences() {
        let mut s =
            Scanner::new("\"a:\x07, b:\x08, d:\x7f, e:\x1b, n:\n, 0:\0, r:\r, t:\t, v:\x7c\"");
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
                end: 45,
                result: Ok(TokenKind::Literal(Literal::String(s))),
            } if s == "a:\x07, b:\x08, d:\x7f, e:\x1b, n:\n, 0:\0, r:\r, t:\t, v:|"
        ));
    }

    #[test]
    fn escape_sequences() {
        let mut s = Scanner::new("\"a:\\a, b:\\b, n:\\n, r:\r, t:\t, q:\\\", s:\\\\, v:\\|\"");
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
                end: 46,
                result: Ok(TokenKind::Literal(Literal::String(s))),
            } if s == "a:\x07, b:\x08, n:\n, r:\r, t:\t, q:\", s:\\, v:|"
        ));
    }

    #[test]
    fn whitespace_escape() {
        let mut s = Scanner::new("\"foo\\   bar\"");
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
                end: 93,
                result: Ok(TokenKind::Literal(Literal::String(s))),
            } if s == "a:\x07, b:\x08, d:\x7f, e:\x1b, n:\n, 0:\0, r:\r, t:\t, q:\", s:\\, v:|"
        ));
    }

    #[test]
    fn hex_case_insensitive() {
        let mut s = Scanner::new("\"\\x4a; \\X4A;\"");
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
                end: 13,
                result: Ok(TokenKind::Literal(Literal::String(s))),
            } if s == "J J"
        ));
    }

    #[test]
    fn higher_plane_raw() {
        let mut s = Scanner::new("\"\u{fff9} \u{e0001} \u{100001}\"");
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
                end: 15,
                result: Ok(TokenKind::Literal(Literal::String(s))),
            } if s == "\u{fff9} \u{e0001} \u{100001}"
        ));
    }

    #[test]
    fn higher_plane_hex() {
        let mut s = Scanner::new("\"\\xfff9; \\xe0001; \\x100001;\"");
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
                end: 28,
                result: Ok(TokenKind::Literal(Literal::String(s))),
            } if s == "\u{fff9} \u{e0001} \u{100001}"
        ));
    }

    #[test]
    fn invalid_escape() {
        let mut s = Scanner::new("\"\\B\"");
        let start = s.next_token().unwrap();
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
                result: Err(TokenErrorKind::StringEscapeInvalid(1, 'B')),
            }
        ));
    }

    #[test]
    fn hex_sign_invalid() {
        let mut s = Scanner::new("\"\\x+A;\"");
        let start = s.next_token().unwrap();
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
        let start = s.next_token().unwrap();
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
        let start = s.next_token().unwrap();
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
        let start = s.next_token().unwrap();
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
    fn string_discard() {
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
    fn string_begin() {
        let mut s = Scanner::new("\"beginning string");
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
                end: 17,
                result: Ok(TokenKind::StringBegin(s, false)),
            } if s == "beginning string"
        ));
    }

    #[test]
    fn string_begin_with_line_continuation() {
        let mut s = Scanner::new("\"beginning string\\");
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
                end: 18,
                result: Ok(TokenKind::StringBegin(s, true)),
            } if s == "beginning string"
        ));
    }

    #[test]
    fn string_begin_with_line_continuation_includes_leading_whitespace() {
        let mut s = Scanner::new("\"beginning string    \\");
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
                end: 22,
                result: Ok(TokenKind::StringBegin(s, true)),
            } if s == "beginning string    "
        ));
    }

    #[test]
    fn string_begin_with_line_continuation_excludes_trailing_whitespace() {
        let mut s = Scanner::new("\"beginning string\\    ");
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
                end: 22,
                result: Ok(TokenKind::StringBegin(s, true)),
            } if s == "beginning string"
        ));
    }

    #[test]
    fn string_begin_only_counts_final_slash_as_line_continuation() {
        let mut s = Scanner::new("\"beginning string\\  \\  \\  ");
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
                end: 26,
                result: Ok(TokenKind::StringBegin(s, true)),
            } if s == "beginning string    "
        ));
    }

    #[test]
    fn string_fragment() {
        let mut s = Scanner::new("continued string");
        let c = Continuation {
            cont: TokenContinuation::StringLiteral(false),
            scan: &mut s,
            start: 0,
        };

        let r = c.extract();

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 16,
                result: Ok(TokenKind::StringFragment(s, false)),
            } if s == "continued string"
        ));
    }

    #[test]
    fn string_fragment_includes_whitespace() {
        let mut s = Scanner::new("   continued string");
        let c = Continuation {
            cont: TokenContinuation::StringLiteral(false),
            scan: &mut s,
            start: 0,
        };

        let r = c.extract();

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 19,
                result: Ok(TokenKind::StringFragment(s, false)),
            } if s == "   continued string"
        ));
    }

    #[test]
    fn string_fragment_with_line_continuation() {
        let mut s = Scanner::new("continued string  \\  \\  ");
        let c = Continuation {
            cont: TokenContinuation::StringLiteral(false),
            scan: &mut s,
            start: 0,
        };

        let r = c.extract();

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 24,
                result: Ok(TokenKind::StringFragment(s, true)),
            } if s == "continued string    "
        ));
    }

    #[test]
    fn string_fragment_from_string_continuation() {
        let mut s = Scanner::new("continued string");
        let c = Continuation {
            cont: TokenContinuation::StringLiteral(true),
            scan: &mut s,
            start: 0,
        };

        let r = c.extract();

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 16,
                result: Ok(TokenKind::StringFragment(s, false)),
            } if s == "continued string"
        ));
    }

    #[test]
    fn string_fragment_from_string_continuation_ignores_leading_whitespace() {
        let mut s = Scanner::new("   continued string   ");
        let c = Continuation {
            cont: TokenContinuation::StringLiteral(true),
            scan: &mut s,
            start: 0,
        };

        let r = c.extract();

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 22,
                result: Ok(TokenKind::StringFragment(s, false)),
            } if s == "continued string   "
        ));
    }

    #[test]
    fn string_fragment_from_string_continuation_all_whitespace() {
        let mut s = Scanner::new("      ");
        let c = Continuation {
            cont: TokenContinuation::StringLiteral(true),
            scan: &mut s,
            start: 0,
        };

        let r = c.extract();

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 6,
                result: Ok(TokenKind::StringFragment(s, false)),
            } if s == ""
        ));
    }

    #[test]
    fn string_fragment_from_string_continuation_to_string_continuation() {
        let mut s = Scanner::new("   continued string  \\  \\  ");
        let c = Continuation {
            cont: TokenContinuation::StringLiteral(true),
            scan: &mut s,
            start: 0,
        };

        let r = c.extract();

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 27,
                result: Ok(TokenKind::StringFragment(s, true)),
            } if s == "continued string    "
        ));
    }

    #[test]
    fn string_end() {
        let mut s = Scanner::new("end string\"");
        let c = Continuation {
            cont: TokenContinuation::StringLiteral(false),
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
    fn string_end_includes_whitespace() {
        let mut s = Scanner::new("   end string  \"");
        let c = Continuation {
            cont: TokenContinuation::StringLiteral(false),
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
    fn string_end_with_escaped_whitespace() {
        let mut s = Scanner::new("end string  \\  \\  \"");
        let c = Continuation {
            cont: TokenContinuation::StringLiteral(false),
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
    fn string_end_from_string_continuation() {
        let mut s = Scanner::new("end string\"");
        let c = Continuation {
            cont: TokenContinuation::StringLiteral(true),
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
    fn string_end_from_string_continuation_ignores_leading_whitespace() {
        let mut s = Scanner::new("   end string   \"");
        let c = Continuation {
            cont: TokenContinuation::StringLiteral(true),
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
    fn string_end_from_string_continuation_all_whitespace() {
        let mut s = Scanner::new("      \"");
        let c = Continuation {
            cont: TokenContinuation::StringLiteral(true),
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
}

mod identifier {
    use super::*;

    #[test]
    fn standard_identifier() {
        let mut s = Scanner::new("foo");
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
                end: 3,
                result: Ok(TokenKind::Identifier(s)),
            } if s == "foo"
        ));
    }

    #[test]
    fn identifier_with_digits() {
        let mut s = Scanner::new("a24");
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
                end: 3,
                result: Ok(TokenKind::Identifier(s)),
            } if s == "a24"
        ));
    }

    #[test]
    fn identifier_with_special_chars() {
        let mut s = Scanner::new("foo?bar@baz.beef");
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
                end: 16,
                result: Ok(TokenKind::Identifier(s)),
            } if s == "foo?bar@baz.beef"
        ));
    }

    #[test]
    fn identifier_stops_at_delimiter() {
        let mut s = Scanner::new("abc;");
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
                end: 3,
                result: Ok(TokenKind::Identifier(txt)),
            } if txt == "abc"
        ));
    }

    #[test]
    fn identifier_starts_with_special_char() {
        let mut s = Scanner::new("!foo");
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
                end: 4,
                result: Ok(TokenKind::Identifier(s)),
            } if s == "!foo"
        ));
    }

    #[test]
    fn identifier_ends_with_peculiar_chars() {
        let mut s = Scanner::new("c++");
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
                end: 3,
                result: Ok(TokenKind::Identifier(s)),
            } if s == "c++"
        ));
    }

    #[test]
    fn identifier_is_only_special_char() {
        let mut s = Scanner::new("!");
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
                result: Ok(TokenKind::Identifier(s)),
            } if s == "!"
        ));
    }

    #[test]
    fn identifier_with_extended_and_higher_chars() {
        let mut s = Scanner::new("λ🦀\u{2401}\u{fffd}");
        let start = s.next_token().unwrap();
        let t = Tokenizer {
            scan: &mut s,
            start,
        };

        let r = t.extract();

        /*assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 16,
                result: Ok(TokenKind::Identifier(s)),
            } if s == "λ🦀\u{2401}\u{fffd}"
        ));*/
        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 12,
                result: Err(TokenErrorKind::IdentifierInvalid('λ')),
            }
        ));
    }

    #[test]
    fn peculiar_identifier() {
        let mut s = Scanner::new("+foo");
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
                end: 4,
                result: Ok(TokenKind::Identifier(s)),
            } if s == "+foo"
        ));
    }

    #[test]
    fn sign_identifier() {
        let cases = ["+", "-"];
        for case in cases {
            let mut s = Scanner::new(case);
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
                    result: Ok(TokenKind::Identifier(s)),
                } if s == case
            ));
        }
    }

    #[test]
    fn double_sign_identifier() {
        let mut s = Scanner::new("+-");
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
                result: Ok(TokenKind::Identifier(s)),
            } if s == "+-"
        ));
    }

    #[test]
    fn sign_dot_identifier() {
        let mut s = Scanner::new("+.");
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
                result: Ok(TokenKind::Identifier(s)),
            } if s == "+."
        ));
    }

    #[test]
    fn double_period_identifier() {
        let mut s = Scanner::new("..");
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
                result: Ok(TokenKind::Identifier(s)),
            } if s == ".."
        ));
    }

    #[test]
    fn double_period_word_identifier() {
        let mut s = Scanner::new("..foo");
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
                end: 5,
                result: Ok(TokenKind::Identifier(s)),
            } if s == "..foo"
        ));
    }

    #[test]
    fn period_identifier() {
        let mut s = Scanner::new(".foo");
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
                end: 4,
                result: Ok(TokenKind::Identifier(s)),
            } if s == ".foo"
        ));
    }

    #[test]
    fn double_sign_number_is_identifer() {
        let mut s = Scanner::new("+-4");
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
                end: 3,
                result: Ok(TokenKind::Identifier(s)),
            } if s == "+-4"
        ));
    }

    #[test]
    fn sign_at_number_is_identifer() {
        let mut s = Scanner::new("+@4");
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
                end: 3,
                result: Ok(TokenKind::Identifier(s)),
            } if s == "+@4"
        ));
    }

    #[test]
    fn sign_double_period_number_is_identifer() {
        let mut s = Scanner::new("+..4");
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
                end: 4,
                result: Ok(TokenKind::Identifier(s)),
            } if s == "+..4"
        ));
    }

    #[test]
    fn sign_period_sign_number_is_identifer() {
        let mut s = Scanner::new("+.-4");
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
                end: 4,
                result: Ok(TokenKind::Identifier(s)),
            } if s == "+.-4"
        ));
    }

    #[test]
    fn double_period_number_is_identifer() {
        let mut s = Scanner::new("..4");
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
                end: 3,
                result: Ok(TokenKind::Identifier(s)),
            } if s == "..4"
        ));
    }

    #[test]
    fn period_sign_is_identifier() {
        let mut s = Scanner::new(".-4");
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
                end: 3,
                result: Ok(TokenKind::Identifier(s)),
            } if s == ".-4"
        ));
    }

    #[test]
    fn period_at_number_is_identifer() {
        let mut s = Scanner::new(".@4");
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
                end: 3,
                result: Ok(TokenKind::Identifier(s)),
            } if s == ".@4"
        ));
    }

    #[test]
    fn identifier_start_digit() {
        let mut s = Scanner::new("4foo");
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
                end: 4,
                result: Err(TokenErrorKind::IdentifierInvalid('4')),
            }
        ));
    }

    #[test]
    fn identifier_start_reserved_char() {
        let mut s = Scanner::new("{foo");
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
                end: 4,
                result: Err(TokenErrorKind::IdentifierInvalid('{')),
            }
        ));
    }

    #[test]
    fn identifier_contains_reserved_char() {
        let mut s = Scanner::new("foo{bar");
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
                end: 7,
                result: Err(TokenErrorKind::IdentifierInvalid('{')),
            }
        ));
    }

    #[test]
    fn sign_start_with_digit_is_number() {
        let mut s = Scanner::new("+4");
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
                result: Err(TokenErrorKind::Unimplemented(s)),
            } if s == "+4"
        ));
    }

    #[test]
    fn period_start_with_digit_is_number() {
        let mut s = Scanner::new(".4");
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
                result: Err(TokenErrorKind::Unimplemented(s)),
            } if s == ".4"
        ));
    }

    #[test]
    fn sign_period_start_with_digit_is_number() {
        let mut s = Scanner::new("-.4");
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
                end: 3,
                result: Err(TokenErrorKind::Unimplemented(s)),
            } if s == "-.4"
        ));
    }

    mod verbatim {
        use super::*;

        #[test]
        fn empty() {
            let mut s = Scanner::new("||");
            let start = s.next_token().unwrap();
            let t = Tokenizer {
                scan: &mut s,
                start,
            };

            let r = t.extract();
            dbg!(&r);

            assert!(matches!(
                r,
                TokenExtract {
                    start: 1,
                    end: 1,
                    result: Ok(TokenKind::Identifier(s)),
                } if s == ""
            ));
        }

        #[test]
        fn basic() {
            let mut s = Scanner::new("|foo|");
            let start = s.next_token().unwrap();
            let t = Tokenizer {
                scan: &mut s,
                start,
            };

            let r = t.extract();
            dbg!(&r);

            assert!(matches!(
                r,
                TokenExtract {
                    start: 1,
                    end: 4,
                    result: Ok(TokenKind::Identifier(s)),
                } if s == "foo"
            ));
        }

        #[test]
        fn includes_whitespace() {
            let mut s = Scanner::new("| foo bar |");
            let start = s.next_token().unwrap();
            let t = Tokenizer {
                scan: &mut s,
                start,
            };

            let r = t.extract();
            dbg!(&r);

            assert!(matches!(
                r,
                TokenExtract {
                    start: 1,
                    end: 10,
                    result: Ok(TokenKind::Identifier(s)),
                } if s == " foo bar "
            ));
        }

        #[test]
        fn escape_pipe() {
            let mut s = Scanner::new("| foo\\|bar |");
            let start = s.next_token().unwrap();
            let t = Tokenizer {
                scan: &mut s,
                start,
            };

            let r = t.extract();
            dbg!(&r);

            assert!(matches!(
                r,
                TokenExtract {
                    start: 1,
                    end: 11,
                    result: Ok(TokenKind::Identifier(s)),
                } if s == " foo|bar "
            ));
        }

        #[test]
        fn contains_string() {
            let mut s = Scanner::new("|foo \"string\" bar|");
            let start = s.next_token().unwrap();
            let t = Tokenizer {
                scan: &mut s,
                start,
            };

            let r = t.extract();
            dbg!(&r);

            assert!(matches!(
                r,
                TokenExtract {
                    start: 1,
                    end: 11,
                    result: Ok(TokenKind::Identifier(s)),
                } if s == "foo \"string\" bar"
            ));
        }
    }
}
