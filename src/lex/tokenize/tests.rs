use super::*;
use crate::lex::tokens::{Token, TokenError};

mod tokenstream {
    use super::*;
    use std::ops::Range;

    #[test]
    fn empty_string() {
        let s = TokenStream::new("", None);

        let r: Vec<_> = s.collect();

        assert!(r.is_empty());
    }

    #[test]
    fn whitespace() {
        let s = TokenStream::new("   \t  \r  \n  ", None);

        let r: Vec<_> = s.collect();

        assert!(r.is_empty());
    }

    #[test]
    fn single_token() {
        let s = TokenStream::new("(", None);

        let r: Vec<_> = s.collect();

        assert_eq!(r.len(), 1);
        assert!(matches!(
            r[0],
            Ok(Token {
                kind: TokenKind::ParenLeft,
                span: Range { start: 0, end: 1 }
            })
        ));
    }

    #[test]
    fn single_token_with_whitespace() {
        let s = TokenStream::new("  (   ", None);

        let r: Vec<_> = s.collect();

        assert_eq!(r.len(), 1);
        assert!(matches!(
            r[0],
            Ok(Token {
                kind: TokenKind::ParenLeft,
                span: Range { start: 2, end: 3 }
            })
        ));
    }

    #[test]
    fn multiple_tokens() {
        let s = TokenStream::new("(#t)", None);

        let r: Vec<_> = s.collect();

        assert_eq!(r.len(), 3);
        assert!(matches!(
            r[0],
            Ok(Token {
                kind: TokenKind::ParenLeft,
                span: Range { start: 0, end: 1 }
            })
        ));
        assert!(matches!(
            r[1],
            Ok(Token {
                kind: TokenKind::Literal(Literal::Boolean(true)),
                span: Range { start: 1, end: 3 }
            })
        ));
        assert!(matches!(
            r[2],
            Ok(Token {
                kind: TokenKind::ParenRight,
                span: Range { start: 3, end: 4 }
            })
        ));
    }

    #[test]
    fn multiple_tokens_with_whitespace() {
        let s = TokenStream::new("   (   #t    )   ", None);

        let r: Vec<_> = s.collect();

        assert_eq!(r.len(), 3);
        assert!(matches!(
            r[0],
            Ok(Token {
                kind: TokenKind::ParenLeft,
                span: Range { start: 3, end: 4 }
            })
        ));
        assert!(matches!(
            r[1],
            Ok(Token {
                kind: TokenKind::Literal(Literal::Boolean(true)),
                span: Range { start: 7, end: 9 }
            })
        ));
        assert!(matches!(
            r[2],
            Ok(Token {
                kind: TokenKind::ParenRight,
                span: Range { start: 13, end: 14 }
            })
        ));
    }

    #[test]
    fn tokens_with_invalid_token() {
        let s = TokenStream::new("(#tdf)", None);

        let r: Vec<_> = s.collect();

        assert_eq!(r.len(), 3);
        assert!(matches!(
            r[0],
            Ok(Token {
                kind: TokenKind::ParenLeft,
                span: Range { start: 0, end: 1 }
            })
        ));
        assert!(matches!(
            r[1],
            Err(TokenError {
                kind: TokenErrorKind::BooleanExpected(true),
                span: Range { start: 1, end: 5 }
            })
        ));
        assert!(matches!(
            r[2],
            Ok(Token {
                kind: TokenKind::ParenRight,
                span: Range { start: 5, end: 6 }
            })
        ));
    }

    #[test]
    fn tokens_with_unterminated_token() {
        let s = TokenStream::new("(#)", None);

        let r: Vec<_> = s.collect();

        assert_eq!(r.len(), 3);
        assert!(matches!(
            r[0],
            Ok(Token {
                kind: TokenKind::ParenLeft,
                span: Range { start: 0, end: 1 }
            })
        ));
        assert!(matches!(
            r[1],
            Err(TokenError {
                kind: TokenErrorKind::HashUnterminated,
                span: Range { start: 1, end: 2 }
            })
        ));
        assert!(matches!(
            r[2],
            Ok(Token {
                kind: TokenKind::ParenRight,
                span: Range { start: 2, end: 3 }
            })
        ));
    }

    #[test]
    fn tokens_with_unterminated_token_to_whitespace() {
        let s = TokenStream::new("# #f", None);

        let r: Vec<_> = s.collect();

        assert_eq!(r.len(), 2);
        assert!(matches!(
            r[0],
            Err(TokenError {
                kind: TokenErrorKind::HashUnterminated,
                span: Range { start: 0, end: 1 }
            })
        ));
        assert!(matches!(
            r[1],
            Ok(Token {
                kind: TokenKind::Literal(Literal::Boolean(false)),
                span: Range { start: 2, end: 4 }
            })
        ));
    }

    #[test]
    fn hash_is_a_token_boundary() {
        let s = TokenStream::new("#t#f", None);

        let r: Vec<_> = s.collect();

        assert_eq!(r.len(), 2);
        assert!(matches!(
            r[0],
            Ok(Token {
                kind: TokenKind::Literal(Literal::Boolean(true)),
                span: Range { start: 0, end: 2 }
            })
        ));
        assert!(matches!(
            r[1],
            Ok(Token {
                kind: TokenKind::Literal(Literal::Boolean(false)),
                span: Range { start: 2, end: 4 }
            })
        ));
    }

    #[test]
    fn quote_is_a_token_boundary() {
        let s = TokenStream::new("#t'#f", None);

        let r: Vec<_> = s.collect();

        assert_eq!(r.len(), 3);
        assert!(matches!(
            r[0],
            Ok(Token {
                kind: TokenKind::Literal(Literal::Boolean(true)),
                span: Range { start: 0, end: 2 }
            })
        ));
        assert!(matches!(
            r[1],
            Ok(Token {
                kind: TokenKind::Quote,
                span: Range { start: 2, end: 3 }
            })
        ));
        assert!(matches!(
            r[2],
            Ok(Token {
                kind: TokenKind::Literal(Literal::Boolean(false)),
                span: Range { start: 3, end: 5 }
            })
        ));
    }

    #[test]
    fn quasiquote_is_a_token_boundary() {
        let s = TokenStream::new("#t`#f", None);

        let r: Vec<_> = s.collect();

        assert_eq!(r.len(), 3);
        assert!(matches!(
            r[0],
            Ok(Token {
                kind: TokenKind::Literal(Literal::Boolean(true)),
                span: Range { start: 0, end: 2 }
            })
        ));
        assert!(matches!(
            r[1],
            Ok(Token {
                kind: TokenKind::Quasiquote,
                span: Range { start: 2, end: 3 }
            })
        ));
        assert!(matches!(
            r[2],
            Ok(Token {
                kind: TokenKind::Literal(Literal::Boolean(false)),
                span: Range { start: 3, end: 5 }
            })
        ));
    }

    #[test]
    fn unquote_is_a_token_boundary() {
        let s = TokenStream::new("#t,#f", None);

        let r: Vec<_> = s.collect();

        assert_eq!(r.len(), 3);
        assert!(matches!(
            r[0],
            Ok(Token {
                kind: TokenKind::Literal(Literal::Boolean(true)),
                span: Range { start: 0, end: 2 }
            })
        ));
        assert!(matches!(
            r[1],
            Ok(Token {
                kind: TokenKind::Unquote,
                span: Range { start: 2, end: 3 }
            })
        ));
        assert!(matches!(
            r[2],
            Ok(Token {
                kind: TokenKind::Literal(Literal::Boolean(false)),
                span: Range { start: 3, end: 5 }
            })
        ));
    }

    #[test]
    fn pair_join_is_not_a_token_boundary() {
        let s = TokenStream::new("#t.#f", None);

        let r: Vec<_> = s.collect();

        assert_eq!(r.len(), 2);
        assert!(matches!(
            r[0],
            Err(TokenError {
                kind: TokenErrorKind::BooleanExpected(true),
                span: Range { start: 0, end: 3 }
            })
        ));
        assert!(matches!(
            r[1],
            Ok(Token {
                kind: TokenKind::Literal(Literal::Boolean(false)),
                span: Range { start: 3, end: 5 }
            })
        ));
    }

    #[test]
    fn block_comment_fragment_uses_whole_line() {
        let s = TokenStream::new(
            "continued comment",
            Some(TokenContinuation::BlockComment(2)),
        );

        let r: Vec<_> = s.collect();

        assert_eq!(r.len(), 1);
        assert!(matches!(
            r[0],
            Ok(Token {
                kind: TokenKind::CommentBlockFragment(2),
                span: Range { start: 0, end: 17 }
            })
        ));
    }

    #[test]
    fn block_comment_end_continues_tokenizing() {
        let s = TokenStream::new(
            "end comment |# #f",
            Some(TokenContinuation::BlockComment(0)),
        );

        let r: Vec<_> = s.collect();

        assert_eq!(r.len(), 2);
        assert!(matches!(
            r[0],
            Ok(Token {
                kind: TokenKind::CommentBlockEnd,
                span: Range { start: 0, end: 14 }
            })
        ));
        assert!(matches!(
            r[1],
            Ok(Token {
                kind: TokenKind::Literal(Literal::Boolean(false)),
                span: Range { start: 15, end: 17 }
            })
        ));
    }
}

mod tokenizer {
    use super::*;

    #[test]
    fn empty_string() {
        let mut s = Scanner::new("");
        let t = Tokenizer {
            start: (0, 'a'),
            scan: &mut s,
        };

        let r = t.extract();

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 0,
                result: Err(TokenErrorKind::Unimplemented(txt)),
            } if txt.is_empty()
        ));
    }

    #[test]
    fn token_not_implemented() {
        let mut s = Scanner::new("abc");
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
                result: Err(TokenErrorKind::Unimplemented(txt)),
            } if txt == "abc"
        ));
    }

    #[test]
    fn token_not_implemented_stops_at_delimiter() {
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
                result: Err(TokenErrorKind::Unimplemented(txt)),
            } if txt == "abc"
        ));
    }

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
                result: Err(TokenErrorKind::Unimplemented(txt)),
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
                result: Err(TokenErrorKind::Unimplemented(txt)),
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
                    end: 11,
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
                };

                let r = c.extract();
                dbg!(&r);

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
}
