use super::*;
use crate::lex::tokens::{Token, TokenError};

mod tokenstream {
    use super::*;
    use std::ops::Range;

    #[test]
    fn empty_string() {
        let s = TokenStream::on("");

        let r: Vec<TokenResult> = s.collect();

        assert!(r.is_empty());
    }

    #[test]
    fn whitespace() {
        let s = TokenStream::on("   \t  \r  \n  ");

        let r: Vec<TokenResult> = s.collect();

        assert!(r.is_empty());
    }

    #[test]
    fn single_token() {
        let s = TokenStream::on("(");

        let r: Vec<TokenResult> = s.collect();

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
        let s = TokenStream::on("  (   ");

        let r: Vec<TokenResult> = s.collect();

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
        let s = TokenStream::on("(#t)");

        let r: Vec<TokenResult> = s.collect();

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
        let s = TokenStream::on("   (   #t    )   ");

        let r: Vec<TokenResult> = s.collect();

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
    fn tokens_with_hash_malformed() {
        let s = TokenStream::on("(#tdf)");

        let r: Vec<TokenResult> = s.collect();

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
    fn tokens_with_unterminated_hash_to_delimiter() {
        let s = TokenStream::on("(#)");

        let r: Vec<TokenResult> = s.collect();

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
    fn tokens_with_unterminated_hash_to_whitespace() {
        let s = TokenStream::on("# #f");

        let r: Vec<TokenResult> = s.collect();

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
}

mod tokenizer {
    use super::*;

    #[test]
    fn empty_string() {
        let mut s = Scanner::new("");
        let t = Tokenizer::start((0, 'a'), &mut s);

        let r = t.extract();

        assert!(matches!(
            r,
            TokenExtract {
                start: 0,
                end: 0,
                result: Err(TokenErrorKind::Unimplemented(txt)),
            } if txt == ""
        ));
    }

    #[test]
    fn token_not_implemented() {
        let mut s = Scanner::new("abc");
        let t = Tokenizer::start(s.next_token().unwrap(), &mut s);

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
        let t = Tokenizer::start(s.next_token().unwrap(), &mut s);

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
        let t = Tokenizer::start(s.next_token().unwrap(), &mut s);

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
        let t = Tokenizer::start(s.next_token().unwrap(), &mut s);

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
    fn token_ends_at_whitespace() {
        let mut s = Scanner::new("(  ");
        let t = Tokenizer::start(s.next_token().unwrap(), &mut s);

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
        let t = Tokenizer::start(s.next_token().unwrap(), &mut s);

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

    mod hashtag {
        use super::*;

        #[test]
        fn unterminated() {
            let mut s = Scanner::new("#");
            let t = Tokenizer::start(s.next_token().unwrap(), &mut s);

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
            let t = Tokenizer::start(s.next_token().unwrap(), &mut s);

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
            let t = Tokenizer::start(s.next_token().unwrap(), &mut s);

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
            let t = Tokenizer::start(s.next_token().unwrap(), &mut s);

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
            let t = Tokenizer::start(s.next_token().unwrap(), &mut s);

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
        fn vector_open() {
            let mut s = Scanner::new("#(");
            let t = Tokenizer::start(s.next_token().unwrap(), &mut s);

            let r = t.extract();

            assert!(matches!(
                r,
                TokenExtract {
                    start: 0,
                    end: 2,
                    result: Ok(TokenKind::VectorOpen),
                }
            ));
        }

        #[test]
        fn true_short() {
            let mut s = Scanner::new("#t");
            let t = Tokenizer::start(s.next_token().unwrap(), &mut s);

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
            let t = Tokenizer::start(s.next_token().unwrap(), &mut s);

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
            let t = Tokenizer::start(s.next_token().unwrap(), &mut s);

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
            let t = Tokenizer::start(s.next_token().unwrap(), &mut s);

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
            let t = Tokenizer::start(s.next_token().unwrap(), &mut s);

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
            let t = Tokenizer::start(s.next_token().unwrap(), &mut s);

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
            let t = Tokenizer::start(s.next_token().unwrap(), &mut s);

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
        fn extended_literal() {
            let mut s = Scanner::new("#\\λ");
            let t = Tokenizer::start(s.next_token().unwrap(), &mut s);

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
            let t = Tokenizer::start(s.next_token().unwrap(), &mut s);

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
            let t = Tokenizer::start(s.next_token().unwrap(), &mut s);

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
            let t = Tokenizer::start(s.next_token().unwrap(), &mut s);

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
                ("alarm", '\u{7}'),
                ("backspace", '\u{8}'),
                ("delete", '\u{7f}'),
                ("escape", '\u{1b}'),
                ("newline", '\n'),
                ("null", '\0'),
                ("return", '\r'),
                ("space", ' '),
                ("tab", '\t'),
            ]);
        }

        #[test]
        fn space_followed_by_alpha() {
            let mut s = Scanner::new("#\\ b");
            let t = Tokenizer::start(s.next_token().unwrap(), &mut s);

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
            let t = Tokenizer::start(s.next_token().unwrap(), &mut s);

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
            let t = Tokenizer::start(s.next_token().unwrap(), &mut s);

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
            let t = Tokenizer::start(s.next_token().unwrap(), &mut s);

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
            let t = Tokenizer::start(s.next_token().unwrap(), &mut s);

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
            let t = Tokenizer::start(s.next_token().unwrap(), &mut s);

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
            let t = Tokenizer::start(s.next_token().unwrap(), &mut s);

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
            let t = Tokenizer::start(s.next_token().unwrap(), &mut s);

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
            let t = Tokenizer::start(s.next_token().unwrap(), &mut s);

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
            let t = Tokenizer::start(s.next_token().unwrap(), &mut s);

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
            let t = Tokenizer::start(s.next_token().unwrap(), &mut s);

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

        fn check_character_list(expected: &[(&str, char)]) {
            for &(inp, ex) in expected {
                let input = format!("#\\{inp}");
                let mut s = Scanner::new(&input);
                let t = Tokenizer::start(s.next_token().unwrap(), &mut s);

                let r = t.extract();

                assert!(
                    matches!(
                        r,
                        TokenExtract {
                            start: 0,
                            end,
                            result: Ok(TokenKind::Literal(Literal::Character(ch))),
                        } if ch == ex && end == input.len()
                    ),
                    "Unexpected match for character input ({inp}, {ex})"
                );
            }
        }
    }
}
