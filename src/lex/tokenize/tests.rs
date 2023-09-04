use super::*;
use crate::lex::tokens::{Token, TokenError};

mod tokenstream {
    use super::*;
    use std::ops::Range;

    #[test]
    fn empty_string() {
        let s = TokenStream::new("");

        let r: Vec<TokenResult> = s.collect();

        assert!(r.is_empty());
    }

    #[test]
    fn whitespace() {
        let s = TokenStream::new("   \t  \r  \n  ");

        let r: Vec<TokenResult> = s.collect();

        assert!(r.is_empty());
    }

    #[test]
    fn single_token() {
        let s = TokenStream::new("(");

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
        let s = TokenStream::new("  (   ");

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
        let s = TokenStream::new("(#t)");

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
        let s = TokenStream::new("   (   #t    )   ");

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
    fn tokens_with_invalid_token() {
        let s = TokenStream::new("(#tdf)");

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
    fn tokens_with_unterminated_token() {
        let s = TokenStream::new("(#)");

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
    fn tokens_with_unterminated_token_to_whitespace() {
        let s = TokenStream::new("# #f");

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
            } if txt.is_empty()
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
    fn pair_joiner() {
        let mut s = Scanner::new(".");
        let t = Tokenizer::start(s.next_token().unwrap(), &mut s);

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
        let t = Tokenizer::start(s.next_token().unwrap(), &mut s);

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
        let t = Tokenizer::start(s.next_token().unwrap(), &mut s);

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
        let t = Tokenizer::start(s.next_token().unwrap(), &mut s);

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
        let t = Tokenizer::start(s.next_token().unwrap(), &mut s);

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

    #[test]
    fn comment() {
        let mut s = Scanner::new(";");
        let t = Tokenizer::start(s.next_token().unwrap(), &mut s);

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
        let t = Tokenizer::start(s.next_token().unwrap(), &mut s);

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
        fn datum_comment() {
            let mut s = Scanner::new("#;");
            let t = Tokenizer::start(s.next_token().unwrap(), &mut s);

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
            let t = Tokenizer::start(s.next_token().unwrap(), &mut s);

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
            let t = Tokenizer::start(s.next_token().unwrap(), &mut s);

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
        fn vector() {
            let mut s = Scanner::new("#(");
            let t = Tokenizer::start(s.next_token().unwrap(), &mut s);

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
    }

    mod bytevector {
        use super::*;

        #[test]
        fn bytevector() {
            let mut s = Scanner::new("#u8(");
            let t = Tokenizer::start(s.next_token().unwrap(), &mut s);

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
            let t = Tokenizer::start(s.next_token().unwrap(), &mut s);

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
            let t = Tokenizer::start(s.next_token().unwrap(), &mut s);

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
            let t = Tokenizer::start(s.next_token().unwrap(), &mut s);

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
            let t = Tokenizer::start(s.next_token().unwrap(), &mut s);

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
            let t = Tokenizer::start(s.next_token().unwrap(), &mut s);

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
            let t = Tokenizer::start(s.next_token().unwrap(), &mut s);

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
            let t = Tokenizer::start(s.next_token().unwrap(), &mut s);

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
        fn true_uppercase() {
            let mut s = Scanner::new("#TRUE");
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
        fn false_uppercase() {
            let mut s = Scanner::new("#FALSE");
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
        fn ascii_uppercase_literal() {
            let mut s = Scanner::new("#\\A");
            let t = Tokenizer::start(s.next_token().unwrap(), &mut s);

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
        fn hex_uppercase_indicator() {
            let mut s = Scanner::new("#\\Xa");
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

        fn check_character_list(cases: &[(&str, char)]) {
            for &(inp, exp) in cases {
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
            let t = Tokenizer::start(s.next_token().unwrap(), &mut s);

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
            let t = Tokenizer::start(s.next_token().unwrap(), &mut s);

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
            let t = Tokenizer::start(s.next_token().unwrap(), &mut s);

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
            let t = Tokenizer::start(s.next_token().unwrap(), &mut s);

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
            let t = Tokenizer::start(s.next_token().unwrap(), &mut s);

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
            let t = Tokenizer::start(s.next_token().unwrap(), &mut s);

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
