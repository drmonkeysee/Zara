mod builder;
mod scan;

use self::{
    builder::{TokenExtract, TokenExtractResult},
    scan::{ScanItem, Scanner},
};
use crate::{
    lex::tokens::{TokenErrorKind, TokenKind, TokenResult},
    literal::Literal,
};

pub(super) struct TokenStream<'a> {
    scan: Scanner<'a>,
}

impl<'a> TokenStream<'a> {
    pub(super) fn on(textline: &'a str) -> Self {
        Self {
            scan: Scanner::new(textline),
        }
    }
}

impl<'a> Iterator for TokenStream<'a> {
    type Item = TokenResult;

    fn next(&mut self) -> Option<Self::Item> {
        self.scan
            .skip_whitespace()
            .map(|item| Tokenizer::start(item, &mut self.scan).extract().build())
    }
}

struct Tokenizer<'me, 'str> {
    scan: &'me mut Scanner<'str>,
    start: ScanItem<'str>,
}

impl<'me, 'str> Tokenizer<'me, 'str> {
    fn start(start: ScanItem, scan: &'me mut Scanner<'str>) -> Self {
        Self { start, scan }
    }

    fn extract(mut self) -> TokenExtract {
        let (result, end) = self.scan();
        TokenExtract {
            start: self.start.0,
            end,
            result,
        }
    }

    fn scan(&mut self) -> (TokenExtractResult, usize) {
        let result = match self.start.1 {
            '#' => self.hashtag(),
            '(' => Ok(TokenKind::ParenLeft),
            ')' => Ok(TokenKind::ParenRight),
            _ => self.not_implemented(),
        };
        (result, self.scan.pos())
    }

    fn hashtag(&mut self) -> TokenExtractResult {
        if let Some((_, ch)) = self.scan.hashcode_non_delimiter() {
            match ch {
                'f' => self.boolean(false),
                't' => self.boolean(true),
                '\\' => self.character(),
                '(' => Ok(TokenKind::VectorOpen),
                _ => {
                    self.scan.end_of_token();
                    Err(TokenErrorKind::HashInvalid)
                }
            }
        } else {
            Err(TokenErrorKind::HashUnterminated)
        }
    }

    fn boolean(&mut self, val: bool) -> TokenExtractResult {
        let cur = self.scan.pos();
        let end = self.scan.end_of_token();
        let rest = self.scan.lexeme(cur..end);
        if rest.is_empty() || rest == if val { "rue" } else { "alse" } {
            Ok(TokenKind::Literal(Literal::Boolean(val)))
        } else {
            Err(TokenErrorKind::ExpectedBoolean(val))
        }
    }

    fn character(&mut self) -> TokenExtractResult {
        if let Some((_, ch)) = self.scan.char() {
            let cur = self.scan.pos();
            if ch.is_ascii_whitespace() {
                Ok(TokenKind::Literal(Literal::Character(ch)))
            } else {
                let end = self.scan.end_of_token();
                let rest = self.scan.lexeme(cur..end);
                match ch {
                    'x' => todo!(), //maybe_hex(),
                    _ if rest.is_empty() => Ok(TokenKind::Literal(Literal::Character(ch))),
                    _ => {
                        if let Some(literal) = match (ch, rest) {
                            ('a', "larm") => Some('\u{7}'),
                            ('b', "ackspace") => Some('\u{8}'),
                            ('d', "elete") => Some('\u{7f}'),
                            ('e', "scape") => Some('\u{1b}'),
                            ('n', "ewline") => Some('\n'),
                            ('n', "ull") => Some('\0'),
                            ('r', "eturn") => Some('\r'),
                            ('s', "pace") => Some(' '),
                            ('t', "ab") => Some('\t'),
                            _ => None,
                        } {
                            Ok(TokenKind::Literal(Literal::Character(literal)))
                        } else {
                            Err(TokenErrorKind::ExpectedCharacter)
                        }
                    }
                }
            }
        } else {
            Ok(TokenKind::Literal(Literal::Character('\n')))
        }
    }

    fn not_implemented(&mut self) -> TokenExtractResult {
        let start = self.start.0;
        let end = self.scan.end_of_token();
        Err(TokenErrorKind::Unimplemented(String::from(
            self.scan.lexeme(start..end),
        )))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lex::tokens::{Token, TokenError};
    use std::ops::Range;

    mod tokenstream {
        use super::*;

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
                    kind: TokenErrorKind::ExpectedBoolean(true),
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

            let r = t.extract().build();

            assert!(matches!(
                r,
                Err(TokenError {
                    kind: TokenErrorKind::Unimplemented(txt),
                    span: Range { start: 0, end: 0 }
                }) if txt == ""
            ));
        }

        #[test]
        fn token_not_implemented() {
            let mut s = Scanner::new("abc");
            let t = Tokenizer::start(s.char().unwrap(), &mut s);

            let r = t.extract().build();

            assert!(matches!(
                r,
                Err(TokenError {
                    kind: TokenErrorKind::Unimplemented(txt),
                    span: Range { start: 0, end: 3 }
                }) if txt == "abc"
            ));
        }

        #[test]
        fn token_not_implemented_stops_at_delimiter() {
            let mut s = Scanner::new("abc;");
            let t = Tokenizer::start(s.char().unwrap(), &mut s);

            let r = t.extract().build();

            assert!(matches!(
                r,
                Err(TokenError {
                    kind: TokenErrorKind::Unimplemented(txt),
                    span: Range { start: 0, end: 3 }
                }) if txt == "abc"
            ));
        }

        #[test]
        fn left_paren() {
            let mut s = Scanner::new("(");
            let t = Tokenizer::start(s.char().unwrap(), &mut s);

            let r = t.extract().build();

            assert!(matches!(
                r,
                Ok(Token {
                    kind: TokenKind::ParenLeft,
                    span: Range { start: 0, end: 1 }
                })
            ));
        }

        #[test]
        fn right_paren() {
            let mut s = Scanner::new(")");
            let t = Tokenizer::start(s.char().unwrap(), &mut s);

            let r = t.extract().build();

            assert!(matches!(
                r,
                Ok(Token {
                    kind: TokenKind::ParenRight,
                    span: Range { start: 0, end: 1 }
                })
            ));
        }

        #[test]
        fn token_ends_at_whitespace() {
            let mut s = Scanner::new("(  ");
            let t = Tokenizer::start(s.char().unwrap(), &mut s);

            let r = t.extract().build();

            assert!(matches!(
                r,
                Ok(Token {
                    kind: TokenKind::ParenLeft,
                    span: Range { start: 0, end: 1 }
                })
            ));
        }

        #[test]
        fn token_ends_at_delimiter() {
            let mut s = Scanner::new("()");
            let t = Tokenizer::start(s.char().unwrap(), &mut s);

            let r = t.extract().build();

            assert!(matches!(
                r,
                Ok(Token {
                    kind: TokenKind::ParenLeft,
                    span: Range { start: 0, end: 1 }
                })
            ));
        }

        mod hashtag {
            use super::*;

            #[test]
            fn unterminated() {
                let mut s = Scanner::new("#");
                let t = Tokenizer::start(s.char().unwrap(), &mut s);

                let r = t.extract().build();

                assert!(matches!(
                    r,
                    Err(TokenError {
                        kind: TokenErrorKind::HashUnterminated,
                        span: Range { start: 0, end: 1 }
                    })
                ));
            }

            #[test]
            fn unterminated_with_whitespace() {
                let mut s = Scanner::new("#  ");
                let t = Tokenizer::start(s.char().unwrap(), &mut s);

                let r = t.extract().build();

                assert!(matches!(
                    r,
                    Err(TokenError {
                        kind: TokenErrorKind::HashUnterminated,
                        span: Range { start: 0, end: 1 }
                    })
                ));
            }

            #[test]
            fn unterminated_with_delimiter() {
                let mut s = Scanner::new("#)");
                let t = Tokenizer::start(s.char().unwrap(), &mut s);

                let r = t.extract().build();

                assert!(matches!(
                    r,
                    Err(TokenError {
                        kind: TokenErrorKind::HashUnterminated,
                        span: Range { start: 0, end: 1 }
                    })
                ));
            }

            #[test]
            fn invalid() {
                let mut s = Scanner::new("#g");
                let t = Tokenizer::start(s.char().unwrap(), &mut s);

                let r = t.extract().build();

                assert!(matches!(
                    r,
                    Err(TokenError {
                        kind: TokenErrorKind::HashInvalid,
                        span: Range { start: 0, end: 2 }
                    })
                ));
            }

            #[test]
            fn invalid_long() {
                let mut s = Scanner::new("#not_a_valid_hashtag");
                let t = Tokenizer::start(s.char().unwrap(), &mut s);

                let r = t.extract().build();

                dbg!(&r);
                assert!(matches!(
                    r,
                    Err(TokenError {
                        kind: TokenErrorKind::HashInvalid,
                        span: Range { start: 0, end: 20 }
                    })
                ));
            }

            #[test]
            fn vector_open() {
                let mut s = Scanner::new("#(");
                let t = Tokenizer::start(s.char().unwrap(), &mut s);

                let r = t.extract().build();

                assert!(matches!(
                    r,
                    Ok(Token {
                        kind: TokenKind::VectorOpen,
                        span: Range { start: 0, end: 2 }
                    })
                ));
            }

            #[test]
            fn true_short() {
                let mut s = Scanner::new("#t");
                let t = Tokenizer::start(s.char().unwrap(), &mut s);

                let r = t.extract().build();

                assert!(matches!(
                    r,
                    Ok(Token {
                        kind: TokenKind::Literal(Literal::Boolean(true)),
                        span: Range { start: 0, end: 2 }
                    })
                ));
            }

            #[test]
            fn true_long() {
                let mut s = Scanner::new("#true");
                let t = Tokenizer::start(s.char().unwrap(), &mut s);

                let r = t.extract().build();

                assert!(matches!(
                    r,
                    Ok(Token {
                        kind: TokenKind::Literal(Literal::Boolean(true)),
                        span: Range { start: 0, end: 5 }
                    })
                ));
            }

            #[test]
            fn true_malformed() {
                let mut s = Scanner::new("#trueasd");
                let t = Tokenizer::start(s.char().unwrap(), &mut s);

                let r = t.extract().build();

                assert!(matches!(
                    r,
                    Err(TokenError {
                        kind: TokenErrorKind::ExpectedBoolean(true),
                        span: Range { start: 0, end: 8 }
                    })
                ));
            }

            #[test]
            fn false_short() {
                let mut s = Scanner::new("#f");
                let t = Tokenizer::start(s.char().unwrap(), &mut s);

                let r = t.extract().build();

                assert!(matches!(
                    r,
                    Ok(Token {
                        kind: TokenKind::Literal(Literal::Boolean(false)),
                        span: Range { start: 0, end: 2 }
                    })
                ));
            }

            #[test]
            fn false_long() {
                let mut s = Scanner::new("#false");
                let t = Tokenizer::start(s.char().unwrap(), &mut s);

                let r = t.extract().build();

                assert!(matches!(
                    r,
                    Ok(Token {
                        kind: TokenKind::Literal(Literal::Boolean(false)),
                        span: Range { start: 0, end: 6 }
                    })
                ));
            }

            #[test]
            fn false_malformed() {
                let mut s = Scanner::new("#fals");
                let t = Tokenizer::start(s.char().unwrap(), &mut s);

                let r = t.extract().build();

                assert!(matches!(
                    r,
                    Err(TokenError {
                        kind: TokenErrorKind::ExpectedBoolean(false),
                        span: Range { start: 0, end: 5 }
                    })
                ));
            }
        }

        mod character {
            use super::*;

            #[test]
            fn ascii_literal() {
                let mut s = Scanner::new("#\\a");
                let t = Tokenizer::start(s.char().unwrap(), &mut s);

                let r = t.extract().build();

                assert!(matches!(
                    r,
                    Ok(Token {
                        kind: TokenKind::Literal(Literal::Character('a')),
                        span: Range { start: 0, end: 3 }
                    })
                ));
            }

            #[test]
            fn extended_literal() {
                let mut s = Scanner::new("#\\λ");
                let t = Tokenizer::start(s.char().unwrap(), &mut s);

                let r = t.extract().build();

                assert!(matches!(
                    r,
                    Ok(Token {
                        kind: TokenKind::Literal(Literal::Character('λ')),
                        span: Range { start: 0, end: 4 }
                    })
                ));
            }

            #[test]
            fn emoji_literal() {
                let mut s = Scanner::new("#\\🦀");
                let t = Tokenizer::start(s.char().unwrap(), &mut s);

                let r = t.extract().build();

                assert!(matches!(
                    r,
                    Ok(Token {
                        kind: TokenKind::Literal(Literal::Character('🦀')),
                        span: Range { start: 0, end: 6 }
                    })
                ));
            }

            #[test]
            fn string_escape_literals() {
                check_character_list(&[("\"", '"'), ("'", '\''), ("\\", '\\'), ("", '\n')]);
            }

            #[test]
            fn space_literal() {
                let mut s = Scanner::new("#\\ ");
                let t = Tokenizer::start(s.char().unwrap(), &mut s);

                let r = t.extract().build();

                assert!(matches!(
                    r,
                    Ok(Token {
                        kind: TokenKind::Literal(Literal::Character(' ')),
                        span: Range { start: 0, end: 3 }
                    })
                ));
            }

            #[test]
            fn tab_literal() {
                let mut s = Scanner::new("#\\\t");
                let t = Tokenizer::start(s.char().unwrap(), &mut s);

                let r = t.extract().build();

                assert!(matches!(
                    r,
                    Ok(Token {
                        kind: TokenKind::Literal(Literal::Character('\t')),
                        span: Range { start: 0, end: 3 }
                    })
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
                let t = Tokenizer::start(s.char().unwrap(), &mut s);

                let r = t.extract().build();

                assert!(matches!(
                    r,
                    Ok(Token {
                        kind: TokenKind::Literal(Literal::Character(' ')),
                        span: Range { start: 0, end: 3 }
                    })
                ));
            }

            #[test]
            fn alpha_followed_by_alpha() {
                let mut s = Scanner::new("#\\ab");
                let t = Tokenizer::start(s.char().unwrap(), &mut s);

                let r = t.extract().build();

                assert!(matches!(
                    r,
                    Err(TokenError {
                        kind: TokenErrorKind::ExpectedCharacter,
                        span: Range { start: 0, end: 4 }
                    })
                ));
            }

            #[test]
            fn emoji_followed_by_alpha() {
                let mut s = Scanner::new("#\\🦀b");
                let t = Tokenizer::start(s.char().unwrap(), &mut s);

                let r = t.extract().build();

                assert!(matches!(
                    r,
                    Err(TokenError {
                        kind: TokenErrorKind::ExpectedCharacter,
                        span: Range { start: 0, end: 7 }
                    })
                ));
            }

            #[test]
            fn alpha_followed_by_delimiter() {
                let mut s = Scanner::new("#\\a(");
                let t = Tokenizer::start(s.char().unwrap(), &mut s);

                let r = t.extract().build();

                assert!(matches!(
                    r,
                    Ok(Token {
                        kind: TokenKind::Literal(Literal::Character('a')),
                        span: Range { start: 0, end: 3 }
                    })
                ));
            }

            fn check_character_list(expected: &[(&str, char)]) {
                for &(inp, ex) in expected {
                    let input = format!("#\\{inp}");
                    let mut s = Scanner::new(&input);
                    let t = Tokenizer::start(s.char().unwrap(), &mut s);

                    let r = t.extract().build();

                    assert!(
                        matches!(
                            r,
                            Ok(Token {
                                kind: TokenKind::Literal(Literal::Character(ch)),
                                span: Range { start: 0, end }
                            }) if ch == ex && end == input.len()
                        ),
                        "Unexpected match for character input ({inp}, {ex})"
                    );
                }
            }
        }
    }
}
