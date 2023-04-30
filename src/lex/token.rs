mod builder;
mod scan;

use self::{
    builder::TokenBuilder,
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
        self.scan.skip_whitespace().map(|item| {
            let mut tokenizer = Tokenizer::start(item, &mut self.scan);
            tokenizer.run();
            tokenizer.extract()
        })
    }
}

struct Tokenizer<'me, 'str> {
    builder: TokenBuilder,
    scan: &'me mut Scanner<'str>,
    start: ScanItem<'str>,
}

impl<'me, 'str> Tokenizer<'me, 'str> {
    fn start(start: ScanItem, scan: &'me mut Scanner<'str>) -> Self {
        Self {
            start,
            scan,
            builder: TokenBuilder::start(start.0),
        }
    }

    fn run(&mut self) {
        match self.start.1 {
            '#' => self.hashtag(),
            '(' => {
                self.builder.token(TokenKind::ParenLeft);
            }
            ')' => {
                self.builder.token(TokenKind::ParenRight);
            }
            _ => self.not_implemented(),
        }
    }

    fn extract(self) -> TokenResult {
        self.builder.build()
    }

    fn hashtag(&mut self) {
        if let Some((idx, ch)) = self.scan.hashcode_non_delimiter() {
            match ch {
                'f' => self.boolean(false, idx),
                't' => self.boolean(true, idx),
                '\\' => {
                    self.character(idx);
                }
                '(' => {
                    self.builder.end(idx + 1).token(TokenKind::VectorOpen);
                }
                _ => {
                    self.builder
                        .end(self.scan.end_of_token())
                        .error(TokenErrorKind::HashInvalid);
                }
            }
        } else {
            self.builder.error(TokenErrorKind::HashUnterminated);
        }
    }

    fn boolean(&mut self, val: bool, at: usize) {
        let end = self.scan.end_of_token();
        // TODO: support getting lexeme from current position to end
        let remaining = self.scan.lexeme(at + 1..end);
        self.builder.end(end).kind(
            if remaining.is_empty() || remaining == if val { "rue" } else { "alse" } {
                Ok(TokenKind::Literal(Literal::Boolean(val)))
            } else {
                Err(TokenErrorKind::ExpectedBoolean(val))
            },
        );
    }

    fn character(&mut self, at: usize) {
        if let Some((idx, ch)) = self.scan.char() {
            let end = self.scan.end_of_token();
            self.builder.end(end);
            let remaining = self.scan.lexeme(idx + 1..end);
            match ch {
                'x' => todo!(), //maybe_hex(),
                _ if remaining.is_empty() => {
                    self.builder
                        .token(TokenKind::Literal(Literal::Character(ch)));
                }
                _ => {
                    if let Some(literal) = match (ch, remaining) {
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
                        self.builder
                            .token(TokenKind::Literal(Literal::Character(literal)));
                    } else {
                        self.builder.error(TokenErrorKind::ExpectedCharacter);
                    }
                }
            }
        } else {
            self.builder
                .end(at + 1)
                .token(TokenKind::Literal(Literal::Character('\n')));
        }
    }

    fn not_implemented(&mut self) {
        let start = self.start.0;
        let end = self.scan.end_of_token();
        self.builder
            .end(end)
            .error(TokenErrorKind::Unimplemented(String::from(
                self.scan.lexeme(start..end),
            )));
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
            let mut t = Tokenizer::start((0, 'a'), &mut s);

            t.run();
            let r = t.extract();

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
            let mut t = Tokenizer::start(s.char().unwrap(), &mut s);

            t.run();
            let r = t.extract();

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
            let mut t = Tokenizer::start(s.char().unwrap(), &mut s);

            t.run();
            let r = t.extract();

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
            let mut t = Tokenizer::start(s.char().unwrap(), &mut s);

            t.run();
            let r = t.extract();

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
            let mut t = Tokenizer::start(s.char().unwrap(), &mut s);

            t.run();
            let r = t.extract();

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
            let mut t = Tokenizer::start(s.char().unwrap(), &mut s);

            t.run();
            let r = t.extract();

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
            let mut t = Tokenizer::start(s.char().unwrap(), &mut s);

            t.run();
            let r = t.extract();

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
                let mut t = Tokenizer::start(s.char().unwrap(), &mut s);

                t.run();
                let r = t.extract();

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
                let mut t = Tokenizer::start(s.char().unwrap(), &mut s);

                t.run();
                let r = t.extract();

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
                let mut t = Tokenizer::start(s.char().unwrap(), &mut s);

                t.run();
                let r = t.extract();

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
                let mut t = Tokenizer::start(s.char().unwrap(), &mut s);

                t.run();
                let r = t.extract();

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
                let mut t = Tokenizer::start(s.char().unwrap(), &mut s);

                t.run();
                let r = t.extract();

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
                let mut t = Tokenizer::start(s.char().unwrap(), &mut s);

                t.run();
                let r = t.extract();

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
                let mut t = Tokenizer::start(s.char().unwrap(), &mut s);

                t.run();
                let r = t.extract();

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
                let mut t = Tokenizer::start(s.char().unwrap(), &mut s);

                t.run();
                let r = t.extract();

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
                let mut t = Tokenizer::start(s.char().unwrap(), &mut s);

                t.run();
                let r = t.extract();

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
                let mut t = Tokenizer::start(s.char().unwrap(), &mut s);

                t.run();
                let r = t.extract();

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
                let mut t = Tokenizer::start(s.char().unwrap(), &mut s);

                t.run();
                let r = t.extract();

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
                let mut t = Tokenizer::start(s.char().unwrap(), &mut s);

                t.run();
                let r = t.extract();

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
                let mut t = Tokenizer::start(s.char().unwrap(), &mut s);

                t.run();
                let r = t.extract();

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
                let mut t = Tokenizer::start(s.char().unwrap(), &mut s);

                t.run();
                let r = t.extract();

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
                let mut t = Tokenizer::start(s.char().unwrap(), &mut s);

                t.run();
                let r = t.extract();

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
                let mut t = Tokenizer::start(s.char().unwrap(), &mut s);

                t.run();
                let r = t.extract();

                assert!(matches!(
                    r,
                    Ok(Token {
                        kind: TokenKind::Literal(Literal::Character(' ')),
                        span: Range { start: 0, end: 3 }
                    })
                ));
            }

            #[test]
            fn space_followed_by_char() {
                let mut s = Scanner::new("#\\ b");
                let mut t = Tokenizer::start(s.char().unwrap(), &mut s);

                t.run();
                let r = t.extract();

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
                let mut t = Tokenizer::start(s.char().unwrap(), &mut s);

                t.run();
                let r = t.extract();

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
            fn invalid_name() {
                let mut s = Scanner::new("#\\ab");
                let mut t = Tokenizer::start(s.char().unwrap(), &mut s);

                t.run();
                let r = t.extract();

                assert!(matches!(
                    r,
                    Err(TokenError {
                        kind: TokenErrorKind::ExpectedCharacter,
                        span: Range { start: 0, end: 4 }
                    })
                ));
            }

            fn check_character_list(expected: &[(&str, char)]) {
                for &(inp, ex) in expected {
                    let input = format!("#\\{inp}");
                    let mut s = Scanner::new(&input);
                    let mut t = Tokenizer::start(s.char().unwrap(), &mut s);

                    t.run();
                    let r = t.extract();

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
