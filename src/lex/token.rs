mod builder;
mod scan;

use self::{
    builder::TokenBuilder,
    scan::{ScanItem, ScanPolicy, Scanner},
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
        if let Some(item) = self.scan.next(ScanPolicy::SkipWhitespace) {
            let mut tokenizer = Tokenizer::start(item, &mut self.scan);
            tokenizer.run();
            Some(tokenizer.extract())
        } else {
            None
        }
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
        if let Some((idx, ch)) = self.scan.next(ScanPolicy::Hashcode) {
            match ch {
                'f' => self.boolean(false, idx),
                't' => self.boolean(true, idx),
                '\\' => {
                    // TODO: handle character literal
                    todo!()
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
            let mut t = Tokenizer::start(s.next(ScanPolicy::Any).unwrap(), &mut s);

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
            let mut t = Tokenizer::start(s.next(ScanPolicy::Any).unwrap(), &mut s);

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
            let mut t = Tokenizer::start(s.next(ScanPolicy::Any).unwrap(), &mut s);

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
            let mut t = Tokenizer::start(s.next(ScanPolicy::Any).unwrap(), &mut s);

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
            let mut t = Tokenizer::start(s.next(ScanPolicy::Any).unwrap(), &mut s);

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
            let mut t = Tokenizer::start(s.next(ScanPolicy::Any).unwrap(), &mut s);

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
                let mut t = Tokenizer::start(s.next(ScanPolicy::Any).unwrap(), &mut s);

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
                let mut t = Tokenizer::start(s.next(ScanPolicy::Any).unwrap(), &mut s);

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
                let mut t = Tokenizer::start(s.next(ScanPolicy::Any).unwrap(), &mut s);

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
                let mut t = Tokenizer::start(s.next(ScanPolicy::Any).unwrap(), &mut s);

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
                let mut t = Tokenizer::start(s.next(ScanPolicy::Any).unwrap(), &mut s);

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
                let mut t = Tokenizer::start(s.next(ScanPolicy::Any).unwrap(), &mut s);

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
                let mut t = Tokenizer::start(s.next(ScanPolicy::Any).unwrap(), &mut s);

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
                let mut t = Tokenizer::start(s.next(ScanPolicy::Any).unwrap(), &mut s);

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
                let mut t = Tokenizer::start(s.next(ScanPolicy::Any).unwrap(), &mut s);

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
                let mut t = Tokenizer::start(s.next(ScanPolicy::Any).unwrap(), &mut s);

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
                let mut t = Tokenizer::start(s.next(ScanPolicy::Any).unwrap(), &mut s);

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
                let mut t = Tokenizer::start(s.next(ScanPolicy::Any).unwrap(), &mut s);

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
    }
}
