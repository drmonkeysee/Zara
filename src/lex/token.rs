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
        if let Some(item) = self.scan.next_char() {
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
        if let Some((idx, ch)) = self.scan.advance() {
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
                // TODO: this will skip if ch is a delimiter
                _ => {
                    self.builder
                        .end(self.scan.until_delimiter())
                        .error(TokenErrorKind::HashInvalid);
                }
            }
        } else {
            self.builder.error(TokenErrorKind::HashUnterminated);
        }
    }

    fn boolean(&mut self, val: bool, at: usize) {
        let end = self.scan.until_delimiter();
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
        let end = self.scan.until_delimiter();
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

    mod tokenizer {
        use super::*;
        use crate::lex::tokens::{Token, TokenError};
        use std::ops::Range;

        #[test]
        fn empty_string() {
            let mut s = Scanner::new("");
            let mut t = Tokenizer::start((0, 'a'), &mut s);

            t.run();

            assert!(matches!(
                t.extract(),
                Err(TokenError {
                    kind: TokenErrorKind::Unimplemented(txt),
                    span: Range { start: 0, end: 0 }
                }) if txt == ""
            ))
        }

        #[test]
        fn token_not_implemented() {
            let mut s = Scanner::new("abc");
            let mut t = Tokenizer::start(s.advance().unwrap(), &mut s);

            t.run();

            assert!(matches!(
                t.extract(),
                Err(TokenError {
                    kind: TokenErrorKind::Unimplemented(txt),
                    span: Range { start: 0, end: 3 }
                }) if txt == "abc"
            ))
        }

        #[test]
        fn token_not_implemented_stops_at_delimiter() {
            let mut s = Scanner::new("abc;");
            let mut t = Tokenizer::start(s.advance().unwrap(), &mut s);

            t.run();

            assert!(matches!(
                t.extract(),
                Err(TokenError {
                    kind: TokenErrorKind::Unimplemented(txt),
                    span: Range { start: 0, end: 3 }
                }) if txt == "abc"
            ))
        }

        #[test]
        fn left_paren() {
            let mut s = Scanner::new("(");
            let mut t = Tokenizer::start(s.advance().unwrap(), &mut s);

            t.run();

            assert!(matches!(
                t.extract(),
                Ok(Token {
                    kind: TokenKind::ParenLeft,
                    span: Range { start: 0, end: 1 }
                })
            ))
        }

        #[test]
        fn right_paren() {
            let mut s = Scanner::new(")");
            let mut t = Tokenizer::start(s.advance().unwrap(), &mut s);

            t.run();

            assert!(matches!(
                t.extract(),
                Ok(Token {
                    kind: TokenKind::ParenRight,
                    span: Range { start: 0, end: 1 }
                })
            ))
        }
    }
}
