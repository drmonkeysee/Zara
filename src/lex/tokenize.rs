mod extract;
mod scan;
mod state;
#[cfg(test)]
mod tests;

use self::{
    extract::{TokenExtract, TokenExtractResult},
    scan::{ScanItem, Scanner},
    state::{BlockComment, Hashtag, StringLiteral},
};
use crate::lex::token::{TokenContinuation, TokenErrorKind, TokenKind, TokenResult};

pub(super) struct TokenStream<'a> {
    cont: Option<TokenContinuation>,
    scan: Scanner<'a>,
}

impl<'a> TokenStream<'a> {
    pub(super) fn new(textline: &'a str, cont: Option<TokenContinuation>) -> Self {
        Self {
            cont,
            scan: Scanner::new(textline),
        }
    }

    fn token(&mut self) -> Option<IterItem<'a>> {
        self.scan.next_token().map(|item| {
            Tokenizer {
                scan: &mut self.scan,
                start: item,
            }
            .extract()
            .build()
        })
    }

    fn continuation(&mut self, cont: TokenContinuation) -> Option<IterItem<'a>> {
        let start = self.scan.pos();
        Some(
            Continuation {
                cont,
                scan: &mut self.scan,
                start: start,
            }
            .extract()
            .build(),
        )
    }
}

impl Iterator for TokenStream<'_> {
    type Item = TokenResult;

    fn next(&mut self) -> Option<Self::Item> {
        match self.cont.take() {
            Some(c) => self.continuation(c),
            None => self.token(),
        }
    }
}

type IterItem<'a> = <TokenStream<'a> as Iterator>::Item;

struct Tokenizer<'me, 'str> {
    scan: &'me mut Scanner<'str>,
    start: ScanItem<'str>,
}

impl<'me, 'str> Tokenizer<'me, 'str> {
    fn extract(mut self) -> TokenExtract {
        let (result, end) = self.scan();
        TokenExtract {
            start: self.start.0,
            end,
            result,
        }
    }

    fn scan(&mut self) -> (TokenExtractResult, usize) {
        (
            match self.start.1 {
                '(' => Ok(TokenKind::ParenLeft),
                ')' => Ok(TokenKind::ParenRight),
                '\'' => Ok(TokenKind::Quote),
                '`' => Ok(TokenKind::Quasiquote),
                '#' => Hashtag { scan: self.scan }.scan(),
                '"' => StringLiteral { scan: self.scan }.scan(),
                ';' => self.comment(),
                '.' => self.period(),
                ',' => self.unquote(),
                _ => self.not_implemented(),
            },
            self.scan.pos(),
        )
    }

    fn comment(&mut self) -> TokenExtractResult {
        self.scan.end_of_line();
        Ok(TokenKind::Comment)
    }

    fn period(&mut self) -> TokenExtractResult {
        let rest = self.scan.rest_of_token();
        if rest.is_empty() {
            Ok(TokenKind::PairJoiner)
        } else {
            // TODO: jump to number tokenization with period and next char?
            self.not_implemented()
        }
    }

    fn unquote(&mut self) -> TokenExtractResult {
        Ok(self
            .scan
            .char_if_eq('@')
            .map_or(TokenKind::Unquote, |_| TokenKind::UnquoteSplice))
    }

    fn not_implemented(&mut self) -> TokenExtractResult {
        let start = self.start.0;
        let end = self.scan.end_of_token();
        Err(TokenErrorKind::Unimplemented(
            self.scan.lexeme(start..end).to_owned(),
        ))
    }
}

struct Continuation<'me, 'str> {
    cont: TokenContinuation,
    scan: &'me mut Scanner<'str>,
    start: usize,
}

impl<'me, 'str> Continuation<'me, 'str> {
    fn extract(mut self) -> TokenExtract {
        let result = Ok(self.consume());
        TokenExtract {
            start: self.start,
            end: self.scan.pos(),
            result,
        }
    }

    fn consume(&mut self) -> TokenKind {
        match self.cont {
            TokenContinuation::BlockComment(depth) => {
                BlockComment::cont(depth, &mut self.scan).consume()
            }
        }
    }
}
