mod extract;
mod scan;
mod state;
#[cfg(test)]
mod tests;

use self::{
    extract::{TokenExtract, TokenExtractResult},
    scan::{ScanItem, Scanner},
    state::{Hashtag, StringLiteralFactory},
};
use super::token::{TokenContinuation, TokenErrorKind, TokenKind, TokenResult};

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
                start,
            }
            .extract()
            .build(),
        )
    }
}

impl Iterator for TokenStream<'_> {
    type Item = TokenResult;

    fn next(&mut self) -> Option<Self::Item> {
        let item = match self.cont.take() {
            Some(c) => self.continuation(c),
            None => self.token(),
        };
        self.cont = item
            .as_ref()
            .and_then(|te| te.as_ref().err().and_then(|err| err.kind.to_continuation()));
        item
    }
}

type IterItem<'a> = <TokenStream<'a> as Iterator>::Item;

struct Tokenizer<'me, 'str> {
    scan: &'me mut Scanner<'str>,
    start: ScanItem<'str>,
}

impl<'me, 'str> Tokenizer<'me, 'str> {
    fn extract(mut self) -> TokenExtract {
        let result = self.scan();
        TokenExtract::new(self.start.0, self.scan.pos(), result)
    }

    fn scan(&mut self) -> TokenExtractResult {
        match self.start.1 {
            '(' => Ok(TokenKind::ParenLeft),
            ')' => Ok(TokenKind::ParenRight),
            '\'' => Ok(TokenKind::Quote),
            '`' => Ok(TokenKind::Quasiquote),
            '#' => Hashtag { scan: self.scan }.scan(),
            '"' => StringLiteralFactory::new(self.scan).scan(),
            ';' => self.comment(),
            '.' => self.period(),
            ',' => self.unquote(),
            _ => self.not_implemented(),
        }
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
        let result = self.scan();
        TokenExtract::new(self.start, self.scan.pos(), result)
    }

    fn scan(&mut self) -> TokenExtractResult {
        match self.cont {
            TokenContinuation::BlockComment(depth) => {
                Ok(state::continue_block_comment(depth, self.scan).consume())
            }
            TokenContinuation::StringLiteral(false) => StringLiteralFactory::cont(self.scan).scan(),
            TokenContinuation::StringLiteral(true) => {
                StringLiteralFactory::line_cont(self.scan).scan()
            }
            TokenContinuation::SubstringError => StringLiteralFactory::cleanup(self.scan).scan(),
        }
    }
}
