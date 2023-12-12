mod extract;
mod scan;
mod state;
#[cfg(test)]
mod tests;

use self::{
    extract::{TokenExtract, TokenExtractResult},
    scan::{ScanItem, Scanner},
    state::{
        BlockComment, Hashtag, Identifier, Numeric, PeculiarIdentifier, StringLiteral,
        VerbatimIdentifer,
    },
};
use super::token::{TokenContinuation, TokenKind, TokenResult};

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

    fn continuation(&mut self, cont: TokenContinuation) -> IterItem<'a> {
        let start = self.scan.pos();
        Continuation {
            cont,
            scan: &mut self.scan,
            start,
        }
        .extract()
        .build()
    }
}

impl Iterator for TokenStream<'_> {
    type Item = TokenResult;

    fn next(&mut self) -> Option<Self::Item> {
        let item = match self.cont.take() {
            Some(c) => Some(self.continuation(c)),
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
        let ch = self.start.1;
        match ch {
            '(' => Ok(TokenKind::ParenLeft),
            ')' => Ok(TokenKind::ParenRight),
            '\'' => Ok(TokenKind::Quote),
            '`' => Ok(TokenKind::Quasiquote),
            '#' => Hashtag { scan: self.scan }.scan(),
            '"' => StringLiteral::new(self.scan).scan(),
            ';' => Ok(self.comment()),
            '.' => self.period(),
            ',' => Ok(self.unquote()),
            _ if ch.is_ascii_digit() => Numeric { scan: self.scan }.scan(),
            _ => Identifier::new(self.scan, &self.start).scan(),
        }
    }

    fn comment(&mut self) -> TokenKind {
        self.scan.end_of_line();
        TokenKind::Comment
    }

    fn period(&mut self) -> TokenExtractResult {
        self.scan
            .char_if_not_delimiter()
            .map_or(Ok(TokenKind::PairJoiner), |ch| {
                PeculiarIdentifier::new(self.scan, &self.start).scan(ch)
            })
    }

    fn unquote(&mut self) -> TokenKind {
        self.scan
            .char_if_eq('@')
            .map_or(TokenKind::Unquote, |_| TokenKind::UnquoteSplice)
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
            TokenContinuation::BlockComment { depth } => {
                Ok(BlockComment::cont(depth, self.scan).consume())
            }
            TokenContinuation::StringLiteral { line_cont: false } => {
                StringLiteral::cont(self.scan).scan()
            }
            TokenContinuation::StringLiteral { line_cont: true } => {
                StringLiteral::line_cont(self.scan).scan()
            }
            TokenContinuation::SubidentifierError => VerbatimIdentifer::cleanup(self.scan).scan(),
            TokenContinuation::SubstringError => StringLiteral::cleanup(self.scan).scan(),
            TokenContinuation::VerbatimIdentifier => VerbatimIdentifer::cont(self.scan).scan(),
        }
    }
}
