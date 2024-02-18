mod lexers;
mod scan;
#[cfg(test)]
mod tests;

use self::{
    lexers::{
        BlockComment, Hashtag, Identifier, PeriodIdentifier, StringLiteral, VerbatimIdentifer,
    },
    scan::{ScanItem, Scanner},
};
use super::{
    token::{TokenContinuation, TokenError, TokenErrorKind, TokenResult},
    Token, TokenKind,
};

pub(super) struct TokenStream<'txt> {
    cont: Option<TokenContinuation>,
    scanner: Scanner<'txt>,
}

impl<'txt> TokenStream<'txt> {
    pub(super) fn new(textline: &'txt str, cont: Option<TokenContinuation>) -> Self {
        Self {
            cont,
            scanner: Scanner::new(textline),
        }
    }

    fn token(&mut self) -> Option<IterItem<'txt>> {
        self.scanner.next_token().map(|item| {
            let (tok, cont) = Tokenizer {
                scanner: &mut self.scanner,
                start: item,
            }
            .extract();
            self.cont = cont;
            tok
        })
    }

    fn continuation(&mut self, cont: TokenContinuation) -> IterItem<'txt> {
        let start = self.scanner.pos();
        let (tok, cont) = Continuation {
            cont,
            scanner: &mut self.scanner,
            start,
        }
        .extract();
        self.cont = cont;
        tok
    }
}

impl Iterator for TokenStream<'_> {
    type Item = TokenResult;

    fn next(&mut self) -> Option<Self::Item> {
        match self.cont.take() {
            Some(c) => Some(self.continuation(c)),
            None => self.token(),
        }
    }
}

type IterItem<'txt> = <TokenStream<'txt> as Iterator>::Item;
type TokenExtractResult = Result<TokenKind, TokenErrorKind>;

struct Tokenizer<'me, 'txt> {
    scanner: &'me mut Scanner<'txt>,
    start: ScanItem<'txt>,
}

impl Tokenizer<'_, '_> {
    fn extract(mut self) -> (TokenResult, Option<TokenContinuation>) {
        extract(self.scan(), self.scanner, self.start.0)
    }

    fn scan(&mut self) -> TokenExtractResult {
        let first = self.start.1;
        match first {
            '(' => Ok(TokenKind::ParenLeft),
            ')' => Ok(TokenKind::ParenRight),
            '\'' => Ok(TokenKind::Quote),
            '`' => Ok(TokenKind::Quasiquote),
            '#' => Hashtag {
                scanner: self.scanner,
            }
            .scan(),
            '"' => StringLiteral::new(self.scanner).scan(),
            ';' => Ok(self.comment()),
            '.' => self.period(),
            ',' => Ok(self.unquote()),
            _ => Identifier::new(self.scanner, self.start).scan(),
        }
    }

    fn comment(&mut self) -> TokenKind {
        self.scanner.end_of_line();
        TokenKind::Comment
    }

    fn period(&mut self) -> TokenExtractResult {
        self.scanner
            .char_if_not_delimiter()
            .map_or(Ok(TokenKind::PairJoiner), |next_ch| {
                PeriodIdentifier::new(self.scanner, self.start).scan(next_ch)
            })
    }

    fn unquote(&mut self) -> TokenKind {
        self.scanner
            .char_if_eq('@')
            .map_or(TokenKind::Unquote, |_| TokenKind::UnquoteSplice)
    }
}

struct Continuation<'me, 'txt> {
    cont: TokenContinuation,
    scanner: &'me mut Scanner<'txt>,
    start: usize,
}

impl Continuation<'_, '_> {
    fn extract(mut self) -> (TokenResult, Option<TokenContinuation>) {
        extract(self.scan(), self.scanner, self.start)
    }

    fn scan(&mut self) -> TokenExtractResult {
        match self.cont {
            TokenContinuation::BlockComment { depth } => {
                Ok(BlockComment::cont(depth, self.scanner).consume())
            }
            TokenContinuation::StringLiteral { line_cont: false } => {
                StringLiteral::cont(self.scanner).scan()
            }
            TokenContinuation::StringLiteral { line_cont: true } => {
                StringLiteral::line_cont(self.scanner).scan()
            }
            TokenContinuation::SubidentifierError => {
                VerbatimIdentifer::cleanup(self.scanner).scan()
            }
            TokenContinuation::SubstringError => StringLiteral::cleanup(self.scanner).scan(),
            TokenContinuation::VerbatimIdentifier => VerbatimIdentifer::cont(self.scanner).scan(),
        }
    }
}

fn extract(
    result: TokenExtractResult,
    scanner: &mut Scanner,
    mut start: usize,
) -> (TokenResult, Option<TokenContinuation>) {
    let cont = result.as_ref().err().and_then(|err| {
        if let Some(idx) = err.sub_idx() {
            start = idx;
        }
        let cont = err.to_continuation();
        if cont.is_none() {
            scanner.end_of_token();
        }
        cont
    });
    let span = start..scanner.pos();
    (
        match result {
            Ok(token) => Ok(Token { kind: token, span }),
            Err(err) => Err(TokenError { kind: err, span }),
        },
        cont,
    )
}
