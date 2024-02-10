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
use super::token::{Token, TokenContinuation, TokenError, TokenErrorKind, TokenKind, TokenResult};

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
            let (tok, cont) = Tokenizer {
                scan: &mut self.scan,
                start: item,
            }
            .extract();
            self.cont = cont;
            tok
        })
    }

    fn continuation(&mut self, cont: TokenContinuation) -> IterItem<'a> {
        let start = self.scan.pos();
        let (tok, cont) = Continuation {
            cont,
            scan: &mut self.scan,
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

type IterItem<'a> = <TokenStream<'a> as Iterator>::Item;
type TokenExtractResult = Result<TokenKind, TokenErrorKind>;

struct Tokenizer<'me, 'str> {
    scan: &'me mut Scanner<'str>,
    start: ScanItem<'str>,
}

impl<'me, 'str> Tokenizer<'me, 'str> {
    fn extract(mut self) -> (TokenResult, Option<TokenContinuation>) {
        extract(self.scan(), self.scan, self.start.0)
    }

    fn scan(&mut self) -> TokenExtractResult {
        let first = self.start.1;
        match first {
            '(' => Ok(TokenKind::ParenLeft),
            ')' => Ok(TokenKind::ParenRight),
            '\'' => Ok(TokenKind::Quote),
            '`' => Ok(TokenKind::Quasiquote),
            '#' => Hashtag { scan: self.scan }.scan(),
            '"' => StringLiteral::new(self.scan).scan(),
            ';' => Ok(self.comment()),
            '.' => self.period(),
            ',' => Ok(self.unquote()),
            _ => Identifier::new(self.scan, self.start).scan(),
        }
    }

    fn comment(&mut self) -> TokenKind {
        self.scan.end_of_line();
        TokenKind::Comment
    }

    fn period(&mut self) -> TokenExtractResult {
        self.scan
            .char_if_not_delimiter()
            .map_or(Ok(TokenKind::PairJoiner), |next_ch| {
                PeriodIdentifier::new(self.scan, self.start).scan(next_ch)
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
    fn extract(mut self) -> (TokenResult, Option<TokenContinuation>) {
        extract(self.scan(), self.scan, self.start)
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

fn extract(
    result: TokenExtractResult,
    scan: &mut Scanner,
    mut start: usize,
) -> (TokenResult, Option<TokenContinuation>) {
    let cont = result.as_ref().err().and_then(|err| {
        if let Some(idx) = err.sub_idx() {
            start = idx;
        }
        let cont = err.to_continuation();
        if cont.is_none() {
            scan.end_of_token();
        }
        cont
    });
    let span = start..scan.pos();
    (
        match result {
            Ok(token) => Ok(Token { kind: token, span }),
            Err(err) => Err(TokenError { kind: err, span }),
        },
        cont,
    )
}
