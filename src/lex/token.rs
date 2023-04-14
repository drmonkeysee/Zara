use crate::{
    lex::scan::{ScanItem, Scanner},
    literal::Literal,
};
use std::ops::Range;

#[derive(Debug)]
pub struct Token {
    kind: TokenKind,
    span: Range<usize>,
}

#[derive(Debug)]
pub(super) struct TokenError {
    kind: TokenErrorKind,
    span: Range<usize>,
}

#[derive(Default)]
pub(super) struct Tokenizer<'a> {
    start: ScanItem<'a>,
    end: Option<usize>,
    kind: Option<TokenKindResult>,
}

impl Tokenizer<'_> {
    pub(super) fn start(start: ScanItem) -> Self {
        Tokenizer {
            start,
            ..Default::default()
        }
    }

    pub(super) fn scan(&mut self, scanner: &mut Scanner) {
        let ch = self.start.1;
        match ch {
            '#' => self.for_hash(scanner),
            '(' => self.token(TokenKind::ParenLeft),
            ')' => self.token(TokenKind::ParenRight),
            _ => self.error(TokenErrorKind::Unimplemented(ch)),
        };
    }

    pub(super) fn extract(self) -> Result<Token, TokenError> {
        let span = self.start.0..self.end.unwrap_or(self.start.0 + 1);
        match self.kind.unwrap_or(Err(TokenErrorKind::Undefined)) {
            Ok(token) => Ok(Token { kind: token, span }),
            Err(err) => Err(TokenError { kind: err, span }),
        }
    }

    fn end(&mut self, end: usize) -> &mut Self {
        self.end = Some(end);
        self
    }

    fn token(&mut self, token: TokenKind) -> &mut Self {
        self.kind(Ok(token))
    }

    fn error(&mut self, error: TokenErrorKind) -> &mut Self {
        self.kind(Err(error))
    }

    fn kind(&mut self, result: TokenKindResult) -> &mut Self {
        self.kind = Some(result);
        self
    }

    fn for_hash(&mut self, scanner: &mut Scanner) -> &mut Self {
        if let Some((idx, ch)) = scanner.eat() {
            match ch {
                'f' => self.for_bool(false, idx, scanner),
                't' => self.for_bool(true, idx, scanner),
                '\\' => {
                    // TODO: handle character literal
                    todo!()
                }
                '(' => self.end(idx + 1).token(TokenKind::VectorOpen),
                _ => self
                    .end(scanner.until_delimiter())
                    .error(TokenErrorKind::HashInvalid),
            }
        } else {
            self.error(TokenErrorKind::HashUnterminated)
        }
    }

    fn for_bool(&mut self, val: bool, at: usize, scanner: &mut Scanner) -> &mut Self {
        let end = scanner.until_delimiter();
        // TODO: support getting lexeme from current position to end
        let remaining = scanner.lexeme(at + 1..end);
        self.end(end).kind(
            if remaining.is_empty() || remaining == if val { "rue" } else { "alse" } {
                Ok(TokenKind::Literal(Literal::Boolean(val)))
            } else {
                Err(TokenErrorKind::ExpectedBoolean(val))
            },
        )
    }
}

type TokenKindResult = Result<TokenKind, TokenErrorKind>;

#[derive(Debug)]
enum TokenKind {
    Literal(Literal),
    ParenLeft,
    ParenRight,
    VectorOpen,
}

#[derive(Debug)]
enum TokenErrorKind {
    ExpectedBoolean(bool),
    HashInvalid,
    HashUnterminated,
    Undefined,
    Unimplemented(char),
}
