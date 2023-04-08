use crate::{
    lex::scan::{ScanItem, Scanner},
    literal::Literal,
};
use std::ops::Range;

#[derive(Default)]
pub struct Tokenizer<'a> {
    start: ScanItem<'a>,
    end: Option<usize>,
    kind: Option<TokenKindResult>,
}

impl<'a> Tokenizer<'a> {
    pub fn start(start: ScanItem) -> Self {
        Tokenizer {
            start,
            ..Default::default()
        }
    }

    pub fn scan(&mut self, scanner: &mut Scanner) {
        let ch = self.start.1;
        match ch {
            '#' => self.for_hash(scanner),
            '(' => self.token(TokenKind::ParenLeft),
            ')' => self.token(TokenKind::ParenRight),
            _ => self.error(TokenErrorKind::Unimplemented(ch)),
        };
    }

    pub fn extract(self) -> Result<Token, TokenError> {
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
        if let Some((idx, ch)) = scanner.next() {
            match ch {
                'f' => self.for_bool("alse", false, scanner),
                't' => self.for_bool("rue", true, scanner),
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

    fn for_bool(&mut self, pattern: &str, val: bool, scanner: &mut Scanner) -> &mut Self {
        let start = scanner.pos();
        let end = scanner.until_delimiter();
        let lexeme = scanner.lexeme(start..end);
        self.end(end)
            .kind(if lexeme.is_empty() || lexeme == pattern {
                Ok(TokenKind::Literal(Literal::Boolean(val)))
            } else {
                Err(TokenErrorKind::ExpectedBoolean(val))
            })
    }
}

#[derive(Debug)]
pub struct Token {
    kind: TokenKind,
    span: Range<usize>,
}

#[derive(Debug)]
pub struct TokenError {
    kind: TokenErrorKind,
    span: Range<usize>,
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
