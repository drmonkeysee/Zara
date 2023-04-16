use crate::{
    lex::{
        scan::{ScanItem, Scanner},
        tokens::{Token, TokenError, TokenErrorKind, TokenKind},
    },
    literal::Literal,
};

pub(super) struct Tokenizer<'t, 's> {
    builder: TokenBuilder,
    scan: &'t mut Scanner<'s>,
    start: ScanItem<'s>,
}

impl<'t, 's> Tokenizer<'t, 's> {
    pub(super) fn start(start: ScanItem, scan: &'t mut Scanner<'s>) -> Self {
        Self {
            start,
            scan,
            builder: TokenBuilder::start(start.0),
        }
    }

    pub(super) fn scan(&mut self) {
        match self.start.1 {
            '#' => self.hashcode(),
            '(' => {
                self.builder.token(TokenKind::ParenLeft);
            }
            ')' => {
                self.builder.token(TokenKind::ParenRight);
            }
            _ => self.not_implemented(),
        }
    }

    pub(super) fn extract(self) -> Result<Token, TokenError> {
        self.builder.build()
    }

    fn hashcode(&mut self) {
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

#[derive(Default)]
struct TokenBuilder {
    start: usize,
    end: Option<usize>,
    kind: Option<TokenKindResult>,
}

impl TokenBuilder {
    fn start(start: usize) -> Self {
        Self {
            start,
            ..Default::default()
        }
    }

    fn end(&mut self, end: usize) -> &mut Self {
        self.end = Some(end);
        self
    }

    fn token(&mut self, token: TokenKind) -> &mut Self {
        self.kind(Ok(token))
    }

    fn error(&mut self, err: TokenErrorKind) -> &mut Self {
        self.kind(Err(err))
    }

    fn kind(&mut self, result: TokenKindResult) -> &mut Self {
        self.kind = Some(result);
        self
    }

    fn build(self) -> Result<Token, TokenError> {
        let span = self.start..self.end.unwrap_or(self.start + 1);
        match self.kind.unwrap_or(Err(TokenErrorKind::Undefined)) {
            Ok(token) => Ok(Token { kind: token, span }),
            Err(err) => Err(TokenError { kind: err, span }),
        }
    }
}

type TokenKindResult = Result<TokenKind, TokenErrorKind>;
