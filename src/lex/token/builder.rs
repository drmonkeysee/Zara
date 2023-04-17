use crate::lex::tokens::{Token, TokenError, TokenErrorKind, TokenKind, TokenResult};

#[derive(Default)]
pub(super) struct TokenBuilder {
    start: usize,
    end: Option<usize>,
    kind: Option<TokenKindResult>,
}

impl TokenBuilder {
    pub(super) fn start(start: usize) -> Self {
        Self {
            start,
            ..Default::default()
        }
    }

    pub(super) fn end(&mut self, end: usize) -> &mut Self {
        self.end = Some(end);
        self
    }

    pub(super) fn token(&mut self, token: TokenKind) -> &mut Self {
        self.kind(Ok(token))
    }

    pub(super) fn error(&mut self, err: TokenErrorKind) -> &mut Self {
        self.kind(Err(err))
    }

    pub(super) fn kind(&mut self, result: TokenKindResult) -> &mut Self {
        self.kind = Some(result);
        self
    }

    pub(super) fn build(self) -> TokenResult {
        let span = self.start..self.end.unwrap_or(self.start + 1);
        match self.kind.unwrap_or(Err(TokenErrorKind::Undefined)) {
            Ok(token) => Ok(Token { kind: token, span }),
            Err(err) => Err(TokenError { kind: err, span }),
        }
    }
}

type TokenKindResult = Result<TokenKind, TokenErrorKind>;

#[cfg(test)]
mod tests {
    use super::*;
    use std::ops::Range;

    #[test]
    fn default() {
        let b = TokenBuilder::default();

        assert!(matches!(
            b.build(),
            Err(TokenError {
                kind: TokenErrorKind::Undefined,
                span: Range { start: 0, end: 1 },
            })
        ));
    }

    #[test]
    fn default_with_init() {
        let b = TokenBuilder::start(5);

        assert!(matches!(
            b.build(),
            Err(TokenError {
                kind: TokenErrorKind::Undefined,
                span: Range { start: 5, end: 6 }
            })
        ));
    }

    #[test]
    fn set_end() {
        let mut b = TokenBuilder::start(5);

        b.end(10);

        assert!(matches!(
            b.build(),
            Err(TokenError {
                kind: TokenErrorKind::Undefined,
                span: Range { start: 5, end: 10 }
            })
        ));
    }

    #[test]
    fn set_backwards_end() {
        let mut b = TokenBuilder::start(5);

        b.end(3);

        assert!(matches!(
            b.build(),
            Err(TokenError {
                kind: TokenErrorKind::Undefined,
                span: Range { start: 5, end: 3 }
            })
        ));
    }

    #[test]
    fn set_token() {
        let mut b = TokenBuilder::start(5);

        b.token(TokenKind::ParenLeft);

        assert!(matches!(
            b.build(),
            Ok(Token {
                kind: TokenKind::ParenLeft,
                span: Range { start: 5, end: 6 }
            })
        ));
    }

    #[test]
    fn set_token_with_end() {
        let mut b = TokenBuilder::start(5);

        b.token(TokenKind::ParenLeft).end(10);

        assert!(matches!(
            b.build(),
            Ok(Token {
                kind: TokenKind::ParenLeft,
                span: Range { start: 5, end: 10 }
            })
        ));
    }

    #[test]
    fn set_err() {
        let mut b = TokenBuilder::start(5);

        b.error(TokenErrorKind::HashInvalid);

        assert!(matches!(
            b.build(),
            Err(TokenError {
                kind: TokenErrorKind::HashInvalid,
                span: Range { start: 5, end: 6 }
            })
        ));
    }

    #[test]
    fn set_err_with_end() {
        let mut b = TokenBuilder::start(5);

        b.error(TokenErrorKind::HashInvalid).end(10);

        assert!(matches!(
            b.build(),
            Err(TokenError {
                kind: TokenErrorKind::HashInvalid,
                span: Range { start: 5, end: 10 }
            })
        ));
    }

    #[test]
    fn set_token_kind() {
        let mut b = TokenBuilder::start(5);

        b.kind(Ok(TokenKind::ParenLeft));

        assert!(matches!(
            b.build(),
            Ok(Token {
                kind: TokenKind::ParenLeft,
                span: Range { start: 5, end: 6 }
            })
        ));
    }

    #[test]
    fn set_err_kind() {
        let mut b = TokenBuilder::start(5);

        b.kind(Err(TokenErrorKind::HashInvalid));

        assert!(matches!(
            b.build(),
            Err(TokenError {
                kind: TokenErrorKind::HashInvalid,
                span: Range { start: 5, end: 6 }
            })
        ));
    }
}
