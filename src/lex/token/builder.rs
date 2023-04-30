use crate::lex::{
    token::scan::ScanItem,
    tokens::{Token, TokenError, TokenErrorKind, TokenKind, TokenResult},
};

#[derive(Default)]
pub(super) struct TokenBuilder {
    start: usize,
    end: usize,
    kind: Option<TokenKindResult>,
}

impl TokenBuilder {
    pub(super) fn start(start: ScanItem) -> Self {
        Self {
            start: start.0,
            end: start.0 + start.1.len_utf8(),
            ..Default::default()
        }
    }

    pub(super) fn end(&mut self, end: usize) -> &mut Self {
        self.end = end;
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
        let span = self.start..self.end;
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

        let r = b.build();

        assert!(matches!(
            r,
            Err(TokenError {
                kind: TokenErrorKind::Undefined,
                span: Range { start: 0, end: 0 },
            })
        ));
    }

    #[test]
    fn default_with_init() {
        let b = TokenBuilder::start((5, 'a'));

        let r = b.build();

        assert!(matches!(
            r,
            Err(TokenError {
                kind: TokenErrorKind::Undefined,
                span: Range { start: 5, end: 6 }
            })
        ));
    }

    #[test]
    fn init_sets_length_correctly() {
        let b = TokenBuilder::start((5, '🦀'));

        let r = b.build();

        assert!(matches!(
            r,
            Err(TokenError {
                kind: TokenErrorKind::Undefined,
                span: Range { start: 5, end: 9 }
            })
        ));
    }

    #[test]
    fn set_end() {
        let mut b = TokenBuilder::start((5, 'a'));

        b.end(10);
        let r = b.build();

        assert!(matches!(
            r,
            Err(TokenError {
                kind: TokenErrorKind::Undefined,
                span: Range { start: 5, end: 10 }
            })
        ));
    }

    #[test]
    fn set_backwards_end() {
        let mut b = TokenBuilder::start((5, 'a'));

        b.end(3);
        let r = b.build();

        assert!(matches!(
            r,
            Err(TokenError {
                kind: TokenErrorKind::Undefined,
                span: Range { start: 5, end: 3 }
            })
        ));
    }

    #[test]
    fn set_token() {
        let mut b = TokenBuilder::start((5, 'a'));

        b.token(TokenKind::ParenLeft);
        let r = b.build();

        assert!(matches!(
            r,
            Ok(Token {
                kind: TokenKind::ParenLeft,
                span: Range { start: 5, end: 6 }
            })
        ));
    }

    #[test]
    fn set_token_with_end() {
        let mut b = TokenBuilder::start((5, 'a'));

        b.token(TokenKind::ParenLeft).end(10);
        let r = b.build();

        assert!(matches!(
            r,
            Ok(Token {
                kind: TokenKind::ParenLeft,
                span: Range { start: 5, end: 10 }
            })
        ));
    }

    #[test]
    fn set_err() {
        let mut b = TokenBuilder::start((5, 'a'));

        b.error(TokenErrorKind::HashInvalid);
        let r = b.build();

        assert!(matches!(
            r,
            Err(TokenError {
                kind: TokenErrorKind::HashInvalid,
                span: Range { start: 5, end: 6 }
            })
        ));
    }

    #[test]
    fn set_err_with_end() {
        let mut b = TokenBuilder::start((5, 'a'));

        b.error(TokenErrorKind::HashInvalid).end(10);
        let r = b.build();

        assert!(matches!(
            r,
            Err(TokenError {
                kind: TokenErrorKind::HashInvalid,
                span: Range { start: 5, end: 10 }
            })
        ));
    }

    #[test]
    fn set_token_kind() {
        let mut b = TokenBuilder::start((5, 'a'));

        b.kind(Ok(TokenKind::ParenLeft));
        let r = b.build();

        assert!(matches!(
            r,
            Ok(Token {
                kind: TokenKind::ParenLeft,
                span: Range { start: 5, end: 6 }
            })
        ));
    }

    #[test]
    fn set_err_kind() {
        let mut b = TokenBuilder::start((5, 'a'));

        b.kind(Err(TokenErrorKind::HashInvalid));
        let r = b.build();

        assert!(matches!(
            r,
            Err(TokenError {
                kind: TokenErrorKind::HashInvalid,
                span: Range { start: 5, end: 6 }
            })
        ));
    }
}
