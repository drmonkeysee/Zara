use crate::lex::tokens::{Token, TokenError, TokenErrorKind, TokenKind, TokenResult};

pub(super) type TokenExtractResult = Result<TokenKind, TokenErrorKind>;

#[derive(Debug)]
pub(super) struct TokenExtract {
    pub(super) start: usize,
    pub(super) end: usize,
    pub(super) result: TokenExtractResult,
}

impl TokenExtract {
    pub(super) fn build(self) -> TokenResult {
        let span = self.start..self.end;
        match self.result {
            Ok(token) => Ok(Token { kind: token, span }),
            Err(err) => Err(TokenError { kind: err, span }),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::ops::Range;

    #[test]
    fn build_ok() {
        let t = TokenExtract {
            start: 4,
            end: 6,
            result: Ok(TokenKind::ParenLeft),
        };

        let r = t.build();

        assert!(matches!(
            r,
            Ok(Token {
                kind: TokenKind::ParenLeft,
                span: Range { start: 4, end: 6 }
            })
        ));
    }

    #[test]
    fn build_err() {
        let t = TokenExtract {
            start: 4,
            end: 6,
            result: Err(TokenErrorKind::HashInvalid),
        };

        let r = t.build();

        assert!(matches!(
            r,
            Err(TokenError {
                kind: TokenErrorKind::HashInvalid,
                span: Range { start: 4, end: 6 }
            })
        ));
    }
}
