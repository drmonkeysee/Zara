use crate::literal::Literal;
use std::{
    fmt::{Display, Error, Formatter},
    ops::Range,
};

pub(crate) type Token = TokenType<TokenKind>;
pub(super) type TokenError = TokenType<TokenErrorKind>;
pub(super) type TokenResult = Result<Token, TokenError>;

#[derive(Debug)]
pub enum TokenKind {
    Literal(Literal),
    ParenLeft,
    ParenRight,
    VectorOpen,
}

#[derive(Debug)]
pub(crate) enum TokenErrorKind {
    BooleanExpected(bool),
    CharacterExpected,
    CharacterExpectedHex,
    CharacterInvalidHex,
    HashInvalid,
    HashUnterminated,
    Unimplemented(String),
}

#[derive(Debug)]
pub struct TokenType<T> {
    pub(crate) kind: T,
    pub(crate) span: Range<usize>,
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write!(f, "{}[{}..{}]", self.kind, self.span.start, self.span.end)
    }
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match self {
            Self::Literal(lit) => write!(f, "LITERAL{{{:?}}}", lit),
            Self::ParenLeft => f.write_str("LPAREN"),
            Self::ParenRight => f.write_str("RPAREN"),
            Self::VectorOpen => f.write_str("OPENVEC"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn display_literal() {
        let token = Token {
            kind: TokenKind::Literal(Literal::Boolean(true)),
            span: Range { start: 0, end: 2 },
        };

        assert_eq!(
            token.to_string(),
            format!("LITERAL{{{:?}}}[0..2]", Literal::Boolean(true))
        );
    }

    #[test]
    fn display_paren_left() {
        let token = Token {
            kind: TokenKind::ParenLeft,
            span: Range { start: 0, end: 1 },
        };

        assert_eq!(token.to_string(), "LPAREN[0..1]");
    }

    #[test]
    fn display_paren_right() {
        let token = Token {
            kind: TokenKind::ParenRight,
            span: Range { start: 0, end: 1 },
        };

        assert_eq!(token.to_string(), "RPAREN[0..1]");
    }

    #[test]
    fn display_vector_open() {
        let token = Token {
            kind: TokenKind::VectorOpen,
            span: Range { start: 0, end: 2 },
        };

        assert_eq!(token.to_string(), "OPENVEC[0..2]");
    }
}
