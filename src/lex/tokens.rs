use crate::literal::Literal;
use std::{
    fmt::{Display, Formatter},
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
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}[{:?}]", self.kind, self.span)
    }
}

impl Display for TokenError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.kind.to_string())
    }
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Literal(lit) => write!(f, "LITERAL{{{:?}}}", lit),
            Self::ParenLeft => f.write_str("LPAREN"),
            Self::ParenRight => f.write_str("RPAREN"),
            Self::VectorOpen => f.write_str("OPENVEC"),
        }
    }
}

impl Display for TokenErrorKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::BooleanExpected(b) => write!(f, "expected boolean literal: {}", b),
            Self::CharacterExpected => f.write_str("expected character literal"),
            Self::CharacterExpectedHex => f.write_str("expected character hex escape"),
            Self::CharacterInvalidHex => f.write_str("invalid character hex escape"),
            Self::HashInvalid => f.write_str("invalid hash literal"),
            Self::HashUnterminated => f.write_str("unterminated hash literal"),
            Self::Unimplemented(s) => write!(f, "unimplemented tokenization: \"{}\"", s),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    mod token {
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

    mod tokenerror {
        use super::*;

        #[test]
        fn display_expected_boolean() {
            let err = TokenError {
                kind: TokenErrorKind::BooleanExpected(true),
                span: Range { start: 0, end: 4 },
            };

            assert_eq!(err.to_string(), "expected boolean literal: true");
        }

        #[test]
        fn display_expected_character() {
            let err = TokenError {
                kind: TokenErrorKind::CharacterExpected,
                span: Range { start: 0, end: 4 },
            };

            assert_eq!(err.to_string(), "expected character literal");
        }

        #[test]
        fn display_expected_character_hex() {
            let err = TokenError {
                kind: TokenErrorKind::CharacterExpectedHex,
                span: Range { start: 0, end: 4 },
            };

            assert_eq!(err.to_string(), "expected character hex escape");
        }

        #[test]
        fn display_invalid_character_hex() {
            let err = TokenError {
                kind: TokenErrorKind::CharacterInvalidHex,
                span: Range { start: 0, end: 4 },
            };

            assert_eq!(err.to_string(), "invalid character hex escape");
        }

        #[test]
        fn display_invalid_hash() {
            let err = TokenError {
                kind: TokenErrorKind::HashInvalid,
                span: Range { start: 0, end: 4 },
            };

            assert_eq!(err.to_string(), "invalid hash literal");
        }

        #[test]
        fn display_unterminated_hash() {
            let err = TokenError {
                kind: TokenErrorKind::HashUnterminated,
                span: Range { start: 0, end: 4 },
            };

            assert_eq!(err.to_string(), "unterminated hash literal");
        }

        #[test]
        fn display_unimplemented() {
            let err = TokenError {
                kind: TokenErrorKind::Unimplemented(String::from("foobar")),
                span: Range { start: 0, end: 4 },
            };

            assert_eq!(err.to_string(), "unimplemented tokenization: \"foobar\"");
        }
    }
}
