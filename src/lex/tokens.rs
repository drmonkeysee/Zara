use crate::literal::Literal;
use std::{
    error::Error,
    fmt,
    fmt::{Display, Formatter},
    ops::Range,
};

pub type Token = TokenType<TokenKind>;

#[derive(Debug)]
pub enum TokenKind {
    ByteVectorOpen,
    Literal(Literal),
    ParenLeft,
    ParenRight,
    PairJoiner,
    Quasiquote,
    Quote,
    Unquote,
    UnquoteSplice,
    VectorOpen,
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::ByteVectorOpen => f.write_str("OPENBYTEVEC"),
            Self::Literal(lit) => write!(f, "LITERAL<{lit:?}>"),
            Self::ParenLeft => f.write_str("LPAREN"),
            Self::ParenRight => f.write_str("RPAREN"),
            Self::PairJoiner => f.write_str("PAIR"),
            Self::Quasiquote => f.write_str("QUASIQUOTE"),
            Self::Quote => f.write_str("QUOTE"),
            Self::Unquote => f.write_str("UNQUOTE"),
            Self::UnquoteSplice => f.write_str("UNQUOTESPLICE"),
            Self::VectorOpen => f.write_str("OPENVEC"),
        }
    }
}

#[derive(Debug)]
pub struct TokenType<T> {
    pub(crate) kind: T,
    pub(crate) span: Range<usize>,
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}[{:?}]", self.kind, self.span)
    }
}

impl Display for TokenError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.kind.fmt(f)
    }
}

impl Error for TokenError {}

#[derive(Debug)]
pub(crate) enum TokenErrorKind {
    BooleanExpected(bool),
    ByteVectorExpected,
    CharacterExpected,
    CharacterExpectedHex,
    CharacterInvalidHex,
    HashInvalid,
    HashUnterminated,
    Unimplemented(String),
}

impl Display for TokenErrorKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::BooleanExpected(b) => write!(f, "expected boolean literal: {b}"),
            Self::ByteVectorExpected => f.write_str("expected bytevector literal: #u8(…)"),
            Self::CharacterExpected => f.write_str("expected character literal"),
            Self::CharacterExpectedHex => f.write_str("expected character hex-sequence"),
            Self::CharacterInvalidHex => write!(
                f,
                "character hex-sequence out of valid range: [{:#x}, {:#x}]",
                0,
                char::MAX as u32,
            ),
            Self::HashInvalid => f.write_str("unexpected #-literal"),
            Self::HashUnterminated => f.write_str("unterminated #-literal"),
            Self::Unimplemented(s) => write!(f, "unimplemented tokenization: \"{s}\""),
        }
    }
}

pub(super) type TokenError = TokenType<TokenErrorKind>;
pub(super) type TokenResult = Result<Token, TokenError>;

#[cfg(test)]
mod tests {
    use super::*;

    mod token {
        use super::*;

        #[test]
        fn display_literal() {
            let token = Token {
                kind: TokenKind::Literal(Literal::Boolean(true)),
                span: 0..2,
            };

            assert_eq!(
                token.to_string(),
                format!("LITERAL<{:?}>[0..2]", Literal::Boolean(true))
            );
        }

        #[test]
        fn display_paren_left() {
            let token = Token {
                kind: TokenKind::ParenLeft,
                span: 0..1,
            };

            assert_eq!(token.to_string(), "LPAREN[0..1]");
        }

        #[test]
        fn display_paren_right() {
            let token = Token {
                kind: TokenKind::ParenRight,
                span: 0..1,
            };

            assert_eq!(token.to_string(), "RPAREN[0..1]");
        }

        #[test]
        fn display_pair_joiner() {
            let token = Token {
                kind: TokenKind::PairJoiner,
                span: 0..1,
            };

            assert_eq!(token.to_string(), "PAIR[0..1]");
        }

        #[test]
        fn display_quasiquote() {
            let token = Token {
                kind: TokenKind::Quasiquote,
                span: 0..1,
            };

            assert_eq!(token.to_string(), "QUASIQUOTE[0..1]");
        }

        #[test]
        fn display_quote() {
            let token = Token {
                kind: TokenKind::Quote,
                span: 0..1,
            };

            assert_eq!(token.to_string(), "QUOTE[0..1]");
        }

        #[test]
        fn display_unquote() {
            let token = Token {
                kind: TokenKind::Unquote,
                span: 0..1,
            };

            assert_eq!(token.to_string(), "UNQUOTE[0..1]");
        }

        #[test]
        fn display_unquote_splice() {
            let token = Token {
                kind: TokenKind::UnquoteSplice,
                span: 0..2,
            };

            assert_eq!(token.to_string(), "UNQUOTESPLICE[0..2]");
        }

        #[test]
        fn display_vector_open() {
            let token = Token {
                kind: TokenKind::VectorOpen,
                span: 0..2,
            };

            assert_eq!(token.to_string(), "OPENVEC[0..2]");
        }

        #[test]
        fn display_bytevector_open() {
            let token = Token {
                kind: TokenKind::ByteVectorOpen,
                span: 0..4,
            };

            assert_eq!(token.to_string(), "OPENBYTEVEC[0..4]");
        }
    }

    mod tokenerror {
        use super::*;

        #[test]
        fn display_expected_boolean() {
            let err = TokenError {
                kind: TokenErrorKind::BooleanExpected(true),
                span: 0..4,
            };

            assert_eq!(err.to_string(), "expected boolean literal: true");
        }

        #[test]
        fn display_expected_bytevector() {
            let err = TokenError {
                kind: TokenErrorKind::ByteVectorExpected,
                span: 0..4,
            };

            assert_eq!(err.to_string(), "expected bytevector literal: #u8(…)");
        }

        #[test]
        fn display_expected_character() {
            let err = TokenError {
                kind: TokenErrorKind::CharacterExpected,
                span: 0..4,
            };

            assert_eq!(err.to_string(), "expected character literal");
        }

        #[test]
        fn display_expected_character_hex() {
            let err = TokenError {
                kind: TokenErrorKind::CharacterExpectedHex,
                span: 0..4,
            };

            assert_eq!(err.to_string(), "expected character hex-sequence");
        }

        #[test]
        fn display_invalid_character_hex() {
            let err = TokenError {
                kind: TokenErrorKind::CharacterInvalidHex,
                span: 0..4,
            };

            assert_eq!(
                err.to_string(),
                "character hex-sequence out of valid range: [0x0, 0x10ffff]"
            );
        }

        #[test]
        fn display_invalid_hash() {
            let err = TokenError {
                kind: TokenErrorKind::HashInvalid,
                span: 0..4,
            };

            assert_eq!(err.to_string(), "unexpected #-literal");
        }

        #[test]
        fn display_unterminated_hash() {
            let err = TokenError {
                kind: TokenErrorKind::HashUnterminated,
                span: 0..4,
            };

            assert_eq!(err.to_string(), "unterminated #-literal");
        }

        #[test]
        fn display_unimplemented() {
            let err = TokenError {
                kind: TokenErrorKind::Unimplemented("foobar".to_owned()),
                span: 0..4,
            };

            assert_eq!(err.to_string(), "unimplemented tokenization: \"foobar\"");
        }
    }
}
