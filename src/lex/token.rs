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
    ByteVector,
    Comment,
    CommentBlock,
    CommentBlockBegin(usize),
    CommentBlockFragment(usize),
    CommentBlockEnd,
    CommentDatum,
    DirectiveCase(bool),
    Literal(Literal),
    ParenLeft,
    ParenRight,
    PairJoiner,
    Quasiquote,
    Quote,
    Unquote,
    UnquoteSplice,
    Vector,
}

impl TokenKind {
    pub(super) fn as_continuation(&self) -> Option<TokenContinuation> {
        match self {
            Self::CommentBlockBegin(depth) | Self::CommentBlockFragment(depth) => {
                Some(TokenContinuation::BlockComment(*depth))
            }
            _ => None,
        }
    }
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::ByteVector => f.write_str("BYTEVECTOR"),
            Self::Comment => f.write_str("COMMENT"),
            Self::CommentBlock => f.write_str("BLOCKCOMMENT"),
            Self::CommentBlockBegin(depth) => write!(f, "BLOCKCOMMENTBEGIN<{depth:?}>"),
            Self::CommentBlockFragment(depth) => write!(f, "BLOCKCOMMENTFRAG<{depth:?}>"),
            Self::CommentBlockEnd => f.write_str("BLOCKCOMMENTEND"),
            Self::CommentDatum => f.write_str("DATUMCOMMENT"),
            Self::DirectiveCase(fold) => write!(f, "DIRFOLDCASE<{fold:?}>"),
            Self::Literal(lit) => write!(f, "LITERAL<{lit:?}>"),
            Self::ParenLeft => f.write_str("LEFTPAREN"),
            Self::ParenRight => f.write_str("RIGHTPAREN"),
            Self::PairJoiner => f.write_str("PAIR"),
            Self::Quasiquote => f.write_str("QUASIQUOTE"),
            Self::Quote => f.write_str("QUOTE"),
            Self::Unquote => f.write_str("UNQUOTE"),
            Self::UnquoteSplice => f.write_str("UNQUOTESPLICE"),
            Self::Vector => f.write_str("VECTOR"),
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

pub(super) type TokenError = TokenType<TokenErrorKind>;
pub(super) type TokenResult = Result<Token, TokenError>;

#[derive(Debug)]
pub(super) enum TokenErrorKind {
    BooleanExpected(bool),
    ByteVectorExpected,
    CharacterExpected,
    CharacterExpectedHex,
    CharacterInvalidHex,
    DirectiveExpected,
    DirectiveInvalid,
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
            Self::DirectiveExpected => f.write_str("expected directive: fold-case or no-fold-case"),
            Self::DirectiveInvalid => {
                f.write_str("unsupported directive: expected fold-case or no-fold-case")
            }
            Self::HashInvalid => f.write_str("unexpected #-literal"),
            Self::HashUnterminated => f.write_str("unterminated #-literal"),
            Self::Unimplemented(s) => write!(f, "unimplemented tokenization: \"{s}\""),
        }
    }
}

#[derive(Debug)]
pub(super) enum TokenContinuation {
    BlockComment(usize),
}

#[cfg(test)]
mod tests {
    use super::*;

    mod token {
        use super::*;

        #[test]
        fn display_bytevector() {
            let token = Token {
                kind: TokenKind::ByteVector,
                span: 0..4,
            };

            assert_eq!(token.to_string(), "BYTEVECTOR[0..4]");
        }

        #[test]
        fn display_comment() {
            let token = Token {
                kind: TokenKind::Comment,
                span: 0..5,
            };

            assert_eq!(token.to_string(), "COMMENT[0..5]");
        }

        #[test]
        fn display_comment_block() {
            let token = Token {
                kind: TokenKind::CommentBlock,
                span: 0..10,
            };

            assert_eq!(token.to_string(), "BLOCKCOMMENT[0..10]");
        }

        #[test]
        fn display_comment_blockbegin() {
            let token = Token {
                kind: TokenKind::CommentBlockBegin(1),
                span: 0..10,
            };

            assert_eq!(token.to_string(), "BLOCKCOMMENTBEGIN<1>[0..10]");
        }

        #[test]
        fn display_comment_blockfragment() {
            let token = Token {
                kind: TokenKind::CommentBlockFragment(1),
                span: 0..10,
            };

            assert_eq!(token.to_string(), "BLOCKCOMMENTFRAG<1>[0..10]");
        }

        #[test]
        fn display_comment_blockend() {
            let token = Token {
                kind: TokenKind::CommentBlockEnd,
                span: 0..10,
            };

            assert_eq!(token.to_string(), "BLOCKCOMMENTEND[0..10]");
        }

        #[test]
        fn display_datum_comment() {
            let token = Token {
                kind: TokenKind::CommentDatum,
                span: 0..2,
            };

            assert_eq!(token.to_string(), "DATUMCOMMENT[0..2]");
        }

        #[test]
        fn display_directive() {
            let token = Token {
                kind: TokenKind::DirectiveCase(false),
                span: 0..10,
            };

            assert_eq!(token.to_string(), "DIRFOLDCASE<false>[0..10]");
        }

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

            assert_eq!(token.to_string(), "LEFTPAREN[0..1]");
        }

        #[test]
        fn display_paren_right() {
            let token = Token {
                kind: TokenKind::ParenRight,
                span: 0..1,
            };

            assert_eq!(token.to_string(), "RIGHTPAREN[0..1]");
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
        fn display_vector() {
            let token = Token {
                kind: TokenKind::Vector,
                span: 0..2,
            };

            assert_eq!(token.to_string(), "VECTOR[0..2]");
        }

        #[test]
        fn no_token_continuation() {
            let kind = TokenKind::Quote;

            assert!(kind.as_continuation().is_none());
        }

        #[test]
        fn block_comment_open_continuation() {
            let kind = TokenKind::CommentBlockBegin(2);

            assert!(matches!(
                kind.as_continuation(),
                Some(TokenContinuation::BlockComment(depth)) if depth == 2
            ));
        }

        #[test]
        fn block_comment_fragment_continuation() {
            let kind = TokenKind::CommentBlockFragment(2);

            assert!(matches!(
                kind.as_continuation(),
                Some(TokenContinuation::BlockComment(depth)) if depth == 2
            ));
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
        fn display_expected_directive() {
            let err = TokenError {
                kind: TokenErrorKind::DirectiveExpected,
                span: 0..2,
            };

            assert_eq!(
                err.to_string(),
                "expected directive: fold-case or no-fold-case"
            );
        }

        #[test]
        fn display_invalid_directive() {
            let err = TokenError {
                kind: TokenErrorKind::DirectiveInvalid,
                span: 0..10,
            };

            assert_eq!(
                err.to_string(),
                "unsupported directive: expected fold-case or no-fold-case"
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
