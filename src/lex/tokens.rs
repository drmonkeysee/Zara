use crate::literal::Literal;
use std::ops::Range;

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
