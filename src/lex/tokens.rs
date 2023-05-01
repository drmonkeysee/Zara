use crate::literal::Literal;
use std::ops::Range;

#[derive(Debug)]
pub(crate) enum TokenKind {
    Literal(Literal),
    ParenLeft,
    ParenRight,
    VectorOpen,
}

#[derive(Debug)]
pub(crate) enum TokenErrorKind {
    ExpectedBoolean(bool),
    ExpectedCharacter,
    HashInvalid,
    HashUnterminated,
    Unimplemented(String),
}

#[derive(Debug)]
pub(crate) struct TokenType<T> {
    pub(crate) kind: T,
    pub(crate) span: Range<usize>,
}

pub(crate) type Token = TokenType<TokenKind>;
pub(crate) type TokenError = TokenType<TokenErrorKind>;
pub(crate) type TokenResult = Result<Token, TokenError>;
