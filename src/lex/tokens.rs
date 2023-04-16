use crate::literal::Literal;
use std::ops::Range;

#[derive(Debug)]
pub enum TokenKind {
    Literal(Literal),
    ParenLeft,
    ParenRight,
    VectorOpen,
}

#[derive(Debug)]
pub enum TokenErrorKind {
    ExpectedBoolean(bool),
    HashInvalid,
    HashUnterminated,
    Undefined,
    Unimplemented(String),
}

#[derive(Debug)]
pub struct TokenType<T> {
    pub(crate) kind: T,
    pub(crate) span: Range<usize>,
}

pub type Token = TokenType<TokenKind>;
pub type TokenError = TokenType<TokenErrorKind>;
