use crate::{lex::Token, literal::Literal};
use std::{
    fmt::{Display, Error, Formatter},
    write,
};

pub(super) type ExpressionResult = Result<Expression, ExpressionError>;

#[derive(Debug)]
pub enum Expression {
    Ast(Box<Expression>),
    Begin(Vec<Expression>),
    Empty,
    Literal(Literal),
    TokenStream(Vec<Token>),
}

#[derive(Debug)]
pub(super) enum ExpressionError {
    Unimplemented(Token),
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match self {
            Self::Literal(lit) => write!(f, "{}", lit),
            _ => write!(f, "#undef({:?})", self),
        }
    }
}
