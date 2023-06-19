use crate::{lex::Token, literal::Literal};

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
