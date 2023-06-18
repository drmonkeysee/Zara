use crate::{lex::Token, literal::Literal};

#[derive(Debug)]
pub enum Expression {
    Empty,
    Literal(Literal),
    TokenStream(Vec<Token>),
}
