use crate::{lex::Token, literal::Literal};

#[derive(Debug)]
pub enum Expression {
    Literal(Literal),
    TokenStream(Vec<Token>),
}
