use crate::{lex::Token, literal::Literal};

#[derive(Debug)]
pub(crate) enum Expression {
    Literal(Literal),
    TokenStream(Vec<Token>),
}
