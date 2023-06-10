use crate::literal::Literal;

#[derive(Debug)]
pub(crate) enum Expression {
    Literal(Literal),
}
