mod lex;

use crate::lex::Token;

pub fn eval(text: String) -> Evaluation {
    let tokens = lex::tokenize(&text);
    Evaluation {
        text: text,
        tokens: tokens,
    }
}

#[derive(Debug)]
pub struct Evaluation {
    text: String,
    tokens: Vec<Token>,
}
