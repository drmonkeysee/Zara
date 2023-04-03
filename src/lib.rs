mod lex;
mod literal;

use crate::lex::LexerResult;

pub fn eval(text: String) -> Evaluation {
    let result = lex::tokenize(&text);
    Evaluation {
        text: text,
        lex_output: result,
    }
}

#[derive(Debug)]
pub struct Evaluation {
    text: String,
    lex_output: LexerResult,
}
