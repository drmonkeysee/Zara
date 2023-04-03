mod lex;
mod literal;

use crate::lex::LexerResult;

pub fn eval(textline: String) -> Evaluation {
    let result = lex::tokenize(&textline);
    Evaluation {
        textline,
        lex_output: result,
    }
}

#[derive(Debug)]
pub struct Evaluation {
    textline: String,
    lex_output: LexerResult,
}
