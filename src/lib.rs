mod lex;
mod literal;
mod syntax;

use self::lex::LexerResult;

pub fn eval(textline: String) -> Evaluation {
    let tokens = lex::tokenize(&textline);
    let ast = syntax::parse(&tokens, &textline);
    Evaluation {
        textline,
        lex_output: tokens,
    }
}

#[derive(Debug)]
pub struct Evaluation {
    textline: String,
    lex_output: LexerResult,
}
