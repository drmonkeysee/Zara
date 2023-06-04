mod lex;
mod literal;
mod syntax;

use self::{lex::LexerResult, syntax::ParserResult};

pub fn eval(textline: String) -> Evaluation {
    let lex_result = lex::tokenize(&textline);
    let output = if lex_result.is_err() {
        OutputKind::Lexer(lex_result)
    } else {
        OutputKind::Parser(syntax::parse(lex_result.unwrap().into_iter()))
    };
    Evaluation { textline, output }
}

#[derive(Debug)]
pub struct Evaluation {
    textline: String,
    output: OutputKind,
}

#[derive(Debug)]
enum OutputKind {
    Lexer(LexerResult),
    Parser(ParserResult),
}
