mod scan;
mod token;
mod tokens;

use self::{
    scan::Scanner,
    token::Tokenizer,
    tokens::{Token, TokenError},
};

pub fn tokenize(textline: &str) -> LexerResult {
    let mut tokens: Vec<Token> = Vec::new();
    let mut errors: Vec<TokenError> = Vec::new();
    let mut scanner = Scanner::new(textline);
    while let Some(start) = scanner.next_char() {
        let mut tokenizer = Tokenizer::start(start, &mut scanner);
        tokenizer.scan();
        match tokenizer.extract() {
            Ok(token) => tokens.push(token),
            Err(err) => errors.push(err),
        }
    }
    if errors.is_empty() {
        Ok(tokens)
    } else {
        Err(LexerFailure(errors, tokens))
    }
}

pub type LexerResult = Result<Vec<Token>, LexerFailure>;

#[derive(Debug)]
pub struct LexerFailure(Vec<TokenError>, Vec<Token>);
