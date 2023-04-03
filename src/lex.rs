mod scan;
mod token;

use self::{
    scan::Scanner,
    token::{Token, TokenError, Tokenizer},
};

pub fn tokenize(textline: &str) -> LexerResult {
    let mut tokens: Vec<Token> = Vec::new();
    let mut errors: Vec<TokenError> = Vec::new();
    let mut scanner = Scanner::new(textline);
    while let Some(start) = scanner.eat() {
        if start.1.is_ascii_whitespace() {
            continue;
        }
        let mut tokenizer = Tokenizer::start(start);
        tokenizer.scan(&mut scanner);
        let result = tokenizer.extract();
        match result {
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
