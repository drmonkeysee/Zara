mod token;
mod tokens;

use self::{
    token::TokenStream,
    tokens::{Token, TokenError},
};

pub fn tokenize(textline: &str) -> LexerResult {
    let mut tokens: Vec<Token> = Vec::new();
    let mut errors: Vec<TokenError> = Vec::new();
    for result in TokenStream::on(textline) {
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
