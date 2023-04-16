mod token;
mod tokens;

use self::{
    token::TokenStream,
    tokens::{Token, TokenError},
};

pub(super) fn tokenize(textline: &str) -> LexerResult {
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

#[derive(Debug)]
pub(super) struct LexerFailure(Vec<TokenError>, Vec<Token>);

pub(super) type LexerResult = Result<Vec<Token>, LexerFailure>;
