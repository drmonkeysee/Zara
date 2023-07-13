mod token;
mod tokens;

pub(super) use self::tokens::{Token, TokenKind};
use self::{token::TokenStream, tokens::TokenError};

type LexerResult = Result<Vec<Token>, LexerError>;

pub(super) fn tokenize(textline: String) -> LexerResult {
    let mut errors: Vec<TokenError> = Vec::new();
    let tokens = TokenStream::on(&textline)
        .filter_map(|tr| tr.map_err(|e| errors.push(e)).ok())
        .collect();
    if errors.is_empty() {
        Ok(tokens)
    } else {
        Err(LexerError(errors, textline))
    }
}

#[derive(Debug)]
pub struct LexerError(Vec<TokenError>, String);
