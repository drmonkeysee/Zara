mod token;
pub(crate) mod tokens;

pub(crate) use self::tokens::Token;
use self::{token::TokenStream, tokens::TokenError};

pub(super) fn tokenize(textline: &str) -> LexerResult {
    let mut errors: Vec<TokenError> = Vec::new();
    let tokens = TokenStream::on(textline)
        .filter_map(|tr| tr.map_err(|e| errors.push(e)).ok())
        .collect();
    if errors.is_empty() {
        Ok(tokens)
    } else {
        Err(LexerFailure(errors, tokens))
    }
}

#[derive(Debug)]
pub(super) struct LexerFailure(Vec<TokenError>, Vec<Token>);

pub(super) type LexerResult = Result<Vec<Token>, LexerFailure>;
