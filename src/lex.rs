mod tokenize;
mod tokens;

pub(super) use self::tokens::{Token, TokenKind};
use self::{tokenize::TokenStream, tokens::TokenError};
use crate::txt::TextContext;

type LexerResult = Result<Vec<Token>, LexerError>;

pub(super) fn tokenize(ctx: &TextContext) -> LexerResult {
    let mut errors: Vec<TokenError> = Vec::new();
    let tokens = TokenStream::on(&ctx.line)
        .filter_map(|tr| tr.map_err(|err| errors.push(err)).ok())
        .collect();
    if errors.is_empty() {
        Ok(tokens)
    } else {
        // TODO: can this clone be removed in favor of packaging errors at lib level
        Err(LexerError(errors, ctx.clone()))
    }
}

#[derive(Debug)]
pub struct LexerError(Vec<TokenError>, TextContext);
