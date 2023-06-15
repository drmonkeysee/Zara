mod exprs;

pub(super) use self::exprs::Expression;
use crate::lex::Token;

pub(super) fn parse(tokens: impl Iterator<Item = Token>) -> ParserResult {
    Ok(vec![Expression::TokenStream(tokens.collect())])
}

#[derive(Debug)]
pub struct ParserFailure;

pub(super) type ParserResult = Result<Vec<Expression>, ParserFailure>;
