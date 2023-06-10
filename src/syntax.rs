mod exprs;

use self::exprs::Expression;
use crate::lex::Token;

pub(super) fn parse(tokens: impl Iterator<Item = Token>) -> ParserResult {
    eprintln!("called parser");
    Ok(Vec::new())
}

#[derive(Debug)]
pub(super) struct ParserFailure;

pub(super) type ParserResult = Result<Vec<Expression>, ParserFailure>;
