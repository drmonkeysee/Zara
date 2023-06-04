mod exprs;

use crate::lex::Token;

pub(super) fn parse(tokens: impl Iterator<Item = Token>) -> ParserResult {
    eprintln!("called parser");
    Ok(())
}

#[derive(Debug)]
pub(super) struct ParserFailure;

pub(super) type ParserResult = Result<(), ParserFailure>;
