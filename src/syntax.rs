mod expr;

use self::expr::ExpressionError;
pub use self::expr::{Datum, Expression};
use crate::lex::{Token, TokenKind, TokenLine};
use std::{
    error::Error,
    fmt::{self, Display, Formatter},
};

#[derive(Debug)]
pub struct ParserError(Vec<ExpressionError>);

impl Display for ParserError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str("fatal error: parsing failure")
    }
}

impl Error for ParserError {}

pub(crate) type ParserResult = Result<Expression, ParserError>;

pub(crate) fn parse(token_lines: impl IntoIterator<Item = TokenLine>) -> ParserResult {
    let mut errors = Vec::new();
    // TODO: for now flatten all lexlines into one stream of tokens
    // this'll need to handle individual lines later
    let ast = token_lines
        .into_iter()
        .flatten()
        .map(parse_expression)
        .filter_map(|expr| expr.map_err(|err| errors.push(err)).ok())
        .collect();
    if errors.is_empty() {
        // NOTE: top-level AST is equivalent to (begin ...)
        Ok(Expression::Begin(ast))
    } else {
        Err(ParserError(errors))
    }
}

pub(crate) fn tokens(token_lines: Vec<TokenLine>) -> ParserResult {
    Ok(Expression::TokenList(token_lines))
}

fn parse_expression(token: Token) -> Result<Expression, ExpressionError> {
    match token.kind {
        TokenKind::Literal(val) => Ok(Expression::Literal(val)),
        _ => Err(ExpressionError::Unimplemented(token)),
    }
}
