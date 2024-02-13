mod expr;

use self::expr::ExpressionError;
pub(crate) use self::expr::{Datum, Expression};
use crate::lex::{Token, TokenKind, TokenLine};
use std::{
    error::Error,
    fmt::{self, Display, Formatter},
};

pub(crate) type ParserResult = Result<Expression, ParserError>;

#[derive(Debug)]
pub(crate) struct ParserError(Vec<ExpressionError>);

impl Display for ParserError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.write_str("fatal error: parsing failure")
    }
}

impl Error for ParserError {}

pub(crate) trait Parser {
    fn parse(&self, token_lines: Vec<TokenLine>) -> ParserResult;
}

pub(crate) struct TokenList;

impl Parser for TokenList {
    fn parse(&self, token_lines: Vec<TokenLine>) -> ParserResult {
        Ok(Expression::TokenList(token_lines))
    }
}

pub(crate) struct ExpressionTree;

impl Parser for ExpressionTree {
    fn parse(&self, token_lines: Vec<TokenLine>) -> ParserResult {
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
}

fn parse_expression(token: Token) -> Result<Expression, ExpressionError> {
    match token.kind {
        TokenKind::Literal(val) => Ok(Expression::Literal(val)),
        _ => Err(ExpressionError::Unimplemented(token)),
    }
}
