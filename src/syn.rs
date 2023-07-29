mod exprs;

pub use self::exprs::Expression;
use self::exprs::{ExpressionError, ExpressionResult};
use crate::lex::{Token, TokenKind};
use std::iter::Peekable;

pub(crate) type ParserResult = Result<Expression, ParserError>;

pub(crate) fn parse(tokens: impl Iterator<Item = Token>) -> ParserResult {
    // TODO: support CLI flag for outputing Token Stream expression
    //return Ok(Expression::TokenStream(tokens.collect()));
    let mut errors: Vec<ExpressionError> = Vec::new();
    let ast = Parser::new(tokens)
        .filter_map(|expr| expr.map_err(|err| errors.push(err)).ok())
        .collect();
    if errors.is_empty() {
        // NOTE: top-level AST is equivalent to (begin ...)
        Ok(Expression::Begin(ast))
    } else {
        Err(ParserError(errors))
    }
}

#[derive(Debug)]
pub struct ParserError(Vec<ExpressionError>);

struct Parser<I: Iterator> {
    tokens: Peekable<I>,
}

impl<I: Iterator<Item = Token>> Parser<I> {
    fn new(tokens: I) -> Self {
        Self {
            tokens: tokens.peekable(),
        }
    }

    fn parse_expression(&mut self, token: Token) -> ExpressionResult {
        match token.kind {
            TokenKind::Literal(val) => Ok(Expression::Literal(val)),
            _ => Err(ExpressionError::Unimplemented(token)),
        }
    }
}

impl<I: Iterator<Item = Token>> Iterator for Parser<I> {
    type Item = ExpressionResult;

    fn next(&mut self) -> Option<Self::Item> {
        self.tokens.next().map(|t| self.parse_expression(t))
    }
}
