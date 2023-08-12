mod exprs;

pub use self::exprs::Expression;
use self::exprs::{ExpressionError, ExpressionResult};
use crate::lex::{LexLine, Token, TokenKind};
use std::iter::Peekable;

pub(crate) type ParserResult = Result<Expression, ParserError>;

pub(crate) fn parse(token_lines: impl Iterator<Item = LexLine>) -> ParserResult {
    // TODO: support CLI flag for outputing Token Stream expression
    //return Ok(Expression::TokenStream(token_lines.collect()));
    let mut errors: Vec<ExpressionError> = Vec::new();
    // TODO: for now flatten all lexlines into one stream of tokens
    // this'll need to handle individual lines later
    let ast = Parser::new(token_lines.flatten())
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
