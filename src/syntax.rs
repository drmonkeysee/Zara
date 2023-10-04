mod expr;

pub use self::expr::Expression;
use self::expr::ExpressionError;
use crate::lex::{LexLine, Token, TokenKind};
use std::{
    error::Error,
    fmt::{self, Display, Formatter},
    iter::Peekable,
};

#[derive(Debug)]
pub struct ParserError(Vec<ExpressionError>);

impl Display for ParserError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str("Fatal error: parsing failure")
    }
}

impl Error for ParserError {}

pub(crate) type ParserResult = Result<Expression, ParserError>;

pub(crate) fn parse(token_lines: impl IntoIterator<Item = LexLine>) -> ParserResult {
    let mut errors = Vec::new();
    // TODO: for now flatten all lexlines into one stream of tokens
    // this'll need to handle individual lines later
    let ast = ExpressionStream::new(token_lines.into_iter().flatten())
        .filter_map(|expr| expr.map_err(|err| errors.push(err)).ok())
        .collect();
    if errors.is_empty() {
        // NOTE: top-level AST is equivalent to (begin ...)
        Ok(Expression::Begin(ast))
    } else {
        Err(ParserError(errors))
    }
}

pub(crate) fn tokens(token_lines: Vec<LexLine>) -> ParserResult {
    Ok(Expression::TokenList(token_lines))
}

type ExpressionResult = Result<Expression, ExpressionError>;

struct ExpressionStream<I: Iterator> {
    tokens: Peekable<I>,
}

impl<I: Iterator<Item = Token>> ExpressionStream<I> {
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

impl<I: Iterator<Item = Token>> Iterator for ExpressionStream<I> {
    type Item = ExpressionResult;

    fn next(&mut self) -> Option<Self::Item> {
        self.tokens.next().map(|t| self.parse_expression(t))
    }
}