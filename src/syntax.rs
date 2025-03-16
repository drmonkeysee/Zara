mod expr;

use self::expr::ExpressionError;
pub(crate) use self::expr::{Datum, Expression};
use crate::{
    lex::{Token, TokenKind, TokenLine},
    literal::Literal,
    number::Number,
};
use std::{
    error::Error,
    fmt::{self, Display, Formatter},
    ops::ControlFlow,
};

pub(crate) type ParserResult = Result<Expression, ParserError>;

#[derive(Debug)]
pub(crate) struct ParserError(Vec<ExpressionError>);

impl Display for ParserError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        "fatal error: parsing failure".fmt(f)
    }
}

impl Error for ParserError {}

pub(crate) trait Parser {
    fn parse(&mut self, token_lines: Vec<TokenLine>) -> ParserResult;
}

pub(crate) struct TokenList;

impl Parser for TokenList {
    fn parse(&mut self, token_lines: Vec<TokenLine>) -> ParserResult {
        Ok(Expression::TokenList(token_lines))
    }
}

#[derive(Default)]
pub(crate) struct ExpressionTree {
    parsers: Vec<ExprTreeNode>,
}

/*
maintain stack of parsers and top-level Sequence in ExpressionTree
parse:
if stack empty:
    push Sequence builder
else:
    pop builder off stack and start there
for each TokenLine:
    for each Token:
        if expr complete:
            return expr
        create builder(token pos, txtline)
        while builder.parse(token) until complete or out of tokens
        if builder.complete:
            return expr
        else:
            return builder
    if expr:
        add to builder on top of stack
    if builder:
        push builder on stack
if stack empty:
    return Expression::Begin(move and reset sequence with mem::take)
else:
    return continuation
*/

impl ExpressionTree {
    fn parse_line(&mut self, mut parser: ExprTreeNode, tokens: TokenLine) -> ExprTreeNode {
        for token in tokens {
            match parser.parse(token) {
                ControlFlow::Break(ParseBreak::Complete) => {
                    let mut done = parser;
                    // TODO: fix this unwrap
                    parser = self.parsers.pop().unwrap();
                    parser.errs.append(&mut done.errs);
                    parser.exprs.push(done.to_expr());
                }
                ControlFlow::Break(ParseBreak::Continuation) => todo!(),
                ControlFlow::Break(ParseBreak::New(n)) => {
                    self.parsers.push(parser);
                    parser = n;
                }
                ControlFlow::Continue(_) => (),
            }
        }
        parser
    }
}

impl Parser for ExpressionTree {
    fn parse(&mut self, token_lines: Vec<TokenLine>) -> ParserResult {
        let mut parser = self
            .parsers
            .pop()
            .unwrap_or(ExprTreeNode::new(NodeKind::Sequence));

        parser = token_lines
            .into_iter()
            .fold(parser, |p, ln| self.parse_line(p, ln));

        if parser.errs.is_empty() {
            Ok(parser.to_expr())
        } else {
            Err(ParserError(parser.errs))
        }
    }
}

struct ExprTreeNode {
    errs: Vec<ExpressionError>,
    exprs: Vec<Expression>,
    kind: NodeKind,
}

impl ExprTreeNode {
    fn new(kind: NodeKind) -> Self {
        Self {
            errs: Vec::new(),
            exprs: Vec::new(),
            kind,
        }
    }

    fn parse(&mut self, token: Token) -> ControlFlow<ParseBreak> {
        match token.kind {
            TokenKind::Imaginary(r) => self
                .exprs
                .push(Expression::Literal(Literal::Number(Number::imaginary(r)))),
            TokenKind::Literal(val) => self.exprs.push(Expression::Literal(val)),
            // TODO: add text line to errors
            _ => self.errs.push(ExpressionError::Unimplemented(token)),
        }
        ControlFlow::Continue(())
    }

    fn to_expr(self) -> Expression {
        match self.kind {
            NodeKind::Sequence => Expression::Begin(self.exprs),
        }
    }
}

enum NodeKind {
    Sequence,
}

enum ParseBreak {
    Complete,
    Continuation,
    New(ExprTreeNode),
}
