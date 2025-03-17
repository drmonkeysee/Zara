mod expr;

pub(crate) use self::expr::{Datum, Expression};
use self::expr::{ExpressionError, ExpressionErrorKind};
use crate::{
    lex::{Token, TokenKind, TokenLine},
    literal::Literal,
    number::Number,
    txt::TextLine,
};
use std::{
    error::Error,
    fmt::{self, Display, Formatter, Write},
    mem,
    ops::ControlFlow,
};

pub(crate) type ParserResult = Result<Expression, ParserError>;

#[derive(Debug)]
pub(crate) struct ParserError(Vec<ParseErrorLine>);

impl ParserError {
    pub(crate) fn display_message(&self) -> ParserErrorMessage {
        ParserErrorMessage(&self.0)
    }
}

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
    errs: Vec<ParseErrorLine>,
    parsers: Vec<ExprTreeNode>,
}

pub(crate) struct ParserErrorMessage<'a>(&'a [ParseErrorLine]);

impl Display for ParserErrorMessage<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        for line in self.0 {
            ParseErrorLineMessage(line).fmt(f)?;
        }
        Ok(())
    }
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
    fn parse_line(&mut self, mut parser: ExprTreeNode, line: TokenLine) -> ExprTreeNode {
        let TokenLine(tokens, txt) = line;
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
        if !parser.errs.is_empty() {
            self.errs
                .push(ParseErrorLine(mem::take(&mut parser.errs), txt));
        }
        parser
    }
}

impl Parser for ExpressionTree {
    fn parse(&mut self, token_lines: Vec<TokenLine>) -> ParserResult {
        let parser = token_lines.into_iter().fold(
            self.parsers
                .pop()
                .unwrap_or(ExprTreeNode::new(NodeKind::Sequence)),
            |p, ln| self.parse_line(p, ln),
        );

        if self.errs.is_empty() {
            Ok(parser.to_expr())
        } else {
            Err(ParserError(mem::take(&mut self.errs)))
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
            _ => self.errs.push(ExpressionError {
                kind: ExpressionErrorKind::Unimplemented(token.kind),
                span: token.span,
            }),
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

#[derive(Debug)]
struct ParseErrorLine(Vec<ExpressionError>, TextLine);

struct ParseErrorLineMessage<'a>(&'a ParseErrorLine);

// TODO: unify this with token error message
impl Display for ParseErrorLineMessage<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let ParseErrorLine(errs, txtline) = self.0;
        txtline.display_header().fmt(f)?;

        if errs.is_empty() {
            return Ok(());
        }

        let mut cursor = 0;
        f.write_char('\t')?;
        for span in errs
            .iter()
            .filter_map(|err| (!err.span.is_empty()).then_some(&err.span))
        {
            write!(
                f,
                "{0:>1$}{2:^<3$}",
                "^",
                span.start + 1 - cursor,
                "",
                span.len() - 1
            )?;
            cursor = span.end;
        }
        f.write_char('\n')?;
        for err in errs {
            writeln!(f, "{}: {err}", err.span.start + 1)?;
        }
        Ok(())
    }
}
