mod expr;
mod parse;

pub(crate) use self::expr::{Datum, Expression};
use self::{
    expr::ExpressionError,
    parse::{ParseBreak, ParseFlow, ParseNode},
};
use crate::{lex::TokenLine, txt::TextLine};
use std::{
    error::Error,
    fmt::{self, Display, Formatter, Write},
    mem,
};

pub(crate) type ParserResult = Result<ParserOutput, ParserError>;

#[derive(Debug)]
pub(crate) enum ParserOutput {
    Complete(Expression),
    Continuation,
}

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
    fn unsupported_continuation(&mut self) -> Option<ParserError>;
}

pub(crate) struct TokenList;

impl Parser for TokenList {
    fn parse(&mut self, token_lines: Vec<TokenLine>) -> ParserResult {
        Ok(ParserOutput::Complete(Expression::TokenList(token_lines)))
    }

    fn unsupported_continuation(&mut self) -> Option<ParserError> {
        None
    }
}

#[derive(Default)]
pub(crate) struct ExpressionTree {
    errs: Vec<ParseErrorLine>,
    parsers: Vec<ParseNode>,
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
    fn parse_line(&mut self, mut parser: ParseNode, line: TokenLine) -> ParseNode {
        let TokenLine(tokens, txt) = line;
        for token in tokens {
            match parser.parse(token) {
                ParseFlow::Break(ParseBreak::Complete) => {
                    let done = parser;
                    // TODO: fix this unwrap
                    debug_assert!(!self.parsers.is_empty());
                    parser = self.parsers.pop().unwrap();
                    parser.merge(done);
                }
                ParseFlow::Break(ParseBreak::New(p)) => {
                    self.parsers.push(parser);
                    parser = p;
                }
                ParseFlow::Continue(_) => (),
            }
        }
        if parser.has_errors() {
            self.errs.push(ParseErrorLine(parser.take_errors(), txt));
        }
        parser
    }
}

impl Parser for ExpressionTree {
    fn parse(&mut self, token_lines: Vec<TokenLine>) -> ParserResult {
        let parser = token_lines
            .into_iter()
            .fold(self.parsers.pop().unwrap_or(ParseNode::prg()), |p, ln| {
                self.parse_line(p, ln)
            });

        if self.errs.is_empty() {
            Ok(if self.parsers.is_empty() {
                debug_assert!(parser.is_prg());
                ParserOutput::Complete(parser.to_expr())
            } else {
                self.parsers.push(parser);
                ParserOutput::Continuation
            })
        } else {
            Err(ParserError(mem::take(&mut self.errs)))
        }
    }

    fn unsupported_continuation(&mut self) -> Option<ParserError> {
        todo!();
    }
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
