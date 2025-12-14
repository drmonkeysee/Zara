mod expr;
mod parse;
#[cfg(test)]
mod tests;

pub(crate) use self::expr::Sequence;
use self::{
    expr::{ExprCtx, ExprEnd, Expression, ExpressionError, ExpressionKind, PeekableExt},
    parse::{MergeFlow, ParseBreak, ParseErrBreak, ParseErrFlow, ParseFlow, ParseNode},
};
use crate::{
    eval::Namespace,
    lex::TokenLine,
    txt::{TextLine, TxtSpan},
    value::Value,
};
use std::{
    error::Error,
    fmt::{self, Display, Formatter, Write},
    iter::FilterMap,
    marker::PhantomData,
    rc::Rc,
};

pub(crate) type ParserResult = Result<ParserOutput, ParserError>;
pub(crate) type ExpressionTree = ExprParser<PrgBasis>;
pub(crate) type DataTree = ExprParser<DataBasis>;

pub(crate) trait Parser {
    fn parse(&mut self, token_lines: Box<[TokenLine]>, ns: Namespace) -> ParserResult;
    fn unsupported_continuation(&mut self) -> Option<ParserError>;

    fn clear(&mut self) {}
}

#[derive(Default)]
pub(crate) struct ExprParser<B> {
    basis: PhantomData<B>,
    parsers: Vec<ParseNode>,
}

impl<B: ParseBasis> Parser for ExprParser<B> {
    fn parse(&mut self, token_lines: Box<[TokenLine]>, ns: Namespace) -> ParserResult {
        ParseDriver::new(&mut self.parsers).parse::<B>(token_lines, &ns)
    }

    fn unsupported_continuation(&mut self) -> Option<ParserError> {
        let parser = self.parsers.pop();
        self.clear();
        Some(parser?.into_continuation_unsupported()?.into())
    }

    fn clear(&mut self) {
        self.parsers.clear();
    }
}

pub(crate) struct TokenList;

impl Parser for TokenList {
    fn parse(&mut self, token_lines: Box<[TokenLine]>, _ns: Namespace) -> ParserResult {
        Ok(ParserOutput::Complete(Sequence::new(
            tokens_expr(token_lines).into_iter().collect::<Box<[_]>>(),
        )))
    }

    fn unsupported_continuation(&mut self) -> Option<ParserError> {
        None
    }
}

#[derive(Default)]
pub(crate) struct PrgBasis;

#[derive(Default)]
pub(crate) struct DataBasis;

#[derive(Debug)]
pub(crate) enum ParserOutput {
    Complete(Sequence),
    Continuation,
}

#[derive(Debug)]
pub(crate) enum ParserError {
    Invalid(InvalidParseError),
    Syntax(SyntaxError),
}

impl ParserError {
    pub(crate) fn display_message(&self) -> ParserErrorMessage<'_> {
        ParserErrorMessage(self)
    }
}

impl Display for ParserError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.write_str("fatal error: ")?;
        match self {
            Self::Invalid(pe) => pe.fmt(f),
            Self::Syntax(se) => se.fmt(f),
        }
    }
}

impl Error for ParserError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        Some(match self {
            Self::Invalid(err) => err,
            Self::Syntax(err) => err,
        })
    }
}

impl From<InvalidParseError> for ParserError {
    fn from(value: InvalidParseError) -> Self {
        Self::Invalid(value)
    }
}

impl From<ExpressionError> for ParserError {
    fn from(value: ExpressionError) -> Self {
        Self::Syntax(SyntaxError(vec![value]))
    }
}

impl From<Vec<ExpressionError>> for ParserError {
    fn from(value: Vec<ExpressionError>) -> Self {
        Self::Syntax(SyntaxError(value))
    }
}

#[derive(Debug)]
pub(crate) enum InvalidParseError {
    EndOfParse,
    InvalidExprSource,
    InvalidExprTarget,
    MissingExprTarget,
}

impl Display for InvalidParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str("invalid parser state reached")
    }
}

impl Error for InvalidParseError {}

#[derive(Debug)]
pub(crate) struct SyntaxError(Vec<ExpressionError>);

impl Display for SyntaxError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str("invalid syntax")
    }
}

impl Error for SyntaxError {}

pub(crate) struct ParserErrorMessage<'a>(&'a ParserError);

impl Display for ParserErrorMessage<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self.0 {
            ParserError::Invalid(err) => InvalidParseErrorMessage(err).fmt(f),
            ParserError::Syntax(SyntaxError(errs)) => {
                for (txt, errs) in errs.iter().peekable().groupby_txt() {
                    SyntaxErrorLineMessage((txt, &errs)).fmt(f)?;
                }
                Ok(())
            }
        }
    }
}

trait ParseBasis {
    fn start() -> ParseNode;
}

impl ParseBasis for PrgBasis {
    fn start() -> ParseNode {
        ParseNode::prg()
    }
}

impl ParseBasis for DataBasis {
    fn start() -> ParseNode {
        ParseNode::data()
    }
}

struct ParseDriver<'a> {
    errs: Vec<ExpressionError>,
    parsers: &'a mut Vec<ParseNode>,
}

impl<'a> ParseDriver<'a> {
    fn new(parsers: &'a mut Vec<ParseNode>) -> Self {
        Self {
            errs: Vec::new(),
            parsers,
        }
    }

    fn parse<B: ParseBasis>(
        mut self,
        token_lines: Box<[TokenLine]>,
        ns: &Namespace,
    ) -> ParserResult {
        let parser = token_lines
            .into_iter()
            .fold(self.parsers.pop().unwrap_or_else(B::start), |p, ln| {
                self.parse_line(p, ln, ns)
            });

        if self.errs.is_empty() {
            Ok(if self.parsers.is_empty() {
                ParserOutput::Complete(parser.try_into()?)
            } else {
                self.parsers.push(parser);
                ParserOutput::Continuation
            })
        } else {
            self.parsers.clear();
            Err(self.errs.into())
        }
    }

    fn parse_line(&mut self, mut parser: ParseNode, line: TokenLine, ns: &Namespace) -> ParseNode {
        let TokenLine(tokens, txt) = line;
        let txt = txt.into();
        let mut errs = Vec::new();
        for token in tokens {
            match parser.parse(token, &txt, ns) {
                ParseFlow::Break(ParseBreak::Complete(end)) => {
                    parser = self.finalize_parser(parser, end, &mut errs, ns);
                }
                ParseFlow::Break(ParseBreak::Err { err, flow }) => {
                    errs.push(err);
                    if let ParseErrFlow::Break(brk) = flow {
                        parser = match brk {
                            ParseErrBreak::FailedNode => self.parsers.pop().unwrap_or(
                                ParseNode::InvalidParseTree(InvalidParseError::EndOfParse),
                            ),
                            ParseErrBreak::InvalidTokenStream => ParseNode::InvalidTokenStream,
                        }
                    }
                }
                ParseFlow::Break(ParseBreak::New(new)) => {
                    self.parsers.push(parser);
                    parser = new.into_node(Rc::clone(&txt));
                }
                ParseFlow::Continue(u) => u,
            }
        }
        if !errs.is_empty() {
            self.errs.extend(errs);
        }
        if parser.is_invalid_parse() {
            self.parsers.clear();
        }
        parser
    }

    fn finalize_parser(
        &mut self,
        parser: ParseNode,
        end: ExprEnd,
        errs: &mut Vec<ExpressionError>,
        ns: &Namespace,
    ) -> ParseNode {
        let err = match self.parsers.pop() {
            None => InvalidParseError::MissingExprTarget,
            Some(mut p) => match parser.into_expr_node(end) {
                None => InvalidParseError::InvalidExprSource,
                Some(done) => match p.merge(done, ns) {
                    Err(ParserError::Invalid(err)) => err,
                    Err(ParserError::Syntax(SyntaxError(errvec))) => {
                        errs.extend(errvec);
                        return p;
                    }
                    Ok(MergeFlow::Continue(())) => return p,
                    Ok(MergeFlow::Break(())) => return self.finalize_parser(p, end, errs, ns),
                },
            },
        };
        ParseNode::InvalidParseTree(err)
    }
}

struct InvalidParseErrorMessage<'a>(&'a InvalidParseError);

impl Display for InvalidParseErrorMessage<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.0 {
            InvalidParseError::EndOfParse => f.write_str("unexpected end-of-parse\n"),
            InvalidParseError::InvalidExprSource => {
                f.write_str("unexpected merge source not an expression\n")
            }
            InvalidParseError::InvalidExprTarget => {
                f.write_str("invalid merge target expression\n")
            }
            InvalidParseError::MissingExprTarget => {
                f.write_str("unexpected end-of-parse for merge target\n")
            }
        }
    }
}

struct SyntaxErrorLineMessage<'a>((&'a TextLine, &'a [&'a ExpressionError]));

impl Display for SyntaxErrorLineMessage<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let (txtline, errs) = self.0;
        f.write_str("Syntax Error\n")?;
        txtline.display_header().fmt(f)?;

        if errs.is_empty() {
            return Ok(());
        }

        for group in errs
            .iter()
            .filter_map(|err| (!err.ctx.span.is_empty()).then_some(&err.ctx.span))
            .partition_overlap()
        {
            let mut cursor = usize::MIN;
            f.write_char('\t')?;
            for span in group {
                write!(
                    f,
                    "{0:>1$}{2:^<3$}",
                    "^",
                    span.start + 1 - cursor,
                    "",
                    span.len() - 1,
                )?;
                cursor = span.end;
            }
            f.write_char('\n')?;
        }
        for err in errs {
            writeln!(f, "{}: {err}", err.ctx.span.start + 1)?;
        }
        Ok(())
    }
}

struct PartitionByOverlap<'a> {
    groups: <Vec<<Self as Iterator>::Item> as IntoIterator>::IntoIter,
}

impl<'a> PartitionByOverlap<'a> {
    fn new<I: IntoIterator<Item = &'a TxtSpan>>(iter: I) -> Self {
        let mut groups = Vec::<<Self as Iterator>::Item>::new();
        for span in iter {
            match groups
                .iter_mut()
                .find(|g| g.last().is_none_or(|last| span.start >= last.end))
            {
                None => groups.push(vec![span]),
                Some(g) => g.push(span),
            }
        }
        Self {
            groups: groups.into_iter(),
        }
    }
}

impl<'a> Iterator for PartitionByOverlap<'a> {
    type Item = Vec<&'a TxtSpan>;

    fn next(&mut self) -> Option<Self::Item> {
        self.groups.next()
    }
}

trait FilterMapExt<'a> {
    fn partition_overlap(self) -> PartitionByOverlap<'a>;
}

impl<
    'a,
    I: Iterator<Item = &'a &'a ExpressionError>,
    F: FnMut(&'a &'a ExpressionError) -> Option<&'a TxtSpan>,
> FilterMapExt<'a> for FilterMap<I, F>
{
    fn partition_overlap(self) -> PartitionByOverlap<'a> {
        PartitionByOverlap::new(self)
    }
}

fn tokens_expr(token_lines: Box<[TokenLine]>) -> Option<Expression> {
    let TokenLine(_, line) = token_lines.first()?;
    Some(
        ExprCtx {
            span: 0..line.line.len(),
            txt: line.clone().into(),
        }
        .into_expr(ExpressionKind::Literal(Value::TokenList(
            token_lines.into(),
        ))),
    )
}
