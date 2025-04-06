mod expr;
mod parse;

pub(crate) use self::expr::{Datum, Expression};
use self::{
    expr::ExpressionError,
    parse::{ErrFlow, ParseBreak, ParseFlow, ParseNode, Recovery},
};
use crate::{lex::TokenLine, txt::TextLine};
use std::{
    error::Error,
    fmt::{self, Display, Formatter, Write},
    mem,
    rc::Rc,
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
        let txt = Rc::new(txt);
        let mut errs = Vec::new();
        for token in tokens {
            match parser.parse(token) {
                ParseFlow::Break(ParseBreak::Complete) => {
                    let done = parser;
                    // TODO: fix this unwrap
                    debug_assert!(!self.parsers.is_empty());
                    parser = self.parsers.pop().unwrap();
                    parser.merge(done);
                }
                ParseFlow::Break(ParseBreak::Err(err, recovery)) => {
                    errs.push(err);
                    match recovery {
                        ErrFlow::Continue(u) => u,
                        ErrFlow::Break(Recovery::DiscardTo(_)) => {
                            todo!("swap existing parser with recovery node")
                        }
                        ErrFlow::Break(Recovery::Fail) => {
                            // NOTE: discard rest of input
                            parser = ParseNode::fail();
                            self.parsers.clear();
                        }
                    }
                }
                ParseFlow::Break(ParseBreak::New(new)) => {
                    self.parsers.push(parser);
                    parser = new.into_node(Rc::clone(&txt));
                }
                ParseFlow::Continue(_) => (),
            }
        }
        if !errs.is_empty() {
            self.errs.push(ParseErrorLine(errs, Rc::clone(&txt)));
        }
        parser
    }

    fn clear(&mut self) {
        // TODO: should i ever shrink the allocations?
        self.parsers.clear();
        self.errs.clear();
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
                ParserOutput::Complete(parser.into_expr())
            } else {
                self.parsers.push(parser);
                ParserOutput::Continuation
            })
        } else {
            // TODO: do parsers need to be cleared here
            Err(ParserError(mem::take(&mut self.errs)))
        }
    }

    fn unsupported_continuation(&mut self) -> Option<ParserError> {
        let parser = self.parsers.pop();
        self.clear();
        let err = parser?.into_continuation_unsupported()?;
        todo!("parsers need enough information to tie TextLine to ExpressionError");
    }
}

#[derive(Debug)]
struct ParseErrorLine(Vec<ExpressionError>, Rc<TextLine>);

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

#[cfg(test)]
mod tests {
    use super::{expr::ExpressionErrorKind, *};
    use crate::{
        lex::{Token, TokenKind},
        literal::Literal,
        testutil::{err_or_fail, extract_or_fail, make_textline_no, ok_or_fail, some_or_fail},
        txt::LineNumber,
    };
    use std::ops::Range;

    fn make_tokenline(kinds: impl IntoIterator<Item = TokenKind>) -> TokenLine {
        make_tokenline_no(kinds, 1)
    }

    fn make_tokenline_no(
        kinds: impl IntoIterator<Item = TokenKind>,
        lineno: LineNumber,
    ) -> TokenLine {
        let tokens = kinds.into_iter().enumerate().map(|(i, kind)| Token {
            kind,
            span: i..i + 1,
        });
        TokenLine(tokens.collect(), make_textline_no(lineno))
    }

    #[test]
    fn no_tokens() {
        let mut et: ExpressionTree = Default::default();
        let tokens = Vec::new();

        let r = et.parse(tokens);

        assert!(matches!(
            r,
            Ok(ParserOutput::Complete(Expression::Begin(seq)))
            if seq.is_empty()
        ));
        assert!(et.parsers.is_empty());
    }

    #[test]
    fn single_literal_sequence() {
        let mut et: ExpressionTree = Default::default();
        let tokens = vec![make_tokenline(vec![TokenKind::Literal(Literal::Boolean(
            true,
        ))])];

        let r = et.parse(tokens);

        let seq = extract_or_fail!(
            extract_or_fail!(ok_or_fail!(r), ParserOutput::Complete),
            Expression::Begin
        );
        assert_eq!(seq.len(), 1);
        assert!(matches!(
            seq[0],
            Expression::Literal(Literal::Boolean(true))
        ));
        assert!(et.parsers.is_empty());
    }

    #[test]
    fn multiple_literals_sequence() {
        let mut et: ExpressionTree = Default::default();
        let tokens = vec![make_tokenline(vec![
            TokenKind::Literal(Literal::Boolean(true)),
            TokenKind::Literal(Literal::Character('a')),
            TokenKind::Literal(Literal::String("foo".into())),
        ])];

        let r = et.parse(tokens);

        let seq = extract_or_fail!(
            extract_or_fail!(ok_or_fail!(r), ParserOutput::Complete),
            Expression::Begin
        );
        assert_eq!(seq.len(), 3);
        assert!(matches!(
            seq[0],
            Expression::Literal(Literal::Boolean(true))
        ));
        assert!(matches!(
            seq[1],
            Expression::Literal(Literal::Character('a'))
        ));
        assert!(matches!(
            &seq[2],
            Expression::Literal(Literal::String(s)) if &**s == "foo"
        ));
        assert!(et.parsers.is_empty());
    }

    #[test]
    fn sequence_multiple_lines() {
        let mut et: ExpressionTree = Default::default();
        let tokens = vec![
            make_tokenline_no(
                vec![
                    TokenKind::Literal(Literal::Boolean(true)),
                    TokenKind::Literal(Literal::Character('a')),
                    TokenKind::Literal(Literal::String("foo".into())),
                ],
                1,
            ),
            make_tokenline_no(
                vec![
                    TokenKind::Literal(Literal::Boolean(false)),
                    TokenKind::Literal(Literal::Character('b')),
                ],
                2,
            ),
        ];

        let r = et.parse(tokens);

        let seq = extract_or_fail!(
            extract_or_fail!(ok_or_fail!(r), ParserOutput::Complete),
            Expression::Begin
        );
        assert_eq!(seq.len(), 5);
        assert!(matches!(
            seq[0],
            Expression::Literal(Literal::Boolean(true))
        ));
        assert!(matches!(
            seq[1],
            Expression::Literal(Literal::Character('a'))
        ));
        assert!(matches!(
            &seq[2],
            Expression::Literal(Literal::String(s)) if &**s == "foo"
        ));
        assert!(matches!(
            seq[3],
            Expression::Literal(Literal::Boolean(false))
        ));
        assert!(matches!(
            seq[4],
            Expression::Literal(Literal::Character('b'))
        ));

        assert!(et.parsers.is_empty());
    }

    #[test]
    fn sequence_line_with_errors() {
        let mut et: ExpressionTree = Default::default();
        let tokens = vec![make_tokenline(vec![
            TokenKind::Literal(Literal::Boolean(true)),
            TokenKind::Identifier("foo".to_owned()),
            TokenKind::Literal(Literal::Character('a')),
            TokenKind::Identifier("bar".to_owned()),
            TokenKind::Literal(Literal::String("foo".into())),
        ])];

        let r = et.parse(tokens);

        let err_lines = err_or_fail!(r).0;
        assert_eq!(err_lines.len(), 1);
        let err_line = &err_lines[0];
        let ParseErrorLine(errs, line) = &err_line;
        assert_eq!(line.lineno, 1);
        assert_eq!(errs.len(), 2);
        assert!(matches!(
            &errs[0],
            ExpressionError {
                kind: ExpressionErrorKind::Unimplemented(TokenKind::Identifier(s)),
                span: Range { start: 1, end: 2 },
            } if s == "foo"
        ));
        assert!(matches!(
            &errs[1],
            ExpressionError {
                kind: ExpressionErrorKind::Unimplemented(TokenKind::Identifier(s)),
                span: Range { start: 3, end: 4 },
            } if s == "bar"
        ));
        assert!(et.parsers.is_empty());
        assert!(et.errs.is_empty());
    }

    #[test]
    fn multiple_sequence_lines_with_errors() {
        let mut et: ExpressionTree = Default::default();
        let tokens = vec![
            make_tokenline_no(
                vec![
                    TokenKind::Literal(Literal::Boolean(true)),
                    TokenKind::Identifier("foo".to_owned()),
                    TokenKind::Literal(Literal::Character('a')),
                    TokenKind::Identifier("bar".to_owned()),
                    TokenKind::Literal(Literal::String("foo".into())),
                ],
                1,
            ),
            make_tokenline_no(
                vec![
                    TokenKind::Identifier("baz".to_owned()),
                    TokenKind::Literal(Literal::Boolean(false)),
                    TokenKind::Literal(Literal::Character('b')),
                ],
                2,
            ),
        ];

        let r = et.parse(tokens);

        let err_lines = err_or_fail!(r).0;
        assert_eq!(err_lines.len(), 2);

        let err_line = &err_lines[0];
        let ParseErrorLine(errs, line) = &err_line;
        assert_eq!(line.lineno, 1);
        assert_eq!(errs.len(), 2);
        assert!(matches!(
            &errs[0],
            ExpressionError {
                kind: ExpressionErrorKind::Unimplemented(TokenKind::Identifier(s)),
                span: Range { start: 1, end: 2 },
            } if s == "foo"
        ));
        assert!(matches!(
            &errs[1],
            ExpressionError {
                kind: ExpressionErrorKind::Unimplemented(TokenKind::Identifier(s)),
                span: Range { start: 3, end: 4 },
            } if s == "bar"
        ));

        let err_line = &err_lines[1];
        let ParseErrorLine(errs, line) = &err_line;
        assert_eq!(line.lineno, 2);
        assert_eq!(errs.len(), 1);
        assert!(matches!(
            &errs[0],
            ExpressionError {
                kind: ExpressionErrorKind::Unimplemented(TokenKind::Identifier(s)),
                span: Range { start: 0, end: 1 },
            } if s == "baz"
        ));
        assert!(et.parsers.is_empty());
        assert!(et.errs.is_empty());
    }

    #[test]
    fn parse_fail_skips_rest_of_tokens() {
        let mut et: ExpressionTree = Default::default();
        let tokens = vec![
            make_tokenline_no(
                vec![
                    TokenKind::Literal(Literal::Boolean(true)),
                    TokenKind::Identifier("foo".to_owned()),
                    TokenKind::Literal(Literal::Character('a')),
                    TokenKind::Identifier("bar".to_owned()),
                    TokenKind::Literal(Literal::String("foo".into())),
                ],
                1,
            ),
            make_tokenline_no(
                vec![
                    TokenKind::Literal(Literal::Character('c')),
                    TokenKind::IdentifierDiscard,
                    TokenKind::Literal(Literal::Character('d')),
                    TokenKind::Identifier("beef".to_owned()),
                ],
                2,
            ),
            make_tokenline_no(
                vec![
                    TokenKind::Identifier("baz".to_owned()),
                    TokenKind::Literal(Literal::Boolean(false)),
                    TokenKind::Literal(Literal::Character('b')),
                ],
                3,
            ),
        ];

        let r = et.parse(tokens);

        let err_lines = err_or_fail!(r).0;
        assert_eq!(err_lines.len(), 2);

        let err_line = &err_lines[0];
        let ParseErrorLine(errs, line) = &err_line;
        assert_eq!(line.lineno, 1);
        assert_eq!(errs.len(), 2);
        assert!(matches!(
            &errs[0],
            ExpressionError {
                kind: ExpressionErrorKind::Unimplemented(TokenKind::Identifier(s)),
                span: Range { start: 1, end: 2 },
            } if s == "foo"
        ));
        assert!(matches!(
            &errs[1],
            ExpressionError {
                kind: ExpressionErrorKind::Unimplemented(TokenKind::Identifier(s)),
                span: Range { start: 3, end: 4 },
            } if s == "bar"
        ));

        let err_line = &err_lines[1];
        let ParseErrorLine(errs, line) = &err_line;
        assert_eq!(line.lineno, 2);
        assert_eq!(errs.len(), 1);
        assert!(matches!(
            &errs[0],
            ExpressionError {
                kind: ExpressionErrorKind::InvalidLex(TokenKind::IdentifierDiscard),
                span: Range { start: 1, end: 2 },
            }
        ));
        assert!(et.parsers.is_empty());
        assert!(et.errs.is_empty());
    }

    #[test]
    fn no_continuation() {
        let mut et: ExpressionTree = Default::default();

        let o = et.unsupported_continuation();

        assert!(o.is_none());
    }

    #[test]
    fn continuation_to_error() {
        let mut et: ExpressionTree = Default::default();
        let tokens = vec![make_tokenline(vec![
            TokenKind::Literal(Literal::Boolean(true)),
            TokenKind::StringBegin {
                s: "foo".to_owned(),
                line_cont: false,
            },
        ])];

        let r = et.parse(tokens);

        assert!(matches!(r, Ok(ParserOutput::Continuation)));
        assert_eq!(et.parsers.len(), 2);
        assert!(et.errs.is_empty());

        let o = et.unsupported_continuation();

        let err = some_or_fail!(o);
        todo!("what kind of error?");
    }

    #[test]
    fn continuation_tied_to_expression_first_line() {
        let mut et: ExpressionTree = Default::default();
        let tokens = vec![
            make_tokenline_no(
                vec![TokenKind::StringBegin {
                    s: "foo".to_owned(),
                    line_cont: false,
                }],
                1,
            ),
            make_tokenline_no(
                vec![TokenKind::StringFragment {
                    s: "bar".to_owned(),
                    line_cont: false,
                }],
                2,
            ),
        ];

        let r = et.parse(tokens);

        assert!(matches!(r, Ok(ParserOutput::Continuation)));
        assert_eq!(et.parsers.len(), 2);
        assert!(et.errs.is_empty());

        let o = et.unsupported_continuation();

        let lines = some_or_fail!(o).0;
        assert_eq!(lines.len(), 1);
        let ParseErrorLine(err, line) = &lines[0];

        todo!("what kind of error?");
    }
}
