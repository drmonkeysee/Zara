mod expr;
mod parse;

pub(crate) use self::{expr::Program, parse::InvalidParseError};
use self::{
    expr::{ExprCtx, Expression, ExpressionError, ExpressionKind, PeekableExt},
    parse::{ParseBreak, ParseFlow, ParseNode},
};
use crate::{lex::TokenLine, txt::TextLine, value::Value};
use std::{
    error::Error,
    fmt::{self, Display, Formatter, Write},
    mem,
    rc::Rc,
};

pub(crate) type ParserResult = Result<ParserOutput, ParserError>;

#[derive(Debug)]
pub(crate) enum ParserOutput {
    Complete(Program),
    Continuation,
}

#[derive(Debug)]
pub(crate) enum ParserError {
    Invalid(InvalidParseError),
    Syntax(SyntaxError),
}

impl ParserError {
    pub(crate) fn display_message(&self) -> ParserErrorMessage {
        ParserErrorMessage(&self)
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

#[derive(Debug)]
pub(crate) struct SyntaxError(Vec<ExpressionError>);

impl Display for SyntaxError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str("invalid syntax")
    }
}

impl Error for SyntaxError {}

pub(crate) trait Parser {
    fn parse(&mut self, token_lines: impl IntoIterator<Item = TokenLine>) -> ParserResult;
    fn unsupported_continuation(&mut self) -> Option<ParserError>;
}

pub(crate) struct TokenList;

impl TokenList {
    fn token_expr(&self, token_lines: impl IntoIterator<Item = TokenLine>) -> Option<Expression> {
        let list: Box<[TokenLine]> = token_lines.into_iter().collect();
        if let Some(TokenLine(_, line)) = list.first() {
            Some(Expression {
                ctx: ExprCtx {
                    span: 0..line.line.len(),
                    txt: Rc::new(line.clone()),
                },
                kind: ExpressionKind::Literal(Value::TokenList(list)),
            })
        } else {
            None
        }
    }
}

impl Parser for TokenList {
    fn parse(&mut self, token_lines: impl IntoIterator<Item = TokenLine>) -> ParserResult {
        Ok(ParserOutput::Complete(Program::new(
            self.token_expr(token_lines)
                .into_iter()
                .collect::<Box<[_]>>(),
        )))
    }

    fn unsupported_continuation(&mut self) -> Option<ParserError> {
        None
    }
}

#[derive(Default)]
pub(crate) struct ExpressionTree {
    errs: Vec<ExpressionError>,
    parsers: Vec<ParseNode>,
}

pub(crate) struct ParserErrorMessage<'a>(&'a ParserError);

impl Display for ParserErrorMessage<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self.0 {
            ParserError::Invalid(_) => f.write_str("unexpected end-of-parse\n"),
            ParserError::Syntax(SyntaxError(errs)) => {
                for (txt, errs) in errs.iter().peekable().groupby_txt() {
                    SyntaxErrorLineMessage((txt, &errs)).fmt(f)?;
                }
                Ok(())
            }
        }
    }
}

impl ExpressionTree {
    fn parse_line(&mut self, mut parser: ParseNode, line: TokenLine) -> ParseNode {
        let TokenLine(tokens, txt) = line;
        let txt = Rc::new(txt);
        let mut errs = Vec::new();
        for token in tokens {
            match parser.parse(token, &txt) {
                ParseFlow::Break(ParseBreak::Complete) => {
                    let done = parser;
                    // TODO: fix this unwrap
                    debug_assert!(!self.parsers.is_empty());
                    parser = self.parsers.pop().unwrap();
                    if let Err(errv) = parser.merge(done) {
                        errs.extend(errv);
                    }
                }
                ParseFlow::Break(ParseBreak::Err { err, fail }) => {
                    errs.push(err);
                    if fail {
                        // NOTE: discard rest of input
                        parser = ParseNode::Fail;
                        self.parsers.clear();
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
        parser
    }

    fn clear(&mut self) {
        // TODO: should i ever shrink the allocations?
        self.parsers.clear();
        self.errs.clear();
    }
}

impl Parser for ExpressionTree {
    fn parse(&mut self, token_lines: impl IntoIterator<Item = TokenLine>) -> ParserResult {
        let parser = token_lines
            .into_iter()
            .fold(self.parsers.pop().unwrap_or(ParseNode::prg()), |p, ln| {
                self.parse_line(p, ln)
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
            Err(ParserError::Syntax(SyntaxError(mem::take(&mut self.errs))))
        }
    }

    fn unsupported_continuation(&mut self) -> Option<ParserError> {
        let parser = self.parsers.pop();
        self.clear();
        Some(parser?.into_continuation_unsupported()?.into())
    }
}

struct SyntaxErrorLineMessage<'a>((&'a TextLine, &'a [&'a ExpressionError]));

// TODO: unify this with token error message
impl Display for SyntaxErrorLineMessage<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let (txtline, errs) = self.0;
        txtline.display_header().fmt(f)?;

        if errs.is_empty() {
            return Ok(());
        }

        let mut cursor = 0;
        f.write_char('\t')?;
        for span in errs
            .iter()
            .filter_map(|err| (!err.ctx.span.is_empty()).then_some(&err.ctx.span))
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
            writeln!(f, "{}: {err}", err.ctx.span.start + 1)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::{expr::ExpressionErrorKind, *};
    use crate::{
        constant::Constant,
        lex::{Token, TokenKind},
        testutil::{err_or_fail, extract_or_fail, make_textline_no, some_or_fail},
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

    mod parsing {
        use super::*;
        use crate::testutil::ok_or_fail;

        #[test]
        fn no_tokens() {
            let mut et: ExpressionTree = Default::default();
            let tokens = Vec::new();

            let r = et.parse(tokens);

            assert!(matches!(
                r,
                Ok(ParserOutput::Complete(Program(seq)))
                if seq.is_empty()
            ));
            assert!(et.parsers.is_empty());
        }

        #[test]
        fn single_literal_sequence() {
            let mut et: ExpressionTree = Default::default();
            let tokens = [make_tokenline([TokenKind::Constant(Constant::Boolean(
                true,
            ))])];

            let r = et.parse(tokens);

            let Program(seq) = extract_or_fail!(ok_or_fail!(r), ParserOutput::Complete);
            assert_eq!(seq.len(), 1);
            assert!(matches!(
                &seq[0],
                Expression {
                    ctx: ExprCtx { span: Range { start: 0, end: 1 }, txt },
                    kind: ExpressionKind::Literal(Value::Constant(Constant::Boolean(true))),
                } if txt.lineno == 1
            ));
            assert!(et.parsers.is_empty());
        }

        #[test]
        fn multiple_literals_sequence() {
            let mut et: ExpressionTree = Default::default();
            let tokens = [make_tokenline([
                TokenKind::Constant(Constant::Boolean(true)),
                TokenKind::Constant(Constant::Character('a')),
                TokenKind::Constant(Constant::String("foo".into())),
            ])];

            let r = et.parse(tokens);

            let Program(seq) = extract_or_fail!(ok_or_fail!(r), ParserOutput::Complete);
            assert_eq!(seq.len(), 3);
            assert!(matches!(
                &seq[0],
                Expression {
                    ctx: ExprCtx { span: Range { start: 0, end: 1 }, txt },
                    kind: ExpressionKind::Literal(Value::Constant(Constant::Boolean(true))),
                } if txt.lineno == 1
            ));
            assert!(matches!(
                &seq[1],
                Expression {
                    ctx: ExprCtx { span: Range { start: 1, end: 2 }, txt },
                    kind: ExpressionKind::Literal(Value::Constant(Constant::Character('a'))),
                } if txt.lineno == 1
            ));
            assert!(matches!(
                &seq[2],
                Expression {
                    ctx: ExprCtx { span: Range { start: 2, end: 3 }, txt },
                    kind: ExpressionKind::Literal(Value::Constant(Constant::String(s))),
                } if txt.lineno == 1 && &**s == "foo"
            ));
            assert!(et.parsers.is_empty());
        }

        #[test]
        fn sequence_multiple_lines() {
            let mut et: ExpressionTree = Default::default();
            let tokens = [
                make_tokenline_no(
                    [
                        TokenKind::Constant(Constant::Boolean(true)),
                        TokenKind::Constant(Constant::Character('a')),
                        TokenKind::Constant(Constant::String("foo".into())),
                    ],
                    1,
                ),
                make_tokenline_no(
                    [
                        TokenKind::Constant(Constant::Boolean(false)),
                        TokenKind::Constant(Constant::Character('b')),
                    ],
                    2,
                ),
            ];

            let r = et.parse(tokens);

            let Program(seq) = extract_or_fail!(ok_or_fail!(r), ParserOutput::Complete);
            assert_eq!(seq.len(), 5);
            assert!(matches!(
                &seq[0],
                Expression {
                    ctx: ExprCtx { span: Range { start: 0, end: 1 }, txt },
                    kind: ExpressionKind::Literal(Value::Constant(Constant::Boolean(true))),
                } if txt.lineno == 1
            ));
            assert!(matches!(
                &seq[1],
                Expression {
                    ctx: ExprCtx { span: Range { start: 1, end: 2 }, txt },
                    kind: ExpressionKind::Literal(Value::Constant(Constant::Character('a'))),
                } if txt.lineno == 1
            ));
            assert!(matches!(
                &seq[2],
                Expression {
                    ctx: ExprCtx { span: Range { start: 2, end: 3 }, txt },
                    kind: ExpressionKind::Literal(Value::Constant(Constant::String(s))),
                } if txt.lineno == 1 && &**s == "foo"
            ));
            assert!(matches!(
                &seq[3],
                Expression {
                    ctx: ExprCtx { span: Range { start: 0, end: 1 }, txt },
                    kind: ExpressionKind::Literal(Value::Constant(Constant::Boolean(false))),
                } if txt.lineno == 2
            ));
            assert!(matches!(
                &seq[4],
                Expression {
                    ctx: ExprCtx { span: Range { start: 1, end: 2 }, txt },
                    kind: ExpressionKind::Literal(Value::Constant(Constant::Character('b'))),
                } if txt.lineno == 2
            ));

            assert!(et.parsers.is_empty());
        }

        #[test]
        fn sequence_line_with_errors() {
            let mut et: ExpressionTree = Default::default();
            let tokens = [make_tokenline([
                TokenKind::Constant(Constant::Boolean(true)),
                TokenKind::DirectiveCase(true),
                TokenKind::Constant(Constant::Character('a')),
                TokenKind::DirectiveCase(false),
                TokenKind::Constant(Constant::String("foo".into())),
            ])];

            let r = et.parse(tokens);

            let errs = extract_or_fail!(err_or_fail!(r), ParserError::Syntax).0;
            assert_eq!(errs.len(), 2);
            assert!(matches!(
                &errs[0],
                ExpressionError {
                    ctx: ExprCtx { span: Range { start: 1, end: 2 }, txt },
                    kind: ExpressionErrorKind::Unimplemented(TokenKind::DirectiveCase(true)),
                } if txt.lineno == 1
            ));
            assert!(matches!(
                &errs[1],
                ExpressionError {
                    ctx: ExprCtx { span: Range { start: 3, end: 4 }, txt },
                    kind: ExpressionErrorKind::Unimplemented(TokenKind::DirectiveCase(false)),
                }  if txt.lineno == 1
            ));
            assert!(et.parsers.is_empty());
            assert!(et.errs.is_empty());
        }

        #[test]
        fn multiple_sequence_lines_with_errors() {
            let mut et: ExpressionTree = Default::default();
            let tokens = [
                make_tokenline_no(
                    [
                        TokenKind::Constant(Constant::Boolean(true)),
                        TokenKind::DirectiveCase(true),
                        TokenKind::Constant(Constant::Character('a')),
                        TokenKind::DirectiveCase(false),
                        TokenKind::Constant(Constant::String("foo".into())),
                    ],
                    1,
                ),
                make_tokenline_no(
                    [
                        TokenKind::CommentDatum,
                        TokenKind::Constant(Constant::Boolean(false)),
                        TokenKind::Constant(Constant::Character('b')),
                    ],
                    2,
                ),
            ];

            let r = et.parse(tokens);

            let errs = extract_or_fail!(err_or_fail!(r), ParserError::Syntax).0;
            assert_eq!(errs.len(), 3);
            assert!(matches!(
                &errs[0],
                ExpressionError {
                    ctx: ExprCtx { span: Range { start: 1, end: 2 }, txt },
                    kind: ExpressionErrorKind::Unimplemented(TokenKind::DirectiveCase(true)),
                } if txt.lineno == 1
            ));
            assert!(matches!(
                &errs[1],
                ExpressionError {
                    ctx: ExprCtx { span: Range { start: 3, end: 4 }, txt },
                    kind: ExpressionErrorKind::Unimplemented(TokenKind::DirectiveCase(false)),
                }  if txt.lineno == 1
            ));
            assert!(matches!(
                &errs[2],
                ExpressionError {
                    ctx: ExprCtx { span: Range { start: 0, end: 1 }, txt },
                    kind: ExpressionErrorKind::Unimplemented(TokenKind::CommentDatum),
                }  if txt.lineno == 2
            ));
            assert!(et.parsers.is_empty());
            assert!(et.errs.is_empty());
        }

        #[test]
        fn parse_fail_skips_rest_of_tokens() {
            let mut et: ExpressionTree = Default::default();
            let tokens = [
                make_tokenline_no(
                    [
                        TokenKind::Constant(Constant::Boolean(true)),
                        TokenKind::DirectiveCase(true),
                        TokenKind::Constant(Constant::Character('a')),
                        TokenKind::DirectiveCase(false),
                        TokenKind::Constant(Constant::String("foo".into())),
                    ],
                    1,
                ),
                make_tokenline_no(
                    [
                        TokenKind::Constant(Constant::Character('c')),
                        TokenKind::IdentifierDiscard,
                        TokenKind::Constant(Constant::Character('d')),
                        TokenKind::CommentDatum,
                    ],
                    2,
                ),
                make_tokenline_no(
                    [
                        TokenKind::CommentDatum,
                        TokenKind::Constant(Constant::Boolean(false)),
                        TokenKind::Constant(Constant::Character('b')),
                    ],
                    3,
                ),
            ];

            let r = et.parse(tokens);

            let errs = extract_or_fail!(err_or_fail!(r), ParserError::Syntax).0;
            assert_eq!(errs.len(), 3);
            assert!(matches!(
                &errs[0],
                ExpressionError {
                    ctx: ExprCtx { span: Range { start: 1, end: 2 }, txt },
                    kind: ExpressionErrorKind::Unimplemented(TokenKind::DirectiveCase(true)),
                } if txt.lineno == 1
            ));
            assert!(matches!(
                &errs[1],
                ExpressionError {
                    ctx: ExprCtx { span: Range { start: 3, end: 4 }, txt },
                    kind: ExpressionErrorKind::Unimplemented(TokenKind::DirectiveCase(false)),
                }  if txt.lineno == 1
            ));
            assert!(matches!(
                &errs[2],
                ExpressionError {
                    ctx: ExprCtx { span: Range { start: 1, end: 2 }, txt },
                    kind: ExpressionErrorKind::SeqInvalid(TokenKind::IdentifierDiscard),
                }  if txt.lineno == 2
            ));
            assert!(et.parsers.is_empty());
            assert!(et.errs.is_empty());
        }
    }

    mod continuation {
        use super::*;

        #[test]
        fn no_continuation() {
            let mut et: ExpressionTree = Default::default();

            let o = et.unsupported_continuation();

            assert!(o.is_none());
        }

        #[test]
        fn continuation_to_error() {
            let mut et: ExpressionTree = Default::default();
            let tokens = [make_tokenline([
                TokenKind::Constant(Constant::Boolean(true)),
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

            let errs = extract_or_fail!(some_or_fail!(o), ParserError::Syntax).0;
            assert_eq!(errs.len(), 1);
            assert!(matches!(
                &errs[0],
                ExpressionError {
                    ctx: ExprCtx { span: Range { start: 1, end: 19 }, txt },
                    kind: ExpressionErrorKind::StrUnterminated,
                } if txt.lineno == 1
            ));

            assert!(et.parsers.is_empty());
            assert!(et.errs.is_empty());
        }

        #[test]
        fn continuation_tied_to_expression_first_line() {
            let mut et: ExpressionTree = Default::default();
            let tokens = [
                make_tokenline_no(
                    [TokenKind::StringBegin {
                        s: "foo".to_owned(),
                        line_cont: false,
                    }],
                    1,
                ),
                make_tokenline_no(
                    [TokenKind::StringFragment {
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

            let errs = extract_or_fail!(some_or_fail!(o), ParserError::Syntax).0;
            assert_eq!(errs.len(), 1);
            assert!(matches!(
                &errs[0],
                ExpressionError {
                    ctx: ExprCtx { span: Range { start: 0, end: 19 }, txt },
                    kind: ExpressionErrorKind::StrUnterminated,
                } if txt.lineno == 1
            ));

            assert!(et.parsers.is_empty());
            assert!(et.errs.is_empty());
        }

        #[test]
        fn continuation_ignored_if_existing_errors() {
            let mut et: ExpressionTree = Default::default();
            let tokens = [make_tokenline([
                TokenKind::Constant(Constant::Boolean(true)),
                TokenKind::DirectiveCase(true),
                TokenKind::StringBegin {
                    s: "foo".to_owned(),
                    line_cont: false,
                },
            ])];

            let r = et.parse(tokens);

            let errs = extract_or_fail!(err_or_fail!(r), ParserError::Syntax).0;
            assert_eq!(errs.len(), 1);
            assert!(matches!(
                &errs[0],
                ExpressionError {
                    ctx: ExprCtx { span: Range { start: 1, end: 2 }, txt },
                    kind: ExpressionErrorKind::Unimplemented(TokenKind::DirectiveCase(true)),
                } if txt.lineno == 1
            ));
            assert!(et.parsers.is_empty());
            assert!(et.errs.is_empty());
        }
    }

    mod errors {
        use super::*;

        #[test]
        fn display_syntax_error() {
            let err = ParserError::Syntax(SyntaxError(Vec::new()));

            assert_eq!(err.to_string(), "fatal error: invalid syntax");
        }

        #[test]
        fn display_invalid_error() {
            let err = ParserError::Invalid(InvalidParseError);

            assert_eq!(err.to_string(), "fatal error: invalid parser state reached");
        }

        #[test]
        fn syntax_source() {
            let err = ParserError::Syntax(SyntaxError(Vec::new()));

            let inner = some_or_fail!(err.source());

            assert!(matches!(
                inner.downcast_ref::<SyntaxError>().unwrap(),
                SyntaxError(_)
            ));
        }

        #[test]
        fn invalid_source() {
            let err = ParserError::Invalid(InvalidParseError);

            let inner = some_or_fail!(err.source());

            assert!(matches!(
                inner.downcast_ref::<InvalidParseError>().unwrap(),
                InvalidParseError
            ));
        }
    }
}
