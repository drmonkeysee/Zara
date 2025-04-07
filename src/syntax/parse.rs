use super::{
    ParseErrorLine,
    expr::{Expression, ExpressionError, ExpressionErrorKind},
};
use crate::{
    lex::{Token, TokenKind},
    literal::Literal,
    number::Number,
    txt::TextLine,
};
use std::{ops::ControlFlow, rc::Rc};

pub(super) type ParseFlow = ControlFlow<ParseBreak>;

pub(super) struct ParseNode {
    ctx: Option<ParseCtx>,
    kind: NodeKind,
}

impl ParseNode {
    pub(super) fn prg() -> Self {
        Self {
            ctx: None,
            kind: NodeKind::Program(Vec::new()),
        }
    }

    pub(super) fn fail() -> Self {
        Self {
            ctx: None,
            kind: NodeKind::Failed,
        }
    }

    fn new(kind: NodeKind, start: usize, txt: impl Into<Rc<TextLine>>) -> Self {
        Self {
            kind,
            ctx: Some(ParseCtx {
                txt: txt.into(),
                start,
            }),
        }
    }

    // TODO: temporary for debug assert
    pub(super) fn is_prg(&self) -> bool {
        matches!(self.kind, NodeKind::Program(_))
    }

    pub(super) fn parse(&mut self, token: Token) -> ParseFlow {
        match &mut self.kind {
            NodeKind::CommentBlock => parse_comment_block(token),
            NodeKind::Failed => ParseFlow::Continue(()),
            NodeKind::Program(seq) => parse_sequence(seq, token),
            NodeKind::StringLiteral(buf) => parse_str(buf, token),
        }
    }

    pub(super) fn merge(&mut self, other: ParseNode) {
        match &mut self.kind {
            NodeKind::Program(exprs) => exprs.push(other.into_expr()),
            _ => todo!("fail here somehow"),
        }
    }

    pub(super) fn into_expr(self) -> Expression {
        match self.kind {
            NodeKind::Program(exprs) => Expression::Begin(exprs),
            NodeKind::StringLiteral(s) => Expression::literal(Literal::String(s.into())),
            _ => Expression::empty(),
        }
    }

    pub(super) fn into_continuation_unsupported(self) -> Option<ParseErrorLine> {
        let err_kind = match self.kind {
            NodeKind::CommentBlock => Some(ExpressionErrorKind::CommentBlockUnterminated),
            NodeKind::StringLiteral(_) => Some(ExpressionErrorKind::StrUnterminated),
            _ => None,
        }?;
        debug_assert!(self.ctx.is_some());
        Some(self.ctx.unwrap().into_errorline(err_kind))
    }
}

#[derive(Debug)]
pub(super) enum ParseBreak {
    Complete,
    Err(ExpressionError, ErrFlow),
    New(ParseNew),
}

#[derive(Debug)]
pub(super) struct ParseNew {
    kind: NodeKind,
    start: usize,
}

impl ParseNew {
    pub(super) fn into_node(self, txt: impl Into<Rc<TextLine>>) -> ParseNode {
        ParseNode::new(self.kind, self.start, txt)
    }
}

pub(super) type ErrFlow = ControlFlow<Recovery>;

#[derive(Debug)]
pub(super) enum Recovery {
    DiscardTo(TokenKind),
    Fail,
}

#[derive(Debug)]
enum NodeKind {
    CommentBlock,
    Failed,
    Program(Vec<Expression>),
    StringLiteral(String),
}

impl NodeKind {
    fn string(mut s: String, newline: bool) -> Self {
        if newline {
            s.push('\n');
        }
        Self::StringLiteral(s)
    }
}

struct ParseCtx {
    txt: Rc<TextLine>,
    start: usize,
}

impl ParseCtx {
    fn into_errorline(self, kind: ExpressionErrorKind) -> ParseErrorLine {
        ParseErrorLine(
            vec![ExpressionError {
                kind,
                span: self.start..self.txt.line.len(),
            }],
            self.txt,
        )
    }
}

fn parse_comment_block(token: Token) -> ParseFlow {
    match token.kind {
        TokenKind::CommentBlockFragment { .. } => ParseFlow::Continue(()),
        TokenKind::CommentBlockEnd => ParseFlow::Break(ParseBreak::Complete),
        _ => ParseFlow::Break(ParseBreak::Err(
            ExpressionError {
                kind: ExpressionErrorKind::CommentBlockInvalid(token.kind),
                span: token.span,
            },
            ErrFlow::Break(Recovery::Fail),
        )),
    }
}

fn parse_sequence(seq: &mut Vec<Expression>, token: Token) -> ParseFlow {
    match token.kind {
        TokenKind::Comment => (),
        TokenKind::CommentBlockBegin { .. } => {
            return ParseFlow::Break(ParseBreak::New(ParseNew {
                kind: NodeKind::CommentBlock,
                start: token.span.start,
            }));
        }
        TokenKind::Imaginary(r) => {
            seq.push(Expression::literal(Literal::Number(Number::imaginary(r))))
        }
        TokenKind::Literal(val) => seq.push(Expression::literal(val)),
        TokenKind::StringBegin { s, line_cont } => {
            return ParseFlow::Break(ParseBreak::New(ParseNew {
                kind: NodeKind::string(s, !line_cont),
                start: token.span.start,
            }));
        }
        // TODO: this should reduce to _ => once Unimplemented is removed
        TokenKind::CommentBlockFragment { .. }
        | TokenKind::CommentBlockEnd
        | TokenKind::IdentifierDiscard
        | TokenKind::IdentifierEnd(_)
        | TokenKind::IdentifierFragment(_)
        | TokenKind::StringDiscard
        | TokenKind::StringEnd(_)
        | TokenKind::StringFragment { .. } => {
            return ParseFlow::Break(ParseBreak::Err(
                ExpressionError {
                    kind: ExpressionErrorKind::SeqInvalid(token.kind),
                    span: token.span,
                },
                ErrFlow::Break(Recovery::Fail),
            ));
        }
        _ => {
            return ParseFlow::Break(ParseBreak::Err(
                ExpressionError {
                    kind: ExpressionErrorKind::Unimplemented(token.kind),
                    span: token.span,
                },
                ErrFlow::Continue(()),
            ));
        }
    }
    ParseFlow::Continue(())
}

fn parse_str(buf: &mut String, token: Token) -> ParseFlow {
    match token.kind {
        TokenKind::StringFragment { s, line_cont } => {
            buf.push_str(&s);
            if !line_cont {
                buf.push('\n');
            }
            ParseFlow::Continue(())
        }
        TokenKind::StringEnd(s) => {
            buf.push_str(&s);
            ParseFlow::Break(ParseBreak::Complete)
        }
        _ => ParseFlow::Break(ParseBreak::Err(
            ExpressionError {
                kind: ExpressionErrorKind::StrInvalid(token.kind),
                span: token.span,
            },
            ErrFlow::Break(Recovery::Fail),
        )),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::ops::Range;

    mod continuation {
        use super::*;
        use crate::testutil::{make_textline, some_or_fail};

        #[test]
        fn prg_no_continuation() {
            let p = ParseNode::prg();

            let o = p.into_continuation_unsupported();

            assert!(o.is_none());
        }

        #[test]
        fn str_continuation() {
            let p = ParseNode::new(
                NodeKind::StringLiteral("foo".to_owned()),
                3,
                make_textline(),
            );

            let o = p.into_continuation_unsupported();

            let err_line = some_or_fail!(o);
            let ParseErrorLine(errs, line) = &err_line;
            assert_eq!(line.lineno, 1);
            assert_eq!(errs.len(), 1);
            assert!(matches!(
                &errs[0],
                ExpressionError {
                    kind: ExpressionErrorKind::StrUnterminated,
                    span: Range { start: 3, end: 19 },
                }
            ));
        }

        #[test]
        fn comment_block_continuation() {
            let p = ParseNode::new(NodeKind::CommentBlock, 3, make_textline());

            let o = p.into_continuation_unsupported();

            let err_line = some_or_fail!(o);
            let ParseErrorLine(errs, line) = &err_line;
            assert_eq!(line.lineno, 1);
            assert_eq!(errs.len(), 1);
            assert!(matches!(
                &errs[0],
                ExpressionError {
                    kind: ExpressionErrorKind::CommentBlockUnterminated,
                    span: Range { start: 3, end: 19 },
                }
            ));
        }
    }

    mod sequence {
        use super::*;
        use crate::{number::Real, testutil::extract_or_fail, value::Value};

        #[test]
        fn literal() {
            let mut seq = Vec::new();
            let token = Token {
                kind: TokenKind::Literal(Literal::Boolean(true)),
                span: 0..3,
            };

            let f = parse_sequence(&mut seq, token);

            assert!(matches!(f, ParseFlow::Continue(())));
            assert_eq!(seq.len(), 1);
            assert!(matches!(
                seq[0],
                Expression::Constant(Value::Literal(Literal::Boolean(true)))
            ));
        }

        #[test]
        fn imaginary() {
            let mut seq = Vec::new();
            let token = Token {
                kind: TokenKind::Imaginary(Real::Float(1.2)),
                span: 0..3,
            };

            let f = parse_sequence(&mut seq, token);

            assert!(matches!(f, ParseFlow::Continue(())));
            assert_eq!(seq.len(), 1);
            let n = extract_or_fail!(
                extract_or_fail!(
                    extract_or_fail!(&seq[0], Expression::Constant),
                    Value::Literal
                ),
                Literal::Number
            );
            assert!(matches!(n, Number::Complex(_) if n.as_datum().to_string() == "+1.2i"));
        }

        #[test]
        fn start_string() {
            let mut seq = Vec::new();
            let token = Token {
                kind: TokenKind::StringBegin {
                    s: "start".to_owned(),
                    line_cont: false,
                },
                span: 3..8,
            };

            let f = parse_sequence(&mut seq, token);

            assert!(matches!(
                f,
                ParseFlow::Break(ParseBreak::New(
                    ParseNew {
                        kind: NodeKind::StringLiteral(s),
                        start: 3
                    }
                )) if s == "start\n"
            ));
            assert!(seq.is_empty());
        }

        #[test]
        fn start_string_line_cont() {
            let mut seq = Vec::new();
            let token = Token {
                kind: TokenKind::StringBegin {
                    s: "start".to_owned(),
                    line_cont: true,
                },
                span: 3..8,
            };

            let f = parse_sequence(&mut seq, token);

            assert!(matches!(
                f,
                ParseFlow::Break(ParseBreak::New(
                    ParseNew {
                        kind: NodeKind::StringLiteral(s),
                        start: 3
                    }
                )) if s == "start"
            ));
            assert!(seq.is_empty());
        }

        #[test]
        fn invalid() {
            let mut seq = Vec::new();
            let token = Token {
                kind: TokenKind::StringEnd("foo".to_owned()),
                span: 0..3,
            };

            let f = parse_sequence(&mut seq, token);

            assert!(matches!(
                f,
                ParseFlow::Break(ParseBreak::Err(
                    ExpressionError {
                        kind: ExpressionErrorKind::SeqInvalid(TokenKind::StringEnd(_)),
                        span: Range { start: 0, end: 3 },
                    },
                    ErrFlow::Break(Recovery::Fail),
                ))
            ));
            assert!(seq.is_empty());
        }

        #[test]
        fn comment() {
            let mut seq = Vec::new();
            let token = Token {
                kind: TokenKind::Comment,
                span: 0..3,
            };

            let f = parse_sequence(&mut seq, token);

            assert!(matches!(f, ParseFlow::Continue(())));
            assert!(seq.is_empty());
        }

        #[test]
        fn comment_begin() {
            let mut seq = Vec::new();
            let token = Token {
                kind: TokenKind::CommentBlockBegin { depth: 0 },
                span: 3..6,
            };

            let f = parse_sequence(&mut seq, token);

            assert!(matches!(
                f,
                ParseFlow::Break(ParseBreak::New(ParseNew {
                    kind: NodeKind::CommentBlock,
                    start: 3
                }))
            ));
            assert!(seq.is_empty());
        }
    }

    mod string {
        use super::*;

        #[test]
        fn end() {
            let mut s = "start\n".to_owned();
            let token = Token {
                kind: TokenKind::StringEnd("end".to_owned()),
                span: 0..4,
            };

            let f = parse_str(&mut s, token);

            assert!(matches!(f, ParseFlow::Break(ParseBreak::Complete)));
            assert_eq!(s, "start\nend");
        }

        #[test]
        fn fragment() {
            let mut s = "start\n".to_owned();
            let token = Token {
                kind: TokenKind::StringFragment {
                    s: "middle".to_owned(),
                    line_cont: false,
                },
                span: 0..6,
            };

            let f = parse_str(&mut s, token);

            assert!(matches!(f, ParseFlow::Continue(())));
            assert_eq!(s, "start\nmiddle\n");
        }

        #[test]
        fn fragment_line_cont() {
            let mut s = "start\n".to_owned();
            let token = Token {
                kind: TokenKind::StringFragment {
                    s: "middle".to_owned(),
                    line_cont: true,
                },
                span: 0..6,
            };

            let f = parse_str(&mut s, token);

            assert!(matches!(f, ParseFlow::Continue(())));
            assert_eq!(s, "start\nmiddle");
        }

        #[test]
        fn invalid() {
            let mut s = "start\n".to_owned();
            let token = Token {
                kind: TokenKind::ParenLeft,
                span: 4..5,
            };

            let f = parse_str(&mut s, token);

            assert!(matches!(
                f,
                ParseFlow::Break(ParseBreak::Err(
                    ExpressionError {
                        kind: ExpressionErrorKind::StrInvalid(TokenKind::ParenLeft),
                        span: Range { start: 4, end: 5 },
                    },
                    ErrFlow::Break(Recovery::Fail),
                ))
            ));
            assert_eq!(s, "start\n");
        }
    }

    mod comment {
        use super::*;

        #[test]
        fn end() {
            let token = Token {
                kind: TokenKind::CommentBlockEnd,
                span: 0..4,
            };

            let f = parse_comment_block(token);

            assert!(matches!(f, ParseFlow::Break(ParseBreak::Complete)));
        }

        #[test]
        fn fragment() {
            let token = Token {
                kind: TokenKind::CommentBlockFragment { depth: 0 },
                span: 0..4,
            };

            let f = parse_comment_block(token);

            assert!(matches!(f, ParseFlow::Continue(())));
        }

        #[test]
        fn unexpected_new_block() {
            let token = Token {
                kind: TokenKind::CommentBlockBegin { depth: 1 },
                span: 0..4,
            };

            let f = parse_comment_block(token);

            assert!(matches!(
                f,
                ParseFlow::Break(ParseBreak::Err(
                    ExpressionError {
                        kind: ExpressionErrorKind::CommentBlockInvalid(
                            TokenKind::CommentBlockBegin { .. }
                        ),
                        span: Range { start: 0, end: 4 },
                    },
                    ErrFlow::Break(Recovery::Fail),
                ))
            ));
        }

        #[test]
        fn invalid() {
            let token = Token {
                kind: TokenKind::ParenLeft,
                span: 0..1,
            };

            let f = parse_comment_block(token);

            assert!(matches!(
                f,
                ParseFlow::Break(ParseBreak::Err(
                    ExpressionError {
                        kind: ExpressionErrorKind::CommentBlockInvalid(TokenKind::ParenLeft),
                        span: Range { start: 0, end: 1 },
                    },
                    ErrFlow::Break(Recovery::Fail),
                ))
            ));
        }
    }
}
