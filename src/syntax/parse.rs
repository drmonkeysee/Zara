use super::expr::{Expression, ExpressionError, ExpressionErrorKind};
use crate::{
    lex::{Token, TokenKind},
    literal::Literal,
    number::Number,
    txt::TextLine,
};
use std::ops::{ControlFlow, Range};

pub(super) type ParseFlow = ControlFlow<ParseBreak>;

pub(super) struct ParseNode {
    kind: NodeKind,
    span: Range<usize>,
    txt: Option<TextLine>,
}

impl ParseNode {
    pub(super) fn prg() -> Self {
        Self {
            kind: NodeKind::Program(Vec::new()),
            span: 0..0,
            txt: None,
        }
    }

    // TODO: temporary for debug assert
    pub(super) fn is_prg(&self) -> bool {
        matches!(self.kind, NodeKind::Program(_))
    }

    pub(super) fn parse(&mut self, token: Token) -> ParseFlow {
        self.span.end = token.span.end;
        match &mut self.kind {
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
            NodeKind::StringLiteral(s) => Expression::Literal(Literal::String(s.into())),
        }
    }

    pub(super) fn into_continuation_unsupported(self) -> Option<ExpressionError> {
        match self.kind {
            NodeKind::Program(_) => None,
            NodeKind::StringLiteral(_) => todo!("need text line information"),
        }
    }
}

pub(super) enum ParseBreak {
    Complete,
    Err(ExpressionError),
    New(ParseNode),
}

// TODO: better name, something like builder but not builder
enum NodeKind {
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

fn parse_sequence(seq: &mut Vec<Expression>, token: Token) -> ParseFlow {
    match token.kind {
        TokenKind::Imaginary(r) => {
            seq.push(Expression::Literal(Literal::Number(Number::imaginary(r))))
        }
        TokenKind::Literal(val) => seq.push(Expression::Literal(val)),
        TokenKind::StringBegin { s, line_cont } => {
            return ParseFlow::Break(ParseBreak::New(ParseNode {
                kind: NodeKind::string(s, !line_cont),
                span: token.span.clone(),
                txt: None,
            }));
        }
        TokenKind::IdentifierDiscard
        | TokenKind::IdentifierEnd(_)
        | TokenKind::IdentifierFragment(_)
        | TokenKind::StringDiscard
        | TokenKind::StringEnd(_)
        | TokenKind::StringFragment { .. } => {
            return ParseFlow::Break(ParseBreak::Err(ExpressionError {
                kind: ExpressionErrorKind::InvalidLex(token.kind),
                span: token.span,
            }));
        }
        _ => {
            return ParseFlow::Break(ParseBreak::Err(ExpressionError {
                kind: ExpressionErrorKind::Unimplemented(token.kind),
                span: token.span,
            }));
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
        _ => ParseFlow::Break(ParseBreak::Err(ExpressionError {
            kind: ExpressionErrorKind::InvalidLex(token.kind),
            span: token.span,
        })),
    }
}
