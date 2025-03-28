use super::expr::{Expression, ExpressionError, ExpressionErrorKind};
use crate::{
    lex::{Token, TokenKind},
    literal::Literal,
    number::Number,
};
use std::ops::ControlFlow;

pub(super) type ParseFlow = ControlFlow<ParseBreak>;

pub(super) struct ParseNode {
    exprs: Vec<Expression>,
    kind: NodeKind,
}

impl ParseNode {
    pub(super) fn prg() -> Self {
        Self::new(NodeKind::Program)
    }

    fn new(kind: NodeKind) -> Self {
        Self {
            exprs: Vec::new(),
            kind,
        }
    }

    // TODO: temporary for debug assert
    pub(super) fn is_prg(&self) -> bool {
        matches!(self.kind, NodeKind::Program)
    }

    pub(super) fn parse(&mut self, token: Token) -> ParseFlow {
        match &mut self.kind {
            NodeKind::StringLiteral(buf) => parse_str(buf, token),
            _ => self.parse_expr(token),
        }
    }

    pub(super) fn merge(&mut self, other: ParseNode) {
        self.exprs.push(other.into_expr());
    }

    pub(super) fn into_expr(self) -> Expression {
        match self.kind {
            NodeKind::Program => Expression::Begin(self.exprs),
            NodeKind::StringLiteral(s) => Expression::Literal(Literal::String(s.into())),
        }
    }

    pub(super) fn into_continuation_unsupported(self) -> Option<ExpressionError> {
        match self.kind {
            NodeKind::Program => None,
            NodeKind::StringLiteral(_) => todo!("need text line information"),
        }
    }

    fn parse_expr(&mut self, token: Token) -> ParseFlow {
        match token.kind {
            TokenKind::Imaginary(r) => self
                .exprs
                .push(Expression::Literal(Literal::Number(Number::imaginary(r)))),
            TokenKind::Literal(val) => self.exprs.push(Expression::Literal(val)),
            TokenKind::StringBegin { s, line_cont } => {
                return ParseFlow::Break(ParseBreak::New(Self::new(NodeKind::string(
                    s, !line_cont,
                ))));
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
}

pub(super) enum ParseBreak {
    Complete,
    Err(ExpressionError),
    New(ParseNode),
}

enum NodeKind {
    Program,
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
