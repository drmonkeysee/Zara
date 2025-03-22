use super::expr::{Expression, ExpressionError, ExpressionErrorKind};
use crate::{
    lex::{Token, TokenKind},
    literal::Literal,
    number::Number,
};
use std::{mem, ops::ControlFlow};

pub(super) struct ParseNode {
    errs: Vec<ExpressionError>,
    pub(super) exprs: Vec<Expression>,
    kind: NodeKind,
}

impl ParseNode {
    pub(super) fn seq() -> Self {
        Self {
            errs: Vec::new(),
            exprs: Vec::new(),
            kind: NodeKind::Sequence,
        }
    }

    pub(super) fn has_errors(&self) -> bool {
        !self.errs.is_empty()
    }

    pub(super) fn parse(&mut self, token: Token) -> ControlFlow<ParseBreak> {
        match token.kind {
            TokenKind::Imaginary(r) => self
                .exprs
                .push(Expression::Literal(Literal::Number(Number::imaginary(r)))),
            TokenKind::Literal(val) => self.exprs.push(Expression::Literal(val)),
            TokenKind::IdentifierDiscard
            | TokenKind::IdentifierEnd(_)
            | TokenKind::IdentifierFragment(_)
            | TokenKind::StringDiscard
            | TokenKind::StringEnd(_)
            | TokenKind::StringFragment { .. } => self.errs.push(ExpressionError {
                kind: ExpressionErrorKind::InvalidLex(token.kind),
                span: token.span,
            }),
            _ => self.errs.push(ExpressionError {
                kind: ExpressionErrorKind::Unimplemented(token.kind),
                span: token.span,
            }),
        }
        ControlFlow::Continue(())
    }

    pub(super) fn merge(&mut self, mut other: ParseNode) {
        self.errs.append(&mut other.errs);
        self.exprs.push(other.to_expr());
    }

    pub(super) fn take_errors(&mut self) -> Vec<ExpressionError> {
        mem::take(&mut self.errs)
    }

    pub(super) fn to_expr(self) -> Expression {
        match self.kind {
            NodeKind::Sequence => Expression::Begin(self.exprs),
        }
    }
}

enum NodeKind {
    Sequence,
}

pub(super) enum ParseBreak {
    Complete,
    Continuation,
    New(ParseNode),
}
