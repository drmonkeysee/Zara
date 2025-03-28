use super::expr::{Expression, ExpressionError, ExpressionErrorKind};
use crate::{
    lex::{Token, TokenKind},
    literal::Literal,
    number::Number,
};
use std::{
    mem,
    ops::{ControlFlow, Range},
};

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
            NodeKind::StringLiteral(ctx) => {
                let flow = ctx.parse(token);
                self.handle_ctx(flow)
            }
            _ => self.parse_general(token),
        }
    }

    pub(super) fn merge(&mut self, other: ParseNode) {
        self.exprs.push(other.into_expr());
    }

    pub(super) fn into_expr(mut self) -> Expression {
        match self.kind {
            NodeKind::Program => Expression::Begin(self.exprs),
            NodeKind::StringLiteral(_) => self.exprs.pop().unwrap_or(Expression::Empty),
        }
    }

    pub(super) fn into_continuation_unsupported(self) -> Option<ExpressionError> {
        match self.kind {
            NodeKind::Program => None,
            NodeKind::StringLiteral(_) => todo!("need text line information"),
        }
    }

    fn parse_general(&mut self, token: Token) -> ParseFlow {
        match token.kind {
            TokenKind::Imaginary(r) => self
                .exprs
                .push(Expression::Literal(Literal::Number(Number::imaginary(r)))),
            TokenKind::Literal(val) => self.exprs.push(Expression::Literal(val)),
            TokenKind::StringBegin { s, line_cont } => {
                return ParseFlow::Break(ParseBreak::New(Self::new(NodeKind::StringLiteral(
                    StringContext::new(s, !line_cont, token.span),
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

    fn handle_ctx(&mut self, flow: ContextFlow) -> ParseFlow {
        match flow {
            ContextFlow::Break(Ok(expr)) => {
                self.exprs.push(expr);
                ParseFlow::Break(ParseBreak::Complete)
            }
            ContextFlow::Break(Err(err)) => ParseFlow::Break(ParseBreak::Err(err)),
            ContextFlow::Continue(u) => ParseFlow::Continue(u),
        }
    }
}

pub(super) enum ParseBreak {
    Complete,
    Err(ExpressionError),
    New(ParseNode),
}

enum NodeKind {
    Program,
    StringLiteral(StringContext),
}

type ContextResult = Result<Expression, ExpressionError>;
type ContextFlow = ControlFlow<ContextResult>;

struct StringContext {
    s: String,
    span: Range<usize>,
}

impl StringContext {
    fn new(s: String, newline: bool, span: Range<usize>) -> Self {
        let mut me = StringContext { s, span };
        if newline {
            me.s.push('\n')
        }
        me
    }

    fn parse(&mut self, token: Token) -> ContextFlow {
        match token.kind {
            TokenKind::StringFragment { s, line_cont } => {
                self.s.push_str(&s);
                if !line_cont {
                    self.s.push('\n');
                }
                ContextFlow::Continue(())
            }
            TokenKind::StringEnd(s) => {
                self.s.push_str(&s);
                let s = mem::take(&mut self.s);
                ContextFlow::Break(Ok(Expression::Literal(Literal::String(s.into()))))
            }
            _ => ContextFlow::Break(Err(ExpressionError {
                kind: ExpressionErrorKind::InvalidLex(token.kind),
                span: token.span,
            })),
        }
    }
}
