use super::expr::{Expression, ExpressionError, ExpressionErrorKind};
use crate::{
    lex::{Token, TokenKind},
    literal::Literal,
    number::Number,
};
use std::ops::ControlFlow;

pub(super) type ParseFlow = ControlFlow<ParseBreak>;

pub(super) struct ParseNode {
    kind: NodeKind,
}

impl ParseNode {
    pub(super) fn prg() -> Self {
        Self {
            kind: NodeKind::Program(Vec::new()),
        }
    }

    pub(super) fn fail() -> Self {
        Self {
            kind: NodeKind::Failed,
        }
    }

    // TODO: temporary for debug assert
    pub(super) fn is_prg(&self) -> bool {
        matches!(self.kind, NodeKind::Program(_))
    }

    pub(super) fn parse(&mut self, token: Token) -> ParseFlow {
        match &mut self.kind {
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
            NodeKind::StringLiteral(s) => Expression::Literal(Literal::String(s.into())),
            _ => Expression::Empty,
        }
    }

    pub(super) fn into_continuation_unsupported(self) -> Option<ExpressionError> {
        match self.kind {
            NodeKind::StringLiteral(_) => todo!("need text line information"),
            _ => None,
        }
    }
}

pub(super) enum ParseBreak {
    Complete,
    Err(ExpressionError, Recovery),
    New(ParseNode),
}

pub(super) enum Recovery {
    Continue,
    DiscardTo(TokenKind),
    Fail,
}

// TODO: better name, something like builder but not builder
enum NodeKind {
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

fn parse_sequence(seq: &mut Vec<Expression>, token: Token) -> ParseFlow {
    match token.kind {
        TokenKind::Imaginary(r) => {
            seq.push(Expression::Literal(Literal::Number(Number::imaginary(r))))
        }
        TokenKind::Literal(val) => seq.push(Expression::Literal(val)),
        TokenKind::StringBegin { s, line_cont } => {
            return ParseFlow::Break(ParseBreak::New(ParseNode {
                kind: NodeKind::string(s, !line_cont),
            }));
        }
        TokenKind::IdentifierDiscard
        | TokenKind::IdentifierEnd(_)
        | TokenKind::IdentifierFragment(_)
        | TokenKind::StringDiscard
        | TokenKind::StringEnd(_)
        | TokenKind::StringFragment { .. } => {
            return ParseFlow::Break(ParseBreak::Err(
                ExpressionError {
                    kind: ExpressionErrorKind::InvalidLex(token.kind),
                    span: token.span,
                },
                Recovery::Fail,
            ));
        }
        _ => {
            return ParseFlow::Break(ParseBreak::Err(
                ExpressionError {
                    kind: ExpressionErrorKind::Unimplemented(token.kind),
                    span: token.span,
                },
                Recovery::Continue,
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
                kind: ExpressionErrorKind::InvalidLex(token.kind),
                span: token.span,
            },
            Recovery::Fail,
        )),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    mod sequence {
        use super::*;
        use crate::{number::Real, testutil::extract_or_fail};

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
                Expression::Literal(Literal::Boolean(true))
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
                extract_or_fail!(&seq[0], Expression::Literal),
                Literal::Number
            );
            assert!(matches!(n, Number::Complex(_) if n.as_datum().to_string() == "+1.2i"));
        }
    }
}
