#[cfg(test)]
mod tests;

// TODO: bv-temp
use crate::value::Value;

use super::{
    ParseErrorLine,
    expr::{Expression, ExpressionError, ExpressionErrorKind},
};
use crate::{
    constant::Constant,
    lex::{Token, TokenKind},
    number::Number,
    txt::TextLine,
};
use std::{ops::ControlFlow, rc::Rc};

pub(super) type ParseFlow = ControlFlow<ParseBreak>;

pub(super) struct ParseNode {
    ctx: Option<ParseCtx>,
    mode: ParseMode,
}

impl ParseNode {
    pub(super) fn prg() -> Self {
        Self {
            ctx: None,
            mode: ParseMode::Program(Vec::new()),
        }
    }

    pub(super) fn fail() -> Self {
        Self {
            ctx: None,
            mode: ParseMode::Failed,
        }
    }

    fn new(mode: ParseMode, start: usize, txt: impl Into<Rc<TextLine>>) -> Self {
        Self {
            mode,
            ctx: Some(ParseCtx {
                txt: txt.into(),
                start,
            }),
        }
    }

    // TODO: temporary for debug assert
    pub(super) fn is_prg(&self) -> bool {
        matches!(self.mode, ParseMode::Program(_))
    }

    pub(super) fn parse(&mut self, token: Token) -> ParseFlow {
        match &mut self.mode {
            ParseMode::ByteVector(seq) | ParseMode::List(seq) => parse_list(seq, token),
            ParseMode::CommentBlock => parse_comment_block(token),
            ParseMode::Failed => ParseFlow::Continue(()),
            ParseMode::Identifier(buf) => parse_verbatim_identifier(buf, token),
            ParseMode::Program(seq) => parse_sequence(seq, token),
            ParseMode::StringLiteral(buf) => parse_str(buf, token),
        }
    }

    pub(super) fn merge(&mut self, other: ParseNode) {
        match &mut self.mode {
            ParseMode::Program(exprs) => exprs.push(other.into_expr()),
            _ => todo!("fail here somehow"),
        }
    }

    pub(super) fn into_expr(self) -> Expression {
        match self.mode {
            ParseMode::ByteVector(seq) => {
                // todo!("filter out everything except bytes")
                let bytes = seq.into_iter().map(|expr| match expr {
                    Expression::Literal(Value::Constant(Constant::Number(n))) => n.into_u8(),
                    _ => 0,
                });
                Expression::Literal(Value::ByteVector(bytes.collect()))
            }
            ParseMode::Identifier(s) => Expression::Identifier(s.into()),
            ParseMode::List(seq) => {
                debug_assert!(
                    !seq.is_empty(),
                    "empty list is invalid syntax unless quoted"
                );
                let mut iter = seq.into_iter();
                let proc = iter.next().unwrap();
                Expression::Call {
                    args: iter.collect(),
                    proc: proc.into(),
                }
            }
            ParseMode::Program(exprs) => Expression::Seq(exprs.into()),
            ParseMode::StringLiteral(s) => Expression::constant(Constant::String(s.into())),
            _ => Expression::Empty,
        }
    }

    pub(super) fn into_continuation_unsupported(self) -> Option<ParseErrorLine> {
        let err_kind = match self.mode {
            ParseMode::CommentBlock => Some(ExpressionErrorKind::CommentBlockUnterminated),
            ParseMode::Identifier(_) => Some(ExpressionErrorKind::IdentifierUnterminated),
            ParseMode::List(_) => Some(ExpressionErrorKind::ListUnterminated),
            ParseMode::StringLiteral(_) => Some(ExpressionErrorKind::StrUnterminated),
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
    mode: ParseMode,
    start: usize,
}

impl ParseNew {
    pub(super) fn into_node(self, txt: impl Into<Rc<TextLine>>) -> ParseNode {
        ParseNode::new(self.mode, self.start, txt)
    }
}

pub(super) type ErrFlow = ControlFlow<Recovery>;

#[derive(Debug)]
pub(super) enum Recovery {
    DiscardTo(TokenKind),
    Fail,
}

#[derive(Debug)]
enum ParseMode {
    ByteVector(Vec<Expression>),
    CommentBlock,
    Failed,
    Identifier(String),
    List(Vec<Expression>),
    Program(Vec<Expression>),
    StringLiteral(String),
}

impl ParseMode {
    fn identifier(mut s: String) -> Self {
        s.push('\n');
        Self::Identifier(s)
    }

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

fn parse_bytevector(seq: &mut Vec<Expression>, token: Token) -> ParseFlow {
    todo!();
    /*
    let f := parse_sequence(vec)
    if error, propogate flow
    expr := vec.last()
    if expr is u8:
        keep in vec
    else if expr is empty:
        pop
    else:
        pop and raise error flow
    continue until rparen or continuation
    this is basically list parsing with some post-fix logic
    */
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

fn parse_list(seq: &mut Vec<Expression>, token: Token) -> ParseFlow {
    match token.kind {
        TokenKind::ParenRight => ParseFlow::Break(ParseBreak::Complete),
        _ => parse_sequence(seq, token),
    }
}

fn parse_sequence(seq: &mut Vec<Expression>, token: Token) -> ParseFlow {
    match token.kind {
        TokenKind::ByteVector => {
            return ParseFlow::Break(ParseBreak::New(ParseNew {
                mode: ParseMode::ByteVector(Vec::new()),
                start: token.span.start,
            }));
        }
        TokenKind::Comment => (),
        TokenKind::CommentBlockBegin { .. } => {
            return ParseFlow::Break(ParseBreak::New(ParseNew {
                mode: ParseMode::CommentBlock,
                start: token.span.start,
            }));
        }
        TokenKind::Constant(con) => seq.push(Expression::constant(con)),
        TokenKind::Imaginary(r) => {
            seq.push(Expression::constant(Constant::Number(Number::imaginary(r))))
        }
        // TODO: check for keywords/special forms here?
        // need to handle cases where e.g. "if" is shadowed by a user definition
        TokenKind::Identifier(s) => seq.push(Expression::Identifier(s.into())),
        TokenKind::IdentifierBegin(s) => {
            return ParseFlow::Break(ParseBreak::New(ParseNew {
                mode: ParseMode::identifier(s),
                start: token.span.start,
            }));
        }
        TokenKind::ParenLeft => {
            return ParseFlow::Break(ParseBreak::New(ParseNew {
                mode: ParseMode::List(Vec::new()),
                start: token.span.start,
            }));
        }
        TokenKind::StringBegin { s, line_cont } => {
            return ParseFlow::Break(ParseBreak::New(ParseNew {
                mode: ParseMode::string(s, !line_cont),
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

fn parse_verbatim_identifier(buf: &mut String, token: Token) -> ParseFlow {
    match token.kind {
        TokenKind::IdentifierFragment(s) => {
            buf.push_str(&s);
            buf.push('\n');
            ParseFlow::Continue(())
        }
        TokenKind::IdentifierEnd(s) => {
            buf.push_str(&s);
            ParseFlow::Break(ParseBreak::Complete)
        }
        _ => ParseFlow::Break(ParseBreak::Err(
            ExpressionError {
                kind: ExpressionErrorKind::IdentifierInvalid(token.kind),
                span: token.span,
            },
            ErrFlow::Break(Recovery::Fail),
        )),
    }
}
