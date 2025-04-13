#[cfg(test)]
mod tests;

// TODO: bv-temp
use crate::value::Value;

use super::expr::{ExprCtx, Expression, ExpressionError, ExpressionErrorKind};
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

    pub(super) fn parse(&mut self, token: Token, txt: &Rc<TextLine>) -> ParseFlow {
        match &mut self.mode {
            ParseMode::ByteVector(seq) | ParseMode::List(seq) => parse_list(seq, token, txt),
            ParseMode::CommentBlock => parse_comment_block(token, txt),
            ParseMode::Failed => ParseFlow::Continue(()),
            ParseMode::Identifier(buf) => parse_verbatim_identifier(buf, token, txt),
            ParseMode::Program(seq) => parse_sequence(seq, token, txt),
            ParseMode::StringLiteral(buf) => parse_str(buf, token, txt),
        }
    }

    pub(super) fn merge(&mut self, other: ParseNode) -> Result<(), Vec<ExpressionError>> {
        match &mut self.mode {
            ParseMode::Program(exprs) => Ok(exprs.push(other.try_into()?)),
            _ => todo!("fail here somehow"),
        }
    }

    pub(super) fn into_continuation_unsupported(self) -> Option<ExpressionError> {
        let ctx = self.ctx?;
        let err_kind = match self.mode {
            ParseMode::ByteVector(_) => Some(ExpressionErrorKind::ByteVectorUnterminated),
            ParseMode::CommentBlock => Some(ExpressionErrorKind::CommentBlockUnterminated),
            ParseMode::Identifier(_) => Some(ExpressionErrorKind::IdentifierUnterminated),
            ParseMode::List(_) => Some(ExpressionErrorKind::ListUnterminated),
            ParseMode::StringLiteral(_) => Some(ExpressionErrorKind::StrUnterminated),
            _ => None,
        }?;
        Some(ctx.into_error(err_kind))
    }
}

impl TryFrom<ParseNode> for Expression {
    type Error = Vec<ExpressionError>;

    fn try_from(node: ParseNode) -> Result<Self, <Self as TryFrom<ParseNode>>::Error> {
        Ok(match node.mode {
            ParseMode::ByteVector(seq) => into_bytevector(seq)?,
            ParseMode::Identifier(s) => Expression::Identifier(s.into()),
            ParseMode::List(seq) => convert_list(seq),
            ParseMode::Program(exprs) => Expression::Seq(exprs.into()),
            ParseMode::StringLiteral(s) => Expression::constant(Constant::String(s.into())),
            _ => Expression::Empty,
        })
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
    fn into_error(self, kind: ExpressionErrorKind) -> ExpressionError {
        ExpressionError {
            ctx: ExprCtx {
                span: self.start..self.txt.line.len(),
                txt: self.txt,
            },
            kind,
        }
    }
}

fn parse_comment_block(token: Token, txt: &Rc<TextLine>) -> ParseFlow {
    match token.kind {
        TokenKind::CommentBlockFragment { .. } => ParseFlow::Continue(()),
        TokenKind::CommentBlockEnd => ParseFlow::Break(ParseBreak::Complete),
        _ => ParseFlow::Break(ParseBreak::Err(
            ExpressionError {
                ctx: ExprCtx {
                    span: token.span,
                    txt: Rc::clone(txt),
                },
                kind: ExpressionErrorKind::CommentBlockInvalid(token.kind),
            },
            ErrFlow::Break(Recovery::Fail),
        )),
    }
}

fn parse_list(seq: &mut Vec<Expression>, token: Token, txt: &Rc<TextLine>) -> ParseFlow {
    match token.kind {
        TokenKind::ParenRight => ParseFlow::Break(ParseBreak::Complete),
        _ => parse_sequence(seq, token, txt),
    }
}

fn parse_sequence(seq: &mut Vec<Expression>, token: Token, txt: &Rc<TextLine>) -> ParseFlow {
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
                    ctx: ExprCtx {
                        span: token.span,
                        txt: Rc::clone(txt),
                    },
                    kind: ExpressionErrorKind::SeqInvalid(token.kind),
                },
                ErrFlow::Break(Recovery::Fail),
            ));
        }
        _ => {
            return ParseFlow::Break(ParseBreak::Err(
                ExpressionError {
                    ctx: ExprCtx {
                        span: token.span,
                        txt: Rc::clone(txt),
                    },
                    kind: ExpressionErrorKind::Unimplemented(token.kind),
                },
                ErrFlow::Continue(()),
            ));
        }
    }
    ParseFlow::Continue(())
}

fn parse_str(buf: &mut String, token: Token, txt: &Rc<TextLine>) -> ParseFlow {
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
                ctx: ExprCtx {
                    span: token.span,
                    txt: Rc::clone(txt),
                },
                kind: ExpressionErrorKind::StrInvalid(token.kind),
            },
            ErrFlow::Break(Recovery::Fail),
        )),
    }
}

fn parse_verbatim_identifier(buf: &mut String, token: Token, txt: &Rc<TextLine>) -> ParseFlow {
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
                ctx: ExprCtx {
                    span: token.span,
                    txt: Rc::clone(txt),
                },
                kind: ExpressionErrorKind::IdentifierInvalid(token.kind),
            },
            ErrFlow::Break(Recovery::Fail),
        )),
    }
}

type ConvertExprResult = Result<Expression, <Expression as TryFrom<ParseNode>>::Error>;

fn into_bytevector(seq: Vec<Expression>) -> ConvertExprResult {
    // todo!("filter out everything except bytes")
    /*
    errors
    - invalid type (anything that's not an Integer)
    - out of range (< 0 or > 255)
    */
    let foo = seq.into_iter().map(|expr| match expr {
        Expression::Literal(Value::Constant(Constant::Number(n))) => Ok(n.into_u8()),
        _ => todo!("expressions need exprctx"), /*Err(ExpressionError {
                                                    kind: ExpressionErrorKind::ByteVectorInvalidItem,
                                                    span: 0..0,
                                                }),*/
    });
    let (bytes, errs): (Vec<_>, Vec<_>) = foo.partition(Result::is_ok);
    //todo!("do something with errors");
    if errs.is_empty() {
        Ok(Expression::Literal(Value::ByteVector(
            bytes.into_iter().flatten().collect(),
        )))
    } else {
        Err(errs.into_iter().filter_map(Result::err).collect::<Vec<_>>())?
    }
}

fn convert_list(seq: Vec<Expression>) -> Expression {
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
