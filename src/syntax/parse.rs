#[cfg(test)]
mod tests;

// TODO: bv-temp
use crate::value::Value;

use super::expr::{
    ExprCtx, Expression, ExpressionError, ExpressionErrorKind, ExpressionKind, Program,
    ProgramError,
};
use crate::{
    constant::Constant,
    lex::{Token, TokenKind},
    number::Number,
    txt::TextLine,
};
use std::{ops::ControlFlow, rc::Rc};

pub(super) type ParseFlow = ControlFlow<ParseBreak>;

pub(super) enum ParseNode {
    Expr(ExprNode),
    Fail,
    Prg(Vec<Expression>),
}

impl ParseNode {
    pub(super) fn prg() -> Self {
        Self::Prg(Vec::new())
    }

    pub(super) fn fail() -> Self {
        Self::Fail
    }

    fn new(mode: ParseMode, start: usize, txt: impl Into<Rc<TextLine>>) -> Self {
        let txt = txt.into();
        Self::Expr(ExprNode {
            ctx: ExprCtx {
                span: start..txt.line.len(),
                txt,
            },
            mode,
        })
    }

    // TODO: temporary for debug assert
    pub(super) fn is_prg(&self) -> bool {
        matches!(self, Self::Prg(_))
    }

    pub(super) fn parse(&mut self, token: Token, txt: &Rc<TextLine>) -> ParseFlow {
        match self {
            Self::Expr(node) => node.parse(token, txt),
            Self::Fail => ParseFlow::Continue(()),
            Self::Prg(seq) => parse_sequence(seq, token, txt),
        }
    }

    pub(super) fn merge(&mut self, other: ParseNode) -> Result<(), Vec<ExpressionError>> {
        let Self::Expr(other_expr) = other else {
            todo!("other always needs to be an expr node");
        };
        match self {
            Self::Prg(seq) => Ok(seq.push(other_expr.try_into()?)),
            _ => todo!("what to do for rest of arms"),
        }
    }

    pub(super) fn into_continuation_unsupported(self) -> Option<ExpressionError> {
        if let Self::Expr(node) = self {
            node.into_continuation_unsupported()
        } else {
            None
        }
    }
}

impl TryFrom<ParseNode> for Program {
    type Error = ProgramError;

    fn try_from(value: ParseNode) -> Result<Self, <Self as TryFrom<ParseNode>>::Error> {
        let ParseNode::Prg(seq) = value else {
            todo!("only prg can convert properly");
        };
        Ok(Program::new(seq))
    }
}

pub(super) struct ExprNode {
    ctx: ExprCtx,
    mode: ParseMode,
}

impl ExprNode {
    fn parse(&mut self, token: Token, txt: &Rc<TextLine>) -> ParseFlow {
        match &mut self.mode {
            ParseMode::ByteVector(seq) | ParseMode::List(seq) => parse_list(seq, token, txt),
            ParseMode::CommentBlock => parse_comment_block(token, txt),
            ParseMode::Identifier(buf) => parse_verbatim_identifier(buf, token, txt),
            ParseMode::StringLiteral(buf) => parse_str(buf, token, txt),
        }
    }

    fn merge(&mut self, other: ExprNode) -> Result<(), Vec<ExpressionError>> {
        todo!();
    }

    fn into_continuation_unsupported(self) -> Option<ExpressionError> {
        Some(self.ctx.into_error(match self.mode {
            ParseMode::ByteVector(_) => ExpressionErrorKind::ByteVectorUnterminated,
            ParseMode::CommentBlock => ExpressionErrorKind::CommentBlockUnterminated,
            ParseMode::Identifier(_) => ExpressionErrorKind::IdentifierUnterminated,
            ParseMode::List(_) => ExpressionErrorKind::ListUnterminated,
            ParseMode::StringLiteral(_) => ExpressionErrorKind::StrUnterminated,
        }))
    }
}

impl TryFrom<ExprNode> for Expression {
    type Error = Vec<ExpressionError>;

    fn try_from(node: ExprNode) -> Result<Self, <Self as TryFrom<ExprNode>>::Error> {
        Ok(match node.mode {
            ParseMode::ByteVector(seq) => into_bytevector(seq, node.ctx)?,
            ParseMode::Identifier(s) => Expression {
                ctx: node.ctx,
                kind: ExpressionKind::Identifier(s.into()),
            },
            ParseMode::List(seq) => convert_list(seq, node.ctx),
            ParseMode::StringLiteral(s) => {
                Expression::constant(Constant::String(s.into()), node.ctx)
            }
            _ => todo!("fill out rest of arms"),
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
    Identifier(String),
    List(Vec<Expression>),
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
        TokenKind::Constant(con) => seq.push(Expression::constant(
            con,
            ExprCtx {
                span: token.span,
                txt: Rc::clone(txt),
            },
        )),
        TokenKind::Imaginary(r) => seq.push(Expression::constant(
            Constant::Number(Number::imaginary(r)),
            ExprCtx {
                span: token.span,
                txt: Rc::clone(txt),
            },
        )),
        // TODO: check for keywords/special forms here?
        // need to handle cases where e.g. "if" is shadowed by a user definition
        TokenKind::Identifier(s) => seq.push(Expression {
            ctx: ExprCtx {
                span: token.span,
                txt: Rc::clone(txt),
            },
            kind: ExpressionKind::Identifier(s.into()),
        }),
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

type ConvertExprResult = Result<Expression, <Expression as TryFrom<ExprNode>>::Error>;

fn into_bytevector(seq: Vec<Expression>, ctx: ExprCtx) -> ConvertExprResult {
    // todo!("filter out everything except bytes")
    /*
    errors
    - invalid type (anything that's not an Integer)
    - out of range (< 0 or > 255)
    */
    let foo = seq.into_iter().map(|expr| match expr.kind {
        ExpressionKind::Literal(Value::Constant(Constant::Number(n))) => Ok(n.into_u8()),
        _ => todo!("expressions need exprctx"), /*Err(ExpressionError {
                                                    kind: ExpressionErrorKind::ByteVectorInvalidItem,
                                                    span: 0..0,
                                                }),*/
    });
    let (bytes, errs): (Vec<_>, Vec<_>) = foo.partition(Result::is_ok);
    //todo!("do something with errors");
    if errs.is_empty() {
        Ok(Expression {
            ctx,
            kind: ExpressionKind::Literal(Value::ByteVector(bytes.into_iter().flatten().collect())),
        })
    } else {
        Err(errs.into_iter().filter_map(Result::err).collect::<Vec<_>>())?
    }
}

fn convert_list(seq: Vec<Expression>, ctx: ExprCtx) -> Expression {
    debug_assert!(
        !seq.is_empty(),
        "empty list is invalid syntax unless quoted"
    );
    let mut iter = seq.into_iter();
    let proc = iter.next().unwrap();
    Expression {
        ctx,
        kind: ExpressionKind::Call {
            args: iter.collect(),
            proc: proc.into(),
        },
    }
}
