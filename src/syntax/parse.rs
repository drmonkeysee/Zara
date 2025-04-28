#[cfg(test)]
mod tests;

use super::{
    InvalidParseError, ParserError, Program,
    expr::{ExprCtx, ExprEnd, Expression, ExpressionError, ExpressionErrorKind, ExpressionKind},
};
use crate::{
    constant::Constant,
    lex::{Token, TokenKind},
    number::Number,
    txt::{LineNumber, TextLine},
    value::Value,
};
use std::{ops::ControlFlow, rc::Rc};

pub(super) type ParseFlow = ControlFlow<ParseBreak>;
pub(super) type ExprConvertResult =
    Result<Option<Expression>, <Option<Expression> as TryFrom<ExprNode>>::Error>;
pub(super) type MergeResult = Result<(), ParserError>;

pub(super) enum ParseNode {
    Expr(ExprNode),
    InvalidParseTree(InvalidParseError),
    InvalidTokenStream,
    Prg(Vec<Expression>),
}

impl ParseNode {
    pub(super) fn prg() -> Self {
        Self::Prg(Vec::new())
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

    pub(super) fn is_invalid_parse(&self) -> bool {
        matches!(
            self,
            ParseNode::InvalidParseTree(_) | ParseNode::InvalidTokenStream
        )
    }

    pub(super) fn parse(&mut self, token: Token, txt: &Rc<TextLine>) -> ParseFlow {
        match self {
            Self::Expr(node) => node.parse(token, txt),
            Self::InvalidParseTree(_) | Self::InvalidTokenStream => ParseFlow::Continue(()),
            Self::Prg(seq) => parse_sequence(seq, token, txt),
        }
    }

    pub(super) fn merge(&mut self, other: ExprNode) -> MergeResult {
        match self {
            Self::Expr(node) => node.merge(other),
            Self::InvalidParseTree(_) | Self::InvalidTokenStream => Ok(()),
            Self::Prg(seq) => other.merge_into(seq),
        }
    }

    pub(super) fn into_expr_node(self, end: ExprEnd) -> Option<ExprNode> {
        if let Self::Expr(mut node) = self {
            if end.lineno == node.ctx.txt.lineno {
                node.ctx.span.end = end.pos;
            }
            Some(node)
        } else {
            None
        }
    }

    pub(super) fn into_continuation_unsupported(self) -> Option<ExpressionError> {
        if let Self::Expr(node) = self {
            Some(node.into_continuation_unsupported())
        } else {
            None
        }
    }
}

impl TryFrom<ParseNode> for Program {
    type Error = InvalidParseError;

    fn try_from(value: ParseNode) -> Result<Self, <Self as TryFrom<ParseNode>>::Error> {
        match value {
            ParseNode::InvalidParseTree(err) => Err(err),
            ParseNode::Prg(seq) => Ok(Program::new(seq)),
            _ => Err(InvalidParseError::EndOfParse),
        }
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
            ParseMode::CommentDatum(inner) => parse_comment_datum(inner, token, txt),
            ParseMode::Identifier(buf) => parse_verbatim_identifier(buf, token, txt),
            ParseMode::StringLiteral(buf) => parse_str(buf, token, txt),
        }
    }

    fn merge(&mut self, other: ExprNode) -> MergeResult {
        match &mut self.mode {
            ParseMode::ByteVector(seq) | ParseMode::List(seq) => other.merge_into(seq),
            ParseMode::CommentDatum(_inner) => {
                // TODO: e.g. invalid special form is commented-out properly #; (if foo)
                todo!("compound datums must be treated as quoted")
            }
            _ => Err(ParserError::Invalid(InvalidParseError::InvalidExprTarget)),
        }
    }

    fn into_continuation_unsupported(self) -> ExpressionError {
        self.ctx.into_error(match self.mode {
            ParseMode::ByteVector(_) => ExpressionErrorKind::ByteVectorUnterminated,
            ParseMode::CommentBlock => ExpressionErrorKind::CommentBlockUnterminated,
            ParseMode::CommentDatum(_) => ExpressionErrorKind::CommentDatumUnterminated,
            ParseMode::Identifier(_) => ExpressionErrorKind::IdentifierUnterminated,
            ParseMode::List(_) => ExpressionErrorKind::ListUnterminated,
            ParseMode::StringLiteral(_) => ExpressionErrorKind::StrUnterminated,
        })
    }

    fn merge_into(self, seq: &mut Vec<Expression>) -> MergeResult {
        #[allow(unused_must_use, reason = "returns unit value")]
        <Self as TryInto<Option<Expression>>>::try_into(self)?.map_or((), |expr| seq.push(expr));
        Ok(())
    }
}

impl TryFrom<ExprNode> for Option<Expression> {
    type Error = Vec<ExpressionError>;

    fn try_from(value: ExprNode) -> ExprConvertResult {
        match value.mode {
            ParseMode::ByteVector(seq) => into_bytevector(seq, value.ctx),
            ParseMode::CommentBlock => Ok(None),
            ParseMode::CommentDatum(inner) => into_comment_datum(inner, value.ctx),
            ParseMode::Identifier(s) => Ok(Some(Expression {
                ctx: value.ctx,
                kind: ExpressionKind::Identifier(s.into()),
            })),
            ParseMode::List(seq) => Ok(Some(convert_list(seq, value.ctx))),
            ParseMode::StringLiteral(s) => Ok(Some(Expression::constant(
                Constant::String(s.into()),
                value.ctx,
            ))),
        }
    }
}

#[derive(Debug)]
pub(super) enum ParseBreak {
    Complete(ExprEnd),
    Err {
        bad_tokens: bool,
        err: ExpressionError,
    },
    New(ParseNew),
}

impl ParseBreak {
    fn complete(lineno: LineNumber, pos: usize) -> Self {
        Self::Complete(ExprEnd { lineno, pos })
    }

    fn new(mode: ParseMode, start: usize) -> Self {
        Self::New(ParseNew { mode, start })
    }

    fn recover(err: ExpressionError) -> Self {
        Self::Err {
            bad_tokens: false,
            err,
        }
    }

    fn token_failure(err: ExpressionError) -> Self {
        Self::Err {
            bad_tokens: true,
            err,
        }
    }
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

type ExprFlow = ControlFlow<ParseBreak, Option<Expression>>;

#[derive(Debug)]
enum ParseMode {
    ByteVector(Vec<Expression>),
    CommentBlock,
    CommentDatum(Option<Expression>),
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
        TokenKind::CommentBlockEnd => {
            ParseFlow::Break(ParseBreak::complete(txt.lineno, token.span.end))
        }
        _ => ParseFlow::Break(ParseBreak::token_failure(ExpressionError {
            ctx: ExprCtx {
                span: token.span,
                txt: Rc::clone(txt),
            },
            kind: ExpressionErrorKind::CommentBlockInvalid(token.kind),
        })),
    }
}

fn parse_comment_datum(
    inner: &mut Option<Expression>,
    token: Token,
    txt: &Rc<TextLine>,
) -> ParseFlow {
    if matches!(token.kind, TokenKind::ParenRight) {
        ParseFlow::Break(ParseBreak::Err {
            bad_tokens: false,
            err: ExpressionError {
                ctx: ExprCtx {
                    span: token.span,
                    txt: Rc::clone(&txt),
                },
                kind: ExpressionErrorKind::CommentDatumUnterminated,
            },
        })
    } else {
        let pos = token.span.end;
        match parse_expr(token, txt) {
            ExprFlow::Break(brk) => ParseFlow::Break(brk),
            ExprFlow::Continue(None) => ParseFlow::Continue(()),
            ExprFlow::Continue(Some(expr)) => {
                inner.replace(expr);
                ParseFlow::Break(ParseBreak::Complete(ExprEnd {
                    lineno: txt.lineno,
                    pos,
                }))
            }
        }
    }
}

fn parse_expr(token: Token, txt: &Rc<TextLine>) -> ExprFlow {
    match token.kind {
        TokenKind::ByteVector => ExprFlow::Break(ParseBreak::new(
            ParseMode::ByteVector(Vec::new()),
            token.span.start,
        )),
        TokenKind::Comment => ExprFlow::Continue(None),
        TokenKind::CommentBlockBegin { .. } => {
            ExprFlow::Break(ParseBreak::new(ParseMode::CommentBlock, token.span.start))
        }
        TokenKind::CommentDatum => ExprFlow::Break(ParseBreak::new(
            ParseMode::CommentDatum(None),
            token.span.start,
        )),
        TokenKind::Constant(con) => ExprFlow::Continue(Some(Expression::constant(
            con,
            ExprCtx {
                span: token.span,
                txt: Rc::clone(txt),
            },
        ))),
        TokenKind::Imaginary(r) => ExprFlow::Continue(Some(Expression::constant(
            Constant::Number(Number::imaginary(r)),
            ExprCtx {
                span: token.span,
                txt: Rc::clone(txt),
            },
        ))),
        // TODO: check for keywords/special forms here?
        // need to handle cases where e.g. "if" is shadowed by a user definition
        TokenKind::Identifier(s) => ExprFlow::Continue(Some(Expression {
            ctx: ExprCtx {
                span: token.span,
                txt: Rc::clone(txt),
            },
            kind: ExpressionKind::Identifier(s.into()),
        })),
        TokenKind::IdentifierBegin(s) => {
            ExprFlow::Break(ParseBreak::new(ParseMode::identifier(s), token.span.start))
        }
        TokenKind::ParenLeft => ExprFlow::Break(ParseBreak::new(
            ParseMode::List(Vec::new()),
            token.span.start,
        )),
        TokenKind::StringBegin { s, line_cont } => ExprFlow::Break(ParseBreak::new(
            ParseMode::string(s, !line_cont),
            token.span.start,
        )),
        // TODO: this should reduce to _ => once Unimplemented is removed
        TokenKind::CommentBlockFragment { .. }
        | TokenKind::CommentBlockEnd
        | TokenKind::IdentifierDiscard
        | TokenKind::IdentifierEnd(_)
        | TokenKind::IdentifierFragment(_)
        | TokenKind::StringDiscard
        | TokenKind::StringEnd(_)
        | TokenKind::StringFragment { .. } => {
            ExprFlow::Break(ParseBreak::token_failure(ExpressionError {
                ctx: ExprCtx {
                    span: token.span,
                    txt: Rc::clone(txt),
                },
                kind: ExpressionErrorKind::SeqInvalid(token.kind),
            }))
        }
        _ => ExprFlow::Break(ParseBreak::recover(ExpressionError {
            ctx: ExprCtx {
                span: token.span,
                txt: Rc::clone(txt),
            },
            kind: ExpressionErrorKind::Unimplemented(token.kind),
        })),
    }
}

fn parse_list(seq: &mut Vec<Expression>, token: Token, txt: &Rc<TextLine>) -> ParseFlow {
    match token.kind {
        TokenKind::ParenRight => ParseFlow::Break(ParseBreak::complete(txt.lineno, token.span.end)),
        _ => parse_sequence(seq, token, txt),
    }
}

fn parse_sequence(seq: &mut Vec<Expression>, token: Token, txt: &Rc<TextLine>) -> ParseFlow {
    match parse_expr(token, txt) {
        ExprFlow::Break(brk) => ParseFlow::Break(brk),
        ExprFlow::Continue(expr) => {
            if let Some(expr) = expr {
                seq.push(expr);
            }
            ParseFlow::Continue(())
        }
    }
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
            ParseFlow::Break(ParseBreak::complete(txt.lineno, token.span.end))
        }
        _ => ParseFlow::Break(ParseBreak::token_failure(ExpressionError {
            ctx: ExprCtx {
                span: token.span,
                txt: Rc::clone(txt),
            },
            kind: ExpressionErrorKind::StrInvalid(token.kind),
        })),
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
            ParseFlow::Break(ParseBreak::complete(txt.lineno, token.span.end))
        }
        _ => ParseFlow::Break(ParseBreak::token_failure(ExpressionError {
            ctx: ExprCtx {
                span: token.span,
                txt: Rc::clone(txt),
            },
            kind: ExpressionErrorKind::IdentifierInvalid(token.kind),
        })),
    }
}

fn into_bytevector(seq: Vec<Expression>, ctx: ExprCtx) -> ExprConvertResult {
    let (bytes, errs): (Vec<_>, Vec<_>) = seq
        .into_iter()
        .map(|expr| match expr.kind {
            ExpressionKind::Literal(Value::Constant(Constant::Number(n))) => {
                n.try_into().map_err(|err| ExpressionError {
                    ctx: expr.ctx,
                    kind: ExpressionErrorKind::ByteVectorInvalidNumber(err),
                })
            }
            _ => Err(ExpressionError {
                ctx: expr.ctx,
                kind: ExpressionErrorKind::ByteVectorInvalidItem(expr.kind),
            }),
        })
        .partition(Result::is_ok);
    if errs.is_empty() {
        Ok(Some(Expression {
            ctx,
            kind: ExpressionKind::Literal(Value::ByteVector(bytes.into_iter().flatten().collect())),
        }))
    } else {
        Err(errs.into_iter().filter_map(Result::err).collect::<Vec<_>>())
    }
}

fn into_comment_datum(inner: Option<Expression>, ctx: ExprCtx) -> ExprConvertResult {
    match inner {
        None => Err(vec![ExpressionError {
            ctx,
            kind: ExpressionErrorKind::CommentDatumUnterminated,
        }]),
        Some(_) => Ok(None),
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
