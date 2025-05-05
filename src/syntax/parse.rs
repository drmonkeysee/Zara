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
pub(super) type MergeFlow = ControlFlow<()>;
pub(super) type MergeResult = Result<MergeFlow, ParserError>;
pub(super) type ParseErrFlow = ControlFlow<ParseErrBreak>;

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
            Self::Prg(seq) => parse_sequence(seq, token, txt, false),
        }
    }

    pub(super) fn merge(&mut self, other: ExprNode) -> MergeResult {
        match self {
            Self::Expr(node) => node.merge(other),
            Self::InvalidParseTree(_) | Self::InvalidTokenStream => Ok(MergeFlow::Continue(())),
            Self::Prg(seq) => other.merge_into(|expr| {
                seq.push(expr);
                MergeFlow::Continue(())
            }),
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
            ParseMode::ByteVector(seq) => parse_list(seq, token, txt, true),
            ParseMode::CommentBlock => parse_comment_block(token, txt),
            ParseMode::CommentDatum(inner) | ParseMode::Quote(inner) => {
                parse_datum(inner, token, txt, &self.ctx)
            }
            ParseMode::Identifier { datum, label } => parse_verbatim_identifier(label, token, txt),
            ParseMode::List { datum, seq } => parse_list(seq, token, txt, *datum),
            ParseMode::StringLiteral(buf) => parse_str(buf, token, txt),
        }
    }

    fn merge(&mut self, other: ExprNode) -> MergeResult {
        match &mut self.mode {
            ParseMode::ByteVector(seq) | ParseMode::List { seq, .. } => other.merge_into(|expr| {
                seq.push(expr);
                MergeFlow::Continue(())
            }),
            ParseMode::CommentDatum(inner) | ParseMode::Quote(inner) => other.merge_into(|expr| {
                inner.replace(expr);
                MergeFlow::Break(())
            }),
            _ => Err(ParserError::Invalid(InvalidParseError::InvalidExprTarget)),
        }
    }

    fn into_continuation_unsupported(self) -> ExpressionError {
        self.ctx.into_error(match self.mode {
            ParseMode::ByteVector(_) => ExpressionErrorKind::ByteVectorUnterminated,
            ParseMode::CommentBlock => ExpressionErrorKind::CommentBlockUnterminated,
            ParseMode::CommentDatum(_) | ParseMode::Quote(_) => ExpressionErrorKind::DatumExpected,
            ParseMode::Identifier { .. } => ExpressionErrorKind::IdentifierUnterminated,
            ParseMode::List { .. } => ExpressionErrorKind::ListUnterminated,
            ParseMode::StringLiteral(_) => ExpressionErrorKind::StrUnterminated,
        })
    }

    fn merge_into(self, slot: impl FnOnce(Expression) -> MergeFlow) -> MergeResult {
        Ok(<Self as TryInto<Option<Expression>>>::try_into(self)?
            .map_or(MergeFlow::Continue(()), slot))
    }
}

impl TryFrom<ExprNode> for Option<Expression> {
    type Error = Vec<ExpressionError>;

    fn try_from(value: ExprNode) -> ExprConvertResult {
        match value.mode {
            ParseMode::ByteVector(seq) => into_bytevector(seq, value.ctx),
            ParseMode::CommentBlock => Ok(None),
            ParseMode::CommentDatum(inner) => into_comment_datum(inner.as_ref(), value.ctx),
            ParseMode::Identifier { datum, label } => Ok(Some(Expression {
                ctx: value.ctx,
                kind: ExpressionKind::Identifier(label.into()),
            })),
            ParseMode::List { datum: false, seq } => Ok(Some(into_syntactic_form(seq, value.ctx))),
            ParseMode::List { datum: true, seq } => Ok(Some(into_list(seq, value.ctx))),
            ParseMode::Quote(inner) => into_datum(inner, value.ctx),
            ParseMode::StringLiteral(s) => Ok(Some(Expression::constant(
                Constant::String(s.into()),
                value.ctx,
            ))),
        }
    }
}

#[derive(Debug)]
pub(super) enum ParseErrBreak {
    FailedParser,
    InvalidTokenStream,
}

#[derive(Debug)]
pub(super) enum ParseBreak {
    Complete(ExprEnd),
    Err {
        err: ExpressionError,
        flow: ParseErrFlow,
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
            err,
            flow: ParseErrFlow::Continue(()),
        }
    }

    fn failed_parser(err: ExpressionError) -> Self {
        Self::Err {
            err,
            flow: ParseErrFlow::Break(ParseErrBreak::FailedParser),
        }
    }

    fn token_failure(err: ExpressionError) -> Self {
        Self::Err {
            err,
            flow: ParseErrFlow::Break(ParseErrBreak::InvalidTokenStream),
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
    Identifier { datum: bool, label: String },
    List { datum: bool, seq: Vec<Expression> },
    Quote(Option<Expression>),
    StringLiteral(String),
}

impl ParseMode {
    fn identifier(mut label: String, datum: bool) -> Self {
        label.push('\n');
        Self::Identifier { datum, label }
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

fn parse_datum(
    inner: &mut Option<Expression>,
    token: Token,
    txt: &Rc<TextLine>,
    node_ctx: &ExprCtx,
) -> ParseFlow {
    if matches!(token.kind, TokenKind::ParenRight) {
        let ctx = ExprCtx {
            span: node_ctx.span.start..if txt.lineno == node_ctx.txt.lineno {
                token.span.end
            } else {
                node_ctx.span.end
            },
            txt: Rc::clone(&node_ctx.txt),
        };
        ParseFlow::Break(ParseBreak::failed_parser(ExpressionError {
            ctx,
            kind: ExpressionErrorKind::DatumExpected,
        }))
    } else {
        let pos = token.span.end;
        match parse_expr(token, txt, true)? {
            None => ParseFlow::Continue(()),
            Some(expr) => {
                inner.replace(expr);
                ParseFlow::Break(ParseBreak::Complete(ExprEnd {
                    lineno: txt.lineno,
                    pos,
                }))
            }
        }
    }
}

fn parse_expr(token: Token, txt: &Rc<TextLine>, datum: bool) -> ExprFlow {
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
        // TODO: need datum flag for identifier -> symbol
        TokenKind::Identifier(s) => ExprFlow::Continue(Some(Expression {
            ctx: ExprCtx {
                span: token.span,
                txt: Rc::clone(txt),
            },
            kind: ExpressionKind::Identifier(s.into()),
        })),
        TokenKind::IdentifierBegin(s) => ExprFlow::Break(ParseBreak::new(
            ParseMode::identifier(s, datum),
            token.span.start,
        )),
        TokenKind::ParenLeft => ExprFlow::Break(ParseBreak::new(
            ParseMode::List {
                datum,
                seq: Vec::new(),
            },
            token.span.start,
        )),
        TokenKind::Quote => {
            let start = token.span.start;
            let mode = if datum {
                ParseMode::List {
                    datum: true,
                    seq: vec![Expression {
                        ctx: ExprCtx {
                            span: token.span,
                            txt: Rc::clone(txt),
                        },
                        kind: ExpressionKind::Identifier("quote".into()),
                    }],
                }
            } else {
                ParseMode::Quote(None)
            };
            ExprFlow::Break(ParseBreak::new(mode, start))
        }
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

fn parse_list(
    seq: &mut Vec<Expression>,
    token: Token,
    txt: &Rc<TextLine>,
    datum: bool,
) -> ParseFlow {
    match token.kind {
        TokenKind::ParenRight => ParseFlow::Break(ParseBreak::complete(txt.lineno, token.span.end)),
        _ => parse_sequence(seq, token, txt, datum),
    }
}

fn parse_sequence(
    seq: &mut Vec<Expression>,
    token: Token,
    txt: &Rc<TextLine>,
    datum: bool,
) -> ParseFlow {
    if let Some(expr) = parse_expr(token, txt, datum)? {
        seq.push(expr);
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

fn into_comment_datum(inner: Option<&Expression>, ctx: ExprCtx) -> ExprConvertResult {
    match inner {
        None => Err(vec![ExpressionError {
            ctx,
            kind: ExpressionErrorKind::DatumExpected,
        }]),
        Some(_) => Ok(None),
    }
}

fn into_datum(inner: Option<Expression>, ctx: ExprCtx) -> ExprConvertResult {
    match inner {
        None => Err(vec![ExpressionError {
            ctx,
            kind: ExpressionErrorKind::DatumExpected,
        }]),
        Some(expr) => match expr.kind {
            ExpressionKind::Call { .. } => {
                let mut expr_ctx = expr.ctx;
                expr_ctx.span.start = ctx.span.start;
                Err(vec![ExpressionError {
                    ctx: expr_ctx,
                    kind: ExpressionErrorKind::DatumInvalid(expr.kind),
                }])
            }
            ExpressionKind::Literal(_) => Ok(Some(expr)),
            // TODO: parse_expr should yield symbol, making identifier invalid
            ExpressionKind::Identifier(_) => todo!(),
            ExpressionKind::List(_) => todo!(),
        },
    }
}

fn into_list(seq: Vec<Expression>, ctx: ExprCtx) -> Expression {
    Expression {
        ctx,
        kind: ExpressionKind::List(seq.into()),
    }
}

fn into_syntactic_form(seq: Vec<Expression>, ctx: ExprCtx) -> Expression {
    // TODO: check for keywords/special forms
    // need to handle cases where e.g. "if" is shadowed by a user definition
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
