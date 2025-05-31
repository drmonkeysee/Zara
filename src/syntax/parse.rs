#[cfg(test)]
mod tests;

use super::{
    InvalidParseError, ParserError, Program, SyntaxError,
    expr::{ExprCtx, ExprEnd, Expression, ExpressionError, ExpressionErrorKind, ExpressionKind},
};
use crate::{
    lex::{Token, TokenKind},
    number::Number,
    txt::{LineNumber, TextLine},
    value::{Pair, Value, zlist},
};
use std::{ops::ControlFlow, rc::Rc};

const LONGFORM_QUOTE: &str = "quote";

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
        matches!(self, Self::InvalidParseTree(_) | Self::InvalidTokenStream)
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
            ParseMode::ByteVector(seq) | ParseMode::Vector(seq) => parse_vector(seq, token, txt),
            ParseMode::CommentBlock => parse_comment_block(token, txt),
            ParseMode::CommentDatum(inner) | ParseMode::Quote { inner, .. } => {
                parse_datum(inner, token, txt, &self.ctx)
            }
            ParseMode::Identifier { name, .. } => parse_verbatim_identifier(name, token, txt),
            ParseMode::List { form, seq } => form.parse_list(seq, token, txt),
            ParseMode::StringLiteral(buf) => parse_str(buf, token, txt),
        }
    }

    fn merge(&mut self, other: Self) -> MergeResult {
        match &mut self.mode {
            ParseMode::ByteVector(seq) | ParseMode::Vector(seq) => other.merge_into(|expr| {
                seq.push(expr);
                MergeFlow::Continue(())
            }),
            ParseMode::CommentDatum(inner) | ParseMode::Quote { inner, .. } => {
                other.merge_into(|expr| {
                    inner.replace(expr);
                    MergeFlow::Break(())
                })
            }
            ParseMode::List { form, seq } => form.merge(seq, other, &self.ctx),
            _ => Err(ParserError::Invalid(InvalidParseError::InvalidExprTarget)),
        }
    }

    fn into_continuation_unsupported(self) -> ExpressionError {
        self.ctx.into_error(match self.mode {
            ParseMode::ByteVector(_) => ExpressionErrorKind::ByteVectorUnterminated,
            ParseMode::CommentBlock => ExpressionErrorKind::CommentBlockUnterminated,
            ParseMode::CommentDatum(_) | ParseMode::Quote { .. } => {
                ExpressionErrorKind::DatumExpected
            }
            ParseMode::Identifier { .. } => ExpressionErrorKind::IdentifierUnterminated,
            ParseMode::List { .. } => ExpressionErrorKind::ListUnterminated,
            ParseMode::StringLiteral(_) => ExpressionErrorKind::StrUnterminated,
            ParseMode::Vector(_) => ExpressionErrorKind::VectorUnterminated,
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
            ParseMode::Identifier { name, quoted } => {
                Ok(Some(identifier_to_expr(name, quoted, value.ctx)))
            }
            ParseMode::List { form, seq } => into_syntactic_form(form, seq, value.ctx),
            ParseMode::Quote { inner, quoted } => into_datum(inner, value.ctx, quoted),
            ParseMode::StringLiteral(s) => Ok(Some(Expression::string(s, value.ctx))),
            ParseMode::Vector(seq) => into_vector(seq, value.ctx),
        }
    }
}

#[derive(Debug)]
pub(super) enum ParseErrBreak {
    FailedNode,
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

    fn node_failure(err: ExpressionError) -> Self {
        Self::Err {
            err,
            flow: ParseErrFlow::Break(ParseErrBreak::FailedNode),
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

#[derive(Clone, Copy, Debug)]
enum SyntacticForm {
    Call,
    Datum,
    PairClosed,
    PairOpen,
    Quote,
}

impl SyntacticForm {
    fn from_str(s: &str) -> Option<Self> {
        match s {
            LONGFORM_QUOTE => Some(Self::Quote),
            _ => None,
        }
    }

    fn quoted(self) -> bool {
        matches!(
            self,
            Self::Datum | Self::PairClosed | Self::PairOpen | Self::Quote
        )
    }

    fn parse_list(
        &mut self,
        seq: &mut Vec<Expression>,
        token: Token,
        txt: &Rc<TextLine>,
    ) -> ParseFlow {
        match token.kind {
            TokenKind::PairJoiner => self.dotted_pair(seq, token, txt),
            TokenKind::ParenRight => self.close_list(token, txt),
            _ => self.next_item(seq, token, txt),
        }
    }

    fn merge(
        &mut self,
        seq: &mut Vec<Expression>,
        other: ExprNode,
        node_ctx: &ExprCtx,
    ) -> MergeResult {
        if let Some(expr) = other.try_into()? {
            match self {
                Self::PairClosed => {
                    return Err(ParserError::Syntax(SyntaxError(vec![
                        node_ctx
                            .clone()
                            .into_error(ExpressionErrorKind::PairUnterminated),
                    ])));
                }
                Self::PairOpen => {
                    *self = Self::PairClosed;
                }
                _ => (),
            }
            seq.push(expr);
        }
        Ok(MergeFlow::Continue(()))
    }

    fn close_list(&mut self, token: Token, txt: &Rc<TextLine>) -> ParseFlow {
        ParseFlow::Break(if let Self::PairOpen = self {
            ParseBreak::node_failure(
                ExprCtx {
                    span: token.span,
                    txt: Rc::clone(txt),
                }
                .into_error(ExpressionErrorKind::PairUnterminated),
            )
        } else {
            ParseBreak::complete(txt.lineno, token.span.end)
        })
    }

    fn dotted_pair(
        &mut self,
        seq: &mut [Expression],
        token: Token,
        txt: &Rc<TextLine>,
    ) -> ParseFlow {
        let err = match self {
            Self::Datum => {
                if seq.is_empty() {
                    ExpressionErrorKind::PairIncomplete
                } else {
                    *self = Self::PairOpen;
                    return ParseFlow::Continue(());
                }
            }
            Self::PairClosed | Self::PairOpen => {
                *self = Self::Datum;
                ExpressionErrorKind::PairUnterminated
            }
            _ => ExpressionErrorKind::PairUnexpected,
        };
        ParseFlow::Break(ParseBreak::recover(
            ExprCtx {
                span: token.span,
                txt: Rc::clone(txt),
            }
            .into_error(err),
        ))
    }

    fn next_item(
        &mut self,
        seq: &mut Vec<Expression>,
        token: Token,
        txt: &Rc<TextLine>,
    ) -> ParseFlow {
        let quoted = self.quoted();
        let token_span = token.span.clone();
        if let Some(expr) = parse_expr(token, txt, quoted)? {
            match self {
                Self::PairClosed => {
                    *self = Self::Datum;
                    return ParseFlow::Break(ParseBreak::recover(
                        ExprCtx {
                            span: token_span,
                            txt: Rc::clone(txt),
                        }
                        .into_error(ExpressionErrorKind::PairUnterminated),
                    ));
                }
                Self::PairOpen => *self = Self::PairClosed,
                _ => (),
            }
            if !quoted && seq.is_empty() {
                if let ExpressionKind::Variable(n) = &expr.kind {
                    // TODO: check for shadowed keywords here
                    if let Some(f) = Self::from_str(n) {
                        *self = f;
                    }
                }
            }
            seq.push(expr);
        }
        ParseFlow::Continue(())
    }
}

#[derive(Debug)]
enum ParseMode {
    ByteVector(Vec<Expression>),
    CommentBlock,
    CommentDatum(Option<Expression>),
    Identifier {
        name: String,
        quoted: bool,
    },
    List {
        form: SyntacticForm,
        seq: Vec<Expression>,
    },
    Quote {
        inner: Option<Expression>,
        quoted: bool,
    },
    StringLiteral(String),
    Vector(Vec<Expression>),
}

impl ParseMode {
    fn identifier(mut name: String, quoted: bool) -> Self {
        name.push('\n');
        Self::Identifier { name, quoted }
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
        _ => ParseFlow::Break(ParseBreak::token_failure(
            ExprCtx {
                span: token.span,
                txt: Rc::clone(txt),
            }
            .into_error(ExpressionErrorKind::CommentBlockInvalid(token.kind)),
        )),
    }
}

fn parse_datum(
    inner: &mut Option<Expression>,
    token: Token,
    txt: &Rc<TextLine>,
    node_ctx: &ExprCtx,
) -> ParseFlow {
    if let TokenKind::ParenRight = token.kind {
        let ctx = extend_node_to_token(token.span.end, txt, node_ctx);
        ParseFlow::Break(ParseBreak::node_failure(
            ctx.into_error(ExpressionErrorKind::DatumExpected),
        ))
    } else {
        let pos = token.span.end;
        parse_expr(token, txt, true)?.map_or(ParseFlow::Continue(()), |expr| {
            inner.replace(expr);
            ParseFlow::Break(ParseBreak::Complete(ExprEnd {
                lineno: txt.lineno,
                pos,
            }))
        })
    }
}

fn parse_expr(token: Token, txt: &Rc<TextLine>, quoted: bool) -> ExprFlow {
    match token.kind {
        TokenKind::Boolean(b) => ExprFlow::Continue(Some(
            ExprCtx {
                span: token.span,
                txt: Rc::clone(txt),
            }
            .into_expr(ExpressionKind::Literal(Value::Boolean(b))),
        )),
        TokenKind::ByteVector => ExprFlow::Break(ParseBreak::new(
            ParseMode::ByteVector(Vec::new()),
            token.span.start,
        )),
        TokenKind::Character(c) => ExprFlow::Continue(Some(
            ExprCtx {
                span: token.span,
                txt: Rc::clone(txt),
            }
            .into_expr(ExpressionKind::Literal(Value::Character(c))),
        )),
        TokenKind::Comment => ExprFlow::Continue(None),
        TokenKind::CommentBlockBegin { .. } => {
            ExprFlow::Break(ParseBreak::new(ParseMode::CommentBlock, token.span.start))
        }
        TokenKind::CommentDatum => ExprFlow::Break(ParseBreak::new(
            ParseMode::CommentDatum(None),
            token.span.start,
        )),
        TokenKind::Imaginary(r) => ExprFlow::Continue(Some(
            ExprCtx {
                span: token.span,
                txt: Rc::clone(txt),
            }
            .into_expr(ExpressionKind::Literal(Value::Number(Number::imaginary(r)))),
        )),
        TokenKind::Identifier(s) => ExprFlow::Continue(Some(identifier_to_expr(
            s,
            quoted,
            ExprCtx {
                span: token.span,
                txt: Rc::clone(txt),
            },
        ))),
        TokenKind::IdentifierBegin(s) => ExprFlow::Break(ParseBreak::new(
            ParseMode::identifier(s, quoted),
            token.span.start,
        )),
        TokenKind::Number(n) => ExprFlow::Continue(Some(
            ExprCtx {
                span: token.span,
                txt: Rc::clone(txt),
            }
            .into_expr(ExpressionKind::Literal(Value::Number(n))),
        )),
        TokenKind::PairJoiner => ExprFlow::Break(ParseBreak::recover(
            ExprCtx {
                span: token.span,
                txt: Rc::clone(txt),
            }
            .into_error(ExpressionErrorKind::PairUnexpected),
        )),
        TokenKind::ParenLeft => ExprFlow::Break(ParseBreak::new(
            ParseMode::List {
                form: if quoted {
                    SyntacticForm::Datum
                } else {
                    SyntacticForm::Call
                },
                seq: Vec::new(),
            },
            token.span.start,
        )),
        TokenKind::Quote => ExprFlow::Break(ParseBreak::new(
            ParseMode::Quote {
                inner: None,
                quoted,
            },
            token.span.start,
        )),
        TokenKind::String(s) => ExprFlow::Continue(Some(Expression::string(
            s,
            ExprCtx {
                span: token.span,
                txt: Rc::clone(txt),
            },
        ))),
        TokenKind::StringBegin { s, line_cont } => ExprFlow::Break(ParseBreak::new(
            ParseMode::string(s, !line_cont),
            token.span.start,
        )),
        TokenKind::Vector => ExprFlow::Break(ParseBreak::new(
            ParseMode::Vector(Vec::new()),
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
        | TokenKind::StringFragment { .. } => ExprFlow::Break(ParseBreak::token_failure(
            ExprCtx {
                span: token.span,
                txt: Rc::clone(txt),
            }
            .into_error(ExpressionErrorKind::SeqInvalid(token.kind)),
        )),
        _ => ExprFlow::Break(ParseBreak::recover(
            ExprCtx {
                span: token.span,
                txt: Rc::clone(txt),
            }
            .into_error(ExpressionErrorKind::Unimplemented(token.kind)),
        )),
    }
}

fn parse_vector(seq: &mut Vec<Expression>, token: Token, txt: &Rc<TextLine>) -> ParseFlow {
    if let TokenKind::ParenRight = token.kind {
        ParseFlow::Break(ParseBreak::complete(txt.lineno, token.span.end))
    } else {
        if let Some(expr) = parse_expr(token, txt, true)? {
            seq.push(expr);
        }
        ParseFlow::Continue(())
    }
}

fn parse_sequence(seq: &mut Vec<Expression>, token: Token, txt: &Rc<TextLine>) -> ParseFlow {
    if let Some(expr) = parse_expr(token, txt, false)? {
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
        _ => ParseFlow::Break(ParseBreak::token_failure(
            ExprCtx {
                span: token.span,
                txt: Rc::clone(txt),
            }
            .into_error(ExpressionErrorKind::StrInvalid(token.kind)),
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
            ParseFlow::Break(ParseBreak::complete(txt.lineno, token.span.end))
        }
        _ => ParseFlow::Break(ParseBreak::token_failure(
            ExprCtx {
                span: token.span,
                txt: Rc::clone(txt),
            }
            .into_error(ExpressionErrorKind::IdentifierInvalid(token.kind)),
        )),
    }
}

fn into_bytevector(seq: Vec<Expression>, ctx: ExprCtx) -> ExprConvertResult {
    into_valid_sequence(
        seq,
        ctx,
        |expr| match expr.kind {
            ExpressionKind::Literal(Value::Number(n)) => n.try_into().map_err(|err| {
                expr.ctx
                    .into_error(ExpressionErrorKind::ByteVectorInvalidNumber(err))
            }),
            _ => Err(expr
                .ctx
                .into_error(ExpressionErrorKind::ByteVectorInvalidItem(expr.kind))),
        },
        |items| ExpressionKind::Literal(Value::ByteVector(items.into())),
    )
}

fn into_vector(seq: Vec<Expression>, ctx: ExprCtx) -> ExprConvertResult {
    into_valid_sequence(
        seq,
        ctx,
        |expr| match expr.kind {
            ExpressionKind::Literal(v) => Ok(v),
            _ => Err(expr
                .ctx
                .into_error(ExpressionErrorKind::VectorInvalidItem(expr.kind))),
        },
        |items| ExpressionKind::Literal(Value::vector(items)),
    )
}

fn into_comment_datum(inner: Option<&Expression>, ctx: ExprCtx) -> ExprConvertResult {
    inner.map_or_else(
        || Err(vec![ctx.into_error(ExpressionErrorKind::DatumExpected)]),
        |_| Ok(None),
    )
}

fn into_datum(inner: Option<Expression>, ctx: ExprCtx, quoted: bool) -> ExprConvertResult {
    match inner {
        None => Err(vec![ctx.into_error(ExpressionErrorKind::DatumExpected)]),
        Some(expr) => {
            if let ExpressionKind::Literal(val) = expr.kind {
                Ok(Some(if quoted {
                    ctx.into_expr(ExpressionKind::Literal(zlist![
                        Value::symbol(LONGFORM_QUOTE),
                        val
                    ]))
                } else {
                    // TODO: can i remove this redundant ctor somehow (it recreates expr)
                    expr.ctx.into_expr(ExpressionKind::Literal(val))
                }))
            } else {
                let mut expr_ctx = expr.ctx;
                expr_ctx.span.start = ctx.span.start;
                Err(vec![
                    expr_ctx.into_error(ExpressionErrorKind::DatumInvalid(expr.kind)),
                ])
            }
        }
    }
}

fn into_syntactic_form(
    form: SyntacticForm,
    seq: Vec<Expression>,
    ctx: ExprCtx,
) -> ExprConvertResult {
    match form {
        SyntacticForm::Call => {
            let mut iter = seq.into_iter();
            match iter.next() {
                None => Err(vec![ctx.into_error(ExpressionErrorKind::ProcedureEmpty)]),
                Some(proc) => Ok(Some(ctx.into_expr(ExpressionKind::Call {
                    args: iter.collect(),
                    proc: proc.into(),
                }))),
            }
        }
        SyntacticForm::Datum => into_list(seq, ctx, false),
        SyntacticForm::PairClosed => into_list(seq, ctx, true),
        SyntacticForm::PairOpen => Err(vec![ctx.into_error(ExpressionErrorKind::PairUnterminated)]),
        SyntacticForm::Quote => {
            debug_assert!(seq.len() == 2, "invalid syntax for quote");
            Ok(Some(seq.into_iter().next_back().unwrap()))
        }
    }
}

fn into_list(seq: Vec<Expression>, ctx: ExprCtx, improper: bool) -> ExprConvertResult {
    into_valid_sequence(
        seq,
        ctx,
        |expr| match expr.kind {
            ExpressionKind::Literal(val) => Ok(val),
            _ => Err(expr
                .ctx
                .into_error(ExpressionErrorKind::DatumInvalid(expr.kind))),
        },
        |vals| {
            ExpressionKind::Literal(if improper {
                Value::improper_list(vals)
            } else {
                Value::list(vals)
            })
        },
    )
}

fn into_valid_sequence<T>(
    seq: Vec<Expression>,
    ctx: ExprCtx,
    valid: impl FnMut(Expression) -> Result<T, ExpressionError>,
    kind: impl FnOnce(Vec<T>) -> ExpressionKind,
) -> ExprConvertResult {
    let (items, errs): (Vec<_>, Vec<_>) = seq.into_iter().map(valid).partition(Result::is_ok);
    if errs.is_empty() {
        Ok(Some(
            ctx.into_expr(kind(items.into_iter().flatten().collect())),
        ))
    } else {
        Err(errs.into_iter().filter_map(Result::err).collect::<Vec<_>>())
    }
}

fn identifier_to_expr(name: String, quoted: bool, ctx: ExprCtx) -> Expression {
    if quoted {
        Expression::symbol(name, ctx)
    } else {
        Expression::variable(name, ctx)
    }
}

fn extend_node_to_token(token_end: usize, txt: &Rc<TextLine>, node_ctx: &ExprCtx) -> ExprCtx {
    ExprCtx {
        span: node_ctx.span.start..if txt.lineno == node_ctx.txt.lineno {
            token_end
        } else {
            node_ctx.span.end
        },
        txt: Rc::clone(&node_ctx.txt),
    }
}
