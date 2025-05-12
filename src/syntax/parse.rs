#[cfg(test)]
mod tests;

use super::{
    InvalidParseError, ParserError, Program, SyntaxError,
    expr::{ExprCtx, ExprEnd, Expression, ExpressionError, ExpressionErrorKind, ExpressionKind},
};
use crate::{
    constant::Constant,
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
            ParseMode::ByteVector(seq) => SyntacticForm::Datum.parse_list(seq, token, txt),
            ParseMode::CommentBlock => parse_comment_block(token, txt),
            ParseMode::CommentDatum(inner) | ParseMode::Quote { inner, .. } => {
                parse_datum(inner, token, txt, &self.ctx)
            }
            ParseMode::Identifier { label, .. } => parse_verbatim_identifier(label, token, txt),
            ParseMode::List { form, seq } => form.parse_list(seq, token, txt),
            ParseMode::StringLiteral(buf) => parse_str(buf, token, txt),
        }
    }

    fn merge(&mut self, other: ExprNode) -> MergeResult {
        match &mut self.mode {
            ParseMode::ByteVector(seq) => other.merge_into(|expr| {
                seq.push(expr);
                MergeFlow::Continue(())
            }),
            ParseMode::CommentDatum(inner) | ParseMode::Quote { inner, .. } => {
                other.merge_into(|expr| {
                    inner.replace(expr);
                    MergeFlow::Break(())
                })
            }
            ParseMode::List { form, seq } => {
                match form {
                    SyntacticForm::PairClosed => {
                        return Err(ParserError::Syntax(SyntaxError(vec![
                            self.ctx
                                .clone()
                                .into_error(ExpressionErrorKind::PairUnterminated),
                        ])));
                    }
                    SyntacticForm::PairOpen => {
                        *form = SyntacticForm::PairClosed;
                    }
                    _ => (),
                }
                other.merge_into(|expr| {
                    seq.push(expr);
                    MergeFlow::Continue(())
                })
            }
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
            ParseMode::Identifier { label, quoted } => {
                Ok(Some(label_to_expr(label, quoted, value.ctx)))
            }
            ParseMode::List { form, seq } => into_syntactic_form(form, seq, value.ctx),
            ParseMode::Quote { inner, quoted } => into_datum(inner, value.ctx, quoted),
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

    fn parser_failure(err: ExpressionError) -> Self {
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
            LONGFORM_QUOTE => Some(SyntacticForm::Quote),
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
            TokenKind::ParenRight => {
                if let SyntacticForm::PairOpen = self {
                    ParseFlow::Break(ParseBreak::parser_failure(
                        ExprCtx {
                            span: token.span,
                            txt: Rc::clone(txt),
                        }
                        .into_error(ExpressionErrorKind::PairUnterminated),
                    ))
                } else {
                    ParseFlow::Break(ParseBreak::complete(txt.lineno, token.span.end))
                }
            }
            TokenKind::PairJoiner => match self {
                SyntacticForm::Datum => {
                    if seq.is_empty() {
                        ParseFlow::Break(ParseBreak::recover(
                            ExprCtx {
                                span: token.span,
                                txt: Rc::clone(txt),
                            }
                            .into_error(ExpressionErrorKind::PairIncomplete),
                        ))
                    } else {
                        *self = SyntacticForm::PairOpen;
                        ParseFlow::Continue(())
                    }
                }
                SyntacticForm::PairClosed | SyntacticForm::PairOpen => {
                    *self = SyntacticForm::Datum;
                    ParseFlow::Break(ParseBreak::recover(
                        ExprCtx {
                            span: token.span,
                            txt: Rc::clone(txt),
                        }
                        .into_error(ExpressionErrorKind::PairUnterminated),
                    ))
                }
                _ => ParseFlow::Break(ParseBreak::recover(
                    ExprCtx {
                        span: token.span,
                        txt: Rc::clone(txt),
                    }
                    .into_error(ExpressionErrorKind::PairUnexpected),
                )),
            },
            _ => {
                let quoted = self.quoted();
                let token_span = token.span.clone();
                if let Some(expr) = parse_expr(token, txt, quoted)? {
                    match self {
                        SyntacticForm::PairClosed => {
                            *self = SyntacticForm::Datum;
                            return ParseFlow::Break(ParseBreak::recover(
                                ExprCtx {
                                    span: token_span,
                                    txt: Rc::clone(txt),
                                }
                                .into_error(ExpressionErrorKind::PairUnterminated),
                            ));
                        }
                        SyntacticForm::PairOpen => *self = SyntacticForm::PairClosed,
                        _ => (),
                    };
                    if !quoted && seq.is_empty() {
                        if let ExpressionKind::Variable(lbl) = &expr.kind {
                            // TODO: check for shadowed keywords here
                            if let Some(f) = SyntacticForm::from_str(lbl) {
                                *self = f
                            }
                        }
                    }
                    seq.push(expr);
                }
                ParseFlow::Continue(())
            }
        }
    }
}

#[derive(Debug)]
enum ParseMode {
    ByteVector(Vec<Expression>),
    CommentBlock,
    CommentDatum(Option<Expression>),
    Identifier {
        label: String,
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
}

impl ParseMode {
    fn identifier(mut label: String, quoted: bool) -> Self {
        label.push('\n');
        Self::Identifier { label, quoted }
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
        ParseFlow::Break(ParseBreak::parser_failure(
            ctx.into_error(ExpressionErrorKind::DatumExpected),
        ))
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

fn parse_expr(token: Token, txt: &Rc<TextLine>, quoted: bool) -> ExprFlow {
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
        TokenKind::Identifier(s) => ExprFlow::Continue(Some(label_to_expr(
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
        TokenKind::PairJoiner => ExprFlow::Break(ParseBreak::recover(
            ExprCtx {
                span: token.span,
                txt: Rc::clone(txt),
            }
            .into_error(ExpressionErrorKind::PairUnexpected),
        )),
        TokenKind::Quote => ExprFlow::Break(ParseBreak::new(
            ParseMode::Quote {
                inner: None,
                quoted,
            },
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
            ExpressionKind::Literal(Value::Constant(Constant::Number(n))) => {
                n.try_into().map_err(|err| {
                    expr.ctx
                        .into_error(ExpressionErrorKind::ByteVectorInvalidNumber(err))
                })
            }
            _ => Err(expr
                .ctx
                .into_error(ExpressionErrorKind::ByteVectorInvalidItem(expr.kind))),
        },
        |items| ExpressionKind::Literal(Value::ByteVector(items)),
    )
}

fn into_comment_datum(inner: Option<&Expression>, ctx: ExprCtx) -> ExprConvertResult {
    match inner {
        None => Err(vec![ctx.into_error(ExpressionErrorKind::DatumExpected)]),
        Some(_) => Ok(None),
    }
}

fn into_datum(inner: Option<Expression>, ctx: ExprCtx, quoted: bool) -> ExprConvertResult {
    match inner {
        None => Err(vec![ctx.into_error(ExpressionErrorKind::DatumExpected)]),
        Some(expr) => match expr.kind {
            ExpressionKind::Literal(val) => Ok(Some(if quoted {
                ctx.into_expr(ExpressionKind::Literal(zlist![
                    Value::Symbol(LONGFORM_QUOTE.into()),
                    val
                ]))
            } else {
                // TODO: can i remove this redundant ctor somehow (it recreates expr)
                expr.ctx.into_expr(ExpressionKind::Literal(val))
            })),
            _ => {
                let mut expr_ctx = expr.ctx;
                expr_ctx.span.start = ctx.span.start;
                Err(vec![
                    expr_ctx.into_error(ExpressionErrorKind::DatumInvalid(expr.kind)),
                ])
            }
        },
    }
}

fn into_syntactic_form(
    form: SyntacticForm,
    seq: Vec<Expression>,
    ctx: ExprCtx,
) -> ExprConvertResult {
    match form {
        SyntacticForm::Call => {
            debug_assert!(
                !seq.is_empty(),
                "empty list is invalid syntax unless quoted"
            );
            let mut iter = seq.into_iter();
            let proc = iter.next().unwrap();
            Ok(Some(ctx.into_expr(ExpressionKind::Call {
                args: iter.collect(),
                proc: proc.into(),
            })))
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
    kind: impl FnOnce(Box<[T]>) -> ExpressionKind,
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

fn label_to_expr(label: String, quoted: bool, ctx: ExprCtx) -> Expression {
    if quoted {
        Expression::symbol(label, ctx)
    } else {
        Expression::variable(label, ctx)
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
