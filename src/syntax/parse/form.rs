use super::{
    ExprConvertResult, ExprCtx, ExprNode, Expression, ExpressionErrorKind, ExpressionKind,
    MergeFlow, MergeResult, ParseBreak, ParseFlow, ParserError, Sequence, SyntaxError,
};
use crate::{
    eval::{Lambda, Namespace},
    lex::{Token, TokenKind},
    string::Symbol,
    txt::TextLine,
    value::Value,
};
use std::{iter, rc::Rc};

#[derive(Clone, Copy, Debug)]
pub(super) enum SyntacticForm {
    Call,
    Datum,
    Define,
    If,
    Lambda,
    PairClosed,
    PairOpen,
    Quote,
    Set,
}

impl SyntacticForm {
    pub(super) const QUOTE: &str = "quote";
    const DEFINE: &str = "define";
    const IF: &str = "if";
    const LAMBDA: &str = "lambda";
    const SET: &str = "set!";

    fn from_str(s: &str) -> Option<Self> {
        match s {
            Self::DEFINE => Some(Self::Define),
            Self::IF => Some(Self::If),
            Self::LAMBDA => Some(Self::Lambda),
            Self::QUOTE => Some(Self::Quote),
            Self::SET => Some(Self::Set),
            _ => None,
        }
    }

    fn quoted(self, seq_len: usize) -> bool {
        matches!(
            self,
            Self::Datum | Self::PairClosed | Self::PairOpen | Self::Quote
        ) || (matches!(self, Self::Define | Self::Lambda) && seq_len == 0)
    }

    pub(super) fn parse_list(
        &mut self,
        seq: &mut Vec<Expression>,
        token: Token,
        txt: &Rc<TextLine>,
        ns: &Namespace,
    ) -> ParseFlow {
        match token.kind {
            TokenKind::PairJoiner => self.dotted_pair(seq, token, txt),
            TokenKind::ParenRight => self.close_list(token, txt),
            _ => self.next_item(seq, token, txt, ns),
        }
    }

    pub(super) fn merge(
        &mut self,
        seq: &mut Vec<Expression>,
        other: ExprNode,
        node_ctx: &ExprCtx,
        ns: &Namespace,
    ) -> MergeResult {
        if let Some(expr) = other.try_into_expr(ns)? {
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
            if let ExpressionKind::Define { .. } = expr.kind {
                // TODO: allow for body or top-level begin
                return Err(ParserError::Syntax(SyntaxError(vec![
                    expr.ctx.into_error(ExpressionErrorKind::DefineNotAllowed),
                ])));
            }
            seq.push(expr);
        }
        Ok(MergeFlow::Continue(()))
    }

    pub(super) fn try_into_expr(self, seq: Vec<Expression>, ctx: ExprCtx) -> ExprConvertResult {
        match self {
            Self::Call => {
                let mut it = seq.into_iter();
                match it.next() {
                    None => Err(vec![ctx.into_error(ExpressionErrorKind::ProcedureEmpty)]),
                    Some(proc) => Ok(Some(ctx.into_expr(ExpressionKind::Call {
                        args: it.collect(),
                        proc: proc.into(),
                    }))),
                }
            }
            Self::Datum => into_list(seq, ctx, false),
            Self::Define => {
                if !seq.is_empty() {
                    let len = seq.len();
                    let mut it = seq.into_iter();
                    let binding = it.next().unwrap();
                    match binding.kind {
                        ExpressionKind::Literal(Value::Symbol(n)) => {
                            if len < 3 {
                                return Ok(Some(ctx.into_expr(ExpressionKind::Define {
                                    name: n,
                                    expr: it.next().map(Box::new),
                                })));
                            }
                        }
                        ExpressionKind::Literal(Value::Pair(Some(p))) => {
                            if let Value::Symbol(name) = &p.car {
                                let formals = binding
                                    .ctx
                                    .clone()
                                    .into_expr(ExpressionKind::Literal(p.cdr.clone()));
                                let lm = into_lambda(
                                    iter::once(formals).chain(it).collect::<Vec<_>>(),
                                    ctx.clone(),
                                )?;
                                return Ok(Some(ctx.into_expr(ExpressionKind::Define {
                                    name: name.clone(),
                                    expr: lm.map(Box::new),
                                })));
                            }
                        }
                        _ => (),
                    }
                }
                Err(vec![ctx.into_error(ExpressionErrorKind::DefineInvalid)])
            }
            Self::If => {
                if (2..4).contains(&seq.len()) {
                    let mut it = seq.into_iter();
                    Ok(Some(ctx.into_expr(ExpressionKind::If {
                        test: it.next().unwrap().into(),
                        con: it.next().unwrap().into(),
                        alt: it.next().map(Box::new),
                    })))
                } else {
                    Err(vec![ctx.into_error(ExpressionErrorKind::IfInvalid)])
                }
            }
            Self::Lambda => into_lambda(seq, ctx),
            Self::PairClosed => into_list(seq, ctx, true),
            Self::PairOpen => Err(vec![ctx.into_error(ExpressionErrorKind::PairUnterminated)]),
            Self::Quote => {
                if seq.len() == 1 {
                    Ok(Some(seq.into_iter().next().unwrap()))
                } else {
                    Err(vec![ctx.into_error(ExpressionErrorKind::QuoteInvalid)])
                }
            }
            Self::Set => {
                if seq.len() == 2 {
                    let mut it = seq.into_iter();
                    let var = it.next().unwrap();
                    if let ExpressionKind::Variable(n) = var.kind {
                        return Ok(Some(ctx.into_expr(ExpressionKind::Set {
                            var: n,
                            expr: it.next().unwrap().into(),
                        })));
                    }
                }
                Err(vec![ctx.into_error(ExpressionErrorKind::SetInvalid)])
            }
        }
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
        ns: &Namespace,
    ) -> ParseFlow {
        let token_span = token.span.clone();
        if let Some(expr) = super::parse_expr(token, txt, self.quoted(seq.len()), ns)? {
            match self {
                Self::Call => {
                    if seq.is_empty()
                        && let ExpressionKind::Variable(n) = &expr.kind
                        && !ns.name_defined(n)
                        && let Some(f) = Self::from_str(n)
                    {
                        *self = f;
                        return ParseFlow::Continue(());
                    }
                }
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
            seq.push(expr);
        }
        ParseFlow::Continue(())
    }
}

fn into_list(seq: Vec<Expression>, ctx: ExprCtx, improper: bool) -> ExprConvertResult {
    super::into_valid_sequence(
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

fn into_lambda(seq: Vec<Expression>, ctx: ExprCtx) -> ExprConvertResult {
    if seq.len() < 2 {
        return Err(vec![ctx.into_error(ExpressionErrorKind::LambdaInvalid)]);
    }
    let mut it = seq.into_iter();
    let formals = it.next().unwrap();
    let ExpressionKind::Literal(params) = formals.kind else {
        return Err(vec![ctx.into_error(ExpressionErrorKind::LambdaInvalid)]);
    };
    let (args, rest) =
        parse_formals(params).map_err(|err| vec![formals.ctx.clone().into_error(err)])?;
    make_lambda(args, rest, it, &formals.ctx, ctx)
}

fn parse_formals(mut params: Value) -> Result<(Vec<Symbol>, Option<Symbol>), ExpressionErrorKind> {
    let mut args = Vec::new();
    let mut rest = None;
    loop {
        match params {
            Value::Pair(None) => break,
            Value::Pair(Some(p)) => {
                if let Value::Symbol(n) = &p.car {
                    args.push(n.clone());
                    params = p.cdr.clone();
                    continue;
                }
            }
            Value::Symbol(n) => {
                rest = Some(n.clone());
                break;
            }
            _ => (),
        }
        return Err(ExpressionErrorKind::LambdaInvalidSignature);
    }
    Ok((args, rest))
}

fn make_lambda(
    args: impl IntoIterator<Item = Symbol>,
    rest: Option<Symbol>,
    body: impl IntoIterator<Item = Expression>,
    formals_ctx: &ExprCtx,
    ctx: ExprCtx,
) -> ExprConvertResult {
    Lambda::new(
        args,
        rest,
        Sequence::new(body.into_iter().collect::<Box<[_]>>()),
    )
    .map_or_else(
        |err| {
            Err(err
                .into_iter()
                .map(|e| {
                    formals_ctx
                        .clone()
                        .into_error(ExpressionErrorKind::LambdaInvalidFormal(e))
                })
                .collect())
        },
        |lm| Ok(Some(Expression::lambda(lm, ctx))),
    )
}
