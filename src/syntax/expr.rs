#[cfg(test)]
mod tests;

use crate::{
    Exception,
    eval::{EvalResult, Frame, InvalidFormal, Operator},
    lex::TokenKind,
    number::NumericError,
    string::Symbol,
    txt::{LineNumber, TextLine, TxtSpan},
    value::{Condition, Value},
};
use std::{
    fmt::{self, Display, Formatter},
    iter::Peekable,
    rc::Rc,
};

#[derive(Debug)]
pub(crate) struct Sequence(Box<[Expression]>);

impl Sequence {
    pub(super) fn new(seq: impl Into<Box<[Expression]>>) -> Self {
        Self(seq.into())
    }

    pub(crate) fn eval(&self, env: &Frame) -> EvalResult {
        Ok(self
            .iter()
            .map(|expr| expr.eval(env))
            .collect::<Result<Vec<Value>, Exception>>()?
            .pop()
            .unwrap_or(Value::Unspecified))
    }

    pub(super) fn iter(&self) -> impl Iterator<Item = &Expression> {
        self.0.iter()
    }
}

#[derive(Debug)]
pub(super) struct ExpressionType<T> {
    pub(super) ctx: ExprCtx,
    pub(super) kind: T,
}

#[derive(Clone, Debug)]
pub(super) struct ExprCtx {
    pub(super) span: TxtSpan,
    pub(super) txt: Rc<TextLine>,
}

impl ExprCtx {
    pub(super) fn into_expr(self, kind: ExpressionKind) -> Expression {
        Expression { kind, ctx: self }
    }

    pub(super) fn into_error(self, kind: ExpressionErrorKind) -> ExpressionError {
        ExpressionError { kind, ctx: self }
    }
}

#[derive(Clone, Copy, Debug, Default)]
pub(super) struct ExprEnd {
    pub(super) lineno: LineNumber,
    pub(super) pos: usize,
}

pub(super) type Expression = ExpressionType<ExpressionKind>;

impl Expression {
    pub(super) fn string(value: impl Into<Rc<str>>, ctx: ExprCtx) -> Self {
        Self {
            ctx,
            kind: ExpressionKind::Literal(Value::string(value)),
        }
    }

    pub(super) fn symbol(name: Symbol, ctx: ExprCtx) -> Self {
        Self {
            ctx,
            kind: ExpressionKind::Literal(Value::Symbol(name)),
        }
    }

    pub(super) fn variable(name: Symbol, ctx: ExprCtx) -> Self {
        Self {
            ctx,
            kind: ExpressionKind::Variable(name),
        }
    }

    fn eval(&self, env: &Frame) -> EvalResult {
        self.kind.eval(env)
    }
}

#[derive(Debug)]
pub(super) enum ExpressionKind {
    Call {
        args: Box<[Expression]>,
        proc: Box<Expression>,
    },
    Define {
        name: Symbol,
        expr: Option<Box<Expression>>,
    },
    If {
        test: Box<Expression>,
        con: Box<Expression>,
        alt: Option<Box<Expression>>,
    },
    Literal(Value),
    Set {
        var: Symbol,
        expr: Box<Expression>,
    },
    Variable(Symbol),
}

impl ExpressionKind {
    fn as_typename(&self) -> TypeName {
        TypeName(self)
    }

    fn eval(&self, env: &Frame) -> EvalResult {
        match self {
            Self::Call { args, proc } => eval_call(proc, args, env),
            Self::Define { name, expr } => {
                let val = expr
                    .as_ref()
                    .map_or(Ok(Value::Unspecified), |expr| expr.eval(env))?;
                env.scope.bind(name.clone(), val);
                Ok(Value::Unspecified)
            }
            Self::If { test, con, alt } => {
                if let Value::Boolean(false) = test.eval(env)? {
                    alt.as_ref()
                        .map_or(Ok(Value::Unspecified), |expr| expr.eval(env))
                } else {
                    con.eval(env)
                }
            }
            Self::Literal(v) => Ok(v.clone()),
            Self::Set { var, expr } => {
                if let Some(b) = env.scope.binding(var) {
                    let val = expr.eval(env)?;
                    b.bind(var.clone(), val);
                    Ok(Value::Unspecified)
                } else {
                    Err(Exception::signal(Condition::bind_error(var)))
                }
            }
            Self::Variable(n) => env
                .scope
                .lookup(n)
                .ok_or_else(|| Exception::signal(Condition::bind_error(n))),
        }
    }
}

pub(super) type ExpressionError = ExpressionType<ExpressionErrorKind>;

impl Display for ExpressionError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        self.kind.fmt(f)
    }
}

#[derive(Debug)]
pub(super) enum ExpressionErrorKind {
    ByteVectorInvalidItem(ExpressionKind),
    ByteVectorInvalidNumber(NumericError),
    ByteVectorUnterminated,
    CommentBlockInvalid(TokenKind),
    CommentBlockUnterminated,
    DatumExpected,
    DatumInvalid(ExpressionKind),
    DefineInvalid,
    DefineNotAllowed,
    IdentifierInvalid(TokenKind),
    IdentifierUnterminated,
    IfInvalid,
    LambdaInvalid,
    LambdaInvalidFormal(InvalidFormal),
    LambdaInvalidSignature,
    ListUnterminated,
    PairIncomplete,
    PairUnexpected,
    PairUnterminated,
    ProcedureEmpty,
    QuoteInvalid,
    SeqInvalid(TokenKind),
    SetInvalid,
    StrInvalid(TokenKind),
    StrUnterminated,
    Unimplemented(TokenKind),
    VectorInvalidItem(ExpressionKind),
    VectorUnterminated,
}

impl Display for ExpressionErrorKind {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::ByteVectorInvalidItem(k) => {
                write!(f, "expected byte literal, got: {}", k.as_typename())
            }
            Self::ByteVectorInvalidNumber(err) => err.fmt(f),
            Self::ByteVectorUnterminated => f.write_str("unterminated bytevector"),
            Self::CommentBlockInvalid(t) => format_unexpected_token("comment block", t, f),
            // TODO: can i share tokenerrorkind display here
            Self::CommentBlockUnterminated => f.write_str("unterminated block comment"),
            Self::DatumExpected => f.write_str("expected datum"),
            Self::DatumInvalid(k) => write!(f, "unexpected datum type: {}", k.as_typename()),
            Self::DefineInvalid => {
                // TODO: have lambda form mode as well
                format_invalid_form("(define <identifier> [expression])", f)
            }
            Self::DefineNotAllowed => f.write_str("define not allowed in this context"),
            Self::IdentifierInvalid(t) => format_unexpected_token("verbatim identifier", t, f),
            Self::IdentifierUnterminated => f.write_str("unterminated verbatim identifier"),
            Self::IfInvalid => format_invalid_form("(if <test> <consequent> [alternate])", f),
            Self::LambdaInvalid => format_invalid_form("(lambda <formals> <body>)", f),
            Self::LambdaInvalidFormal(err) => err.fmt(f),
            Self::LambdaInvalidSignature => f.write_str("invalid formals syntax"),
            Self::ListUnterminated => f.write_str("unterminated list expression"),
            Self::PairIncomplete => f.write_str("missing first pair expression"),
            Self::PairUnexpected => f.write_str("unexpected pair syntax"),
            Self::PairUnterminated => f.write_str("unterminated pair expression"),
            Self::ProcedureEmpty => f.write_str("empty procedure call"),
            Self::QuoteInvalid => format_invalid_form("(quote <datum>)", f),
            Self::SeqInvalid(t) => format_unexpected_token("sequence", t, f),
            Self::SetInvalid => format_invalid_form("(set! <variable> <expression>)", f),
            Self::StrInvalid(t) => format_unexpected_token("string", t, f),
            Self::StrUnterminated => f.write_str("unterminated string literal"),
            Self::Unimplemented(t) => write!(f, "{t} parsing not yet implemented"),
            Self::VectorInvalidItem(k) => {
                write!(f, "unexpected vector item type: {}", k.as_typename())
            }
            Self::VectorUnterminated => f.write_str("unterminated vector"),
        }
    }
}

pub(super) struct GroupBy<I> {
    peek: I,
}

impl<'a, I: Iterator<Item = &'a ExpressionError>> Iterator for GroupBy<Peekable<I>> {
    type Item = (&'a TextLine, Vec<&'a ExpressionError>);

    // NOTE: this assumes grouped items are contiguous in the original sequence
    fn next(&mut self) -> Option<Self::Item> {
        let start = self.peek.next()?;
        let key = &start.ctx.txt;
        let mut group = vec![start];
        while let Some(err) = self.peek.next_if(|err| Rc::ptr_eq(key, &err.ctx.txt)) {
            group.push(err);
        }
        Some((Rc::as_ref(key), group))
    }
}

pub(super) trait PeekableExt {
    fn groupby_txt(self) -> GroupBy<Self>
    where
        Self: Sized;
}

impl<'a, I: Iterator<Item = &'a ExpressionError>> PeekableExt for Peekable<I> {
    fn groupby_txt(self) -> GroupBy<Self> {
        GroupBy { peek: self }
    }
}

struct TypeName<'a>(&'a ExpressionKind);

impl Display for TypeName<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.0 {
            ExpressionKind::Call { .. } => f.write_str("procedure call"),
            ExpressionKind::Define { .. } => f.write_str("definition"),
            ExpressionKind::If { .. } => f.write_str("conditional"),
            ExpressionKind::Literal(val) => val.as_typename().fmt(f),
            ExpressionKind::Set { .. } => f.write_str("assignment"),
            ExpressionKind::Variable(_) => f.write_str("variable"),
        }
    }
}

fn eval_call(expr: &Expression, args: &[Expression], env: &Frame) -> EvalResult {
    let v = expr.eval(env)?;
    match v {
        Value::Intrinsic(proc) => apply_op(proc.as_ref(), args, env),
        Value::Procedure(proc) => apply_op(proc.as_ref(), args, env),
        _ => Err(Condition::proc_error(v.as_typename()).into()),
    }
}

fn apply_op(op: &impl Operator, args: &[Expression], env: &Frame) -> EvalResult {
    if op.matches_arity(args.len()) {
        let args = args
            .iter()
            .map(|expr| expr.eval(env))
            .collect::<Result<Vec<Value>, Exception>>()?;
        op.apply(&args, env)
    } else {
        Err(Condition::arity_error(op.name(), op.arity(), args.len()).into())
    }
}

fn format_unexpected_token(kind: &str, token: &TokenKind, f: &mut Formatter) -> fmt::Result {
    write!(f, "unexpected token in {kind}: {token}")
}

fn format_invalid_form(form: &str, f: &mut Formatter) -> fmt::Result {
    write!(f, "invalid form, expected: {form}")
}
