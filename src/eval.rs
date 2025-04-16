use crate::{
    lex::TokenLine,
    syntax::Program,
    value::{Datum as ValueDatum, Value},
};
use std::fmt::{self, Display, Formatter};

#[derive(Debug)]
pub enum Evaluation {
    Continuation,
    Value(Option<Val>),
}

impl Evaluation {
    fn val(v: Option<Value>) -> Self {
        Self::Value(Val::wrap(v))
    }

    #[must_use]
    pub fn display_message(&self) -> EvaluationMessage {
        EvaluationMessage(self)
    }
}

#[derive(Debug)]
pub struct Val(Value);

impl Val {
    pub(crate) fn tokens(tokens: impl Into<Box<[TokenLine]>>) -> Self {
        Self(Value::TokenList(tokens.into()))
    }

    fn wrap(v: Option<Value>) -> Option<Self> {
        Some(Self(v?))
    }

    #[must_use]
    pub fn as_datum(&self) -> Datum {
        Datum(self.0.as_datum())
    }
}

pub struct Datum<'a>(ValueDatum<'a>);

impl Display for Datum<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

pub struct EvaluationMessage<'a>(&'a Evaluation);

impl Display for EvaluationMessage<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self.0 {
            Evaluation::Continuation => "fatal error: unexpected continuation".fmt(f),
            Evaluation::Value(None) => Ok(()),
            Evaluation::Value(Some(v)) => v.0.display_message().fmt(f),
        }
    }
}

pub(crate) trait Evaluator {
    fn evaluate(&self, prg: Program) -> Evaluation;
}

pub(crate) struct Ast;

impl Evaluator for Ast {
    fn evaluate(&self, prg: Program) -> Evaluation {
        Evaluation::val(Some(Value::Ast(prg)))
    }
}

pub(crate) struct Environment;

impl Evaluator for Environment {
    fn evaluate(&self, prg: Program) -> Evaluation {
        Evaluation::val(prg.eval())
    }
}
