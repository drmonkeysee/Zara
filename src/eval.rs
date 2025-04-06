use crate::{
    syntax::Expression,
    value::{Datum as ValueDatum, Value},
};
use std::fmt::{self, Display, Formatter};

#[derive(Debug)]
pub enum Evaluation {
    Continuation,
    Value(Val),
}

impl Evaluation {
    #[must_use]
    pub fn display_message(&self) -> EvaluationMessage {
        EvaluationMessage(self)
    }

    fn val(v: Value) -> Self {
        Self::Value(Val(v))
    }
}

#[derive(Debug)]
pub struct Val(Value);

impl Val {
    #[must_use]
    pub fn has_value(&self) -> bool {
        self.0.has_value()
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
            Evaluation::Value(val) => val.0.display_message().fmt(f),
            Evaluation::Continuation => "fatal error: unexpected continuation".fmt(f),
        }
    }
}

pub(crate) trait Evaluator {
    fn evaluate(&self, expression: Expression) -> Evaluation;
}

pub(crate) struct Ast;

impl Evaluator for Ast {
    fn evaluate(&self, expression: Expression) -> Evaluation {
        Evaluation::val(Value::Ast(expression.into()))
    }
}

pub(crate) struct Environment;

impl Evaluator for Environment {
    fn evaluate(&self, expression: Expression) -> Evaluation {
        Evaluation::val(expression.eval())
    }
}
