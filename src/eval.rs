mod env;

use self::env::{Frame, SymbolTable};
use crate::{syntax::Program, value};
use std::{
    fmt::{self, Display, Formatter},
    rc::Rc,
};

#[derive(Debug)]
pub enum Evaluation {
    Continuation,
    Val(Option<Value>),
}

impl Evaluation {
    fn val(v: Option<Rc<value::Value>>) -> Self {
        Self::Val(Value::wrap(v))
    }

    #[must_use]
    pub fn display_message(&self) -> EvaluationMessage {
        EvaluationMessage(self)
    }
}

#[derive(Debug)]
pub struct Value(Rc<value::Value>);

impl Value {
    fn wrap(v: Option<Rc<value::Value>>) -> Option<Self> {
        Some(Self(v?))
    }

    #[must_use]
    pub fn as_datum(&self) -> Datum {
        Datum(self.0.as_datum())
    }
}

pub struct Datum<'a>(value::Datum<'a>);

impl Display for Datum<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

pub struct EvaluationMessage<'a>(&'a Evaluation);

impl Display for EvaluationMessage<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self.0 {
            Evaluation::Continuation => f.write_str("fatal error: unexpected continuation"),
            Evaluation::Val(None) => Ok(()),
            Evaluation::Val(Some(v)) => v.0.display_message().fmt(f),
        }
    }
}

pub(crate) trait Evaluator {
    fn evaluate(&self, prg: Program) -> Evaluation;
}

pub(crate) struct Ast;

impl Evaluator for Ast {
    fn evaluate(&self, prg: Program) -> Evaluation {
        Evaluation::val(Some(value::Value::Ast(prg).into()))
    }
}

pub(crate) struct Environment {
    global: Rc<Frame>,
    symbols: Rc<SymbolTable>,
}

impl Evaluator for Environment {
    fn evaluate(&self, prg: Program) -> Evaluation {
        Evaluation::val(prg.eval())
    }
}

impl Default for Environment {
    fn default() -> Self {
        let s = Rc::new(SymbolTable::default());
        Self {
            global: Frame::root(Rc::downgrade(&s)).into(),
            symbols: s,
        }
    }
}
