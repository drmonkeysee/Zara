mod env;
mod form;

pub(crate) use self::{
    env::{Frame, SymbolTable},
    form::{IntrinsicFn, Procedure},
};
use crate::{core, syntax::Program, value};
use std::{
    fmt::{self, Display, Formatter},
    rc::Rc,
};

#[derive(Debug)]
pub enum Evaluation {
    Continuation,
    Val(Value),
}

impl Evaluation {
    fn val(v: value::ValueRef) -> Self {
        Self::Val(Value(v))
    }

    #[must_use]
    pub fn display_message(&self) -> EvaluationMessage {
        EvaluationMessage(self)
    }
}

#[derive(Debug)]
pub struct Value(value::ValueRef);

impl Value {
    #[must_use]
    pub fn is_unspecified(&self) -> bool {
        self.0.is_none()
    }

    #[must_use]
    pub fn as_datum(&self) -> Datum {
        Datum(self.0.as_ref().map(|v| v.as_datum()))
    }
}

pub struct Datum<'a>(Option<value::Datum<'a>>);

impl Display for Datum<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        self.0.as_ref().map_or(Ok(()), |d| d.fmt(f))
    }
}

pub struct EvaluationMessage<'a>(&'a Evaluation);

impl Display for EvaluationMessage<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self.0 {
            Evaluation::Continuation => f.write_str("fatal error: unexpected continuation"),
            Evaluation::Val(v) => v.0.as_ref().map_or(Ok(()), |v| v.display_message().fmt(f)),
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
        Evaluation::val(prg.eval(&self.global))
    }
}

impl Default for Environment {
    fn default() -> Self {
        let s = Rc::new(SymbolTable::default());
        let mut env = Frame::root(Rc::downgrade(&s));
        core::load(&mut env);
        Self {
            global: env.into(),
            symbols: s,
        }
    }
}
