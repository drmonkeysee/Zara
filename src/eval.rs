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
    Ex(Exception),
    Val(Value),
}

impl Evaluation {
    fn result(r: EvalResult) -> Self {
        match r {
            Err(ex) => Self::Ex(ex),
            Ok(v) => Self::Val(Value(v)),
        }
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
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        self.0.as_ref().map_or(Ok(()), |v| v.as_datum().fmt(f))
    }
}

#[derive(Debug)]
pub struct Exception(Rc<value::Condition>);

impl Exception {
    pub(crate) fn new(cond: impl Into<Rc<value::Condition>>) -> Self {
        Self(cond.into())
    }
}

impl Exception {
    pub fn as_datum(&self) -> ExceptionDatum {
        ExceptionDatum(self)
    }
}

pub struct ExceptionDatum<'a>(&'a Exception);

impl Display for ExceptionDatum<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.0.0.as_datum().fmt(f)
    }
}

pub struct EvaluationMessage<'a>(&'a Evaluation);

impl Display for EvaluationMessage<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self.0 {
            Evaluation::Continuation => f.write_str("fatal error: unexpected continuation"),
            Evaluation::Ex(_) => todo!("exception cli message"),
            Evaluation::Val(v) => v.0.as_ref().map_or(Ok(()), |v| v.display_message().fmt(f)),
        }
    }
}

pub(crate) type EvalResult = Result<value::ValueRef, Exception>;

pub(crate) trait Evaluator {
    fn evaluate(&self, prg: Program) -> Evaluation;
}

pub(crate) struct Ast;

impl Evaluator for Ast {
    fn evaluate(&self, prg: Program) -> Evaluation {
        Evaluation::result(Ok(Some(value::Value::Ast(prg).into())))
    }
}

pub(crate) struct Environment {
    global: Rc<Frame>,
    symbols: Rc<SymbolTable>,
}

impl Evaluator for Environment {
    fn evaluate(&self, prg: Program) -> Evaluation {
        Evaluation::result(prg.eval(&self.global))
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
