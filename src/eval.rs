mod env;
mod form;

use self::env::{EnvNamespace, SimpleNamespace};
pub(crate) use self::{
    env::{Binding, Frame, SymbolTable, System},
    form::{Arity, IntrinsicFn, MAX_ARITY, Procedure},
};
use crate::{
    core, ext,
    syntax::{Namespace, Program},
    value::{Condition, Value as ValueImpl},
};
use std::{
    fmt::{self, Display, Formatter},
    process::ExitCode,
};

pub(crate) trait Evaluator {
    fn evaluate(&mut self, prg: Program) -> Evaluation;
    fn create_namespace(&mut self) -> impl Namespace;
}

pub(crate) struct Environment {
    global: Binding,
    symbols: SymbolTable,
    system: System,
}

impl Environment {
    fn make_frame(&mut self) -> Frame {
        Frame {
            scope: &mut self.global,
            sym: &mut self.symbols,
            sys: &self.system,
        }
    }
}

impl Evaluator for Environment {
    fn evaluate(&mut self, prg: Program) -> Evaluation {
        let mut frame = self.make_frame();
        Evaluation::result(prg.eval(&mut frame))
    }

    fn create_namespace(&mut self) -> impl Namespace {
        EnvNamespace(self.make_frame())
    }
}

impl Environment {
    pub(crate) fn new(args: impl IntoIterator<Item = String>) -> Self {
        let mut global = Binding::default();
        core::load(&mut global);
        ext::load(&mut global);
        Self {
            global,
            symbols: SymbolTable::default(),
            system: System::new(args),
        }
    }
}

pub(crate) struct Ast;

impl Evaluator for Ast {
    fn evaluate(&mut self, prg: Program) -> Evaluation {
        Evaluation::result(Ok(ValueImpl::Ast(prg.into())))
    }

    fn create_namespace(&mut self) -> impl Namespace {
        SimpleNamespace
    }
}

#[derive(Debug)]
pub enum Evaluation {
    Continuation,
    Ex(Exception),
    Val(Value),
}

impl Evaluation {
    fn result(r: EvalResult) -> Self {
        r.map_or_else(Self::Ex, |v| Self::Val(Value(v)))
    }

    #[must_use]
    pub fn display_message(&self) -> EvaluationMessage {
        EvaluationMessage(self)
    }
}

#[derive(Debug)]
pub struct Value(ValueImpl);

impl Value {
    #[must_use]
    pub fn is_unspecified(&self) -> bool {
        matches!(self.0, ValueImpl::Unspecified)
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

#[derive(Debug)]
pub enum Exception {
    Exit(ExitCode),
    Signal(Signal),
}

impl Exception {
    pub(crate) fn signal(cond: Condition) -> Self {
        Self::Signal(Signal(cond))
    }
}

impl From<Condition> for Exception {
    fn from(value: Condition) -> Self {
        Exception::signal(value)
    }
}

#[derive(Debug)]
pub struct Signal(Condition);

impl Display for Signal {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

pub struct EvaluationMessage<'a>(&'a Evaluation);

impl Display for EvaluationMessage<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self.0 {
            Evaluation::Continuation => f.write_str("fatal error: unexpected continuation"),
            Evaluation::Ex(_) => todo!("exception cli message"),
            Evaluation::Val(v) => v.0.display_message().fmt(f),
        }
    }
}

pub(crate) type EvalResult = Result<ValueImpl, Exception>;
