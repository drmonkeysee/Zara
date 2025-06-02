mod env;
mod form;

pub(crate) use self::{
    env::{Binding, Frame, SymbolTable, System},
    form::{Arity, IntrinsicFn, MAX_ARITY, Procedure},
};
use crate::{
    core,
    syntax::Program,
    value::{Condition, Value as ValueImpl},
};
use std::{
    fmt::{self, Display, Formatter},
    process::ExitCode,
};

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

pub(crate) trait Evaluator {
    fn evaluate(&mut self, prg: Program) -> Evaluation;
}

pub(crate) struct Ast;

impl Evaluator for Ast {
    fn evaluate(&mut self, prg: Program) -> Evaluation {
        Evaluation::result(Ok(ValueImpl::Ast(prg.into())))
    }
}

pub(crate) struct Environment {
    global: Binding,
    symbols: SymbolTable,
    system: System,
}

impl Evaluator for Environment {
    fn evaluate(&mut self, prg: Program) -> Evaluation {
        let mut frame = Frame {
            scope: &mut self.global,
            sym: &self.symbols,
            sys: &self.system,
        };
        Evaluation::result(prg.eval(&mut frame))
    }
}

impl Environment {
    pub(crate) fn new<'a>(args: impl IntoIterator<Item = &'a str>) -> Self {
        let mut global = Binding::default();
        core::load(&mut global);
        Self {
            global,
            symbols: SymbolTable,
            system: System::new(args),
        }
    }
}
