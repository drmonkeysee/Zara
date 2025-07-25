mod env;
mod proc;

pub(crate) use self::{
    env::{Binding, Frame, Namespace, SymbolTable, System},
    proc::{Arity, IntrinsicFn, InvalidFormal, MAX_ARITY, Procedure},
};
use crate::{
    core,
    syntax::Program,
    value::{Condition, Value as ValueImpl},
};
use std::{
    fmt::{self, Display, Formatter},
    marker::PhantomData,
    process::ExitCode,
};

pub(crate) type Eval = Environment<EvalDriver>;
pub(crate) type Ast = Environment<AstDriver>;

pub(crate) trait Evaluator {
    fn eval(&self, prg: Program, frame: &mut Frame) -> Evaluation;
}

pub(crate) struct Environment<T> {
    driver: PhantomData<T>,
    global: Binding,
    symbols: SymbolTable,
    system: System,
}

impl<T: Evaluator + Default> Environment<T> {
    pub(crate) fn new(args: impl IntoIterator<Item = String>) -> Self {
        let mut me = Self {
            driver: PhantomData,
            global: Binding::default(),
            symbols: SymbolTable::default(),
            system: System::new(args),
        };
        core::load(&mut me.make_frame());
        me
    }

    pub(crate) fn evaluate(&mut self, prg: Program) -> Evaluation {
        let mut frame = self.make_frame();
        T::default().eval(prg, &mut frame)
    }

    pub(crate) fn make_namespace(&mut self) -> Namespace {
        Namespace(self.make_frame())
    }

    fn make_frame(&mut self) -> Frame {
        Frame {
            scope: &mut self.global,
            sym: &mut self.symbols,
            sys: &self.system,
        }
    }
}

#[derive(Default)]
pub(crate) struct EvalDriver;

impl Evaluator for EvalDriver {
    fn eval(&self, prg: Program, frame: &mut Frame) -> Evaluation {
        Evaluation::result(prg.eval(frame))
    }
}

#[derive(Default)]
pub(crate) struct AstDriver;

impl Evaluator for AstDriver {
    fn eval(&self, prg: Program, _frame: &mut Frame) -> Evaluation {
        Evaluation::result(Ok(ValueImpl::Ast(prg.into())))
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
