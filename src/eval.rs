mod env;
mod proc;

pub(crate) use self::{
    env::{Binding, Frame, Namespace, System},
    proc::{Arity, Intrinsic, IntrinsicFn, InvalidFormal, Lambda, MAX_ARITY, Operator, Procedure},
};
use crate::{
    core,
    string::SymbolTable,
    syntax::Sequence,
    value::{Condition, Value as ValueImpl},
};
use std::{
    fmt::{self, Display, Formatter},
    marker::PhantomData,
    process::ExitCode,
    rc::Rc,
};

pub(crate) type Eval = Environment<EvalDriver>;
pub(crate) type Ast = Environment<AstDriver>;

pub(crate) trait Evaluator {
    fn eval(&self, prg: Sequence, frame: Frame) -> Evaluation;
}

pub(crate) struct Environment<T> {
    driver: PhantomData<T>,
    global: Rc<Binding>,
    symbols: SymbolTable,
    system: System,
}

impl<T: Evaluator + Default> Environment<T> {
    pub(crate) fn new(args: impl IntoIterator<Item = String>) -> Self {
        let me = Self {
            driver: PhantomData,
            global: Binding::default().into(),
            symbols: SymbolTable::default(),
            system: System::new(args),
        };
        core::load(&me.make_frame());
        me
    }

    pub(crate) fn evaluate(&self, prg: Sequence) -> Evaluation {
        T::default().eval(prg, self.make_frame())
    }

    pub(crate) fn make_namespace(&self) -> Namespace<'_> {
        Namespace(self.make_frame())
    }

    fn make_frame(&self) -> Frame<'_> {
        Frame {
            scope: Rc::clone(&self.global),
            sym: &self.symbols,
            sys: &self.system,
        }
    }
}

#[derive(Default)]
pub(crate) struct EvalDriver;

impl Evaluator for EvalDriver {
    fn eval(&self, prg: Sequence, frame: Frame) -> Evaluation {
        Evaluation::result(prg.eval(&frame))
    }
}

#[derive(Default)]
pub(crate) struct AstDriver;

impl Evaluator for AstDriver {
    fn eval(&self, prg: Sequence, _frame: Frame) -> Evaluation {
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
    pub fn display_message(&self) -> EvaluationMessage<'_> {
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

    fn display_message(&self) -> ExceptionMessage<'_> {
        ExceptionMessage(self)
    }
}

impl From<Condition> for Exception {
    fn from(value: Condition) -> Self {
        Self::signal(value)
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
            Evaluation::Continuation => f.write_str("fatal error: unexpected continuation\n"),
            Evaluation::Ex(ex) => ex.display_message().fmt(f),
            Evaluation::Val(v) => v.0.display_message().fmt(f),
        }
    }
}

pub(crate) type EvalResult = Result<ValueImpl, Exception>;

struct ExceptionMessage<'a>(&'a Exception);

impl Display for ExceptionMessage<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.0 {
            Exception::Exit(code) => writeln!(f, "exit: {code:?}"),
            Exception::Signal(sig) => writeln!(f, "{sig}\ntodo: display callstack"),
        }
    }
}
