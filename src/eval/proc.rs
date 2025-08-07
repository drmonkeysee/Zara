#[cfg(test)]
mod tests;

use super::{Binding, EvalResult, Frame};
use crate::{string::Symbol, syntax::Sequence, value::Value};
use std::{
    collections::HashSet,
    fmt::{self, Display, Formatter, Write},
    iter,
    ops::Range,
    rc::Rc,
};

pub(crate) const MAX_ARITY: u8 = u8::MAX;

pub(crate) type IntrinsicFn = fn(&[Value], &Frame) -> EvalResult;
pub(crate) type Arity = Range<u8>;
pub(crate) type LambdaResult = Result<Lambda, Vec<InvalidFormal>>;

pub(crate) trait Operator {
    fn name(&self) -> Option<&str>;
    fn arity(&self) -> &Arity;
    fn apply(&self, args: &[Value], env: &Frame) -> EvalResult;

    fn matches_arity(&self, args_len: usize) -> bool {
        self.arity().start as usize <= args_len && args_len <= self.arity().end as usize
    }
}

#[derive(Debug)]
pub(crate) struct Intrinsic {
    pub(crate) arity: Arity,
    pub(crate) def: IntrinsicFn,
    pub(crate) name: Symbol,
}

impl Operator for Intrinsic {
    fn name(&self) -> Option<&str> {
        Some(&self.name)
    }

    fn arity(&self) -> &Arity {
        &self.arity
    }

    fn apply(&self, args: &[Value], env: &Frame) -> EvalResult {
        (self.def)(args, env)
    }
}

impl Display for Intrinsic {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "#<intrinsic {}", self.name.as_datum())?;
        write_intrinsics(&self.arity, f)?;
        f.write_char('>')
    }
}

#[derive(Debug)]
pub(crate) struct Procedure {
    // TODO: make this optional for pure functions
    closure: Rc<Binding>,
    def: Rc<Lambda>,
    name: Option<Symbol>,
}

impl Procedure {
    pub(crate) fn new(
        closure: impl Into<Rc<Binding>>,
        def: impl Into<Rc<Lambda>>,
        name: Option<Symbol>,
    ) -> Self {
        Self {
            closure: closure.into(),
            def: def.into(),
            name,
        }
    }
}

impl Operator for Procedure {
    fn name(&self) -> Option<&str> {
        self.name.as_deref()
    }

    fn arity(&self) -> &Arity {
        &self.def.arity
    }

    fn apply(&self, args: &[Value], env: &Frame) -> EvalResult {
        // TODO: tail-call optimization does not create a new frame
        let call_frame = env.new_child(Rc::clone(&self.closure));
        self.def.apply(args, &call_frame)
    }
}

impl Display for Procedure {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str("#<procedure")?;
        if let Some(n) = &self.name {
            write!(f, " {}", n.as_datum())?;
        }
        write!(f, "{}>", self.def)
    }
}

#[derive(Debug)]
pub(crate) struct Lambda {
    arity: Arity,
    body: Sequence,
    formals: Box<[Symbol]>,
    variadic: Option<Symbol>,
}

impl Lambda {
    pub(crate) fn new(
        named: impl IntoIterator<Item = Symbol>,
        variadic: Option<Symbol>,
        body: Sequence,
    ) -> LambdaResult {
        let formals = into_formals(named)?;
        let min = formals.len();
        let (max, param_count) = if variadic.is_some() {
            (MAX_ARITY as usize, min + 1)
        } else {
            (min, min)
        };
        if param_count > MAX_ARITY.into() {
            Err(vec![InvalidFormal::MaxFormals])
        } else {
            Ok(Self {
                #[allow(clippy::cast_possible_truncation, reason = "truncation guarded above")]
                arity: min as u8..max as u8,
                body,
                formals: formals.into(),
                variadic,
            })
        }
    }

    fn apply(&self, args: &[Value], env: &Frame) -> EvalResult {
        let args_it = args.iter();
        self.formals
            .iter()
            .zip(args_it.take(self.formals.len()))
            .for_each(|(var, val)| {
                env.scope.bind(var.clone(), val.clone());
            });
        if self.variadic.is_some() {
            todo!("support variadic args");
        }
        self.body.eval(env)
    }
}

impl Display for Lambda {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if no_params(&self.arity) {
            Ok(())
        } else {
            write_formals(&self.formals, self.variadic.as_ref(), f)
        }
    }
}

#[derive(Debug)]
pub(crate) enum InvalidFormal {
    DuplicateFormal(Symbol),
    MaxFormals,
}

impl Display for InvalidFormal {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::DuplicateFormal(n) => write!(f, "duplicate formal: {}", n.as_datum()),
            Self::MaxFormals => write!(
                f,
                "lambda definition exceeds formal arguments limit: {}",
                MAX_ARITY as usize + 1
            ),
        }
    }
}

fn no_params(arity: &Arity) -> bool {
    arity.start == 0 && arity.is_empty()
}

fn write_formals(
    formals: &[Symbol],
    variadic: Option<&Symbol>,
    f: &mut Formatter<'_>,
) -> fmt::Result {
    f.write_char(' ')?;
    let mut buf = formals.join(" ");
    if let Some(v) = variadic {
        if !buf.is_empty() {
            buf.push(' ');
        }
        buf.push_str(v);
        buf.push('…');
        if formals.is_empty() {
            return f.write_str(&buf);
        }
    }
    write!(f, "({buf})")
}

fn write_intrinsics(arity: &Arity, f: &mut Formatter<'_>) -> fmt::Result {
    if no_params(arity) {
        Ok(())
    } else {
        let params = iter::repeat_n("_", arity.start.into())
            .chain(if arity.end == MAX_ARITY {
                iter::repeat_n("…", 1)
            } else {
                iter::repeat_n("?", arity.len())
            })
            .collect::<Vec<_>>()
            .join(" ");
        write!(f, " ({params})")
    }
}

fn into_formals(
    named_args: impl IntoIterator<Item = Symbol>,
) -> Result<Vec<Symbol>, Vec<InvalidFormal>> {
    let mut names = HashSet::new();
    let (singles, duplicates): (Vec<_>, Vec<_>) = named_args
        .into_iter()
        .map(|n| {
            if names.contains(&n) {
                Err(InvalidFormal::DuplicateFormal(n))
            } else {
                names.insert(n.clone());
                Ok(n)
            }
        })
        .partition(Result::is_ok);
    if duplicates.is_empty() {
        Ok(singles.into_iter().flatten().collect::<Vec<_>>())
    } else {
        Err(duplicates.into_iter().filter_map(Result::err).collect())
    }
}
