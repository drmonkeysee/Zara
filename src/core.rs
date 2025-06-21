macro_rules! invalid_target {
    ($valname:expr, $arg:expr) => {
        Err(Condition::arg_error(FIRST_ARG_LABEL, $valname, $arg).into())
    };
}

macro_rules! try_predicate {
    ($name:ident, $kind:path, $valname:expr, $pred:expr) => {
        fn $name(args: &[Value], _env: &mut Frame) -> EvalResult {
            let arg = args.first().unwrap();
            if let $kind(val) = arg {
                Ok(Value::Boolean($pred(val)))
            } else {
                invalid_target!($valname, arg)
            }
        }
    };
}

macro_rules! cadr_compose {
    ($val:expr, a) => {
        pcar($val)
    };
    ($val:expr, d) => {
        pcdr($val)
    };
    ($val:expr, a $(, $rest:ident)+) => {
        pcar(&cadr_compose!($val, $($rest),+)?)
    };
    ($val:expr, d $(, $rest:ident)+) => {
        pcdr(&cadr_compose!($val, $($rest),+)?)
    };
}

macro_rules! cadr_func {
    ($name:ident $(, $compose:ident)+) => {
        fn $name(args: &[Value], _env: &mut Frame) -> EvalResult {
            let arg = args.first().unwrap();
            cadr_compose!(&arg, $($compose),+)
        }
    };
}

mod base;
mod charuni;
mod cxr;
mod inexact;
mod procctx;
mod time;

use crate::{
    eval::{Arity, Binding, EvalResult, IntrinsicFn, Procedure},
    value::{Condition, TypeName, Value},
};

/*
 * R7RS Core Library
 *
 * All intrinsic functions assume required arguments are present and will use
 * unchecked access to retrieve them; arity is checked by call-expr evaluation
 * so this should always be a safe assumption.
 */

const FIRST_ARG_LABEL: &str = "0";
const SECOND_ARG_LABEL: &str = "1";

pub(crate) fn load(scope: &mut Binding) {
    base::load(scope);
    charuni::load(scope);
    cxr::load(scope);
    inexact::load(scope);
    procctx::load(scope);
    time::load(scope);
}

pub(crate) fn bind_intrinsic(scope: &mut Binding, name: &str, arity: Arity, body: IntrinsicFn) {
    scope.bind(
        name,
        Value::Procedure(Procedure::intrinsic(name, arity, body).into()),
    );
}

fn pcar(arg: &Value) -> EvalResult {
    if let Value::Pair(Some(p)) = arg {
        Ok(p.car.clone())
    } else {
        invalid_target!(TypeName::PAIR, arg)
    }
}

fn pcdr(arg: &Value) -> EvalResult {
    if let Value::Pair(Some(p)) = arg {
        Ok(p.cdr.clone())
    } else {
        invalid_target!(TypeName::PAIR, arg)
    }
}
