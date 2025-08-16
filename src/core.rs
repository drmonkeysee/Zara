macro_rules! invalid_target_ex {
    ($valname:expr, $arg:expr) => {
        Condition::arg_error(FIRST_ARG_LABEL, $valname, $arg).into()
    };
}

macro_rules! invalid_target {
    ($valname:expr, $arg:expr) => {
        Err(invalid_target_ex!($valname, $arg))
    };
}

macro_rules! try_predicate {
    ($name:ident, $kind:path, $valname:expr, $pred:expr) => {
        fn $name(args: &[Value], _env: &Frame) -> EvalResult {
            let arg = first(args);
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
        fn $name(args: &[Value], _env: &Frame) -> EvalResult {
            let arg = first(args);
            cadr_compose!(arg, $($compose),+)
        }
    };
}

mod base;
mod charuni;
mod complex;
mod cxr;
mod ext;
mod inexact;
mod procctx;
mod time;

use crate::{
    eval::{Arity, EvalResult, Frame, Intrinsic, IntrinsicFn},
    value::{Condition, TypeName, Value},
};

/*
 * Zara Core Library including all the standard R7RS libraries
 *
 * All intrinsic functions assume required arguments are present and will use
 * unchecked access to retrieve them; arity is checked by call-expr evaluation
 * so this should always be a safe assumption.
 */

const FIRST_ARG_LABEL: &str = "0";
const SECOND_ARG_LABEL: &str = "1";
const THIRD_ARG_LABEL: &str = "2";

pub(crate) fn load(env: &Frame) {
    base::load(env);
    charuni::load(env);
    complex::load(env);
    cxr::load(env);
    // TODO: add cli arg for excluding this
    ext::load(env);
    inexact::load(env);
    procctx::load(env);
    time::load(env);
}

fn bind_intrinsic(env: &Frame, name: &str, arity: Arity, def: IntrinsicFn) {
    let name = env.sym.get(name);
    env.scope.bind(
        name.clone(),
        Value::Intrinsic(Intrinsic { arity, def, name }.into()),
    );
}

fn first(args: &[Value]) -> &Value {
    args.first().expect("first argument arity failure")
}

fn second(args: &[Value]) -> &Value {
    args.get(1).expect("second argument arity failure")
}

fn third(args: &[Value]) -> &Value {
    args.get(2).expect("third argument arity failure")
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
