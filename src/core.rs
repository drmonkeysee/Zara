macro_rules! predicate {
    ($name:ident, $pred:pat $(if $guard:expr)?) => {
        fn $name(args: &[Value], _env: &Frame) -> EvalResult {
            Ok(Value::Boolean(matches!(first(args), $pred $(if $guard)?)))
        }
    };
}

macro_rules! try_predicate {
    ($name:ident, $kind:path, $valname:expr, $pred:expr) => {
        fn $name(args: &[Value], _env: &Frame) -> EvalResult {
            let arg = first(args);
            if let $kind(val) = arg {
                Ok(Value::Boolean($pred(val)))
            } else {
                Err(invalid_target($valname, arg))
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

macro_rules! num_convert {
    ($name:ident, $type:ty, $err:path) => {
        fn $name(arg: &Value, lbl: impl Display) -> Result<$type, Exception> {
            let Value::Number(n) = arg else {
                return Err(Condition::arg_error(lbl, NumericTypeName::INTEGER, arg).into());
            };
            <$type>::try_from(n).map_err(|err| {
                if let $err = err {
                    Condition::value_error($err, arg)
                } else {
                    Condition::arg_type_error(lbl, NumericTypeName::INTEGER, n.as_typename(), arg)
                }
                .into()
            })
        }
    };
}

mod base;
mod charuni;
mod complex;
mod cxr;
mod ext;
mod file;
mod inexact;
mod procctx;
mod time;

use crate::{
    Exception,
    eval::{Arity, EvalResult, Frame, Intrinsic, IntrinsicFn},
    value::{Condition, TypeName, Value},
};
use std::{fmt::Display, io};

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
    file::load(env);
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
    args.first()
        .expect("procedure apply should ensure at least 1-arity")
}

fn second(args: &[Value]) -> &Value {
    args.get(1)
        .expect("procedure apply should ensure at least 2-arity")
}

fn third(args: &[Value]) -> &Value {
    args.get(2)
        .expect("procedure apply should ensure at least 3-arity")
}

fn pcar(arg: &Value) -> EvalResult {
    if let Some(p) = arg.as_refpair() {
        Ok(p.as_ref().car.clone())
    } else {
        Err(invalid_target(TypeName::PAIR, arg))
    }
}

fn pcdr(arg: &Value) -> EvalResult {
    if let Some(p) = arg.as_refpair() {
        Ok(p.as_ref().cdr.clone())
    } else {
        Err(invalid_target(TypeName::PAIR, arg))
    }
}

fn invalid_target(expected_type: impl Display, arg: &Value) -> Exception {
    Condition::arg_error(FIRST_ARG_LABEL, expected_type, arg).into()
}

fn fs_cmd(arg: &Value, env: &Frame, op: impl FnOnce(&str) -> io::Result<()>) -> EvalResult {
    fs_op(arg, env, op, |()| Ok(Value::Unspecified))
}

fn fs_op<T>(
    arg: &Value,
    env: &Frame,
    op: impl FnOnce(&str) -> io::Result<T>,
    ret: impl FnOnce(T) -> EvalResult,
) -> EvalResult {
    arg.as_refstr().map_or_else(
        || Err(invalid_target(TypeName::STRING, arg)),
        |path| {
            op(path.as_ref()).map_or_else(
                |err| Err(Condition::io_error(&(err.into()), env.sym, arg).into()),
                ret,
            )
        },
    )
}
