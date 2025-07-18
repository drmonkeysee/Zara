// (scheme process-context)
use super::FIRST_ARG_LABEL;
use crate::{
    Exception,
    eval::{EvalResult, Frame},
    number::Number,
    value::{Condition, TypeName, Value},
};
use std::{
    env::{self, VarError},
    process::{self, ExitCode},
};

const PEXIT_SUCCESS: i32 = 0;
const PEXIT_FAILURE: i32 = 1;

pub(super) fn load(env: &mut Frame) {
    super::bind_intrinsic(env, "command-line", 0..0, command_line);

    super::bind_intrinsic(env, "exit", 0..1, exit);
    super::bind_intrinsic(env, "emergency-exit", 0..1, emergency_exit);

    super::bind_intrinsic(
        env,
        "get-environment-variable",
        1..1,
        get_environment_variable,
    );
    super::bind_intrinsic(
        env,
        "get-environment-variables",
        0..0,
        get_environment_variables,
    );
}

#[allow(clippy::unnecessary_wraps, reason = "infallible intrinsic")]
fn command_line(_args: &[Value], env: &mut Frame) -> EvalResult {
    Ok(env.sys.args.clone())
}

fn exit(args: &[Value], _env: &mut Frame) -> EvalResult {
    Err(Exception::Exit(resolve_exit(
        args.first(),
        ExitCode::SUCCESS,
        ExitCode::FAILURE,
        |n| (u8::try_from(n)).map_or(ExitCode::FAILURE, ExitCode::from),
    )))
}

fn emergency_exit(args: &[Value], _env: &mut Frame) -> EvalResult {
    process::exit(resolve_exit(
        args.first(),
        PEXIT_SUCCESS,
        PEXIT_FAILURE,
        |n| n.try_into().unwrap_or(PEXIT_FAILURE),
    ));
}

fn get_environment_variable(args: &[Value], _env: &mut Frame) -> EvalResult {
    let arg = args.first().unwrap();
    let Value::String(s) = arg else {
        return invalid_target!(TypeName::STRING, arg);
    };
    env::var(s.as_ref()).map_or_else(
        |err| match err {
            VarError::NotPresent => Ok(Value::Boolean(false)),
            VarError::NotUnicode(_) => {
                Err(Condition::system_error("invalid env var encoding").into())
            }
        },
        |v| Ok(Value::string(v)),
    )
}

#[allow(clippy::unnecessary_wraps, reason = "infallible intrinsic")]
fn get_environment_variables(_args: &[Value], _env: &mut Frame) -> EvalResult {
    Ok(Value::list(
        env::vars()
            .map(|(n, v)| Value::cons(Value::string(n), Value::string(v)))
            .collect::<Vec<_>>(),
    ))
}

fn resolve_exit<T: Copy>(
    v: Option<&Value>,
    success: T,
    failure: T,
    convert: impl FnOnce(&Number) -> T,
) -> T {
    v.map_or(success, |val| match val {
        Value::Boolean(true) => success,
        Value::Number(n) => convert(n),
        _ => failure,
    })
}
