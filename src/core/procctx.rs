// (scheme process-context)
use crate::{
    Exception,
    eval::{Binding, EvalResult, Frame},
    number::Number,
    value::{Condition, TypeName, Value},
};
use std::{
    env::{self, VarError},
    process::{self, ExitCode},
};

const PEXIT_SUCCESS: i32 = 0;
const PEXIT_FAILURE: i32 = 1;

pub(super) fn load(scope: &mut Binding) {
    super::bind_intrinsic(scope, "command-line", 0..0, command_line);
    super::bind_intrinsic(scope, "emergency-exit", 0..1, emergency_exit);
    super::bind_intrinsic(scope, "exit", 0..1, exit);
    super::bind_intrinsic(
        scope,
        "get-environment-variable",
        1..1,
        get_environment_variable,
    );
    super::bind_intrinsic(
        scope,
        "get-environment-variables",
        0..0,
        get_environment_variables,
    );
}

fn command_line(_args: &[Value], _env: &mut Frame) -> EvalResult {
    todo!("command_line");
}

fn emergency_exit(args: &[Value], _env: &mut Frame) -> EvalResult {
    process::exit(resolve_exit(
        args.first(),
        PEXIT_SUCCESS,
        PEXIT_FAILURE,
        |n| n.try_into().unwrap_or(PEXIT_FAILURE),
    ));
}

fn exit(args: &[Value], _env: &mut Frame) -> EvalResult {
    Err(Exception::Exit(resolve_exit(
        args.first(),
        ExitCode::SUCCESS,
        ExitCode::FAILURE,
        |n| (<&Number as TryInto<u8>>::try_into(n)).map_or(ExitCode::FAILURE, ExitCode::from),
    )))
}

fn get_environment_variable(args: &[Value], _env: &mut Frame) -> EvalResult {
    let arg = args.first().unwrap();
    let Value::String(s) = arg else {
        return Err(Condition::arg_error("0", TypeName::STRING, arg).into());
    };
    env::var(s.as_ref()).map_or_else(
        |err| match err {
            VarError::NotPresent => Ok(Value::Boolean(false)),
            VarError::NotUnicode(_) => {
                Err(Condition::system_error("invalid env var format").into())
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
