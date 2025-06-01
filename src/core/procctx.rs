// (scheme process-context)
use crate::{
    eval::{Binding, EvalResult, Frame},
    value::Value,
};
use std::process;

const EXIT_SUCCESS: i32 = 0;
const EXIT_FAILURE: i32 = 1;

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
    process::exit(resolve_exit_code(args.first()));
}

fn exit(_args: &[Value], _env: &mut Frame) -> EvalResult {
    todo!("exit");
}

fn get_environment_variable(_args: &[Value], _env: &mut Frame) -> EvalResult {
    todo!("get_environment_variable");
}

fn get_environment_variables(_args: &[Value], _env: &mut Frame) -> EvalResult {
    todo!("get_environment_variables");
}

fn resolve_exit_code(v: Option<&Value>) -> i32 {
    v.map_or(EXIT_SUCCESS, |val| match val {
        Value::Boolean(true) => EXIT_SUCCESS,
        Value::Number(n) => n.try_into().unwrap_or(EXIT_FAILURE),
        _ => EXIT_FAILURE,
    })
}
