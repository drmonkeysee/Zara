// (scheme file)
use crate::{
    eval::{EvalResult, Frame},
    value::{Condition, TypeName, Value},
};
use std::fs;

pub(super) fn load(env: &Frame) {
    super::bind_intrinsic(env, "open-input-file", 1..1, input_file);
    super::bind_intrinsic(env, "open-binary-input-file", 1..1, input_file);

    // TODO: add extension for append vs write?
    super::bind_intrinsic(env, "open-output-file", 1..1, output_file);
    super::bind_intrinsic(env, "open-binary-output-file", 1..1, output_file);

    super::bind_intrinsic(env, "file-exists?", 1..1, path_exists);
    super::bind_intrinsic(env, "delete-file", 1..1, delete_file);
}

fn input_file(args: &[Value], env: &Frame) -> EvalResult {
    let arg = super::first(args);
    arg.as_refstr().map_or_else(
        || Err(super::invalid_target(TypeName::STRING, arg)),
        |path| {
            Value::port_file_input(path.as_ref())
                .map_err(|err| Condition::file_error(&err, env.sym, arg).into())
        },
    )
}

fn output_file(args: &[Value], env: &Frame) -> EvalResult {
    let arg = super::first(args);
    arg.as_refstr().map_or_else(
        || Err(super::invalid_target(TypeName::STRING, arg)),
        |path| {
            Value::port_file_output(path.as_ref())
                .map_err(|err| Condition::file_error(&err, env.sym, arg).into())
        },
    )
}

fn path_exists(args: &[Value], env: &Frame) -> EvalResult {
    let arg = super::first(args);
    arg.as_refstr().map_or_else(
        || Err(super::invalid_target(TypeName::STRING, arg)),
        |path| {
            fs::exists(path.as_ref()).map_or_else(
                |err| Err(Condition::file_error(&(err.into()), env.sym, arg).into()),
                |b| Ok(Value::Boolean(b)),
            )
        },
    )
}

fn delete_file(args: &[Value], env: &Frame) -> EvalResult {
    let arg = super::first(args);
    arg.as_refstr().map_or_else(
        || Err(super::invalid_target(TypeName::STRING, arg)),
        |path| {
            fs::remove_file(path.as_ref()).map_or_else(
                |err| Err(Condition::file_error(&(err.into()), env.sym, arg).into()),
                |()| Ok(Value::Unspecified),
            )
        },
    )
}
