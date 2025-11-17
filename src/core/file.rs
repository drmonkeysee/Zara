// (scheme file)
use crate::{
    eval::{EvalResult, Frame},
    value::{Condition, TypeName, Value},
};

pub(super) fn load(env: &Frame) {
    super::bind_intrinsic(env, "open-input-file", 1..1, input_file);
    super::bind_intrinsic(env, "open-binary-input-file", 1..1, input_file);

    // TODO: add extension for append vs write?
    super::bind_intrinsic(env, "open-output-file", 1..1, output_file);
    super::bind_intrinsic(env, "open-binary-output-file", 1..1, output_file);
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
