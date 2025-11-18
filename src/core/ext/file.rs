use super::SECOND_ARG_LABEL;
use crate::{
    Exception,
    eval::{EvalResult, Frame},
    value::{Condition, TypeName, Value},
};
use std::fs;

pub(super) fn load(env: &Frame) {
    // TODO ADD
    /*
    read-directory (?)
    is-directory?
    is-file?
    other stuff in std::fs
    */

    super::bind_intrinsic(env, "make-directory", 1..2, mk_dir);
    super::bind_intrinsic(env, "delete-directory", 1..1, rm_dir);
}

fn mk_dir(args: &[Value], env: &Frame) -> EvalResult {
    let arg = super::first(args);
    arg.as_refstr().map_or_else(
        || Err(super::invalid_target(TypeName::STRING, arg)),
        |path| {
            let all = args.get(1).map_or(Ok::<_, Exception>(false), |arg| {
                if let Value::Symbol(s) = arg {
                    Ok(s.is(&env.sym.get("parents")))
                } else {
                    Err(Condition::arg_error(SECOND_ARG_LABEL, TypeName::SYMBOL, arg).into())
                }
            })?;
            let op = if all {
                fs::create_dir_all
            } else {
                fs::create_dir
            };
            op(path.as_ref()).map_or_else(
                |err| Err(Condition::file_error(&(err.into()), env.sym, arg).into()),
                |()| Ok(Value::Unspecified),
            )
        },
    )
}

fn rm_dir(args: &[Value], env: &Frame) -> EvalResult {
    let arg = super::first(args);
    arg.as_refstr().map_or_else(
        || Err(super::invalid_target(TypeName::STRING, arg)),
        |path| {
            fs::remove_dir(path.as_ref()).map_or_else(
                |err| Err(Condition::file_error(&(err.into()), env.sym, arg).into()),
                |()| Ok(Value::Unspecified),
            )
        },
    )
}
