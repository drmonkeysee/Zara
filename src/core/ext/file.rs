use super::SECOND_ARG_LABEL;
use crate::{
    eval::{EvalResult, Frame},
    value::{Condition, TypeName, Value},
};
use std::fs;

pub(super) fn load(env: &Frame) {
    // TODO ADD
    /*
    metadata?
    */

    super::bind_intrinsic(env, "canonical-path", 1..1, canon_path);

    super::bind_intrinsic(env, "file?", 1..1, is_file);
    super::bind_intrinsic(env, "directory?", 1..1, is_dir);

    super::bind_intrinsic(env, "copy-file", 2..2, copy);
    super::bind_intrinsic(env, "rename-file", 2..2, rename);

    super::bind_intrinsic(env, "make-directory", 1..1, mk_dir);
    super::bind_intrinsic(env, "make-directories", 1..1, mk_dirs);

    super::bind_intrinsic(env, "delete-directory", 1..1, rm_dir);
    super::bind_intrinsic(env, "delete-directories", 1..1, rm_dirs);
}

fn canon_path(args: &[Value], env: &Frame) -> EvalResult {
    super::fs_op(
        super::first(args),
        env,
        |p| fs::canonicalize(p),
        |p| {
            p.into_os_string().into_string().map_or_else(
                |_| Err(Condition::path_error(env.sym).into()),
                |s| Ok(Value::string_mut(s)),
            )
        },
    )
}

fn is_file(args: &[Value], env: &Frame) -> EvalResult {
    super::fs_op(
        super::first(args),
        env,
        |p| fs::metadata(p),
        |m| Ok(Value::Boolean(m.is_file())),
    )
}

fn is_dir(args: &[Value], env: &Frame) -> EvalResult {
    super::fs_op(
        super::first(args),
        env,
        |p| fs::metadata(p),
        |m| Ok(Value::Boolean(m.is_dir())),
    )
}

fn copy(args: &[Value], env: &Frame) -> EvalResult {
    let arg = super::second(args);
    arg.as_refstr().map_or_else(
        || Err(Condition::arg_error(SECOND_ARG_LABEL, TypeName::STRING, arg).into()),
        |target| {
            super::fs_cmd(super::first(args), env, |p| {
                fs::copy(p, target.as_ref())?;
                Ok(())
            })
        },
    )
}

fn rename(args: &[Value], env: &Frame) -> EvalResult {
    let arg = super::second(args);
    arg.as_refstr().map_or_else(
        || Err(Condition::arg_error(SECOND_ARG_LABEL, TypeName::STRING, arg).into()),
        |target| super::fs_cmd(super::first(args), env, |p| fs::rename(p, target.as_ref())),
    )
}

fn mk_dir(args: &[Value], env: &Frame) -> EvalResult {
    super::fs_cmd(super::first(args), env, |p| fs::create_dir(p))
}

fn mk_dirs(args: &[Value], env: &Frame) -> EvalResult {
    super::fs_cmd(super::first(args), env, |p| fs::create_dir_all(p))
}

fn rm_dir(args: &[Value], env: &Frame) -> EvalResult {
    super::fs_cmd(super::first(args), env, |p| fs::remove_dir(p))
}

fn rm_dirs(args: &[Value], env: &Frame) -> EvalResult {
    super::fs_cmd(super::first(args), env, |p| fs::remove_dir_all(p))
}
