use super::SECOND_ARG_LABEL;
use crate::{
    Exception,
    eval::{EvalResult, Frame},
    value::{Condition, FileMode, TypeName, Value},
};
use std::fs::{self, ReadDir};

pub(super) fn load(env: &Frame) {
    // TODO ADD
    /*
    metadata?
    seek port
    */

    super::bind_intrinsic(env, "canonical-path", 1..1, canon_path);

    super::bind_intrinsic(env, "file?", 1..1, is_file);
    super::bind_intrinsic(env, "directory?", 1..1, is_dir);

    super::bind_intrinsic(env, "copy-file", 2..2, copy);
    super::bind_intrinsic(env, "rename-file", 2..2, rename);

    super::bind_intrinsic(env, "append-output-file", 1..1, append_output_file);
    super::bind_intrinsic(env, "append-binary-output-file", 1..1, append_output_file);

    super::bind_intrinsic(env, "make-directory", 1..1, mk_dir);
    super::bind_intrinsic(env, "make-directories", 1..1, mk_dirs);

    super::bind_intrinsic(env, "read-directory", 1..1, read_dir);

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

fn append_output_file(args: &[Value], env: &Frame) -> EvalResult {
    let arg = super::first(args);
    arg.as_refstr().map_or_else(
        || Err(super::invalid_target(TypeName::STRING, arg)),
        |path| {
            Value::port_file_output(path.as_ref(), FileMode::Append)
                .map_err(|err| Condition::file_error(&err, env.sym, arg).into())
        },
    )
}

fn mk_dir(args: &[Value], env: &Frame) -> EvalResult {
    super::fs_cmd(super::first(args), env, |p| fs::create_dir(p))
}

fn mk_dirs(args: &[Value], env: &Frame) -> EvalResult {
    super::fs_cmd(super::first(args), env, |p| fs::create_dir_all(p))
}

fn read_dir(args: &[Value], env: &Frame) -> EvalResult {
    let arg = super::first(args);
    super::fs_op(
        arg,
        env,
        |p| fs::read_dir(p),
        |dir| dir_into_list(dir, env, arg),
    )
}

fn rm_dir(args: &[Value], env: &Frame) -> EvalResult {
    super::fs_cmd(super::first(args), env, |p| fs::remove_dir(p))
}

fn rm_dirs(args: &[Value], env: &Frame) -> EvalResult {
    super::fs_cmd(super::first(args), env, |p| fs::remove_dir_all(p))
}

fn dir_into_list(dir: ReadDir, env: &Frame, arg: &Value) -> EvalResult {
    Ok(Value::list_mut(
        dir.collect::<Result<Vec<_>, _>>()
            .map_err(|err| Exception::from(Condition::io_error(&(err.into()), env.sym, arg)))?
            .into_iter()
            .map(|d| {
                d.file_name().into_string().map_or_else(
                    |_| Err(Exception::from(Condition::path_error(env.sym))),
                    |s| Ok(Value::string_mut(s)),
                )
            })
            .collect::<Result<Vec<_>, _>>()?,
    ))
}
