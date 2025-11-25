macro_rules! port_pos {
    ($port: expr, $op: expr, $sym: expr, $arg: expr) => {
        $op(&mut $port.borrow_mut()).map_or_else(
            |err| Err(Condition::io_error(&err, $sym, $arg).into()),
            |pos| Ok(Value::Number(Number::from_usize(pos))),
        )
    };
}

use super::{SECOND_ARG_LABEL, THIRD_ARG_LABEL};
use crate::{
    Exception,
    eval::{EvalResult, Frame},
    number::{Number, NumericError, NumericTypeName},
    string::{Symbol, SymbolTable},
    value::{Condition, FileMode, PortSeek, ReadPort, TypeName, Value, WritePort, zlist},
};
use std::{fmt::Display, fs};

const SEEK_BEG: &str = "start";
const SEEK_CUR: &str = "current";
const SEEK_END: &str = "end";

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

    super::bind_intrinsic(env, "seekable-port?", 1..1, is_seekable);
    super::bind_intrinsic(env, "port-position", 1..1, port_tell);
    super::bind_intrinsic(env, "set-port-position!", 2..3, port_seek);

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

#[allow(clippy::unnecessary_wraps, reason = "infallible intrinsic")]
fn is_seekable(args: &[Value], _env: &Frame) -> EvalResult {
    Ok(Value::Boolean(match super::first(args) {
        Value::PortInput(p) => p.borrow().is_seekable(),
        Value::PortOutput(p) => p.borrow().is_seekable(),
        _ => false,
    }))
}

fn port_tell(args: &[Value], env: &Frame) -> EvalResult {
    let arg = super::first(args);
    match arg {
        Value::PortInput(p) => port_pos!(p, ReadPort::tell, env.sym, arg),
        Value::PortOutput(p) => port_pos!(p, WritePort::tell, env.sym, arg),
        _ => Err(super::invalid_target(TypeName::PORT, arg)),
    }
}

fn port_seek(args: &[Value], env: &Frame) -> EvalResult {
    let arg = super::first(args);
    let parg = super::second(args);
    let pos = try_val_to_port_pos(parg, SECOND_ARG_LABEL)?;
    let whence = args.get(2).map_or_else(
        || Ok(env.sym.get(SEEK_BEG)),
        |val| {
            if let Value::Symbol(s) = val {
                Ok(s.clone())
            } else {
                Err(Exception::from(Condition::arg_error(
                    THIRD_ARG_LABEL,
                    TypeName::SYMBOL,
                    val,
                )))
            }
        },
    )?;
    let seek_pos = make_seek_pos(pos, whence, env.sym, parg)?;
    match arg {
        Value::PortInput(p) => port_pos!(p, |p: &mut ReadPort| p.seek(seek_pos), env.sym, arg),
        Value::PortOutput(p) => port_pos!(p, |p: &mut WritePort| p.seek(seek_pos), env.sym, arg),
        _ => Err(super::invalid_target(TypeName::PORT, arg)),
    }
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
        |dir| {
            Ok(Value::list_mut(
                dir.collect::<Result<Vec<_>, _>>()
                    .map_err(|err| {
                        Exception::from(Condition::io_error(&(err.into()), env.sym, arg))
                    })?
                    .into_iter()
                    .map(|d| {
                        d.file_name().into_string().map_or_else(
                            |_| Err(Exception::from(Condition::path_error(env.sym))),
                            |s| Ok(Value::string_mut(s)),
                        )
                    })
                    .collect::<Result<Vec<_>, _>>()?,
            ))
        },
    )
}

fn rm_dir(args: &[Value], env: &Frame) -> EvalResult {
    super::fs_cmd(super::first(args), env, |p| fs::remove_dir(p))
}

fn rm_dirs(args: &[Value], env: &Frame) -> EvalResult {
    super::fs_cmd(super::first(args), env, |p| fs::remove_dir_all(p))
}

num_convert!(
    try_val_to_port_pos,
    i64,
    NumericError::Int64ConversionInvalidRange
);

fn make_seek_pos(
    pos: i64,
    whence: Symbol,
    sym: &SymbolTable,
    arg: &Value,
) -> Result<PortSeek, Exception> {
    let psz = pos
        .try_into()
        .map_err(|_| Condition::value_error(NumericError::IsizeConversionInvalidRange, arg))?;
    match whence.as_ref() {
        SEEK_BEG => Ok(PortSeek::Start(psz)),
        SEEK_CUR => Ok(PortSeek::Current(psz)),
        SEEK_END => Ok(PortSeek::End(psz)),
        _ => Err(Condition::bi_value_error(
            "invalid seek-position choice",
            &Value::Symbol(whence),
            &zlist![
                Value::Symbol(sym.get(SEEK_BEG)),
                Value::Symbol(sym.get(SEEK_CUR)),
                Value::Symbol(sym.get(SEEK_END)),
            ],
        )
        .into()),
    }
}
