// (scheme base)
macro_rules! seq_predicate {
    ($name:ident, $kind:path, $valname:expr, $pred:expr) => {
        fn $name(args: &[Value], _env: &Frame) -> EvalResult {
            let mut it = args.iter().enumerate();
            let first = match it.next() {
                None => return Ok(Value::Boolean(true)),
                Some((_, $kind(a))) => a,
                Some((idx, v)) => {
                    return Err(Condition::arg_error(idx, $valname, v).into());
                }
            };
            it.try_fold((true, first), |(acc, prev), (idx, val)| {
                if let $kind(next) = val {
                    Ok((acc && $pred(prev, next), next))
                } else {
                    Err(Condition::arg_error(idx, $valname, val).into())
                }
            })
            .map(|(b, _)| Value::Boolean(b))
        }
    };
}

mod collections;
mod num;
#[cfg(test)]
mod tests;

use super::{
    FIRST_ARG_LABEL, SECOND_ARG_LABEL, THIRD_ARG_LABEL, bind_intrinsic, first, invalid_target,
    pcar, pcdr, second, third,
};
use crate::{
    Exception,
    eval::{EvalResult, Frame, MAX_ARITY},
    number::{Number, NumericError, NumericTypeName},
    string::{Symbol, unicode::UnicodeError},
    value::{
        Condition, InputPortRef, OutputPortRef, PortResult, PortSpec, ReadPort, TypeName, Value,
    },
};
use std::fmt::Display;

pub(super) fn load(env: &Frame) {
    load_bool(env);
    load_char(env);
    load_eq(env);
    load_ex(env);
    load_io(env);
    load_proc(env);
    load_symbol(env);
    collections::load(env);
    num::load(env);
}

//
// Booleans
//

fn load_bool(env: &Frame) {
    bind_intrinsic(env, "not", 1..1, not);

    bind_intrinsic(env, "boolean?", 1..1, is_boolean);
    bind_intrinsic(env, "boolean=?", 0..MAX_ARITY, booleans_eq);
}

predicate!(not, Value::Boolean(false));
predicate!(is_boolean, Value::Boolean(_));
seq_predicate!(booleans_eq, Value::Boolean, TypeName::BOOL, bool::eq);

//
// Characters
//

fn load_char(env: &Frame) {
    bind_intrinsic(env, "char?", 1..1, is_char);

    bind_intrinsic(env, "char=?", 0..MAX_ARITY, chars_eq);
    bind_intrinsic(env, "char<?", 0..MAX_ARITY, chars_lt);
    bind_intrinsic(env, "char>?", 0..MAX_ARITY, chars_gt);
    bind_intrinsic(env, "char<=?", 0..MAX_ARITY, chars_lte);
    bind_intrinsic(env, "char>=?", 0..MAX_ARITY, chars_gte);

    bind_intrinsic(env, "char->integer", 1..1, char_to_integer);
    bind_intrinsic(env, "integer->char", 1..1, char_from_integer);
}

predicate!(is_char, Value::Character(_));
seq_predicate!(chars_eq, Value::Character, TypeName::CHAR, char::eq);
seq_predicate!(chars_lt, Value::Character, TypeName::CHAR, char::lt);
seq_predicate!(chars_gt, Value::Character, TypeName::CHAR, char::gt);
seq_predicate!(chars_lte, Value::Character, TypeName::CHAR, char::le);
seq_predicate!(chars_gte, Value::Character, TypeName::CHAR, char::ge);

fn char_to_integer(args: &[Value], _env: &Frame) -> EvalResult {
    let arg = first(args);
    if let Value::Character(c) = arg {
        let n = u32::from(*c);
        Ok(Value::real(i64::from(n)))
    } else {
        Err(invalid_target(TypeName::CHAR, arg))
    }
}

fn char_from_integer(args: &[Value], _env: &Frame) -> EvalResult {
    let arg = first(args);
    if let Value::Number(n) = arg {
        try_num_into_char(n, arg)
    } else {
        Err(invalid_target(NumericTypeName::INTEGER, arg))
    }
}

//
// Equivalence
//

fn load_eq(env: &Frame) {
    bind_intrinsic(env, "eqv?", 2..2, is_eqv);
    bind_intrinsic(env, "eq?", 2..2, is_eq);
    bind_intrinsic(env, "equal?", 2..2, is_equal);
}

#[allow(clippy::unnecessary_wraps, reason = "infallible intrinsic")]
fn is_eqv(args: &[Value], _env: &Frame) -> EvalResult {
    let a = first(args);
    let b = second(args);
    Ok(Value::Boolean(a.is_eqv(b)))
}

#[allow(clippy::unnecessary_wraps, reason = "infallible intrinsic")]
fn is_eq(args: &[Value], _env: &Frame) -> EvalResult {
    let a = first(args);
    let b = second(args);
    Ok(Value::Boolean(a.is(b)))
}

#[allow(clippy::unnecessary_wraps, reason = "infallible intrinsic")]
fn is_equal(args: &[Value], _env: &Frame) -> EvalResult {
    let a = first(args);
    let b = second(args);
    Ok(Value::Boolean(a == b))
}

//
// Exceptions
//

fn load_ex(env: &Frame) {
    bind_intrinsic(env, "error-object?", 1..1, is_error);

    bind_intrinsic(env, "error-object-message", 1..1, error_msg);
    bind_intrinsic(env, "error-object-irritants", 1..1, error_irritants);

    bind_intrinsic(env, "read-error?", 1..1, is_read_error);
    bind_intrinsic(env, "file-error?", 1..1, is_file_error);
}

predicate!(is_error, Value::Error(_));
predicate!(is_read_error, Value::Error(c) if c.is_read_err());
predicate!(is_file_error, Value::Error(c) if c.is_file_err());

fn error_msg(args: &[Value], _env: &Frame) -> EvalResult {
    let arg = first(args);
    if let Value::Error(c) = arg {
        Ok(Value::string(c.message()))
    } else {
        Err(invalid_target(TypeName::ERROR, arg))
    }
}

fn error_irritants(args: &[Value], _env: &Frame) -> EvalResult {
    let arg = first(args);
    if let Value::Error(c) = arg {
        Ok(c.irritants().map_or(Value::Null, Value::clone))
    } else {
        Err(invalid_target(TypeName::ERROR, arg))
    }
}

//
// Input/Output
//

fn load_io(env: &Frame) {
    bind_intrinsic(env, "input-port?", 1..1, is_input_port);
    bind_intrinsic(env, "output-port?", 1..1, is_output_port);
    bind_intrinsic(env, "textual-port?", 1..1, is_textual_port);
    bind_intrinsic(env, "binary-port?", 1..1, is_binary_port);
    bind_intrinsic(env, "port?", 1..1, is_port);

    bind_intrinsic(env, "input-port-open?", 1..1, is_open_input);
    bind_intrinsic(env, "output-port-open?", 1..1, is_open_output);

    bind_intrinsic(env, "current-input-port", 0..0, current_stdin);
    bind_intrinsic(env, "current-output-port", 0..0, current_stdout);
    bind_intrinsic(env, "current-error-port", 0..0, current_stderr);

    bind_intrinsic(env, "close-port", 1..1, close_port);
    bind_intrinsic(env, "close-input-port", 1..1, close_input_port);
    bind_intrinsic(env, "close-output-port", 1..1, close_output_port);

    bind_intrinsic(env, "read-char", 0..1, read_char);
    bind_intrinsic(env, "peek-char", 0..1, peek_char);
    bind_intrinsic(env, "read-line", 0..1, read_line);

    bind_intrinsic(env, "eof-object?", 1..1, is_eof);
    bind_intrinsic(env, "eof-object", 0..0, eof);

    bind_intrinsic(env, "char-ready?", 0..1, char_ready);
    bind_intrinsic(env, "read-u8", 0..1, read_byte);
    bind_intrinsic(env, "peek-u8", 0..1, peek_byte);
    bind_intrinsic(env, "u8-ready?", 0..1, byte_ready);

    bind_intrinsic(env, "write", 1..2, write_datum);
    bind_intrinsic(env, "write-shared", 1..2, write_shared);
    bind_intrinsic(env, "write-simple", 1..2, write_simple);
    bind_intrinsic(env, "display", 1..2, write_display);
    bind_intrinsic(env, "newline", 0..1, newline);
    bind_intrinsic(env, "write-char", 1..2, write_char);

    bind_intrinsic(env, "flush-output-port", 0..1, flush_port);
}

predicate!(is_input_port, Value::PortInput(_));
predicate!(is_output_port, Value::PortOutput(_));
predicate!(is_eof, Value::Eof);

#[allow(clippy::unnecessary_wraps, reason = "infallible intrinsic")]
fn is_port(args: &[Value], _env: &Frame) -> EvalResult {
    Ok(Value::Boolean(first(args).is_port()))
}

#[allow(clippy::unnecessary_wraps, reason = "infallible intrinsic")]
fn is_textual_port(args: &[Value], _env: &Frame) -> EvalResult {
    Ok(Value::Boolean(match first(args) {
        Value::PortInput(p) => p.borrow().is_textual(),
        Value::PortOutput(p) => p.borrow().is_textual(),
        _ => false,
    }))
}

#[allow(clippy::unnecessary_wraps, reason = "infallible intrinsic")]
fn is_binary_port(args: &[Value], _env: &Frame) -> EvalResult {
    Ok(Value::Boolean(match first(args) {
        Value::PortInput(p) => p.borrow().is_binary(),
        Value::PortOutput(p) => p.borrow().is_binary(),
        _ => false,
    }))
}

fn is_open_input(args: &[Value], _env: &Frame) -> EvalResult {
    let arg = first(args);
    let p = guard_input_port(arg, PortSpec::Input)?;
    Ok(Value::Boolean(p.borrow().is_open()))
}

fn is_open_output(args: &[Value], _env: &Frame) -> EvalResult {
    let arg = first(args);
    let p = guard_output_port(arg, PortSpec::Output)?;
    Ok(Value::Boolean(p.borrow().is_open()))
}

#[allow(clippy::unnecessary_wraps, reason = "infallible intrinsic")]
fn current_stdin(_args: &[Value], env: &Frame) -> EvalResult {
    Ok(env.sys.stdin.clone())
}

#[allow(clippy::unnecessary_wraps, reason = "infallible intrinsic")]
fn current_stdout(_args: &[Value], env: &Frame) -> EvalResult {
    Ok(env.sys.stdout.clone())
}

#[allow(clippy::unnecessary_wraps, reason = "infallible intrinsic")]
fn current_stderr(_args: &[Value], env: &Frame) -> EvalResult {
    Ok(env.sys.stderr.clone())
}

fn close_port(args: &[Value], env: &Frame) -> EvalResult {
    let arg = first(args);
    match arg {
        Value::PortInput(_) => close_input_port(args, env),
        Value::PortOutput(_) => close_output_port(args, env),
        _ => Err(invalid_target(TypeName::PORT, arg)),
    }
}

fn close_input_port(args: &[Value], _env: &Frame) -> EvalResult {
    let arg = first(args);
    let p = guard_input_port(arg, PortSpec::Input)?;
    p.borrow_mut().close();
    Ok(Value::Unspecified)
}

fn close_output_port(args: &[Value], _env: &Frame) -> EvalResult {
    let arg = first(args);
    let p = guard_output_port(arg, PortSpec::Output)?;
    p.borrow_mut().close();
    Ok(Value::Unspecified)
}

fn read_char(args: &[Value], env: &Frame) -> EvalResult {
    read_op_mut(
        args.first(),
        env,
        PortSpec::TextualInput,
        ReadPort::read_char,
        |ch| Ok(ch.map_or(Value::Eof, Value::Character)),
    )
}

fn peek_char(args: &[Value], env: &Frame) -> EvalResult {
    read_op_mut(
        args.first(),
        env,
        PortSpec::TextualInput,
        ReadPort::peek_char,
        |ch| Ok(ch.map_or(Value::Eof, Value::Character)),
    )
}

// NOTE: this does not strictly conform to the R7RS standard as it does
// not consider carriage return (\r) to be a line-delimiter (same as Chez Scheme).
fn read_line(args: &[Value], env: &Frame) -> EvalResult {
    read_op_mut(
        args.first(),
        env,
        PortSpec::TextualInput,
        ReadPort::read_line,
        |s| Ok(s.map_or(Value::Eof, Value::string_mut)),
    )
}

fn char_ready(args: &[Value], env: &Frame) -> EvalResult {
    read_op(
        args.first(),
        env,
        PortSpec::TextualInput,
        ReadPort::char_ready,
        |b| Ok(Value::Boolean(b)),
    )
}

#[allow(clippy::unnecessary_wraps, reason = "infallible intrinsic")]
fn eof(_args: &[Value], _env: &Frame) -> EvalResult {
    Ok(Value::Eof)
}

fn read_byte(args: &[Value], env: &Frame) -> EvalResult {
    read_op_mut(
        args.first(),
        env,
        PortSpec::BinaryInput,
        ReadPort::read_byte,
        |b| Ok(b.map_or(Value::Eof, |b| Value::real(i64::from(b)))),
    )
}

fn peek_byte(args: &[Value], env: &Frame) -> EvalResult {
    read_op_mut(
        args.first(),
        env,
        PortSpec::BinaryInput,
        ReadPort::peek_byte,
        |b| Ok(b.map_or(Value::Eof, |b| Value::real(i64::from(b)))),
    )
}

fn byte_ready(args: &[Value], env: &Frame) -> EvalResult {
    read_op(
        args.first(),
        env,
        PortSpec::BinaryInput,
        ReadPort::byte_ready,
        |b| Ok(Value::Boolean(b)),
    )
}

fn write_datum(args: &[Value], env: &Frame) -> EvalResult {
    write_object(first(args), args.get(1), env, Value::as_datum)
}

fn write_shared(args: &[Value], env: &Frame) -> EvalResult {
    write_object(first(args), args.get(1), env, Value::as_shared_datum)
}

fn write_simple(args: &[Value], env: &Frame) -> EvalResult {
    write_object(first(args), args.get(1), env, Value::as_simple_datum)
}

fn write_display(args: &[Value], env: &Frame) -> EvalResult {
    write_object(first(args), args.get(1), env, Value::as_display_datum)
}

fn newline(args: &[Value], env: &Frame) -> EvalResult {
    put_char('\n', args.first(), env)
}

fn write_char(args: &[Value], env: &Frame) -> EvalResult {
    let arg = first(args);
    let Value::Character(ch) = arg else {
        return Err(invalid_target(TypeName::CHAR, arg));
    };
    put_char(*ch, args.get(1), env)
}

fn flush_port(args: &[Value], env: &Frame) -> EvalResult {
    let arg = args.first().unwrap_or(&env.sys.stdout);
    let p = guard_output_port(arg, PortSpec::Output)?;
    p.borrow_mut().flush().map_or_else(
        |err| Err(Condition::io_error(&err, env.sym, arg).into()),
        |()| Ok(Value::Unspecified),
    )
}

//
// Procedures
//

fn load_proc(env: &Frame) {
    bind_intrinsic(env, "procedure?", 1..1, is_procedure);
}

predicate!(is_procedure, Value::Intrinsic(_) | Value::Procedure(_));

//
// Symbols
//

fn load_symbol(env: &Frame) {
    bind_intrinsic(env, "symbol?", 1..1, is_symbol);
    bind_intrinsic(env, "symbol=?", 0..MAX_ARITY, symbols_eq);

    bind_intrinsic(env, "symbol->string", 1..1, symbol_to_string);
    bind_intrinsic(env, "string->symbol", 1..1, symbol_from_string);
}

predicate!(is_symbol, Value::Symbol(_));
seq_predicate!(symbols_eq, Value::Symbol, TypeName::SYMBOL, Symbol::is);

fn symbol_to_string(args: &[Value], _env: &Frame) -> EvalResult {
    let arg = first(args);
    if let Value::Symbol(s) = arg {
        Ok(Value::string(s.as_rc()))
    } else {
        Err(invalid_target(TypeName::SYMBOL, arg))
    }
}

fn symbol_from_string(args: &[Value], env: &Frame) -> EvalResult {
    let arg = first(args);
    arg.as_refstr().map_or_else(
        || Err(invalid_target(TypeName::STRING, arg)),
        |s| Ok(Value::Symbol(env.sym.get(s))),
    )
}

//
// Helpers
//

fn try_num_into_char(n: &Number, arg: &Value) -> EvalResult {
    u32::try_from(n).map_or_else(
        |err| {
            Err(if let NumericError::Uint32ConversionInvalidRange = err {
                Condition::value_error(UnicodeError::CodePointOutOfRange, arg)
            } else {
                Condition::arg_type_error(
                    FIRST_ARG_LABEL,
                    NumericTypeName::INTEGER,
                    n.as_typename(),
                    arg,
                )
            }
            .into())
        },
        |u| {
            char::from_u32(u).map_or_else(
                || Err(Condition::value_error(UnicodeError::CodePointOutOfRange, arg).into()),
                |c| Ok(Value::Character(c)),
            )
        },
    )
}

fn guard_input_port(arg: &Value, expected: PortSpec) -> Result<&InputPortRef, Exception> {
    guard_port_value(arg, expected, |v| {
        if let Value::PortInput(p) = v {
            p
        } else {
            unreachable!("unexpected non-input-port value")
        }
    })
}

fn guard_output_port(arg: &Value, expected: PortSpec) -> Result<&OutputPortRef, Exception> {
    guard_port_value(arg, expected, |v| {
        if let Value::PortOutput(p) = v {
            p
        } else {
            unreachable!("unexpected non-output-port value")
        }
    })
}

fn guard_port_value<T>(
    arg: &Value,
    expected: PortSpec,
    unwrap: impl FnOnce(&Value) -> &T,
) -> Result<&T, Exception> {
    match expected.check(arg) {
        Err(None) => Err(invalid_target(TypeName::PORT, arg)),
        Err(Some(spec)) => {
            Err(Condition::arg_type_error(FIRST_ARG_LABEL, expected, spec, arg).into())
        }
        Ok(()) => Ok(unwrap(arg)),
    }
}

fn read_op<T>(
    arg: Option<&Value>,
    env: &Frame,
    spec: PortSpec,
    op: impl FnOnce(&ReadPort) -> PortResult<T>,
    map: impl FnOnce(T) -> EvalResult,
) -> EvalResult {
    let port = arg.unwrap_or(&env.sys.stdin);
    let p = guard_input_port(port, spec)?;
    op(&p.borrow()).map_or_else(
        |err| Err(Condition::io_error(&err, env.sym, port).into()),
        map,
    )
}

fn read_op_mut<T>(
    arg: Option<&Value>,
    env: &Frame,
    spec: PortSpec,
    op: impl FnOnce(&mut ReadPort) -> PortResult<T>,
    map: impl FnOnce(T) -> EvalResult,
) -> EvalResult {
    let port = arg.unwrap_or(&env.sys.stdin);
    let p = guard_input_port(port, spec)?;
    op(&mut p.borrow_mut()).map_or_else(
        |err| Err(Condition::io_error(&err, env.sym, port).into()),
        map,
    )
}

fn put_char(ch: char, arg: Option<&Value>, env: &Frame) -> EvalResult {
    // TODO: port fallback must be dynamic
    let port = arg.unwrap_or(&env.sys.stdout);
    let p = guard_output_port(port, PortSpec::TextualOutput)?;
    p.borrow_mut().put_char(ch).map_or_else(
        |err| Err(Condition::io_error(&err, env.sym, port).into()),
        |()| Ok(Value::Unspecified),
    )
}

fn write_object<'a, T: Display + 'a>(
    obj: &'a Value,
    port: Option<&Value>,
    env: &Frame,
    disp: impl FnOnce(&'a Value) -> T,
) -> EvalResult {
    let port = port.unwrap_or(&env.sys.stdout);
    let p = guard_output_port(port, PortSpec::TextualOutput)?;
    p.borrow_mut()
        .put_string(&disp(obj).to_string())
        .map_or_else(
            |err| Err(Condition::io_error(&err, env.sym, port).into()),
            |()| Ok(Value::Unspecified),
        )
}
