// (scheme base)
macro_rules! predicate {
    ($name:ident, $pred:pat $(if $guard:expr)?) => {
        fn $name(args: &[Value], _env: &Frame) -> EvalResult {
            Ok(Value::Boolean(matches!(first(args), $pred $(if $guard)?)))
        }
    };
}

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
    eval::{EvalResult, Frame, MAX_ARITY},
    number::{Number, NumericError, NumericTypeName},
    string::{Symbol, unicode::UnicodeError},
    value::{Condition, Port, PortProp, TypeName, Value},
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

    bind_intrinsic(env, "eof-object?", 1..1, is_eof);
    bind_intrinsic(env, "eof-object", 0..0, eof);
}

predicate!(is_input_port, Value::Port(p) if p.is_input());
predicate!(is_output_port, Value::Port(p) if p.is_output());
predicate!(is_textual_port, Value::Port(p) if p.is_textual());
predicate!(is_binary_port, Value::Port(p) if p.is_binary());
predicate!(is_port, Value::Port(_));
predicate!(is_eof, Value::Eof);

fn is_open_input(args: &[Value], _env: &Frame) -> EvalResult {
    guarded_port_op(first(args), PortProp::Input, PortProp::Output, is_port_open)
}

fn is_open_output(args: &[Value], _env: &Frame) -> EvalResult {
    guarded_port_op(first(args), PortProp::Output, PortProp::Input, is_port_open)
}

fn current_stdin(_args: &[Value], _env: &Frame) -> EvalResult {
    Ok(Value::stdin())
}

fn current_stdout(_args: &[Value], _env: &Frame) -> EvalResult {
    Ok(Value::stdout())
}

fn current_stderr(_args: &[Value], _env: &Frame) -> EvalResult {
    Ok(Value::stderr())
}

#[allow(clippy::unnecessary_wraps, reason = "infallible intrinsic")]
fn eof(_args: &[Value], _env: &Frame) -> EvalResult {
    Ok(Value::Eof)
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

fn is_port_open(p: &Port) -> EvalResult {
    Ok(Value::Boolean(p.is_open()))
}

fn guarded_port_op(
    arg: &Value,
    expected_prop: PortProp,
    invalid_prop: impl Display,
    op: impl FnOnce(&Port) -> EvalResult,
) -> EvalResult {
    let Value::Port(p) = arg else {
        return Err(invalid_target(expected_prop, arg));
    };
    if p.has_prop(&expected_prop) {
        op(p)
    } else {
        Err(Condition::arg_type_error(FIRST_ARG_LABEL, expected_prop, invalid_prop, arg).into())
    }
}
