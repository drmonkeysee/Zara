// (zara ext)
use super::first;
use crate::{
    Exception,
    eval::{EvalResult, Frame},
    value::{Condition, StrRef, TypeName, Value},
};

/*
 * Zara Extended Library
 *
 * Additional functionality added on top of the core R7RS standard for testing,
 * debugging, inspection, and other utilities.
 */

pub(super) fn load(env: &Frame) {
    super::bind_intrinsic(env, "all-bindings", 0..0, bindings);
    super::bind_intrinsic(env, "all-symbols", 0..0, symbols);
    super::bind_intrinsic(env, "apropos", 0..1, apropos);
    super::bind_intrinsic(env, "mutable?", 1..1, is_mutable);

    // TODO ADD
    /*
    io-error?
    <other>-error?
    make-directory
    delete-directory
    read-directory (?)
    network ports
    is-directory?
    is-file?
    */

    super::bind_intrinsic(env, "io-error?", 1..1, is_io_error);
    super::bind_intrinsic(env, "system-error?", 1..1, is_system_error);

    // NOTE: convenience vars
    env.scope.bind(env.sym.get("null"), Value::Null);
    env.scope.bind(env.sym.get("void"), Value::Unspecified);

    // TODO: test variable
    env.scope.bind(
        env.sym.get("ex"),
        Value::Error(Condition::system_error("test error").into()),
    );
}

predicate!(is_io_error, Value::Error(c) if c.is_io_err());
predicate!(is_system_error, Value::Error(c) if c.is_sys_err());

// TODO: support passing in environment specifier
#[allow(clippy::unnecessary_wraps, reason = "infallible intrinsic")]
fn bindings(_args: &[Value], env: &Frame) -> EvalResult {
    Ok(Value::list(
        env.scope
            .sorted_bindings()
            .into_iter()
            .map(|(k, v)| Value::cons(Value::Symbol(k), v))
            .collect::<Vec<_>>(),
    ))
}

#[allow(clippy::unnecessary_wraps, reason = "infallible intrinsic")]
fn symbols(_args: &[Value], env: &Frame) -> EvalResult {
    Ok(Value::list(
        env.sym
            .sorted_symbols()
            .into_iter()
            .map(Value::Symbol)
            .collect::<Vec<_>>(),
    ))
}

// TODO: support passing in environment specifier
fn apropos(args: &[Value], env: &Frame) -> EvalResult {
    // TODO: map_or_default https://github.com/rust-lang/rust/issues/138099
    let pat = args.first().map_or_else(
        || Ok::<_, Exception>(StrRef::default()),
        |v| {
            v.as_refstr()
                .ok_or_else(|| super::invalid_target(TypeName::STRING, v))
        },
    )?;
    Ok(Value::list(
        env.scope
            .sorted_bindings()
            .into_iter()
            .filter(|(n, _)| n.contains(pat.as_ref()))
            .map(|(n, _)| Value::Symbol(n))
            .collect::<Vec<_>>(),
    ))
}

#[allow(clippy::unnecessary_wraps, reason = "infallible intrinsic")]
fn is_mutable(args: &[Value], _env: &Frame) -> EvalResult {
    Ok(Value::Boolean(matches!(
        super::first(args),
        Value::ByteVectorMut(_) | Value::PairMut(_) | Value::StringMut(_) | Value::VectorMut(_)
    )))
}
