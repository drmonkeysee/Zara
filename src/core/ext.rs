// (zara ext)
use super::FIRST_ARG_LABEL;
use crate::{
    Exception,
    eval::{EvalResult, Frame},
    value::{Condition, TypeName, Value},
};

/*
 * Zara Extended Library
 *
 * Additional functionality added on top of the core R7RS standard for testing,
 * debugging, inspection, and other utilities.
 */

pub(crate) fn load(env: &mut Frame) {
    super::bind_intrinsic(env, "all-bindings", 0..0, bindings);
    super::bind_intrinsic(env, "all-symbols", 0..0, symbols);
    super::bind_intrinsic(env, "apropos", 0..1, apropos);

    // NOTE: convenience vars
    env.scope.bind(env.sym.get("null"), Value::null());
    env.scope.bind(env.sym.get("void"), Value::Unspecified);

    // TODO: test variable
    env.scope.bind(
        env.sym.get("ex"),
        Value::Error(Condition::system_error("test error").into()),
    );
}

// TODO: support passing in environment specifier
#[allow(clippy::unnecessary_wraps, reason = "infallible intrinsic")]
fn bindings(_args: &[Value], env: &mut Frame) -> EvalResult {
    Ok(Value::list(
        env.scope
            .sorted_bindings()
            .into_iter()
            .map(|(k, v)| Value::cons(Value::Symbol(env.sym.get(&k)), v))
            .collect::<Vec<_>>(),
    ))
}

#[allow(clippy::unnecessary_wraps, reason = "infallible intrinsic")]
fn symbols(_args: &[Value], env: &mut Frame) -> EvalResult {
    Ok(Value::list(
        env.sym
            .sorted_symbols()
            .into_iter()
            .map(Value::Symbol)
            .collect::<Vec<_>>(),
    ))
}

// TODO: support passing in environment specifier
fn apropos(args: &[Value], env: &mut Frame) -> EvalResult {
    let pat = args.first().map_or(Ok::<_, Exception>(""), |v| {
        if let Value::String(s) = v {
            Ok(s)
        } else {
            invalid_target!(TypeName::STRING, v)
        }
    })?;
    Ok(Value::list(
        env.scope
            .sorted_bindings()
            .into_iter()
            .filter(|(n, _)| n.contains(pat))
            .map(|(n, _)| Value::Symbol(env.sym.get(&n)))
            .collect::<Vec<_>>(),
    ))
}
