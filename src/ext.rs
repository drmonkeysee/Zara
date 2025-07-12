use crate::{
    Exception,
    core::{self, FIRST_ARG_LABEL, invalid_target},
    eval::{Binding, EvalResult, Frame},
    value::{Condition, TypeName, Value},
};
use std::rc::Rc;

/*
 * Zara Extended Library
 *
 * Additional functionality added on top of the core R7RS standard for testing,
 * debugging, inspection, and other utilities.
 */

// TODO: add cli arg for excluding this
pub(crate) fn load(scope: &mut Binding) {
    core::bind_intrinsic(scope, "all-bindings", 0..0, bindings);
    core::bind_intrinsic(scope, "all-symbols", 0..0, symbols);
    core::bind_intrinsic(scope, "apropos", 0..1, apropos);

    // NOTE: convenience vars
    scope.bind("null", Value::null());
    scope.bind("void", Value::Unspecified);

    // TODO: test variable
    scope.bind(
        "ex",
        Value::Error(Condition::system_error("test error").into()),
    );
}

// TODO: support passing in environment specifier
#[allow(clippy::unnecessary_wraps, reason = "infallible intrinsic")]
fn bindings(_args: &[Value], env: &mut Frame) -> EvalResult {
    Ok(Value::list(
        env.scope
            .get_refs()
            .into_iter()
            .map(|(k, v)| Value::cons(Value::symbol(env.sym.get(k)), v.clone()))
            .collect::<Vec<_>>(),
    ))
}

#[allow(clippy::unnecessary_wraps, reason = "infallible intrinsic")]
fn symbols(_args: &[Value], env: &mut Frame) -> EvalResult {
    Ok(Value::list(
        env.sym
            .get_refs()
            .into_iter()
            .map(|s| Value::symbol(Rc::clone(s)))
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
            .get_refs()
            .into_iter()
            .filter_map(|(n, _)| n.contains(pat).then(|| Value::symbol(env.sym.get(n))))
            .collect::<Vec<_>>(),
    ))
}
