use crate::{
    core,
    eval::{Binding, EvalResult, Frame},
    value::{Condition, Value},
};
use std::rc::Rc;

/*
 * Zara Extended Library
 *
 * Additional functionality added on top of the core R7RS standard for testing,
 * debugging, inspection, and other utilities.
 */

pub(crate) fn load(scope: &mut Binding) {
    core::bind_intrinsic(scope, "zara-bindings", 0..0, bindings);
    core::bind_intrinsic(scope, "zara-symbols", 0..0, symbols);

    // TODO: test variables
    scope.bind("x", Value::real(5));
    scope.bind("z", Value::Unspecified);
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
