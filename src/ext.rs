use crate::{
    core,
    eval::{Binding, EvalResult, Frame},
    value::Value,
};
use std::rc::Rc;

/*
 * Zara Extended Library
 *
 * Additional functionality added on top of the core R7RS standard for testing,
 * debugging, inspection, and other utilities.
 */

pub(crate) fn load(scope: &mut Binding) {
    core::bind_intrinsic(scope, "zara-symbols", 0..0, symbols);

    // TODO: test variables
    scope.bind("x", Value::real(5));
    scope.bind("z", Value::Unspecified);
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
