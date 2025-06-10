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
    core::bind_intrinsic(scope, "zara-symbol-table", 0..0, symbol_table);

    // TODO: test variables
    scope.bind("x", Value::real(5));
    scope.bind("z", Value::Unspecified);
}

fn symbol_table(_args: &[Value], env: &mut Frame) -> EvalResult {
    Ok(Value::list(
        env.sym
            .get_all()
            .into_iter()
            .map(|s| Value::symbol(Rc::clone(s)))
            .collect::<Vec<_>>(),
    ))
}
