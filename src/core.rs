mod procctx;
mod time;

use crate::{
    eval::{Arity, Binding, IntrinsicFn, Procedure},
    value::Value,
};

/*
 * R7RS Core Library
 *
 * All intrinsic functions assume required arguments are present and will use
 * unchecked access to retrieve them; arity is checked by call-expr evaluation
 * so this should always be a safe assumption.
 */

pub(crate) fn load(scope: &mut Binding) {
    procctx::load(scope);
    time::load(scope);
}

pub(crate) fn bind_intrinsic(scope: &mut Binding, name: &str, arity: Arity, body: IntrinsicFn) {
    scope.bind(
        name,
        Value::Procedure(Procedure::intrinsic(name, arity, body).into()),
    );
}
