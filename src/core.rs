mod time;

use crate::{
    eval::{Arity, Frame, IntrinsicFn, Procedure},
    value::Value,
};

pub(crate) fn load(env: &mut Frame) {
    time::load(env);
}

fn bind_intrinsic(env: &mut Frame, name: &str, arity: Arity, body: IntrinsicFn) {
    env.bind(
        name,
        Value::Procedure(Procedure::intrinsic(name, arity, body).into()),
    );

    // TODO: test variables
    env.bind(
        "x",
        Value::Constant(crate::constant::Constant::Number(
            crate::number::Number::real(5),
        )),
    );
    env.bind("z", Value::Unspecified);
}
