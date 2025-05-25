mod time;

use crate::{
    eval::{Frame, IntrinsicFn, Procedure},
    value::Value,
};
use std::ops::Range;

pub(crate) fn load(env: &mut Frame) {
    time::load(env);
}

fn bind_intrinsic(env: &mut Frame, name: &str, arity: Range<u8>, body: IntrinsicFn) {
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
