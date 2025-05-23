mod time;

use crate::{
    eval::{Frame, IntrinsicFn, Procedure},
    value::Value,
};

pub(crate) fn load(env: &mut Frame) {
    time::load(env);
}

fn bind_intrinsic(env: &mut Frame, name: &str, arity: u8, body: IntrinsicFn) {
    env.bind(
        name,
        Value::Procedure(Procedure::intrinsic(name, arity, body)),
    );
}
