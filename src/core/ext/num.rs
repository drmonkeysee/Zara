use crate::{
    eval::{EvalResult, Frame},
    value::{TypeName, Value},
};

pub(super) fn load(env: &Frame) {
    super::bind_intrinsic(env, "complex-conjugate", 1..1, conjugate);
}

fn conjugate(args: &[Value], _env: &Frame) -> EvalResult {
    let arg = super::first(args);
    if let Value::Number(x) = arg {
        Ok(Value::Number(x.clone().into_complex_conjugate()))
    } else {
        Err(super::invalid_target(TypeName::NUMBER, arg))
    }
}
