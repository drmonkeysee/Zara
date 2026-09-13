use crate::{
    eval::{EvalResult, Frame},
    number::Number,
    value::{TypeName, Value},
};

pub(super) fn load(env: &Frame) {
    super::bind_intrinsic(env, "complex-conjugate", 1..1, conjugate);

    env.scope.bind(
        env.sym.get("inexact-max"),
        Value::Number(Number::float_max()),
    );
    env.scope.bind(
        env.sym.get("inexact-min"),
        Value::Number(Number::float_min()),
    );
    env.scope.bind(
        env.sym.get("inexact-min-pos"),
        Value::Number(Number::float_min_positive()),
    );
    env.scope.bind(
        env.sym.get("inexact-epsilon"),
        Value::Number(Number::epsilon()),
    );
    env.scope.bind(
        env.sym.get("inexact-max-exact"),
        Value::Number(Number::float_max_int()),
    );
    env.scope.bind(
        env.sym.get("inexact-min-exact"),
        Value::Number(Number::float_min_int()),
    );
}

fn conjugate(args: &[Value], _env: &Frame) -> EvalResult {
    let arg = super::first(args);
    if let Value::Number(x) = arg {
        Ok(Value::Number(x.clone().into_complex_conjugate()))
    } else {
        Err(super::invalid_target(TypeName::NUMBER, arg))
    }
}
