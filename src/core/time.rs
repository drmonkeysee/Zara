use crate::{
    Exception,
    constant::Constant,
    eval::{EvalResult, Frame},
    number::Number,
    value::{Condition, Value, ValueRef},
};
use std::time::{SystemTime, UNIX_EPOCH};

pub(super) fn load(env: &mut Frame) {
    super::bind_intrinsic(env, "current-second", 0..0, current_second);
}

fn current_second(_args: &[ValueRef], _env: &Frame) -> EvalResult {
    SystemTime::now().duration_since(UNIX_EPOCH).map_or_else(
        |_| {
            Err(Exception::new(Condition::system_error(
                "system time failure",
            )))
        },
        |d| Ok(Value::Constant(Constant::Number(Number::real(d.as_secs_f64()))).into()),
    )
}
