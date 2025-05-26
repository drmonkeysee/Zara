use crate::{
    Exception,
    eval::{EvalResult, Frame},
    value::{Condition, Value, ValueRef},
};
use std::time::{SystemTime, UNIX_EPOCH};

pub(super) fn load(env: &mut Frame) {
    super::bind_intrinsic(env, "current-jiffy", 0..0, current_jiffy);
    super::bind_intrinsic(env, "current-second", 0..0, current_second);
    super::bind_intrinsic(env, "jiffies-per-second", 0..0, jiffies_per_second);
}

fn current_jiffy(_args: &[ValueRef], _env: &Frame) -> EvalResult {
    todo!("current-jiffy");
}

fn current_second(_args: &[ValueRef], _env: &Frame) -> EvalResult {
    SystemTime::now().duration_since(UNIX_EPOCH).map_or_else(
        |_| {
            Err(Exception::new(Condition::system_error(
                "system time failure",
            )))
        },
        |d| Ok(Value::number(d.as_secs_f64()).into()),
    )
}

fn jiffies_per_second(_args: &[ValueRef], _env: &Frame) -> EvalResult {
    todo!("jiffies-per-second");
}
