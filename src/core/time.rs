use crate::{
    Exception,
    eval::{Binding, EvalResult, Frame},
    number::Sign,
    value::{Condition, Value, ValueRef},
};
use std::time::{SystemTime, UNIX_EPOCH};

pub(super) fn load(scope: &mut Binding) {
    super::bind_intrinsic(scope, "current-jiffy", 0..0, current_jiffy);
    super::bind_intrinsic(scope, "current-second", 0..0, current_second);
    super::bind_intrinsic(scope, "jiffies-per-second", 0..0, jiffies_per_second);
}

fn current_jiffy(_args: &[ValueRef], env: &Frame) -> EvalResult {
    Ok(Value::number((
        Sign::Positive,
        env.sys.start_time.elapsed().as_micros() as u64,
    ))
    .into())
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
    // NOTE: jiffy = microsecond (µs)
    Ok(Value::number(1_000_000).into())
}
