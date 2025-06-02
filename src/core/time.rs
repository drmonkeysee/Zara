// (scheme time)
use crate::{
    Exception,
    eval::{Binding, EvalResult, Frame},
    number::Sign,
    value::{Condition, Value},
};
use std::time::{SystemTime, UNIX_EPOCH};

pub(super) fn load(scope: &mut Binding) {
    super::bind_intrinsic(scope, "current-jiffy", 0..0, current_jiffy);
    super::bind_intrinsic(scope, "current-second", 0..0, current_second);
    super::bind_intrinsic(scope, "jiffies-per-second", 0..0, jiffies_per_second);
}

fn current_jiffy(_args: &[Value], env: &mut Frame) -> EvalResult {
    let jiffies = env
        .sys
        .start_time
        .elapsed()
        .as_micros()
        .try_into()
        .map_err(|_| Exception::signal(Condition::system_error("jiffy clock overflow")))?;
    Ok(Value::real((Sign::Positive, jiffies)))
}

fn current_second(_args: &[Value], _env: &mut Frame) -> EvalResult {
    SystemTime::now().duration_since(UNIX_EPOCH).map_or_else(
        |_| {
            Err(Exception::signal(Condition::system_error(
                "system time negative overflow",
            )))
        },
        |d| Ok(Value::real(d.as_secs_f64())),
    )
}

#[allow(clippy::unnecessary_wraps, reason = "infallible intrinsic")]
fn jiffies_per_second(_args: &[Value], _env: &mut Frame) -> EvalResult {
    // NOTE: jiffy = microsecond (µs)
    Ok(Value::real(1_000_000))
}
