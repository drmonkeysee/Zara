use crate::{
    eval::{EvalResult, Frame},
    value::Value,
};
use std::{env, mem, os};

const FEATURE_NAMES: [&str; 11] = [
    "r7rs",
    // TODO: "exact-closed",
    "exact-complex",
    "ieee-float",
    // TODO: "full-unicode",
    "ratios",
    env::consts::FAMILY,
    env::consts::OS,
    env::consts::ARCH,
    c_data_model(),
    endian(),
    env!("CARGO_PKG_NAME"),
    concat!(env!("CARGO_PKG_NAME"), "-", env!("CARGO_PKG_VERSION"),),
];

pub(super) fn load(env: &Frame) {
    super::bind_intrinsic(env, "features", 0..0, feature_list);
}

#[allow(clippy::unnecessary_wraps, reason = "infallible intrinsic")]
fn feature_list(_args: &[Value], env: &Frame) -> EvalResult {
    Ok(Value::list(
        FEATURE_NAMES.iter().map(|n| Value::Symbol(env.sym.get(n))),
    ))
}

const fn c_data_model() -> &'static str {
    match (
        mem::size_of::<os::raw::c_int>(),
        mem::size_of::<os::raw::c_long>(),
        mem::size_of::<os::raw::c_longlong>(),
    ) {
        (4, 4, 4) => "ilp32",
        (4, 4, 8) => "llp64",
        (4, 8, 8) => "lp64",
        (8, 8, 8) => "ilp64",
        _ => "unknown",
    }
}

const fn endian() -> &'static str {
    if cfg!(target_endian = "big") {
        "big-endian"
    } else {
        "little-endian"
    }
}
