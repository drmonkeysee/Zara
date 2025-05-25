use crate::{
    eval::{EvalResult, Frame},
    value::ValueRef,
};

pub(super) fn load(env: &mut Frame) {
    super::bind_intrinsic(env, "current-second", 0..0, current_second);
}

fn current_second(args: &[ValueRef], env: &Frame) -> EvalResult {
    todo!("current second");
}
