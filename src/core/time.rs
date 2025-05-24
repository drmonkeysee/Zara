use crate::{
    eval::Frame,
    value::{ValueObj, ValueRef},
};

pub(super) fn load(env: &mut Frame) {
    super::bind_intrinsic(env, "current-second", 0..0, current_second);
}

fn current_second(args: &[ValueRef], env: &Frame) -> ValueObj {
    todo!("current second");
}
