use crate::{eval::Frame, value::Value};
use std::rc::Rc;

pub(super) fn load(env: &mut Frame) {
    super::bind_intrinsic(env, "current-second", 0, current_second);
}

fn current_second(args: &[Rc<Value>], env: &Frame) -> Option<Rc<Value>> {
    todo!();
}
