// (scheme complex)
use crate::{
    eval::{Binding, EvalResult, Frame},
    value::Value,
};

pub(super) fn load(scope: &mut Binding) {
    super::bind_intrinsic(scope, "make-rectangular", 2..2, make_rect);
    super::bind_intrinsic(scope, "make-polar", 2..2, make_polar);
    super::bind_intrinsic(scope, "real-part", 1..1, get_real);
    super::bind_intrinsic(scope, "imag-part", 1..1, get_imag);
    super::bind_intrinsic(scope, "magnitude", 1..1, get_mag);
    super::bind_intrinsic(scope, "angle", 1..1, get_angle);
}

fn make_rect(args: &[Value], _env: &mut Frame) -> EvalResult {
    todo!();
}

fn make_polar(args: &[Value], _env: &mut Frame) -> EvalResult {
    todo!();
}

fn get_real(args: &[Value], _env: &mut Frame) -> EvalResult {
    todo!();
}

fn get_imag(args: &[Value], _env: &mut Frame) -> EvalResult {
    todo!();
}

fn get_mag(args: &[Value], _env: &mut Frame) -> EvalResult {
    todo!();
}

fn get_angle(args: &[Value], _env: &mut Frame) -> EvalResult {
    todo!();
}
