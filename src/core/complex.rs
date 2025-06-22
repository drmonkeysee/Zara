// (scheme complex)
use super::FIRST_ARG_LABEL;
use crate::{
    eval::{Binding, EvalResult, Frame},
    number::{Complex, Number, NumericTypeName, Real},
    value::{Condition, TypeName, Value},
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
    let x = args.first().unwrap();
    let y = args.get(1).unwrap();
    make_complex(x, y, Number::complex)
}

fn make_polar(args: &[Value], _env: &mut Frame) -> EvalResult {
    let x = args.first().unwrap();
    let y = args.get(1).unwrap();
    make_complex(x, y, Number::polar)
}

fn get_real(args: &[Value], _env: &mut Frame) -> EvalResult {
    let arg = args.first().unwrap();
    get_complex_part(arg, |z| z.real().clone(), |r| r.clone())
}

fn get_imag(args: &[Value], _env: &mut Frame) -> EvalResult {
    let arg = args.first().unwrap();
    get_complex_part(arg, |z| z.imaginary().clone(), |_| Real::zero())
}

fn get_mag(args: &[Value], _env: &mut Frame) -> EvalResult {
    let arg = args.first().unwrap();
    get_complex_part(arg, |z| z.magnitude(), |r| todo!("absolute value of r"))
}

fn get_angle(args: &[Value], _env: &mut Frame) -> EvalResult {
    let arg = args.first().unwrap();
    get_complex_part(arg, |z| z.angle(), |_| Real::zero())
}

fn make_complex(x: &Value, y: &Value, ctor: impl FnOnce(Real, Real) -> Number) -> EvalResult {
    let Value::Number(real) = x else {
        return invalid_target!(NumericTypeName::REAL, x);
    };
    let Number::Real(r) = real else {
        return Err(Condition::arg_type_error(
            FIRST_ARG_LABEL,
            NumericTypeName::REAL,
            real.as_typename(),
            x,
        )
        .into());
    };
    let Value::Number(imag) = y else {
        return invalid_target!(NumericTypeName::REAL, y);
    };
    let Number::Real(i) = imag else {
        return Err(Condition::arg_type_error(
            FIRST_ARG_LABEL,
            NumericTypeName::REAL,
            real.as_typename(),
            y,
        )
        .into());
    };
    Ok(Value::Number(ctor(r.clone(), i.clone())))
}

fn get_complex_part(
    arg: &Value,
    get: impl FnOnce(&Complex) -> Real,
    fallback: impl FnOnce(&Real) -> Real,
) -> EvalResult {
    if let Value::Number(n) = arg {
        Ok(Value::Number(Number::real(match n {
            Number::Complex(z) => get(z),
            Number::Real(r) => fallback(r),
        })))
    } else {
        invalid_target!(TypeName::NUMBER, arg)
    }
}
