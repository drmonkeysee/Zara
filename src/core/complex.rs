// (scheme complex)
use super::FIRST_ARG_LABEL;
use crate::{
    eval::{EvalResult, Frame},
    number::{Number, NumericTypeName, Real},
    value::{Condition, TypeName, Value},
};

pub(super) fn load(env: &Frame) {
    super::bind_intrinsic(env, "make-rectangular", 2..2, make_rect);
    super::bind_intrinsic(env, "make-polar", 2..2, make_polar);
    super::bind_intrinsic(env, "real-part", 1..1, get_real);
    super::bind_intrinsic(env, "imag-part", 1..1, get_imag);
    super::bind_intrinsic(env, "magnitude", 1..1, get_mag);
    super::bind_intrinsic(env, "angle", 1..1, get_angle);
}

fn make_rect(args: &[Value], _env: &Frame) -> EvalResult {
    make_complex(super::first(args), super::second(args), Number::complex)
}

fn make_polar(args: &[Value], _env: &Frame) -> EvalResult {
    make_complex(super::first(args), super::second(args), Number::polar)
}

fn get_real(args: &[Value], _env: &Frame) -> EvalResult {
    get_complex_part(super::first(args), Number::into_real)
}

fn get_imag(args: &[Value], _env: &Frame) -> EvalResult {
    get_complex_part(super::first(args), Number::into_imag)
}

fn get_mag(args: &[Value], _env: &Frame) -> EvalResult {
    get_complex_part(super::first(args), Number::into_magnitude)
}

fn get_angle(args: &[Value], _env: &Frame) -> EvalResult {
    get_complex_part(super::first(args), Number::into_angle)
}

fn make_complex(x: &Value, y: &Value, ctor: impl FnOnce(Real, Real) -> Number) -> EvalResult {
    let Value::Number(real) = x else {
        return Err(super::invalid_target(NumericTypeName::REAL, x));
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
        return Err(super::invalid_target(NumericTypeName::REAL, y));
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

fn get_complex_part(arg: &Value, get: impl FnOnce(Number) -> Real) -> EvalResult {
    if let Value::Number(x) = arg {
        Ok(Value::real(get(x.clone())))
    } else {
        Err(super::invalid_target(TypeName::NUMBER, arg))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::testutil::{TestEnv, ok_or_fail};
    use std::assert_matches;

    #[test]
    fn get_real_complex() {
        let args = [Value::Number(Number::complex(4, 5))];
        let env = TestEnv::default();

        let v = get_real(&args, &env.new_frame());

        let r = ok_or_fail!(v);
        assert_matches!(r, Value::Number(Number::Real(Real::Integer(_))));
        assert_eq!(r.as_datum().to_string(), "4");
    }

    #[test]
    fn get_imag_complex() {
        let args = [Value::Number(Number::complex(4, 5))];
        let env = TestEnv::default();

        let v = get_imag(&args, &env.new_frame());

        let r = ok_or_fail!(v);
        assert_matches!(r, Value::Number(Number::Real(Real::Integer(_))));
        assert_eq!(r.as_datum().to_string(), "5");
    }

    #[test]
    fn get_real_real() {
        let args = [Value::real(8)];
        let env = TestEnv::default();

        let v = get_real(&args, &env.new_frame());

        let r = ok_or_fail!(v);
        assert_matches!(r, Value::Number(Number::Real(Real::Integer(_))));
        assert_eq!(r.as_datum().to_string(), "8");
    }

    #[test]
    fn get_imag_real() {
        let args = [Value::real(8)];
        let env = TestEnv::default();

        let v = get_imag(&args, &env.new_frame());

        let r = ok_or_fail!(v);
        assert_matches!(r, Value::Number(Number::Real(Real::Integer(_))));
        assert_eq!(r.as_datum().to_string(), "0");
    }

    #[test]
    fn get_magnitude_complex() {
        let args = [Value::Number(Number::complex(4, 5))];
        let env = TestEnv::default();

        let v = get_mag(&args, &env.new_frame());

        let r = ok_or_fail!(v);
        assert_matches!(r, Value::Number(Number::Real(Real::Float(_))));
        assert_eq!(r.as_datum().to_string(), "6.4031242374328485");
    }

    #[test]
    fn get_angle_complex() {
        let args = [Value::Number(Number::complex(4, 5))];
        let env = TestEnv::default();

        let v = get_angle(&args, &env.new_frame());

        let r = ok_or_fail!(v);
        assert_matches!(r, Value::Number(Number::Real(Real::Float(_))));
        assert_eq!(r.as_datum().to_string(), "0.8960553845713439");
    }

    #[test]
    fn get_mag_real() {
        let args = [Value::real(8)];
        let env = TestEnv::default();

        let v = get_mag(&args, &env.new_frame());

        let r = ok_or_fail!(v);
        assert_matches!(r, Value::Number(Number::Real(Real::Integer(_))));
        assert_eq!(r.as_datum().to_string(), "8");
    }

    #[test]
    fn get_mag_negative_real() {
        let args = [Value::real(-8)];
        let env = TestEnv::default();

        let v = get_mag(&args, &env.new_frame());

        let r = ok_or_fail!(v);
        assert_matches!(r, Value::Number(Number::Real(Real::Integer(_))));
        assert_eq!(r.as_datum().to_string(), "8");
    }

    #[test]
    fn get_angle_real() {
        let args = [Value::real(8)];
        let env = TestEnv::default();

        let v = get_angle(&args, &env.new_frame());

        let r = ok_or_fail!(v);
        assert_matches!(r, Value::Number(Number::Real(Real::Integer(_))));
        assert_eq!(r.as_datum().to_string(), "0");
    }

    #[test]
    fn get_angle_negative_real() {
        let args = [Value::real(-8)];
        let env = TestEnv::default();

        let v = get_angle(&args, &env.new_frame());

        let r = ok_or_fail!(v);
        assert_matches!(r, Value::Number(Number::Real(Real::Float(_))));
        assert_eq!(r.as_datum().to_string(), "3.141592653589793");
    }

    #[test]
    fn get_angle_negative_float() {
        let args = [Value::real(-1.0)];
        let env = TestEnv::default();

        let v = get_angle(&args, &env.new_frame());

        let r = ok_or_fail!(v);
        assert_matches!(r, Value::Number(Number::Real(Real::Float(_))));
        assert_eq!(r.as_datum().to_string(), "3.141592653589793");
    }

    #[test]
    fn get_angle_negative_infinity() {
        let args = [Value::real(f64::NEG_INFINITY)];
        let env = TestEnv::default();

        let v = get_angle(&args, &env.new_frame());

        let r = ok_or_fail!(v);
        assert_matches!(r, Value::Number(Number::Real(Real::Float(_))));
        assert_eq!(r.as_datum().to_string(), "3.141592653589793");
    }
}
