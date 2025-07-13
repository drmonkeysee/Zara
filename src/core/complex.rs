// (scheme complex)
use super::FIRST_ARG_LABEL;
use crate::{
    eval::{EvalResult, Frame},
    number::{Complex, Number, NumericTypeName, Real},
    value::{Condition, TypeName, Value},
};

pub(super) fn load(env: &mut Frame) {
    super::bind_intrinsic(env, "make-rectangular", 2..2, make_rect);
    super::bind_intrinsic(env, "make-polar", 2..2, make_polar);
    super::bind_intrinsic(env, "real-part", 1..1, get_real);
    super::bind_intrinsic(env, "imag-part", 1..1, get_imag);
    super::bind_intrinsic(env, "magnitude", 1..1, get_mag);
    super::bind_intrinsic(env, "angle", 1..1, get_angle);
}

fn make_rect(args: &[Value], _env: &mut Frame) -> EvalResult {
    make_complex(args.first().unwrap(), args.get(1).unwrap(), Number::complex)
}

fn make_polar(args: &[Value], _env: &mut Frame) -> EvalResult {
    make_complex(args.first().unwrap(), args.get(1).unwrap(), Number::polar)
}

fn get_real(args: &[Value], _env: &mut Frame) -> EvalResult {
    get_complex_part(
        args.first().unwrap(),
        |z| z.real_part().clone(),
        Real::clone,
    )
}

fn get_imag(args: &[Value], _env: &mut Frame) -> EvalResult {
    get_complex_part(
        args.first().unwrap(),
        |z| z.imag_part().clone(),
        |_| Real::zero(),
    )
}

fn get_mag(args: &[Value], _env: &mut Frame) -> EvalResult {
    get_complex_part(args.first().unwrap(), Complex::to_magnitude, |r| {
        r.clone().into_abs()
    })
}

fn get_angle(args: &[Value], _env: &mut Frame) -> EvalResult {
    get_complex_part(args.first().unwrap(), Complex::to_angle, |_| Real::zero())
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::testutil::{TestEnv, ok_or_fail};

    #[test]
    fn get_real_complex() {
        let args = [Value::Number(Number::complex(4, 5))];
        let mut env = TestEnv::default();

        let v = get_real(&args, &mut env.new_frame());

        let r = ok_or_fail!(v);
        assert!(matches!(r, Value::Number(Number::Real(Real::Integer(_)))));
        assert_eq!(r.to_string(), "4");
    }

    #[test]
    fn get_imag_complex() {
        let args = [Value::Number(Number::complex(4, 5))];
        let mut env = TestEnv::default();

        let v = get_imag(&args, &mut env.new_frame());

        let r = ok_or_fail!(v);
        assert!(matches!(r, Value::Number(Number::Real(Real::Integer(_)))));
        assert_eq!(r.to_string(), "5");
    }

    #[test]
    fn get_real_real() {
        let args = [Value::Number(Number::real(8))];
        let mut env = TestEnv::default();

        let v = get_real(&args, &mut env.new_frame());

        let r = ok_or_fail!(v);
        assert!(matches!(r, Value::Number(Number::Real(Real::Integer(_)))));
        assert_eq!(r.to_string(), "8");
    }

    #[test]
    fn get_imag_real() {
        let args = [Value::Number(Number::real(8))];
        let mut env = TestEnv::default();

        let v = get_imag(&args, &mut env.new_frame());

        let r = ok_or_fail!(v);
        assert!(matches!(r, Value::Number(Number::Real(Real::Integer(_)))));
        assert_eq!(r.to_string(), "0");
    }

    #[test]
    fn get_magnitude_complex() {
        let args = [Value::Number(Number::complex(4, 5))];
        let mut env = TestEnv::default();

        let v = get_mag(&args, &mut env.new_frame());

        let r = ok_or_fail!(v);
        assert!(matches!(r, Value::Number(Number::Real(Real::Float(_)))));
        assert_eq!(r.to_string(), "6.4031242374328485");
    }

    #[test]
    fn get_angle_complex() {
        let args = [Value::Number(Number::complex(4, 5))];
        let mut env = TestEnv::default();

        let v = get_angle(&args, &mut env.new_frame());

        let r = ok_or_fail!(v);
        assert!(matches!(r, Value::Number(Number::Real(Real::Float(_)))));
        assert_eq!(r.to_string(), "0.8960553845713439");
    }

    #[test]
    fn get_mag_real() {
        let args = [Value::Number(Number::real(8))];
        let mut env = TestEnv::default();

        let v = get_mag(&args, &mut env.new_frame());

        let r = ok_or_fail!(v);
        assert!(matches!(r, Value::Number(Number::Real(Real::Integer(_)))));
        assert_eq!(r.to_string(), "8");
    }

    #[test]
    fn get_angle_real() {
        let args = [Value::Number(Number::real(8))];
        let mut env = TestEnv::default();

        let v = get_angle(&args, &mut env.new_frame());

        let r = ok_or_fail!(v);
        assert!(matches!(r, Value::Number(Number::Real(Real::Integer(_)))));
        assert_eq!(r.to_string(), "0");
    }
}
