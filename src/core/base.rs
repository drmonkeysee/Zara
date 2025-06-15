// (scheme base)
macro_rules! predicate {
    ($name:ident, $pred:pat $(if $guard:expr)? $(,)?) => {
        fn $name(args: &[Value], _env: &mut Frame) -> EvalResult {
            let arg = args.first().unwrap();
            Ok(Value::Boolean(matches!(arg, $pred $(if $guard)?)))
        }
    };
}

macro_rules! try_predicate {
    ($name:ident, $kind:path, $valname:expr, $pred:expr $(,)?) => {
        fn $name(args: &[Value], _env: &mut Frame) -> EvalResult {
            let arg = args.first().unwrap();
            if let $kind(val) = arg {
                Ok(Value::Boolean($pred(val)))
            } else {
                Err(Condition::arg_error(FIRST_ARG_LABEL, $valname, arg).into())
            }
        }
    };
}

macro_rules! real_predicate {
    ($name:ident, $pred:expr $(,)?) => {
        fn $name(args: &[Value], _env: &mut Frame) -> EvalResult {
            let arg = args.first().unwrap();
            if let Value::Number(n) = arg {
                if let Number::Real(r) = n {
                    Ok(Value::Boolean($pred(r)))
                } else {
                    Err(Condition::arg_type_error(
                        FIRST_ARG_LABEL,
                        REAL_ARG_TNAME,
                        COMPLEX_ARG_TNAME,
                        arg,
                    )
                    .into())
                }
            } else {
                Err(Condition::arg_error(FIRST_ARG_LABEL, REAL_ARG_TNAME, arg).into())
            }
        }
    };
}

use super::FIRST_ARG_LABEL;
use crate::{
    Exception,
    eval::{Binding, EvalResult, Frame, MAX_ARITY},
    number::{Number, Real},
    value::{Condition, TypeName, Value},
};

const COMPLEX_ARG_TNAME: &'static str = "complex number";
const REAL_ARG_TNAME: &'static str = "real number";

pub(super) fn load(scope: &mut Binding) {
    // boolean
    super::bind_intrinsic(scope, "boolean?", 1..1, is_boolean);
    super::bind_intrinsic(scope, "boolean=?", 0..MAX_ARITY, all_boolean_equal);
    super::bind_intrinsic(scope, "not", 1..1, not);

    // number
    // NOTE: complex and number predicates are identical sets
    super::bind_intrinsic(scope, "complex?", 1..1, is_number);
    super::bind_intrinsic(scope, "exact?", 1..1, is_exact);
    super::bind_intrinsic(scope, "exact-integer?", 1..1, is_exact_integer);
    super::bind_intrinsic(scope, "finite?", 1..1, is_finite);
    super::bind_intrinsic(scope, "inexact?", 1..1, is_inexact);
    super::bind_intrinsic(scope, "infinite?", 1..1, is_infinite);
    super::bind_intrinsic(scope, "integer?", 1..1, is_integer);
    super::bind_intrinsic(scope, "nan?", 1..1, is_nan);
    super::bind_intrinsic(scope, "negative?", 1..1, is_negative);
    super::bind_intrinsic(scope, "number?", 1..1, is_number);
    super::bind_intrinsic(scope, "positive?", 1..1, is_positive);
    super::bind_intrinsic(scope, "rational?", 1..1, is_rational);
    super::bind_intrinsic(scope, "real?", 1..1, is_real);
    super::bind_intrinsic(scope, "zero?", 1..1, is_zero);
}

//
// Boolean
//

predicate!(is_boolean, Value::Boolean(_));
predicate!(not, Value::Boolean(false));

fn all_boolean_equal(args: &[Value], _env: &mut Frame) -> EvalResult {
    let mut it = args.iter().enumerate();
    let first = match it.next() {
        None => return Ok(Value::Boolean(true)),
        Some((_, Value::Boolean(b))) => b,
        Some((idx, v)) => {
            return Err(Condition::arg_error(&idx.to_string(), TypeName::BOOL, v).into());
        }
    };
    let result: Result<_, Exception> = it.try_fold(true, |acc, (idx, val)| {
        if let Value::Boolean(b) = val {
            Ok(acc && b == first)
        } else {
            Err(Condition::arg_error(&idx.to_string(), TypeName::BOOL, val).into())
        }
    });
    Ok(Value::Boolean(result?))
}

//
// Number
//

try_predicate!(is_exact, Value::Number, TypeName::NUMBER, |n: &Number| {
    !n.is_inexact()
});
try_predicate!(
    is_exact_integer,
    Value::Number,
    TypeName::NUMBER,
    |n: &Number| matches!(n, Number::Real(Real::Integer(_)))
);
try_predicate!(is_finite, Value::Number, TypeName::NUMBER, |n: &Number| {
    !n.is_infinite() && !n.is_nan()
});
try_predicate!(is_inexact, Value::Number, TypeName::NUMBER, |n: &Number| {
    n.is_inexact()
});
try_predicate!(
    is_infinite,
    Value::Number,
    TypeName::NUMBER,
    |n: &Number| n.is_infinite()
);
predicate!(is_integer, Value::Number(Number::Real(r)) if r.is_integer());
try_predicate!(is_nan, Value::Number, TypeName::NUMBER, |n: &Number| n
    .is_nan());
real_predicate!(is_negative, |r: &Real| r.is_negative());
predicate!(is_number, Value::Number(_));
real_predicate!(is_positive, |r: &Real| r.is_positive());
predicate!(is_rational, Value::Number(Number::Real(r)) if r.is_rational());
predicate!(is_real, Value::Number(Number::Real(_)));
try_predicate!(is_zero, Value::Number, TypeName::NUMBER, |n: &Number| n
    .is_zero());

#[cfg(test)]
mod tests {
    use super::*;
    use crate::testutil::{TestEnv, err_or_fail, extract_or_fail, ok_or_fail};

    #[test]
    fn all_boolean_empty() {
        let args = [];
        let mut env = TestEnv::default();

        let r = all_boolean_equal(&args, &mut env.new_frame());

        let v = ok_or_fail!(r);
        assert!(matches!(v, Value::Boolean(true)));
    }

    #[test]
    fn all_boolean_single() {
        let cases = [[Value::Boolean(true)], [Value::Boolean(false)]];
        for case in cases {
            let mut env = TestEnv::default();

            let r = all_boolean_equal(&case, &mut env.new_frame());

            let v = ok_or_fail!(r);
            assert!(matches!(v, Value::Boolean(true)));
        }
    }

    #[test]
    fn all_boolean_trues() {
        let args = [
            Value::Boolean(true),
            Value::Boolean(true),
            Value::Boolean(true),
        ];
        let mut env = TestEnv::default();

        let r = all_boolean_equal(&args, &mut env.new_frame());

        let v = ok_or_fail!(r);
        assert!(matches!(v, Value::Boolean(true)));
    }

    #[test]
    fn all_boolean_falses() {
        let args = [
            Value::Boolean(false),
            Value::Boolean(false),
            Value::Boolean(false),
        ];
        let mut env = TestEnv::default();

        let r = all_boolean_equal(&args, &mut env.new_frame());

        let v = ok_or_fail!(r);
        assert!(matches!(v, Value::Boolean(true)));
    }

    #[test]
    fn all_boolean_mix() {
        let args = [
            Value::Boolean(false),
            Value::Boolean(true),
            Value::Boolean(false),
        ];
        let mut env = TestEnv::default();

        let r = all_boolean_equal(&args, &mut env.new_frame());

        let v = ok_or_fail!(r);
        assert!(matches!(v, Value::Boolean(false)));
    }

    #[test]
    fn all_boolean_invalid_param() {
        let args = [
            Value::Boolean(false),
            Value::String("foo".into()),
            Value::Boolean(false),
        ];
        let mut env = TestEnv::default();

        let r = all_boolean_equal(&args, &mut env.new_frame());

        let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
        assert_eq!(
            err.to_string(),
            "#<env-error \"invalid type for arg `1` - expected: boolean, got: string\" (\"foo\")>"
        );
    }

    #[test]
    fn all_boolean_bails_on_first_invalid_param() {
        let args = [
            Value::Boolean(false),
            Value::String("foo".into()),
            Value::Boolean(false),
            Value::null(),
        ];
        let mut env = TestEnv::default();

        let r = all_boolean_equal(&args, &mut env.new_frame());

        let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
        assert_eq!(
            err.to_string(),
            "#<env-error \"invalid type for arg `1` - expected: boolean, got: string\" (\"foo\")>"
        );
    }
}
