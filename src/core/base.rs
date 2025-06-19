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

macro_rules! seq_predicate {
    ($name:ident, $kind:path, $valname:expr, $pred:expr $(,)?) => {
        fn $name(args: &[Value], _env: &mut Frame) -> EvalResult {
            let mut it = args.iter().enumerate();
            let first = match it.next() {
                None => return Ok(Value::Boolean(true)),
                Some((_, $kind(a))) => a,
                Some((idx, v)) => {
                    return Err(Condition::arg_error(&idx.to_string(), $valname, v).into());
                }
            };
            let result: Result<_, Exception> =
                it.try_fold((true, first), |(acc, prev), (idx, val)| {
                    if let $kind(next) = val {
                        Ok((acc && $pred(prev, next), next))
                    } else {
                        Err(Condition::arg_error(&idx.to_string(), $valname, val).into())
                    }
                });
            Ok(Value::Boolean(result?.0))
        }
    };
}

macro_rules! vec_length {
    ($name:ident, $kind:path, $valname:expr $(,)?) => {
        fn $name(args: &[Value], _env: &mut Frame) -> EvalResult {
            let arg = args.first().unwrap();
            if let $kind(v) = arg {
                Ok(Value::Number(Number::from_usize(v.len())))
            } else {
                Err(Condition::arg_error(FIRST_ARG_LABEL, $valname, arg).into())
            }
        }
    };
}

macro_rules! cadr_compose {
    ($val:expr, a $(,)?) => {
        pcar($val)
    };
    ($val:expr, d $(,)?) => {
        pcdr($val)
    };
    ($val:expr, a $(, $rest:ident)+ $(,)?) => {
        pcar(&cadr_compose!($val, $($rest),+)?)
    };
    ($val:expr, d $(, $rest:ident)+ $(,)?) => {
        pcdr(&cadr_compose!($val, $($rest),+)?)
    };
}

macro_rules! cadr_func {
    ($name:ident $(, $compose:ident)+ $(,)?) => {
        fn $name(args: &[Value], _env: &mut Frame) -> EvalResult {
            let arg = args.first().unwrap();
            cadr_compose!(&arg, $($compose),+)
        }
    };
}

use super::FIRST_ARG_LABEL;
use crate::{
    Exception,
    eval::{Binding, EvalResult, Frame, MAX_ARITY},
    number::{Integer, Number, NumericTypeName, Real},
    value::{Condition, TypeName, Value},
};
use std::rc::Rc;

const REAL_ARG_TNAME: &str = "real";

pub(super) fn load(scope: &mut Binding) {
    // booleans
    super::bind_intrinsic(scope, "boolean?", 1..1, is_boolean);
    super::bind_intrinsic(scope, "boolean=?", 0..MAX_ARITY, booleans_equal);
    super::bind_intrinsic(scope, "not", 1..1, not);

    // bytevectors
    super::bind_intrinsic(scope, "bytevector?", 1..1, is_bytevector);
    super::bind_intrinsic(scope, "bytevector-length", 1..1, bytevector_length);

    // characters
    super::bind_intrinsic(scope, "char?", 1..1, is_char);
    super::bind_intrinsic(scope, "char=?", 0..MAX_ARITY, chars_eq);
    super::bind_intrinsic(scope, "char<?", 0..MAX_ARITY, chars_lt);
    super::bind_intrinsic(scope, "char<=?", 0..MAX_ARITY, chars_lte);
    super::bind_intrinsic(scope, "char>?", 0..MAX_ARITY, chars_gt);
    super::bind_intrinsic(scope, "char>=?", 0..MAX_ARITY, chars_gte);
    super::bind_intrinsic(scope, "char-alphabetic?", 1..1, is_alphabetic);
    super::bind_intrinsic(scope, "char-lower-case?", 1..1, is_lowercase);
    super::bind_intrinsic(scope, "char-numeric?", 1..1, is_numeric);
    super::bind_intrinsic(scope, "char-upper-case?", 1..1, is_uppercase);
    super::bind_intrinsic(scope, "char-whitespace?", 1..1, is_whitespace);
    super::bind_intrinsic(scope, "char->integer", 1..1, char_to_integer);

    // equivalence
    super::bind_intrinsic(scope, "eq?", 2..2, is_eq);
    super::bind_intrinsic(scope, "equal?", 2..2, is_equal);
    super::bind_intrinsic(scope, "eqv?", 2..2, is_eqv);

    // numbers
    // NOTE: complex and number predicates are identical sets
    super::bind_intrinsic(scope, "complex?", 1..1, is_number);
    super::bind_intrinsic(scope, "even?", 1..1, is_even);
    super::bind_intrinsic(scope, "exact?", 1..1, is_exact);
    super::bind_intrinsic(scope, "exact-integer?", 1..1, is_exact_integer);
    super::bind_intrinsic(scope, "finite?", 1..1, is_finite);
    super::bind_intrinsic(scope, "inexact?", 1..1, is_inexact);
    super::bind_intrinsic(scope, "infinite?", 1..1, is_infinite);
    super::bind_intrinsic(scope, "integer?", 1..1, is_integer);
    super::bind_intrinsic(scope, "nan?", 1..1, is_nan);
    super::bind_intrinsic(scope, "negative?", 1..1, is_negative);
    super::bind_intrinsic(scope, "number?", 1..1, is_number);
    super::bind_intrinsic(scope, "odd?", 1..1, is_odd);
    super::bind_intrinsic(scope, "positive?", 1..1, is_positive);
    super::bind_intrinsic(scope, "rational?", 1..1, is_rational);
    super::bind_intrinsic(scope, "real?", 1..1, is_real);
    super::bind_intrinsic(scope, "zero?", 1..1, is_zero);

    // pairs and lists
    super::bind_intrinsic(scope, "car", 1..1, car);
    super::bind_intrinsic(scope, "cdr", 1..1, cdr);
    super::bind_intrinsic(scope, "caar", 1..1, caar);
    super::bind_intrinsic(scope, "cadr", 1..1, cadr);
    super::bind_intrinsic(scope, "cdar", 1..1, cdar);
    super::bind_intrinsic(scope, "cddr", 1..1, cddr);
    super::bind_intrinsic(scope, "caaar", 1..1, caaar);
    super::bind_intrinsic(scope, "caadr", 1..1, caadr);
    super::bind_intrinsic(scope, "cadar", 1..1, cadar);
    super::bind_intrinsic(scope, "caddr", 1..1, caddr);
    super::bind_intrinsic(scope, "cdaar", 1..1, cdaar);
    super::bind_intrinsic(scope, "cdadr", 1..1, cdadr);
    super::bind_intrinsic(scope, "cddar", 1..1, cddar);
    super::bind_intrinsic(scope, "cdddr", 1..1, cdddr);
    super::bind_intrinsic(scope, "caaaar", 1..1, caaaar);
    super::bind_intrinsic(scope, "caaadr", 1..1, caaadr);
    super::bind_intrinsic(scope, "caadar", 1..1, caadar);
    super::bind_intrinsic(scope, "caaddr", 1..1, caaddr);
    super::bind_intrinsic(scope, "cadaar", 1..1, cadaar);
    super::bind_intrinsic(scope, "cadadr", 1..1, cadadr);
    super::bind_intrinsic(scope, "caddar", 1..1, caddar);
    super::bind_intrinsic(scope, "cadddr", 1..1, cadddr);
    super::bind_intrinsic(scope, "cdaaar", 1..1, cdaaar);
    super::bind_intrinsic(scope, "cdaadr", 1..1, cdaadr);
    super::bind_intrinsic(scope, "cdadar", 1..1, cdadar);
    super::bind_intrinsic(scope, "cdaddr", 1..1, cdaddr);
    super::bind_intrinsic(scope, "cddaar", 1..1, cddaar);
    super::bind_intrinsic(scope, "cddadr", 1..1, cddadr);
    super::bind_intrinsic(scope, "cdddar", 1..1, cdddar);
    super::bind_intrinsic(scope, "cddddr", 1..1, cddddr);
    super::bind_intrinsic(scope, "length", 1..1, list_length);
    super::bind_intrinsic(scope, "list?", 1..1, is_list);
    super::bind_intrinsic(scope, "null?", 1..1, is_null);
    super::bind_intrinsic(scope, "pair?", 1..1, is_pair);

    // procedures
    super::bind_intrinsic(scope, "procedure?", 1..1, is_procedure);

    // strings
    super::bind_intrinsic(scope, "string?", 1..1, is_string);
    super::bind_intrinsic(scope, "string=?", 0..MAX_ARITY, strings_eq);
    super::bind_intrinsic(scope, "string<?", 0..MAX_ARITY, strings_lt);
    super::bind_intrinsic(scope, "string<=?", 0..MAX_ARITY, strings_lte);
    super::bind_intrinsic(scope, "string>?", 0..MAX_ARITY, strings_gt);
    super::bind_intrinsic(scope, "string>=?", 0..MAX_ARITY, strings_gte);
    super::bind_intrinsic(scope, "string-length", 1..1, string_length);

    // symbols
    super::bind_intrinsic(scope, "symbol?", 1..1, is_symbol);
    super::bind_intrinsic(scope, "symbol=?", 0..MAX_ARITY, symbols_eq);

    // vectors
    super::bind_intrinsic(scope, "vector?", 1..1, is_vector);
    super::bind_intrinsic(scope, "vector-length", 1..1, vector_length);
}

//
// Booleans
//

predicate!(is_boolean, Value::Boolean(_));
predicate!(not, Value::Boolean(false));
seq_predicate!(booleans_equal, Value::Boolean, TypeName::BOOL, bool::eq);

//
// Bytevectors
//

predicate!(is_bytevector, Value::ByteVector(_));
vec_length!(bytevector_length, Value::ByteVector, TypeName::BYTEVECTOR);

//
// Characters
//

predicate!(is_char, Value::Character(_));
seq_predicate!(chars_eq, Value::Character, TypeName::CHAR, char::eq);
seq_predicate!(chars_lt, Value::Character, TypeName::CHAR, char::lt);
seq_predicate!(chars_lte, Value::Character, TypeName::CHAR, char::le);
seq_predicate!(chars_gt, Value::Character, TypeName::CHAR, char::gt);
seq_predicate!(chars_gte, Value::Character, TypeName::CHAR, char::ge);
try_predicate!(
    is_alphabetic,
    Value::Character,
    TypeName::CHAR,
    |c: &char| c.is_alphabetic()
);
try_predicate!(
    is_lowercase,
    Value::Character,
    TypeName::CHAR,
    |c: &char| c.is_lowercase()
);
try_predicate!(
    is_numeric,
    Value::Character,
    TypeName::CHAR,
    |_c: &char| todo!("is_numeric is too broad, need only Nd")
);
try_predicate!(
    is_uppercase,
    Value::Character,
    TypeName::CHAR,
    |c: &char| c.is_uppercase()
);
try_predicate!(
    is_whitespace,
    Value::Character,
    TypeName::CHAR,
    |c: &char| c.is_whitespace()
);

fn char_to_integer(args: &[Value], _env: &mut Frame) -> EvalResult {
    let arg = args.first().unwrap();
    if let Value::Character(c) = arg {
        let n = <char as Into<u32>>::into(*c);
        Ok(Value::Number(Number::real(i64::from(n))))
    } else {
        Err(Condition::arg_error(FIRST_ARG_LABEL, TypeName::CHAR, arg).into())
    }
}

//
// Equivalence
//

#[allow(clippy::unnecessary_wraps, reason = "infallible intrinsic")]
fn is_eq(args: &[Value], _env: &mut Frame) -> EvalResult {
    let a = args.first().unwrap();
    let b = args.get(1).unwrap();
    Ok(Value::Boolean(a.is(b)))
}

#[allow(clippy::unnecessary_wraps, reason = "infallible intrinsic")]
fn is_equal(args: &[Value], _env: &mut Frame) -> EvalResult {
    let a = args.first().unwrap();
    let b = args.get(1).unwrap();
    Ok(Value::Boolean(a == b))
}

#[allow(clippy::unnecessary_wraps, reason = "infallible intrinsic")]
fn is_eqv(args: &[Value], _env: &mut Frame) -> EvalResult {
    let a = args.first().unwrap();
    let b = args.get(1).unwrap();
    Ok(Value::Boolean(a.is_eqv(b)))
}

//
// Numbers
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
predicate!(is_number, Value::Number(_));
predicate!(is_rational, Value::Number(Number::Real(r)) if r.is_rational());
predicate!(is_real, Value::Number(Number::Real(_)));
try_predicate!(is_zero, Value::Number, TypeName::NUMBER, |n: &Number| n
    .is_zero());

fn is_even(args: &[Value], _env: &mut Frame) -> EvalResult {
    int_predicate(args.first().unwrap(), Integer::is_even)
}

fn is_negative(args: &[Value], _env: &mut Frame) -> EvalResult {
    real_predicate(args.first().unwrap(), Real::is_negative)
}

fn is_odd(args: &[Value], _env: &mut Frame) -> EvalResult {
    int_predicate(args.first().unwrap(), |n| !n.is_even())
}

fn is_positive(args: &[Value], _env: &mut Frame) -> EvalResult {
    real_predicate(args.first().unwrap(), Real::is_positive)
}

fn real_predicate(arg: &Value, pred: impl FnOnce(&Real) -> bool) -> EvalResult {
    if let Value::Number(n) = arg {
        if let Number::Real(r) = n {
            Ok(Value::Boolean(pred(r)))
        } else {
            Err(Condition::arg_type_error(
                FIRST_ARG_LABEL,
                REAL_ARG_TNAME,
                NumericTypeName::COMPLEX,
                arg,
            )
            .into())
        }
    } else {
        Err(Condition::arg_error(FIRST_ARG_LABEL, REAL_ARG_TNAME, arg).into())
    }
}

fn int_predicate(arg: &Value, pred: impl FnOnce(&Integer) -> bool) -> EvalResult {
    if let Value::Number(n) = arg {
        match n.clone().into_exact_integer() {
            None => Err(Condition::arg_type_error(
                FIRST_ARG_LABEL,
                NumericTypeName::INTEGER,
                n.as_typename(),
                arg,
            )
            .into()),
            Some(n) => Ok(Value::Boolean(pred(&n))),
        }
    } else {
        Err(Condition::arg_error(FIRST_ARG_LABEL, NumericTypeName::INTEGER, arg).into())
    }
}

//
// Pairs and Lists
//

predicate!(is_null, Value::Pair(None));
predicate!(is_pair, Value::Pair(Some(_)));
cadr_func!(car, a);
cadr_func!(cdr, d);
cadr_func!(caar, a, a);
cadr_func!(cadr, a, d);
cadr_func!(cdar, d, a);
cadr_func!(cddr, d, d);
cadr_func!(caaar, a, a, a);
cadr_func!(caadr, a, a, d);
cadr_func!(cadar, a, d, a);
cadr_func!(caddr, a, d, d);
cadr_func!(cdaar, d, a, a);
cadr_func!(cdadr, d, a, d);
cadr_func!(cddar, d, d, a);
cadr_func!(cdddr, d, d, d);
cadr_func!(caaaar, a, a, a, a);
cadr_func!(caaadr, a, a, a, d);
cadr_func!(caadar, a, a, d, a);
cadr_func!(caaddr, a, a, d, d);
cadr_func!(cadaar, a, d, a, a);
cadr_func!(cadadr, a, d, a, d);
cadr_func!(caddar, a, d, d, a);
cadr_func!(cadddr, a, d, d, d);
cadr_func!(cdaaar, d, a, a, a);
cadr_func!(cdaadr, d, a, a, d);
cadr_func!(cdadar, d, a, d, a);
cadr_func!(cdaddr, d, a, d, d);
cadr_func!(cddaar, d, d, a, a);
cadr_func!(cddadr, d, d, a, d);
cadr_func!(cdddar, d, d, d, a);
cadr_func!(cddddr, d, d, d, d);

fn list_length(args: &[Value], _env: &mut Frame) -> EvalResult {
    let arg = args.first().unwrap();
    match arg {
        Value::Pair(None) => Ok(Value::Number(Number::real(0))),
        Value::Pair(Some(p)) => p.length().map_or_else(
            || {
                Err(Condition::arg_type_error(
                    FIRST_ARG_LABEL,
                    TypeName::LIST,
                    TypeName::IMPLIST,
                    arg,
                )
                .into())
            },
            |len| Ok(Value::Number(Number::from_usize(len))),
        ),
        _ => Err(Condition::arg_error(FIRST_ARG_LABEL, TypeName::LIST, arg).into()),
    }
}

#[allow(clippy::unnecessary_wraps, reason = "infallible intrinsic")]
fn is_list(args: &[Value], _env: &mut Frame) -> EvalResult {
    let arg = args.first().unwrap();
    Ok(Value::Boolean(match arg {
        Value::Pair(None) => true,
        Value::Pair(Some(p)) => p.is_list(),
        _ => false,
    }))
}

fn pcar(arg: &Value) -> EvalResult {
    if let Value::Pair(Some(p)) = arg {
        Ok(p.car.clone())
    } else {
        Err(Condition::arg_error(FIRST_ARG_LABEL, TypeName::PAIR, arg).into())
    }
}

fn pcdr(arg: &Value) -> EvalResult {
    if let Value::Pair(Some(p)) = arg {
        Ok(p.cdr.clone())
    } else {
        Err(Condition::arg_error(FIRST_ARG_LABEL, TypeName::PAIR, arg).into())
    }
}

//
// Procedures
//

predicate!(is_procedure, Value::Procedure(_));

//
// Strings
//

predicate!(is_string, Value::String(_));
seq_predicate!(strings_eq, Value::String, TypeName::STRING, Rc::eq);
seq_predicate!(strings_lt, Value::String, TypeName::STRING, Rc::lt);
seq_predicate!(strings_lte, Value::String, TypeName::STRING, Rc::le);
seq_predicate!(strings_gt, Value::String, TypeName::STRING, Rc::gt);
seq_predicate!(strings_gte, Value::String, TypeName::STRING, Rc::ge);
vec_length!(string_length, Value::String, TypeName::STRING);

//
// Symbols
//

predicate!(is_symbol, Value::Symbol(_));
seq_predicate!(symbols_eq, Value::Symbol, TypeName::SYMBOL, Rc::ptr_eq);

//
// Vectors
//

predicate!(is_vector, Value::Vector(_));
vec_length!(vector_length, Value::Vector, TypeName::VECTOR);

#[cfg(test)]
mod tests {
    use super::*;
    use crate::testutil::{TestEnv, err_or_fail, extract_or_fail, ok_or_fail};

    #[test]
    fn all_boolean_empty() {
        let args = [];
        let mut env = TestEnv::default();

        let r = booleans_equal(&args, &mut env.new_frame());

        let v = ok_or_fail!(r);
        assert!(matches!(v, Value::Boolean(true)));
    }

    #[test]
    fn all_boolean_single() {
        let cases = [[Value::Boolean(true)], [Value::Boolean(false)]];
        for case in cases {
            let mut env = TestEnv::default();

            let r = booleans_equal(&case, &mut env.new_frame());

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

        let r = booleans_equal(&args, &mut env.new_frame());

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

        let r = booleans_equal(&args, &mut env.new_frame());

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

        let r = booleans_equal(&args, &mut env.new_frame());

        let v = ok_or_fail!(r);
        assert!(matches!(v, Value::Boolean(false)));
    }

    #[test]
    fn all_boolean_invalid_param() {
        let args = [
            Value::Boolean(false),
            Value::string("foo"),
            Value::Boolean(false),
        ];
        let mut env = TestEnv::default();

        let r = booleans_equal(&args, &mut env.new_frame());

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
            Value::string("foo"),
            Value::Boolean(false),
            Value::null(),
        ];
        let mut env = TestEnv::default();

        let r = booleans_equal(&args, &mut env.new_frame());

        let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
        assert_eq!(
            err.to_string(),
            "#<env-error \"invalid type for arg `1` - expected: boolean, got: string\" (\"foo\")>"
        );
    }

    #[test]
    fn all_symbols_empty() {
        let args = [];
        let mut env = TestEnv::default();

        let r = symbols_eq(&args, &mut env.new_frame());

        let v = ok_or_fail!(r);
        assert!(matches!(v, Value::Boolean(true)));
    }

    #[test]
    fn all_symbols_single() {
        let args = [Value::symbol("a")];
        let mut env = TestEnv::default();

        let r = symbols_eq(&args, &mut env.new_frame());

        let v = ok_or_fail!(r);
        assert!(matches!(v, Value::Boolean(true)));
    }

    #[test]
    fn all_symbols_equal() {
        let name = "a".into();
        let args = [
            Value::symbol(Rc::clone(&name)),
            Value::symbol(Rc::clone(&name)),
            Value::symbol(Rc::clone(&name)),
        ];
        let mut env = TestEnv::default();

        let r = symbols_eq(&args, &mut env.new_frame());

        let v = ok_or_fail!(r);
        assert!(matches!(v, Value::Boolean(true)));
    }

    #[test]
    fn all_symbols_mixed() {
        let (a, b) = ("a".into(), "b".into());
        let args = [
            Value::symbol(Rc::clone(&a)),
            Value::symbol(Rc::clone(&b)),
            Value::symbol(Rc::clone(&a)),
        ];
        let mut env = TestEnv::default();

        let r = symbols_eq(&args, &mut env.new_frame());

        let v = ok_or_fail!(r);
        assert!(matches!(v, Value::Boolean(false)));
    }

    #[test]
    fn all_symbols_uninterned_not_equal() {
        let args = [Value::symbol("a"), Value::symbol("a"), Value::symbol("a")];
        let mut env = TestEnv::default();

        let r = symbols_eq(&args, &mut env.new_frame());

        let v = ok_or_fail!(r);
        assert!(matches!(v, Value::Boolean(false)));
    }

    #[test]
    fn all_symbols_invalid_param() {
        let args = [Value::symbol("a"), Value::string("a"), Value::symbol("a")];
        let mut env = TestEnv::default();

        let r = symbols_eq(&args, &mut env.new_frame());

        let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
        assert_eq!(
            err.to_string(),
            "#<env-error \"invalid type for arg `1` - expected: symbol, got: string\" (\"a\")>"
        );
    }

    #[test]
    fn all_chars_empty() {
        let args = [];
        let mut env = TestEnv::default();

        let r = chars_eq(&args, &mut env.new_frame());

        let v = ok_or_fail!(r);
        assert!(matches!(v, Value::Boolean(true)));
    }

    #[test]
    fn all_chars_single() {
        let args = [Value::Character('a')];
        let mut env = TestEnv::default();

        let r = chars_eq(&args, &mut env.new_frame());

        let v = ok_or_fail!(r);
        assert!(matches!(v, Value::Boolean(true)));
    }

    #[test]
    fn all_chars_equal() {
        let args = [
            Value::Character('a'),
            Value::Character('a'),
            Value::Character('a'),
        ];
        let mut env = TestEnv::default();

        let r = chars_eq(&args, &mut env.new_frame());

        let v = ok_or_fail!(r);
        assert!(matches!(v, Value::Boolean(true)));
    }

    #[test]
    fn all_chars_mixed() {
        let args = [
            Value::Character('a'),
            Value::Character('b'),
            Value::Character('a'),
        ];
        let mut env = TestEnv::default();

        let r = chars_eq(&args, &mut env.new_frame());

        let v = ok_or_fail!(r);
        assert!(matches!(v, Value::Boolean(false)));
    }

    #[test]
    fn all_chars_invalid_param() {
        let args = [
            Value::Character('a'),
            Value::string("a"),
            Value::Character('a'),
        ];
        let mut env = TestEnv::default();

        let r = chars_eq(&args, &mut env.new_frame());

        let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
        assert_eq!(
            err.to_string(),
            "#<env-error \"invalid type for arg `1` - expected: character, got: string\" (\"a\")>"
        );
    }

    #[test]
    fn all_chars_lt() {
        let args = [
            Value::Character('a'),
            Value::Character('b'),
            Value::Character('c'),
        ];
        let mut env = TestEnv::default();

        let r = chars_lt(&args, &mut env.new_frame());

        let v = ok_or_fail!(r);
        assert!(matches!(v, Value::Boolean(true)));
    }

    #[test]
    fn all_chars_not_lt() {
        let args = [
            Value::Character('a'),
            Value::Character('e'),
            Value::Character('c'),
        ];
        let mut env = TestEnv::default();

        let r = chars_lt(&args, &mut env.new_frame());

        let v = ok_or_fail!(r);
        assert!(matches!(v, Value::Boolean(false)));
    }

    #[test]
    fn all_strings_empty() {
        let args = [];
        let mut env = TestEnv::default();

        let r = strings_eq(&args, &mut env.new_frame());

        let v = ok_or_fail!(r);
        assert!(matches!(v, Value::Boolean(true)));
    }

    #[test]
    fn all_strings_single() {
        let args = [Value::string("foo")];
        let mut env = TestEnv::default();

        let r = strings_eq(&args, &mut env.new_frame());

        let v = ok_or_fail!(r);
        assert!(matches!(v, Value::Boolean(true)));
    }

    #[test]
    fn all_strings_equal() {
        let args = [
            Value::string("foo"),
            Value::string("foo"),
            Value::string("foo"),
        ];
        let mut env = TestEnv::default();

        let r = strings_eq(&args, &mut env.new_frame());

        let v = ok_or_fail!(r);
        assert!(matches!(v, Value::Boolean(true)));
    }

    #[test]
    fn all_strings_mixed() {
        let args = [
            Value::string("foo"),
            Value::string("bar"),
            Value::string("foo"),
        ];
        let mut env = TestEnv::default();

        let r = strings_eq(&args, &mut env.new_frame());

        let v = ok_or_fail!(r);
        assert!(matches!(v, Value::Boolean(false)));
    }

    #[test]
    fn all_strings_invalid_param() {
        let args = [
            Value::string("foo"),
            Value::symbol("foo"),
            Value::string("foo"),
        ];
        let mut env = TestEnv::default();

        let r = strings_eq(&args, &mut env.new_frame());

        let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
        assert_eq!(
            err.to_string(),
            "#<env-error \"invalid type for arg `1` - expected: string, got: symbol\" (foo)>"
        );
    }

    #[test]
    fn all_strings_lt() {
        let args = [
            Value::string("abc"),
            Value::string("def"),
            Value::string("ghi"),
        ];
        let mut env = TestEnv::default();

        let r = strings_lt(&args, &mut env.new_frame());

        let v = ok_or_fail!(r);
        assert!(matches!(v, Value::Boolean(true)));
    }

    #[test]
    fn all_strings_not_lt() {
        let args = [
            Value::string("abc"),
            Value::string("123"),
            Value::string("ghi"),
        ];
        let mut env = TestEnv::default();

        let r = strings_lt(&args, &mut env.new_frame());

        let v = ok_or_fail!(r);
        assert!(matches!(v, Value::Boolean(false)));
    }

    #[test]
    fn is_even_integer() {
        let args = [Value::Number(Number::real(4))];
        let mut env = TestEnv::default();

        let r = is_even(&args, &mut env.new_frame());

        let v = ok_or_fail!(r);
        assert!(matches!(v, Value::Boolean(true)));

        let r = is_odd(&args, &mut env.new_frame());

        let v = ok_or_fail!(r);
        assert!(matches!(v, Value::Boolean(false)));
    }

    #[test]
    fn is_even_float_with_no_frac() {
        let args = [Value::Number(Number::real(4.0))];
        let mut env = TestEnv::default();

        let r = is_even(&args, &mut env.new_frame());

        let v = ok_or_fail!(r);
        assert!(matches!(v, Value::Boolean(true)));

        let r = is_odd(&args, &mut env.new_frame());

        let v = ok_or_fail!(r);
        assert!(matches!(v, Value::Boolean(false)));
    }

    #[test]
    fn is_even_float_with_frac() {
        let args = [Value::Number(Number::real(4.2))];
        let mut env = TestEnv::default();

        let r = is_even(&args, &mut env.new_frame());

        let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
        assert_eq!(
            err.to_string(),
            "#<env-error \"invalid type for arg `0` - expected: integer, got: floating-point\" (4.2)>"
        );

        let r = is_odd(&args, &mut env.new_frame());

        let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
        assert_eq!(
            err.to_string(),
            "#<env-error \"invalid type for arg `0` - expected: integer, got: floating-point\" (4.2)>"
        );
    }

    #[test]
    fn is_odd_integer() {
        let args = [Value::Number(Number::real(3))];
        let mut env = TestEnv::default();

        let r = is_even(&args, &mut env.new_frame());

        let v = ok_or_fail!(r);
        assert!(matches!(v, Value::Boolean(false)));

        let r = is_odd(&args, &mut env.new_frame());

        let v = ok_or_fail!(r);
        assert!(matches!(v, Value::Boolean(true)));
    }

    #[test]
    fn is_odd_float_with_no_frac() {
        let args = [Value::Number(Number::real(3.0))];
        let mut env = TestEnv::default();

        let r = is_even(&args, &mut env.new_frame());

        let v = ok_or_fail!(r);
        assert!(matches!(v, Value::Boolean(false)));

        let r = is_odd(&args, &mut env.new_frame());

        let v = ok_or_fail!(r);
        assert!(matches!(v, Value::Boolean(true)));
    }

    #[test]
    fn is_odd_float_with_frac() {
        let args = [Value::Number(Number::real(3.2))];
        let mut env = TestEnv::default();

        let r = is_even(&args, &mut env.new_frame());

        let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
        assert_eq!(
            err.to_string(),
            "#<env-error \"invalid type for arg `0` - expected: integer, got: floating-point\" (3.2)>"
        );

        let r = is_odd(&args, &mut env.new_frame());

        let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
        assert_eq!(
            err.to_string(),
            "#<env-error \"invalid type for arg `0` - expected: integer, got: floating-point\" (3.2)>"
        );
    }
}
