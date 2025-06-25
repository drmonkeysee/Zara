// (scheme base)
macro_rules! predicate {
    ($name:ident, $pred:pat $(if $guard:expr)?) => {
        fn $name(args: &[Value], _env: &mut Frame) -> EvalResult {
            let arg = args.first().unwrap();
            Ok(Value::Boolean(matches!(arg, $pred $(if $guard)?)))
        }
    };
}

macro_rules! seq_predicate {
    ($name:ident, $kind:path, $valname:expr, $pred:expr) => {
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
    ($name:ident, $kind:path, $valname:expr) => {
        fn $name(args: &[Value], _env: &mut Frame) -> EvalResult {
            let arg = args.first().unwrap();
            if let $kind(v) = arg {
                Ok(Value::Number(Number::from_usize(v.len())))
            } else {
                invalid_target!($valname, arg)
            }
        }
    };
}

macro_rules! vec_get {
    ($name:ident, $kind: path, $valname:expr, $get:expr, $map:expr) => {
        fn $name(args: &[Value], _env: &mut Frame) -> EvalResult {
            let vec = args.first().unwrap();
            let k = args.get(1).unwrap();
            if let $kind(v) = vec {
                vec_item(v, k, $get, $map)
            } else {
                invalid_target!($valname, vec)
            }
        }
    };
}

use super::{FIRST_ARG_LABEL, SECOND_ARG_LABEL, pcar, pcdr};
use crate::{
    Exception,
    eval::{Binding, EvalResult, Frame, MAX_ARITY},
    number::{Integer, Number, NumericError, NumericTypeName, Real},
    string::unicode::UnicodeError,
    value::{Condition, TypeName, Value},
};
use std::{convert, fmt::Display, rc::Rc};

pub(super) fn load(scope: &mut Binding) {
    load_bool(scope);
    load_bv(scope);
    load_char(scope);
    load_eq(scope);
    load_ex(scope);
    load_num(scope);
    load_list(scope);
    load_proc(scope);
    load_string(scope);
    load_symbol(scope);
    load_vec(scope);
}

//
// Booleans
//

fn load_bool(scope: &mut Binding) {
    super::bind_intrinsic(scope, "not", 1..1, not);

    super::bind_intrinsic(scope, "boolean?", 1..1, is_boolean);
    super::bind_intrinsic(scope, "boolean=?", 0..MAX_ARITY, booleans_eq);
}

predicate!(not, Value::Boolean(false));
predicate!(is_boolean, Value::Boolean(_));
seq_predicate!(booleans_eq, Value::Boolean, TypeName::BOOL, bool::eq);

//
// Bytevectors
//

fn load_bv(scope: &mut Binding) {
    super::bind_intrinsic(scope, "bytevector?", 1..1, is_bytevector);

    super::bind_intrinsic(scope, "bytevector-length", 1..1, bytevector_length);
    super::bind_intrinsic(scope, "bytevector-u8-ref", 2..2, bytevector_get);
}

predicate!(is_bytevector, Value::ByteVector(_));
vec_length!(bytevector_length, Value::ByteVector, TypeName::BYTEVECTOR);
vec_get!(
    bytevector_get,
    Value::ByteVector,
    TypeName::BYTEVECTOR,
    |bv, u| bv.get(u).copied(),
    |item| Value::Number(Number::real(i64::from(item)))
);

//
// Characters
//

fn load_char(scope: &mut Binding) {
    super::bind_intrinsic(scope, "char?", 1..1, is_char);

    super::bind_intrinsic(scope, "char=?", 0..MAX_ARITY, chars_eq);
    super::bind_intrinsic(scope, "char<?", 0..MAX_ARITY, chars_lt);
    super::bind_intrinsic(scope, "char<=?", 0..MAX_ARITY, chars_lte);
    super::bind_intrinsic(scope, "char>?", 0..MAX_ARITY, chars_gt);
    super::bind_intrinsic(scope, "char>=?", 0..MAX_ARITY, chars_gte);

    super::bind_intrinsic(scope, "char->integer", 1..1, char_to_integer);
    super::bind_intrinsic(scope, "integer->char", 1..1, integer_to_char);
}

predicate!(is_char, Value::Character(_));
seq_predicate!(chars_eq, Value::Character, TypeName::CHAR, char::eq);
seq_predicate!(chars_lt, Value::Character, TypeName::CHAR, char::lt);
seq_predicate!(chars_lte, Value::Character, TypeName::CHAR, char::le);
seq_predicate!(chars_gt, Value::Character, TypeName::CHAR, char::gt);
seq_predicate!(chars_gte, Value::Character, TypeName::CHAR, char::ge);

fn char_to_integer(args: &[Value], _env: &mut Frame) -> EvalResult {
    let arg = args.first().unwrap();
    if let Value::Character(c) = arg {
        let n = u32::from(*c);
        Ok(Value::Number(Number::real(i64::from(n))))
    } else {
        invalid_target!(TypeName::CHAR, arg)
    }
}

fn integer_to_char(args: &[Value], _env: &mut Frame) -> EvalResult {
    let arg = args.first().unwrap();
    if let Value::Number(n) = arg {
        try_num_into_char(n, arg)
    } else {
        invalid_target!(NumericTypeName::INTEGER, arg)
    }
}

fn try_num_into_char(n: &Number, arg: &Value) -> EvalResult {
    u32::try_from(n).map_or_else(
        |err| {
            Err(if let NumericError::Uint32ConversionInvalidRange = err {
                Condition::value_error(UnicodeError::CodePointOutOfRange, arg)
            } else {
                Condition::arg_type_error(
                    FIRST_ARG_LABEL,
                    NumericTypeName::INTEGER,
                    n.as_typename(),
                    arg,
                )
            }
            .into())
        },
        |u| {
            char::from_u32(u).map_or_else(
                || Err(Condition::value_error(UnicodeError::CodePointOutOfRange, arg).into()),
                |c| Ok(Value::Character(c)),
            )
        },
    )
}

//
// Equivalence
//

fn load_eq(scope: &mut Binding) {
    super::bind_intrinsic(scope, "eqv?", 2..2, is_eqv);
    super::bind_intrinsic(scope, "eq?", 2..2, is_eq);
    super::bind_intrinsic(scope, "equal?", 2..2, is_equal);
}

#[allow(clippy::unnecessary_wraps, reason = "infallible intrinsic")]
fn is_eqv(args: &[Value], _env: &mut Frame) -> EvalResult {
    let a = args.first().unwrap();
    let b = args.get(1).unwrap();
    Ok(Value::Boolean(a.is_eqv(b)))
}

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

//
// Exceptions
//

fn load_ex(scope: &mut Binding) {
    super::bind_intrinsic(scope, "error-object?", 1..1, is_error);

    super::bind_intrinsic(scope, "error-object-message", 1..1, error_msg);
    super::bind_intrinsic(scope, "error-object-irritants", 1..1, error_irritants);

    super::bind_intrinsic(scope, "read-error?", 1..1, is_read_error);
    super::bind_intrinsic(scope, "file-error?", 1..1, is_file_error);
}

predicate!(is_error, Value::Error(_));
predicate!(is_read_error, Value::Error(c) if c.is_read_err());
predicate!(is_file_error, Value::Error(c) if c.is_file_err());

// TODO: this should be a mutable string
fn error_msg(args: &[Value], _env: &mut Frame) -> EvalResult {
    let arg = args.first().unwrap();
    if let Value::Error(c) = arg {
        Ok(Value::string(c.message()))
    } else {
        invalid_target!(TypeName::ERROR, arg)
    }
}

fn error_irritants(args: &[Value], _env: &mut Frame) -> EvalResult {
    let arg = args.first().unwrap();
    if let Value::Error(c) = arg {
        Ok(c.irritants().map_or(Value::null(), Value::clone))
    } else {
        invalid_target!(TypeName::ERROR, arg)
    }
}

//
// Numbers
//

fn load_num(scope: &mut Binding) {
    // NOTE: complex and number predicates are identical sets
    super::bind_intrinsic(scope, "number?", 1..1, is_number);
    super::bind_intrinsic(scope, "complex?", 1..1, is_number);
    super::bind_intrinsic(scope, "real?", 1..1, is_real);
    super::bind_intrinsic(scope, "rational?", 1..1, is_rational);
    super::bind_intrinsic(scope, "integer?", 1..1, is_integer);

    super::bind_intrinsic(scope, "exact?", 1..1, is_exact);
    super::bind_intrinsic(scope, "inexact?", 1..1, is_inexact);
    super::bind_intrinsic(scope, "exact-integer?", 1..1, is_exact_integer);

    super::bind_intrinsic(scope, "zero?", 1..1, is_zero);
    super::bind_intrinsic(scope, "positive?", 1..1, is_positive);
    super::bind_intrinsic(scope, "negative?", 1..1, is_negative);
    super::bind_intrinsic(scope, "odd?", 1..1, is_odd);
    super::bind_intrinsic(scope, "even?", 1..1, is_even);

    super::bind_intrinsic(scope, "abs", 1..1, abs);

    super::bind_intrinsic(scope, "numerator", 1..1, get_numerator);
    super::bind_intrinsic(scope, "denominator", 1..1, get_denominator);

    super::bind_intrinsic(scope, "inexact", 1..1, into_inexact);
    super::bind_intrinsic(scope, "exact", 1..1, into_exact);
}

predicate!(is_number, Value::Number(_));
predicate!(is_real, Value::Number(Number::Real(_)));
predicate!(is_rational, Value::Number(Number::Real(r)) if r.is_rational());
predicate!(is_integer, Value::Number(Number::Real(r)) if r.is_integer());
try_predicate!(is_exact, Value::Number, TypeName::NUMBER, |n: &Number| {
    !n.is_inexact()
});
try_predicate!(is_inexact, Value::Number, TypeName::NUMBER, |n: &Number| {
    n.is_inexact()
});
try_predicate!(
    is_exact_integer,
    Value::Number,
    TypeName::NUMBER,
    |n: &Number| matches!(n, Number::Real(Real::Integer(_)))
);
try_predicate!(is_zero, Value::Number, TypeName::NUMBER, |n: &Number| n
    .is_zero());

fn is_positive(args: &[Value], _env: &mut Frame) -> EvalResult {
    real_op(args.first().unwrap(), |r| {
        Ok(Value::Boolean(r.is_positive()))
    })
}

fn is_negative(args: &[Value], _env: &mut Frame) -> EvalResult {
    real_op(args.first().unwrap(), |r| {
        Ok(Value::Boolean(r.is_negative()))
    })
}

fn is_odd(args: &[Value], _env: &mut Frame) -> EvalResult {
    int_predicate(args.first().unwrap(), |n| !n.is_even())
}

fn is_even(args: &[Value], _env: &mut Frame) -> EvalResult {
    int_predicate(args.first().unwrap(), Integer::is_even)
}

fn abs(args: &[Value], _env: &mut Frame) -> EvalResult {
    real_op(args.first().unwrap(), |r| {
        Ok(Value::Number(Number::real(r.clone().into_abs())))
    })
}

fn get_numerator(args: &[Value], _env: &mut Frame) -> EvalResult {
    let arg = args.first().unwrap();
    rational_op(arg, |r| {
        r.clone().try_into_numerator().map_or_else(
            |err| Err(Condition::value_error(err, arg).into()),
            |r| Ok(Value::Number(Number::real(r))),
        )
    })
}

fn get_denominator(args: &[Value], _env: &mut Frame) -> EvalResult {
    let arg = args.first().unwrap();
    rational_op(arg, |r| {
        r.clone().try_into_denominator().map_or_else(
            |err| Err(Condition::value_error(err, arg).into()),
            |r| Ok(Value::Number(Number::real(r))),
        )
    })
}

fn into_inexact(args: &[Value], _env: &mut Frame) -> EvalResult {
    let arg = args.first().unwrap();
    if let Value::Number(n) = arg {
        Ok(Value::Number(n.clone().into_inexact()))
    } else {
        invalid_target!(TypeName::NUMBER, arg)
    }
}

fn into_exact(args: &[Value], _env: &mut Frame) -> EvalResult {
    let arg = args.first().unwrap();
    if let Value::Number(n) = arg {
        n.clone().try_into_exact().map_or_else(
            |err| Err(Condition::value_error(err, arg).into()),
            |n| Ok(Value::Number(n)),
        )
    } else {
        invalid_target!(TypeName::NUMBER, arg)
    }
}

fn real_op(arg: &Value, op: impl FnOnce(&Real) -> EvalResult) -> EvalResult {
    guarded_real_op(arg, NumericTypeName::REAL, op)
}

fn rational_op(arg: &Value, op: impl FnOnce(&Real) -> EvalResult) -> EvalResult {
    guarded_real_op(arg, NumericTypeName::RATIONAL, op)
}

fn guarded_real_op(
    arg: &Value,
    expected_type: impl Display,
    op: impl FnOnce(&Real) -> EvalResult,
) -> EvalResult {
    let Value::Number(n) = arg else {
        return invalid_target!(NumericTypeName::REAL, arg);
    };
    if let Number::Real(r) = n {
        op(r)
    } else {
        Err(Condition::arg_type_error(FIRST_ARG_LABEL, expected_type, n.as_typename(), arg).into())
    }
}

fn int_predicate(arg: &Value, pred: impl FnOnce(&Integer) -> bool) -> EvalResult {
    let Value::Number(n) = arg else {
        return invalid_target!(NumericTypeName::INTEGER, arg);
    };
    n.to_exact_integer().map_or_else(
        || {
            Err(Condition::arg_type_error(
                FIRST_ARG_LABEL,
                NumericTypeName::INTEGER,
                n.as_typename(),
                arg,
            )
            .into())
        },
        |n| Ok(Value::Boolean(pred(&n))),
    )
}

//
// Pairs and Lists
//

fn load_list(scope: &mut Binding) {
    super::bind_intrinsic(scope, "pair?", 1..1, is_pair);

    super::bind_intrinsic(scope, "car", 1..1, car);
    super::bind_intrinsic(scope, "cdr", 1..1, cdr);
    super::bind_intrinsic(scope, "caar", 1..1, caar);
    super::bind_intrinsic(scope, "cadr", 1..1, cadr);
    super::bind_intrinsic(scope, "cdar", 1..1, cdar);
    super::bind_intrinsic(scope, "cddr", 1..1, cddr);

    super::bind_intrinsic(scope, "null?", 1..1, is_null);
    super::bind_intrinsic(scope, "list?", 1..1, is_list);

    super::bind_intrinsic(scope, "length", 1..1, list_length);
    super::bind_intrinsic(scope, "list-tail", 2..2, list_tail);
    super::bind_intrinsic(scope, "list-ref", 2..2, list_get);
}

predicate!(is_pair, Value::Pair(Some(_)));
predicate!(is_null, Value::Pair(None));
cadr_func!(car, a);
cadr_func!(cdr, d);
cadr_func!(caar, a, a);
cadr_func!(cadr, a, d);
cadr_func!(cdar, d, a);
cadr_func!(cddr, d, d);

// TODO: circular lists => #f
#[allow(clippy::unnecessary_wraps, reason = "infallible intrinsic")]
fn is_list(args: &[Value], _env: &mut Frame) -> EvalResult {
    let arg = args.first().unwrap();
    Ok(Value::Boolean(match arg {
        Value::Pair(None) => true,
        Value::Pair(Some(p)) => p.is_list(),
        _ => false,
    }))
}

// TODO: circular lists => error
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
        _ => invalid_target!(TypeName::LIST, arg),
    }
}

fn list_tail(args: &[Value], _env: &mut Frame) -> EvalResult {
    let lst = args.first().unwrap();
    let k = args.get(1).unwrap();
    sub_list(lst, number_to_index(k)?, k).cloned()
}

fn list_get(args: &[Value], _env: &mut Frame) -> EvalResult {
    let lst = args.first().unwrap();
    let k = args.get(1).unwrap();
    let subl = sub_list(lst, number_to_index(k)?, k)?;
    if let Value::Pair(None) = subl {
        Err(Condition::index_error(k).into())
    } else {
        pcar(subl)
    }
}

fn sub_list<'a>(lst: &'a Value, idx: usize, k: &Value) -> Result<&'a Value, Exception> {
    if idx == 0 {
        Ok(lst)
    } else if let Value::Pair(Some(p)) = lst {
        sub_list(&p.cdr, idx - 1, k)
    } else if let Value::Pair(None) = lst {
        Err(Condition::index_error(k).into())
    } else {
        invalid_target!(TypeName::PAIR, lst)
    }
}

//
// Procedures
//

fn load_proc(scope: &mut Binding) {
    super::bind_intrinsic(scope, "procedure?", 1..1, is_procedure);
}

predicate!(is_procedure, Value::Procedure(_));

//
// Strings
//

fn load_string(scope: &mut Binding) {
    super::bind_intrinsic(scope, "string?", 1..1, is_string);

    super::bind_intrinsic(scope, "string-length", 1..1, string_length);
    super::bind_intrinsic(scope, "string-ref", 2..2, string_get);

    super::bind_intrinsic(scope, "string=?", 0..MAX_ARITY, strings_eq);
    super::bind_intrinsic(scope, "string<?", 0..MAX_ARITY, strings_lt);
    super::bind_intrinsic(scope, "string<=?", 0..MAX_ARITY, strings_lte);
    super::bind_intrinsic(scope, "string>?", 0..MAX_ARITY, strings_gt);
    super::bind_intrinsic(scope, "string>=?", 0..MAX_ARITY, strings_gte);
}

predicate!(is_string, Value::String(_));
vec_length!(string_length, Value::String, TypeName::STRING);
vec_get!(
    string_get,
    Value::String,
    TypeName::STRING,
    |s, u| s.chars().nth(u),
    Value::Character
);
seq_predicate!(strings_eq, Value::String, TypeName::STRING, Rc::eq);
seq_predicate!(strings_lt, Value::String, TypeName::STRING, Rc::lt);
seq_predicate!(strings_lte, Value::String, TypeName::STRING, Rc::le);
seq_predicate!(strings_gt, Value::String, TypeName::STRING, Rc::gt);
seq_predicate!(strings_gte, Value::String, TypeName::STRING, Rc::ge);

//
// Symbols
//

fn load_symbol(scope: &mut Binding) {
    super::bind_intrinsic(scope, "symbol?", 1..1, is_symbol);
    super::bind_intrinsic(scope, "symbol=?", 0..MAX_ARITY, symbols_eq);

    super::bind_intrinsic(scope, "symbol->string", 1..1, symbol_to_string);
    super::bind_intrinsic(scope, "string->symbol", 1..1, string_to_symbol);
}

predicate!(is_symbol, Value::Symbol(_));
seq_predicate!(symbols_eq, Value::Symbol, TypeName::SYMBOL, Rc::ptr_eq);

fn symbol_to_string(args: &[Value], _env: &mut Frame) -> EvalResult {
    let arg = args.first().unwrap();
    if let Value::Symbol(s) = arg {
        Ok(Value::string(Rc::clone(s)))
    } else {
        invalid_target!(TypeName::SYMBOL, arg)
    }
}

fn string_to_symbol(args: &[Value], env: &mut Frame) -> EvalResult {
    let arg = args.first().unwrap();
    if let Value::String(s) = arg {
        Ok(Value::symbol(env.sym.get(s)))
    } else {
        invalid_target!(TypeName::STRING, arg)
    }
}

//
// Vectors
//

fn load_vec(scope: &mut Binding) {
    super::bind_intrinsic(scope, "vector?", 1..1, is_vector);

    super::bind_intrinsic(scope, "vector-length", 1..1, vector_length);
    super::bind_intrinsic(scope, "vector-ref", 2..2, vector_get);
}

predicate!(is_vector, Value::Vector(_));
vec_length!(vector_length, Value::Vector, TypeName::VECTOR);
vec_get!(
    vector_get,
    Value::Vector,
    TypeName::VECTOR,
    |v, u| v.get(u).cloned(),
    convert::identity
);

fn vec_item<T, U>(
    vec: &T,
    k: &Value,
    get: impl FnOnce(&T, usize) -> Option<U>,
    map: impl FnOnce(U) -> Value,
) -> EvalResult {
    get(vec, number_to_index(k)?).map_or_else(
        || Err(Condition::index_error(k).into()),
        |item| Ok(map(item)),
    )
}

fn number_to_index(k: &Value) -> Result<usize, Exception> {
    let Value::Number(n) = k else {
        return Err(Condition::arg_error(SECOND_ARG_LABEL, NumericTypeName::INTEGER, k).into());
    };
    usize::try_from(n).map_err(|err| {
        if let NumericError::UsizeConversionInvalidRange = err {
            Condition::value_error(NumericError::UsizeConversionInvalidRange, k)
        } else {
            Condition::arg_type_error(
                SECOND_ARG_LABEL,
                NumericTypeName::INTEGER,
                n.as_typename(),
                k,
            )
        }
        .into()
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        testutil::{TestEnv, err_or_fail, extract_or_fail, ok_or_fail, some_or_fail},
        value::zlist,
    };

    #[test]
    fn all_boolean_empty() {
        let args = [];
        let mut env = TestEnv::default();

        let r = booleans_eq(&args, &mut env.new_frame());

        let v = ok_or_fail!(r);
        assert!(matches!(v, Value::Boolean(true)));
    }

    #[test]
    fn all_boolean_single() {
        let cases = [[Value::Boolean(true)], [Value::Boolean(false)]];
        for case in cases {
            let mut env = TestEnv::default();

            let r = booleans_eq(&case, &mut env.new_frame());

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

        let r = booleans_eq(&args, &mut env.new_frame());

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

        let r = booleans_eq(&args, &mut env.new_frame());

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

        let r = booleans_eq(&args, &mut env.new_frame());

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

        let r = booleans_eq(&args, &mut env.new_frame());

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

        let r = booleans_eq(&args, &mut env.new_frame());

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
        let name = "a".into();
        let args = [
            Value::symbol(Rc::clone(&name)),
            Value::string(Rc::clone(&name)),
            Value::symbol(Rc::clone(&name)),
        ];
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

    #[test]
    fn list_tail_normal_list() {
        let args = [
            zlist![Value::symbol("a"), Value::symbol("b"), Value::symbol("c")],
            Value::Number(Number::real(1)),
        ];
        let mut env = TestEnv::default();

        let r = list_tail(&args, &mut env.new_frame());

        let v = ok_or_fail!(r);
        let second_item = some_or_fail!(
            extract_or_fail!(
                &some_or_fail!(extract_or_fail!(&args[0], Value::Pair).as_ref()).cdr,
                Value::Pair
            )
            .as_ref()
        );
        assert!(Rc::ptr_eq(
            &some_or_fail!(extract_or_fail!(v, Value::Pair)),
            second_item
        ));
    }

    #[test]
    fn list_tail_empty_list() {
        let args = [Value::null(), Value::Number(Number::real(0))];
        let mut env = TestEnv::default();

        let r = list_tail(&args, &mut env.new_frame());

        let v = ok_or_fail!(r);
        assert!(matches!(v, Value::Pair(None)));
    }

    #[test]
    fn list_tail_index_to_empty_list() {
        let args = [
            zlist![Value::symbol("a"), Value::symbol("b"), Value::symbol("c")],
            Value::Number(Number::real(3)),
        ];
        let mut env = TestEnv::default();

        let r = list_tail(&args, &mut env.new_frame());

        let v = ok_or_fail!(r);
        assert!(matches!(v, Value::Pair(None)));
    }

    #[test]
    fn list_tail_non_list() {
        let args = [Value::symbol("a"), Value::Number(Number::real(0))];
        let mut env = TestEnv::default();

        let r = list_tail(&args, &mut env.new_frame());

        let v = ok_or_fail!(r);
        assert!(matches!(v, Value::Symbol(s) if s.as_ref() == "a"));
    }

    #[test]
    fn list_tail_end_of_improper_list() {
        let args = [
            Value::cons(
                Value::symbol("a"),
                Value::cons(Value::symbol("b"), Value::symbol("c")),
            ),
            Value::Number(Number::real(2)),
        ];
        let mut env = TestEnv::default();

        let r = list_tail(&args, &mut env.new_frame());

        let v = ok_or_fail!(r);
        assert!(matches!(v, Value::Symbol(s) if s.as_ref() == "c"));
    }

    #[test]
    fn list_tail_index_out_of_range() {
        let args = [
            zlist![Value::symbol("a"), Value::symbol("b"), Value::symbol("c")],
            Value::Number(Number::real(4)),
        ];
        let mut env = TestEnv::default();

        let r = list_tail(&args, &mut env.new_frame());

        let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
        assert_eq!(err.to_string(), "#<env-error \"index out of range\" (4)>");
    }

    #[test]
    fn list_tail_non_list_out_of_range() {
        let args = [Value::symbol("a"), Value::Number(Number::real(1))];
        let mut env = TestEnv::default();

        let r = list_tail(&args, &mut env.new_frame());

        let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
        assert_eq!(
            err.to_string(),
            "#<env-error \"invalid type for arg `0` - expected: pair, got: symbol\" (a)>"
        );
    }

    #[test]
    fn list_tail_improper_list_out_of_range() {
        let args = [
            Value::cons(
                Value::symbol("a"),
                Value::cons(Value::symbol("b"), Value::symbol("c")),
            ),
            Value::Number(Number::real(3)),
        ];
        let mut env = TestEnv::default();

        let r = list_tail(&args, &mut env.new_frame());

        let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
        assert_eq!(
            err.to_string(),
            "#<env-error \"invalid type for arg `0` - expected: pair, got: symbol\" (c)>"
        );
    }

    #[test]
    fn list_tail_wrong_index_type() {
        let args = [Value::null(), Value::string("foo")];
        let mut env = TestEnv::default();

        let r = list_tail(&args, &mut env.new_frame());

        let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
        assert_eq!(
            err.to_string(),
            "#<env-error \"invalid type for arg `1` - expected: integer, got: string\" (\"foo\")>"
        );
    }

    #[test]
    fn list_tail_invalid_index_type() {
        let args = [Value::null(), Value::Number(Number::real(4.2))];
        let mut env = TestEnv::default();

        let r = list_tail(&args, &mut env.new_frame());

        let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
        assert_eq!(
            err.to_string(),
            "#<env-error \"invalid type for arg `1` - expected: integer, got: floating-point\" (4.2)>"
        );
    }

    #[test]
    fn list_tail_index_invalid_range() {
        let args = [Value::null(), Value::Number(Number::real(-4))];
        let mut env = TestEnv::default();

        let r = list_tail(&args, &mut env.new_frame());

        let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
        assert_eq!(
            err.to_string(),
            "#<env-error \"integer literal out of range: [0, 18446744073709551615]\" (-4)>"
        );
    }

    #[test]
    fn list_ref_normal_list() {
        let args = [
            zlist![Value::symbol("a"), Value::symbol("b"), Value::symbol("c")],
            Value::Number(Number::real(1)),
        ];
        let mut env = TestEnv::default();

        let r = list_get(&args, &mut env.new_frame());

        let v = ok_or_fail!(r);
        assert!(matches!(v, Value::Symbol(s) if s.as_ref() == "b"));
    }

    #[test]
    fn list_ref_empty_list() {
        let args = [Value::null(), Value::Number(Number::real(0))];
        let mut env = TestEnv::default();

        let r = list_get(&args, &mut env.new_frame());

        let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
        assert_eq!(err.to_string(), "#<env-error \"index out of range\" (0)>");
    }

    #[test]
    fn list_ref_non_list() {
        let args = [Value::symbol("a"), Value::Number(Number::real(0))];
        let mut env = TestEnv::default();

        let r = list_get(&args, &mut env.new_frame());

        let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
        assert_eq!(
            err.to_string(),
            "#<env-error \"invalid type for arg `0` - expected: pair, got: symbol\" (a)>"
        );
    }

    #[test]
    fn list_ref_improper_list_item() {
        let args = [
            Value::cons(
                Value::symbol("a"),
                Value::cons(Value::symbol("b"), Value::symbol("c")),
            ),
            Value::Number(Number::real(1)),
        ];
        let mut env = TestEnv::default();

        let r = list_get(&args, &mut env.new_frame());

        let v = ok_or_fail!(r);
        assert!(matches!(v, Value::Symbol(s) if s.as_ref() == "b"));
    }

    #[test]
    fn list_ref_end_of_improper_list() {
        let args = [
            Value::cons(
                Value::symbol("a"),
                Value::cons(Value::symbol("b"), Value::symbol("c")),
            ),
            Value::Number(Number::real(2)),
        ];
        let mut env = TestEnv::default();

        let r = list_get(&args, &mut env.new_frame());

        let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
        assert_eq!(
            err.to_string(),
            "#<env-error \"invalid type for arg `0` - expected: pair, got: symbol\" (c)>"
        );
    }

    #[test]
    fn list_ref_index_out_of_range() {
        let args = [
            zlist![Value::symbol("a"), Value::symbol("b"), Value::symbol("c")],
            Value::Number(Number::real(4)),
        ];
        let mut env = TestEnv::default();

        let r = list_get(&args, &mut env.new_frame());

        let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
        assert_eq!(err.to_string(), "#<env-error \"index out of range\" (4)>");
    }

    #[test]
    fn list_ref_improper_list_out_of_range() {
        let args = [
            Value::cons(
                Value::symbol("a"),
                Value::cons(Value::symbol("b"), Value::symbol("c")),
            ),
            Value::Number(Number::real(3)),
        ];
        let mut env = TestEnv::default();

        let r = list_get(&args, &mut env.new_frame());

        let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
        assert_eq!(
            err.to_string(),
            "#<env-error \"invalid type for arg `0` - expected: pair, got: symbol\" (c)>"
        );
    }

    #[test]
    fn int_to_char() {
        let args = [Value::Number(Number::real(0x41))];
        let mut env = TestEnv::default();

        let r = integer_to_char(&args, &mut env.new_frame());

        let v = ok_or_fail!(r);
        assert!(matches!(v, Value::Character('A')));
    }

    #[test]
    fn int_to_char_invalid_arg() {
        let args = [Value::symbol("a")];
        let mut env = TestEnv::default();

        let r = integer_to_char(&args, &mut env.new_frame());

        let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
        assert_eq!(
            err.to_string(),
            "#<env-error \"invalid type for arg `0` - expected: integer, got: symbol\" (a)>"
        );
    }

    #[test]
    fn int_to_char_invalid_range() {
        let args = [Value::Number(Number::real(-4))];
        let mut env = TestEnv::default();

        let r = integer_to_char(&args, &mut env.new_frame());

        let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
        assert_eq!(
            err.to_string(),
            "#<env-error \"unicode code point out of ranges [#x0, #xD7FF], [#xE000, #x10FFFF] ([0, 55295], [57344, 1114111])\" (-4)>"
        );
    }

    #[test]
    fn int_to_char_not_a_code_point() {
        let args = [Value::Number(Number::real(0xdff0))];
        let mut env = TestEnv::default();

        let r = integer_to_char(&args, &mut env.new_frame());

        let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
        assert_eq!(
            err.to_string(),
            "#<env-error \"unicode code point out of ranges [#x0, #xD7FF], [#xE000, #x10FFFF] ([0, 55295], [57344, 1114111])\" (57328)>"
        );
    }
}
