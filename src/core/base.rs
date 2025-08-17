// (scheme base)
macro_rules! predicate {
    ($name:ident, $pred:pat $(if $guard:expr)?) => {
        fn $name(args: &[Value], _env: &Frame) -> EvalResult {
            let arg = first(args);
            Ok(Value::Boolean(matches!(arg, $pred $(if $guard)?)))
        }
    };
}

macro_rules! seq_predicate {
    ($name:ident, $kind:path, $valname:expr, $pred:expr) => {
        fn $name(args: &[Value], _env: &Frame) -> EvalResult {
            let mut it = args.iter().enumerate();
            let first = match it.next() {
                None => return Ok(Value::Boolean(true)),
                Some((_, $kind(a))) => a,
                Some((idx, v)) => {
                    return Err(Condition::arg_error(idx, $valname, v).into());
                }
            };
            let result: Result<_, Exception> =
                it.try_fold((true, first), |(acc, prev), (idx, val)| {
                    if let $kind(next) = val {
                        Ok((acc && $pred(prev, next), next))
                    } else {
                        Err(Condition::arg_error(idx, $valname, val).into())
                    }
                });
            Ok(Value::Boolean(result?.0))
        }
    };
}

macro_rules! num_convert {
    ($name:ident, $type:ty, $err:path) => {
        fn $name(arg: &Value, lbl: impl Display) -> Result<$type, Exception> {
            let Value::Number(n) = arg else {
                return Err(Condition::arg_error(lbl, NumericTypeName::INTEGER, arg).into());
            };
            <$type>::try_from(n).map_err(|err| {
                if let $err = err {
                    Condition::value_error($err, arg)
                } else {
                    Condition::arg_type_error(lbl, NumericTypeName::INTEGER, n.as_typename(), arg)
                }
                .into()
            })
        }
    };
}

macro_rules! vec_new {
    ($name:ident, $map:expr, $ctor:expr, $coll:ty) => {
        fn $name(args: &[Value], _env: &Frame) -> EvalResult {
            let v = args
                .iter()
                .enumerate()
                .map($map)
                .collect::<Result<$coll, _>>()?;
            Ok($ctor(v))
        }
    };
}

macro_rules! vec_length {
    ($name:ident, $asref:expr, $valname:expr) => {
        fn $name(args: &[Value], _env: &Frame) -> EvalResult {
            let arg = first(args);
            let v = $asref(arg).ok_or_else(|| invalid_target($valname, arg))?;
            Ok(Value::Number(Number::from_usize(v.as_ref().len())))
        }
    };
}

macro_rules! vec_get {
    ($name:ident, $asref:expr, $get:expr, $map:expr, $valname:expr) => {
        fn $name(args: &[Value], _env: &Frame) -> EvalResult {
            let vec = first(args);
            let k = super::second(args);
            let v = $asref(vec).ok_or_else(|| invalid_target($valname, vec))?;
            vec_item(v.as_ref(), k, $get, $map)
        }
    };
}

macro_rules! vec_set {
    ($name:ident, $mutkind:path, $kind: path, $valname:expr, $valconv:expr, $setval:expr) => {
        fn $name(args: &[Value], _env: &Frame) -> EvalResult {
            let arg = first(args);
            let k = super::second(args);
            let val = super::third(args);
            match arg {
                $mutkind(v) => {
                    let idx = valnum_to_index(k, SECOND_ARG_LABEL)?;
                    if idx < v.borrow().len() {
                        let item = $valconv(val)?;
                        $setval(v.borrow_mut(), idx, item);
                        Ok(Value::Unspecified)
                    } else {
                        Err(Condition::index_error(k).into())
                    }
                }
                $kind(_) => Err(Condition::literal_mut_error(arg).into()),
                _ => Err(invalid_target($valname, arg)),
            }
        }
    };
}

#[cfg(test)]
mod tests;

use super::{
    FIRST_ARG_LABEL, SECOND_ARG_LABEL, THIRD_ARG_LABEL, first, invalid_target, pcar, pcdr,
};
use crate::{
    Exception,
    eval::{EvalResult, Frame, MAX_ARITY},
    number::{Integer, Number, NumericError, NumericTypeName, Real},
    string::{Symbol, unicode::UnicodeError},
    value::{Condition, TypeName, Value},
};
use std::{cell::RefMut, convert, fmt::Display, iter};

pub(super) fn load(env: &Frame) {
    load_bool(env);
    load_bv(env);
    load_char(env);
    load_eq(env);
    load_ex(env);
    load_num(env);
    load_list(env);
    load_proc(env);
    load_string(env);
    load_symbol(env);
    load_vec(env);
}

//
// Booleans
//

fn load_bool(env: &Frame) {
    super::bind_intrinsic(env, "not", 1..1, not);

    super::bind_intrinsic(env, "boolean?", 1..1, is_boolean);
    super::bind_intrinsic(env, "boolean=?", 0..MAX_ARITY, booleans_eq);
}

predicate!(not, Value::Boolean(false));
predicate!(is_boolean, Value::Boolean(_));
seq_predicate!(booleans_eq, Value::Boolean, TypeName::BOOL, bool::eq);

//
// Bytevectors
//

fn load_bv(env: &Frame) {
    super::bind_intrinsic(env, "bytevector?", 1..1, is_bytevector);

    super::bind_intrinsic(env, "make-bytevector", 1..2, make_bytevector);
    super::bind_intrinsic(env, "bytevector", 0..MAX_ARITY, bytevector);

    super::bind_intrinsic(env, "bytevector-length", 1..1, bytevector_length);
    super::bind_intrinsic(env, "bytevector-u8-ref", 2..2, bytevector_get);
    super::bind_intrinsic(env, "bytevector-u8-set!", 3..3, bytevector_set);
}

predicate!(
    is_bytevector,
    Value::ByteVector(_) | Value::ByteVectorMut(_)
);
vec_new!(
    bytevector,
    |(idx, v)| valnum_to_byte(v, idx),
    Value::bytevector_mut,
    Vec<_>
);
vec_length!(bytevector_length, Value::as_refbv, TypeName::BYTEVECTOR);
vec_get!(
    bytevector_get,
    Value::as_refbv,
    |bv, u| bv.get(u).copied(),
    |item| Value::Number(Number::real(i64::from(item))),
    TypeName::BYTEVECTOR
);
vec_set!(
    bytevector_set,
    Value::ByteVectorMut,
    Value::ByteVector,
    TypeName::BYTEVECTOR,
    |v| valnum_to_byte(v, THIRD_ARG_LABEL),
    |mut v: RefMut<'_, Vec<_>>, idx, item| v[idx] = item
);

fn make_bytevector(args: &[Value], _env: &Frame) -> EvalResult {
    let k = first(args);
    let byte = args
        .get(1)
        .map_or(Ok(0), |v| valnum_to_byte(v, SECOND_ARG_LABEL))?;
    Ok(Value::bytevector_mut(iter::repeat_n(
        byte,
        valnum_to_index(k, FIRST_ARG_LABEL)?,
    )))
}

//
// Characters
//

fn load_char(env: &Frame) {
    super::bind_intrinsic(env, "char?", 1..1, is_char);

    super::bind_intrinsic(env, "char=?", 0..MAX_ARITY, chars_eq);
    super::bind_intrinsic(env, "char<?", 0..MAX_ARITY, chars_lt);
    super::bind_intrinsic(env, "char<=?", 0..MAX_ARITY, chars_lte);
    super::bind_intrinsic(env, "char>?", 0..MAX_ARITY, chars_gt);
    super::bind_intrinsic(env, "char>=?", 0..MAX_ARITY, chars_gte);

    super::bind_intrinsic(env, "char->integer", 1..1, char_to_integer);
    super::bind_intrinsic(env, "integer->char", 1..1, integer_to_char);
}

predicate!(is_char, Value::Character(_));
seq_predicate!(chars_eq, Value::Character, TypeName::CHAR, char::eq);
seq_predicate!(chars_lt, Value::Character, TypeName::CHAR, char::lt);
seq_predicate!(chars_lte, Value::Character, TypeName::CHAR, char::le);
seq_predicate!(chars_gt, Value::Character, TypeName::CHAR, char::gt);
seq_predicate!(chars_gte, Value::Character, TypeName::CHAR, char::ge);

fn char_to_integer(args: &[Value], _env: &Frame) -> EvalResult {
    let arg = first(args);
    if let Value::Character(c) = arg {
        let n = u32::from(*c);
        Ok(Value::Number(Number::real(i64::from(n))))
    } else {
        Err(invalid_target(TypeName::CHAR, arg))
    }
}

fn integer_to_char(args: &[Value], _env: &Frame) -> EvalResult {
    let arg = first(args);
    if let Value::Number(n) = arg {
        try_num_into_char(n, arg)
    } else {
        Err(invalid_target(NumericTypeName::INTEGER, arg))
    }
}

//
// Equivalence
//

fn load_eq(env: &Frame) {
    super::bind_intrinsic(env, "eqv?", 2..2, is_eqv);
    super::bind_intrinsic(env, "eq?", 2..2, is_eq);
    super::bind_intrinsic(env, "equal?", 2..2, is_equal);
}

#[allow(clippy::unnecessary_wraps, reason = "infallible intrinsic")]
fn is_eqv(args: &[Value], _env: &Frame) -> EvalResult {
    let a = first(args);
    let b = super::second(args);
    Ok(Value::Boolean(a.is_eqv(b)))
}

#[allow(clippy::unnecessary_wraps, reason = "infallible intrinsic")]
fn is_eq(args: &[Value], _env: &Frame) -> EvalResult {
    let a = first(args);
    let b = super::second(args);
    Ok(Value::Boolean(a.is(b)))
}

#[allow(clippy::unnecessary_wraps, reason = "infallible intrinsic")]
fn is_equal(args: &[Value], _env: &Frame) -> EvalResult {
    let a = first(args);
    let b = super::second(args);
    Ok(Value::Boolean(a == b))
}

//
// Exceptions
//

fn load_ex(env: &Frame) {
    super::bind_intrinsic(env, "error-object?", 1..1, is_error);

    super::bind_intrinsic(env, "error-object-message", 1..1, error_msg);
    super::bind_intrinsic(env, "error-object-irritants", 1..1, error_irritants);

    super::bind_intrinsic(env, "read-error?", 1..1, is_read_error);
    super::bind_intrinsic(env, "file-error?", 1..1, is_file_error);
}

predicate!(is_error, Value::Error(_));
predicate!(is_read_error, Value::Error(c) if c.is_read_err());
predicate!(is_file_error, Value::Error(c) if c.is_file_err());

fn error_msg(args: &[Value], _env: &Frame) -> EvalResult {
    let arg = first(args);
    if let Value::Error(c) = arg {
        Ok(Value::string_mut(c.message()))
    } else {
        Err(invalid_target(TypeName::ERROR, arg))
    }
}

fn error_irritants(args: &[Value], _env: &Frame) -> EvalResult {
    let arg = first(args);
    if let Value::Error(c) = arg {
        Ok(c.irritants().map_or(Value::null(), Value::clone))
    } else {
        Err(invalid_target(TypeName::ERROR, arg))
    }
}

//
// Numbers
//

fn load_num(env: &Frame) {
    // NOTE: complex and number predicates are identical sets
    super::bind_intrinsic(env, "number?", 1..1, is_number);
    super::bind_intrinsic(env, "complex?", 1..1, is_number);
    super::bind_intrinsic(env, "real?", 1..1, is_real);
    super::bind_intrinsic(env, "rational?", 1..1, is_rational);
    super::bind_intrinsic(env, "integer?", 1..1, is_integer);

    super::bind_intrinsic(env, "exact?", 1..1, is_exact);
    super::bind_intrinsic(env, "inexact?", 1..1, is_inexact);
    super::bind_intrinsic(env, "exact-integer?", 1..1, is_exact_integer);

    super::bind_intrinsic(env, "zero?", 1..1, is_zero);
    super::bind_intrinsic(env, "positive?", 1..1, is_positive);
    super::bind_intrinsic(env, "negative?", 1..1, is_negative);
    super::bind_intrinsic(env, "odd?", 1..1, is_odd);
    super::bind_intrinsic(env, "even?", 1..1, is_even);

    super::bind_intrinsic(env, "abs", 1..1, abs);

    super::bind_intrinsic(env, "numerator", 1..1, get_numerator);
    super::bind_intrinsic(env, "denominator", 1..1, get_denominator);

    super::bind_intrinsic(env, "inexact", 1..1, into_inexact);
    super::bind_intrinsic(env, "exact", 1..1, into_exact);
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

fn is_positive(args: &[Value], _env: &Frame) -> EvalResult {
    real_op(first(args), |r| Ok(Value::Boolean(r.is_positive())))
}

fn is_negative(args: &[Value], _env: &Frame) -> EvalResult {
    real_op(first(args), |r| Ok(Value::Boolean(r.is_negative())))
}

fn is_odd(args: &[Value], _env: &Frame) -> EvalResult {
    exact_int_predicate(first(args), |n| !n.is_even())
}

fn is_even(args: &[Value], _env: &Frame) -> EvalResult {
    exact_int_predicate(first(args), Integer::is_even)
}

fn abs(args: &[Value], _env: &Frame) -> EvalResult {
    real_op(first(args), |r| {
        Ok(Value::Number(Number::real(r.clone().into_abs())))
    })
}

fn get_numerator(args: &[Value], _env: &Frame) -> EvalResult {
    let arg = first(args);
    rational_op(arg, |r| {
        r.clone().try_into_numerator().map_or_else(
            |err| Err(Condition::value_error(err, arg).into()),
            |r| Ok(Value::Number(Number::real(r))),
        )
    })
}

fn get_denominator(args: &[Value], _env: &Frame) -> EvalResult {
    let arg = first(args);
    rational_op(arg, |r| {
        r.clone().try_into_denominator().map_or_else(
            |err| Err(Condition::value_error(err, arg).into()),
            |r| Ok(Value::Number(Number::real(r))),
        )
    })
}

fn into_inexact(args: &[Value], _env: &Frame) -> EvalResult {
    let arg = first(args);
    if let Value::Number(n) = arg {
        Ok(Value::Number(n.clone().into_inexact()))
    } else {
        Err(invalid_target(TypeName::NUMBER, arg))
    }
}

fn into_exact(args: &[Value], _env: &Frame) -> EvalResult {
    let arg = first(args);
    if let Value::Number(n) = arg {
        n.clone().try_into_exact().map_or_else(
            |err| Err(Condition::value_error(err, arg).into()),
            |n| Ok(Value::Number(n)),
        )
    } else {
        Err(invalid_target(TypeName::NUMBER, arg))
    }
}

//
// Pairs and Lists
//

fn load_list(env: &Frame) {
    super::bind_intrinsic(env, "pair?", 1..1, is_pair);

    super::bind_intrinsic(env, "car", 1..1, car);
    super::bind_intrinsic(env, "cdr", 1..1, cdr);
    super::bind_intrinsic(env, "caar", 1..1, caar);
    super::bind_intrinsic(env, "cadr", 1..1, cadr);
    super::bind_intrinsic(env, "cdar", 1..1, cdar);
    super::bind_intrinsic(env, "cddr", 1..1, cddr);

    super::bind_intrinsic(env, "null?", 1..1, is_null);
    super::bind_intrinsic(env, "list?", 1..1, is_list);

    super::bind_intrinsic(env, "length", 1..1, list_length);
    super::bind_intrinsic(env, "list-tail", 2..2, list_tail);
    super::bind_intrinsic(env, "list-ref", 2..2, list_get);
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
fn is_list(args: &[Value], _env: &Frame) -> EvalResult {
    let arg = first(args);
    Ok(Value::Boolean(match arg {
        Value::Pair(None) => true,
        Value::Pair(Some(p)) => p.is_list(),
        _ => false,
    }))
}

// TODO: circular lists => error
fn list_length(args: &[Value], _env: &Frame) -> EvalResult {
    let arg = first(args);
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
        _ => Err(invalid_target(TypeName::LIST, arg)),
    }
}

fn list_tail(args: &[Value], _env: &Frame) -> EvalResult {
    let lst = first(args);
    let k = super::second(args);
    sub_list(lst, valnum_to_index(k, SECOND_ARG_LABEL)?, k).cloned()
}

fn list_get(args: &[Value], _env: &Frame) -> EvalResult {
    let lst = first(args);
    let k = super::second(args);
    let subl = sub_list(lst, valnum_to_index(k, SECOND_ARG_LABEL)?, k)?;
    if let Value::Pair(None) = subl {
        Err(Condition::index_error(k).into())
    } else {
        pcar(subl)
    }
}

//
// Procedures
//

fn load_proc(env: &Frame) {
    super::bind_intrinsic(env, "procedure?", 1..1, is_procedure);
}

predicate!(is_procedure, Value::Intrinsic(_) | Value::Procedure(_));

//
// Strings
//

fn load_string(env: &Frame) {
    super::bind_intrinsic(env, "string?", 1..1, is_string);

    super::bind_intrinsic(env, "string", 0..MAX_ARITY, string);

    super::bind_intrinsic(env, "string-length", 1..1, string_length);
    super::bind_intrinsic(env, "string-ref", 2..2, string_get);
    super::bind_intrinsic(env, "string-set!", 3..3, string_set);

    super::bind_intrinsic(env, "string=?", 0..MAX_ARITY, strings_eq);
    super::bind_intrinsic(env, "string<?", 0..MAX_ARITY, strings_lt);
    super::bind_intrinsic(env, "string<=?", 0..MAX_ARITY, strings_lte);
    super::bind_intrinsic(env, "string>?", 0..MAX_ARITY, strings_gt);
    super::bind_intrinsic(env, "string>=?", 0..MAX_ARITY, strings_gte);
}

predicate!(is_string, Value::String(_) | Value::StringMut(_));
vec_new!(
    string,
    |(idx, v)| val_to_char(v, idx),
    Value::string_mut,
    String
);
vec_length!(string_length, Value::as_refstr, TypeName::STRING);
vec_get!(
    string_get,
    Value::as_refstr,
    |s, u| s.chars().nth(u),
    Value::Character,
    TypeName::STRING
);
vec_set!(
    string_set,
    Value::StringMut,
    Value::String,
    TypeName::STRING,
    |v| val_to_char(v, THIRD_ARG_LABEL),
    replace_str_char
);

fn strings_eq(args: &[Value], _env: &Frame) -> EvalResult {
    strs_predicate(args, str::eq)
}

fn strings_lt(args: &[Value], _env: &Frame) -> EvalResult {
    strs_predicate(args, str::lt)
}

fn strings_lte(args: &[Value], _env: &Frame) -> EvalResult {
    strs_predicate(args, str::le)
}

fn strings_gt(args: &[Value], _env: &Frame) -> EvalResult {
    strs_predicate(args, str::gt)
}

fn strings_gte(args: &[Value], _env: &Frame) -> EvalResult {
    strs_predicate(args, str::ge)
}

//
// Symbols
//

fn load_symbol(env: &Frame) {
    super::bind_intrinsic(env, "symbol?", 1..1, is_symbol);
    super::bind_intrinsic(env, "symbol=?", 0..MAX_ARITY, symbols_eq);

    super::bind_intrinsic(env, "symbol->string", 1..1, symbol_to_string);
    super::bind_intrinsic(env, "string->symbol", 1..1, string_to_symbol);
}

predicate!(is_symbol, Value::Symbol(_));
seq_predicate!(symbols_eq, Value::Symbol, TypeName::SYMBOL, Symbol::is);

fn symbol_to_string(args: &[Value], _env: &Frame) -> EvalResult {
    let arg = first(args);
    if let Value::Symbol(s) = arg {
        Ok(Value::string(s.as_rc()))
    } else {
        Err(invalid_target(TypeName::SYMBOL, arg))
    }
}

fn string_to_symbol(args: &[Value], env: &Frame) -> EvalResult {
    let arg = first(args);
    arg.as_refstr().map_or_else(
        || Err(invalid_target(TypeName::STRING, arg)),
        |s| Ok(Value::Symbol(env.sym.get(s))),
    )
}

//
// Vectors
//

fn load_vec(env: &Frame) {
    super::bind_intrinsic(env, "vector?", 1..1, is_vector);

    super::bind_intrinsic(env, "vector", 0..MAX_ARITY, vector);

    super::bind_intrinsic(env, "vector-length", 1..1, vector_length);
    super::bind_intrinsic(env, "vector-ref", 2..2, vector_get);
    super::bind_intrinsic(env, "vector-set!", 3..3, vector_set);
}

predicate!(is_vector, Value::Vector(_) | Value::VectorMut(_));
vec_new!(
    vector,
    |(_, v)| Ok::<_, Exception>(v.clone()),
    Value::vector_mut,
    Vec<_>
);
vec_length!(vector_length, Value::as_refvec, TypeName::VECTOR);
vec_get!(
    vector_get,
    Value::as_refvec,
    |v, u| v.get(u).cloned(),
    convert::identity,
    TypeName::VECTOR
);
vec_set!(
    vector_set,
    Value::VectorMut,
    Value::Vector,
    TypeName::VECTOR,
    |val: &Value| Ok::<_, Exception>(val.clone()),
    |mut v: RefMut<'_, Vec<_>>, idx, item| v[idx] = item
);

//
// Helpers
//

num_convert!(
    valnum_to_index,
    usize,
    NumericError::UsizeConversionInvalidRange
);
num_convert!(valnum_to_byte, u8, NumericError::ByteConversionInvalidRange);

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

fn real_op(arg: &Value, op: impl FnOnce(&Real) -> EvalResult) -> EvalResult {
    guarded_real_op(arg, NumericTypeName::REAL, op)
}

fn rational_op(arg: &Value, op: impl FnOnce(&Real) -> EvalResult) -> EvalResult {
    guarded_real_op(arg, NumericTypeName::RATIONAL, op)
}

fn exact_int_predicate(arg: &Value, pred: impl FnOnce(&Integer) -> bool) -> EvalResult {
    guarded_real_op(arg, NumericTypeName::INTEGER, |r| {
        r.clone().try_into_exact_integer().map_or_else(
            |err| Err(Condition::value_error(err, arg).into()),
            |n| Ok(Value::Boolean(pred(&n))),
        )
    })
}

fn guarded_real_op(
    arg: &Value,
    expected_type: impl Display,
    op: impl FnOnce(&Real) -> EvalResult,
) -> EvalResult {
    let Value::Number(n) = arg else {
        return Err(invalid_target(expected_type, arg));
    };
    if let Number::Real(r) = n {
        op(r)
    } else {
        Err(Condition::arg_type_error(FIRST_ARG_LABEL, expected_type, n.as_typename(), arg).into())
    }
}

fn val_to_char(arg: &Value, lbl: impl Display) -> Result<char, Exception> {
    if let Value::Character(c) = arg {
        Ok(*c)
    } else {
        Err(Condition::arg_error(lbl, TypeName::CHAR, arg).into())
    }
}

fn replace_str_char(mut s: RefMut<'_, String>, idx: usize, ch: char) {
    let mut chars = s.chars().collect::<Vec<_>>();
    let c = chars.get_mut(idx).expect("expected valid index");
    *c = ch;
    s.replace_range(.., &chars.into_iter().collect::<String>());
}

fn strs_predicate(args: &[Value], pred: impl Fn(&str, &str) -> bool) -> EvalResult {
    let result: Result<_, Exception> = args
        .iter()
        .enumerate()
        .try_fold::<(bool, Option<&Value>), _, _>((true, None), |(acc, prev), (idx, val)| {
            let b = val
                .as_refstr()
                .ok_or_else(|| Exception::from(Condition::arg_error(idx, TypeName::STRING, val)))?;
            let a = match prev {
                None => return Ok((acc, Some(val))),
                Some(v) => v.as_refstr().expect("unexpected value type from prev"),
            };
            Ok((acc && pred(a.as_ref(), b.as_ref()), Some(val)))
        });
    Ok(Value::Boolean(result?.0))
}

fn sub_list<'a>(lst: &'a Value, idx: usize, k: &Value) -> Result<&'a Value, Exception> {
    if idx == 0 {
        Ok(lst)
    } else if let Value::Pair(Some(p)) = lst {
        sub_list(&p.cdr, idx - 1, k)
    } else if let Value::Pair(None) = lst {
        Err(Condition::index_error(k).into())
    } else {
        Err(invalid_target(TypeName::PAIR, lst))
    }
}

fn vec_item<T, U>(
    vec: T,
    k: &Value,
    get: impl FnOnce(T, usize) -> Option<U>,
    map: impl FnOnce(U) -> Value,
) -> EvalResult {
    get(vec, valnum_to_index(k, SECOND_ARG_LABEL)?).map_or_else(
        || Err(Condition::index_error(k).into()),
        |item| Ok(map(item)),
    )
}
