// (scheme base)
macro_rules! predicate {
    ($name:ident, $pred:pat $(if $guard:expr)?) => {
        fn $name(args: &[Value], _env: &Frame) -> EvalResult {
            Ok(Value::Boolean(matches!(first(args), $pred $(if $guard)?)))
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
    value::{BvRef, CollRef, CollSized, Condition, StrRef, TypeName, Value, VecRef},
};
use std::{
    cell::RefMut,
    convert,
    fmt::Display,
    iter::{self, RepeatN},
    mem,
    ops::Range,
};

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

    super::bind_intrinsic(env, "bytevector-copy", 1..3, bytevector_copy);
    super::bind_intrinsic(env, "bytevector-copy!", 3..5, bytevector_copy_inline);
    super::bind_intrinsic(env, "bytevector-append", 0..MAX_ARITY, bytevector_append);

    super::bind_intrinsic(env, "utf8->string", 1..3, bytevector_to_str);
    super::bind_intrinsic(env, "string->utf8", 1..3, bytevector_from_str);
}

predicate!(
    is_bytevector,
    Value::ByteVector(_) | Value::ByteVectorMut(_)
);

fn make_bytevector(args: &[Value], _env: &Frame) -> EvalResult {
    coll_fill(
        args,
        u8::MIN,
        |v| try_val_to_byte(v, SECOND_ARG_LABEL),
        Value::bytevector_mut,
    )
}

fn bytevector(args: &[Value], _env: &Frame) -> EvalResult {
    coll_new(
        args,
        |(idx, v)| try_val_to_byte(v, idx),
        Value::bytevector_mut,
    )
}

fn bytevector_length(args: &[Value], _env: &Frame) -> EvalResult {
    coll_length(first(args), TypeName::BYTEVECTOR, Value::as_refbv)
}

fn bytevector_get(args: &[Value], _env: &Frame) -> EvalResult {
    coll_get(
        first(args),
        super::second(args),
        Value::as_refbv,
        TypeName::BYTEVECTOR,
        |bv, u| bv.get(u).copied(),
        |item| Value::Number(Number::real(i64::from(item))),
    )
}

fn bytevector_set(args: &[Value], _env: &Frame) -> EvalResult {
    coll_set(
        first(args),
        super::second(args),
        super::third(args),
        TypeName::VECTOR,
        Value::as_refbv,
        Value::as_mutrefbv,
        |v| try_val_to_byte(v, THIRD_ARG_LABEL),
        |mut v, idx, item| v[idx] = item,
    )
}

fn bytevector_copy(args: &[Value], _env: &Frame) -> EvalResult {
    coll_copy(
        first(args),
        args.get(1),
        args.get(2),
        TypeName::BYTEVECTOR,
        Value::as_refbv,
        |bytes: &[u8], span| Value::bytevector_mut(bytes[span].into_iter().copied()),
    )
}

fn bytevector_copy_inline(args: &[Value], _env: &Frame) -> EvalResult {
    let to = first(args);
    let tolen = to
        .as_refbv()
        .ok_or_else(|| invalid_target(TypeName::BYTEVECTOR, to))?
        .len();
    let at = super::second(args);
    let atidx = try_val_to_index(&at, SECOND_ARG_LABEL)?;
    if tolen < atidx {
        return Err(Condition::value_error("target index out of range", &at).into());
    }
    let from = super::third(args);
    let source = from.as_refbv().ok_or_else(|| {
        Exception::from(Condition::arg_error(
            THIRD_ARG_LABEL,
            TypeName::BYTEVECTOR,
            from,
        ))
    })?;
    let span = try_coll_span(args.get(3), args.get(4), source.len())?;
    if tolen - atidx < span.len() {
        return Err(Condition::bi_value_error(
            "source span too large for target range",
            &Value::cons(
                Value::Number(Number::from_usize(span.start)),
                Value::Number(Number::from_usize(span.end)),
            ),
            &Value::cons(
                Value::Number(Number::from_usize(atidx)),
                Value::Number(Number::from_usize(tolen)),
            ),
        )
        .into());
    }
    if let Value::ByteVectorMut(target) = to {
        if to.is(&from) {
            mem::drop(source);
            target.borrow_mut().copy_within(span, atidx);
        } else {
            target.borrow_mut()[atidx..(atidx + span.len())]
                .copy_from_slice(&source.as_ref()[span]);
        }
        Ok(Value::Unspecified)
    } else {
        Err(Condition::literal_mut_error(to).into())
    }
}

fn bytevector_append(args: &[Value], _env: &Frame) -> EvalResult {
    coll_append(
        args,
        Value::as_refbv,
        TypeName::BYTEVECTOR,
        |bvs: &[BvRef]| Value::bytevector_mut(bvs.iter().flat_map(BvRef::as_ref).copied()),
    )
}

fn bytevector_to_str(args: &[Value], _env: &Frame) -> EvalResult {
    let arg = first(args);
    let bv = arg
        .as_refbv()
        .ok_or_else(|| invalid_target(TypeName::BYTEVECTOR, arg))?;
    let clen = bv.len();
    let span = try_coll_span(args.get(1), args.get(2), clen)?;
    String::from_utf8(bv.as_ref()[span].to_vec()).map_or_else(
        |e| {
            let err = e.utf8_error();
            let start = err.valid_up_to();
            let end = err.error_len().map_or(clen, |len| start + len);
            Err(Condition::bi_value_error(
                "invalid UTF-8 byte sequence",
                &Value::bytevector_mut(bv.as_ref()[start..end].into_iter().copied()),
                &Value::cons(
                    Value::Number(Number::from_usize(start)),
                    Value::Number(Number::from_usize(end)),
                ),
            )
            .into())
        },
        |s| Ok(Value::string_mut(s)),
    )
}

fn bytevector_from_str(args: &[Value], _env: &Frame) -> EvalResult {
    let arg = first(args);
    let s = arg
        .as_refstr()
        .ok_or_else(|| invalid_target(TypeName::STRING, arg))?;
    let span = try_coll_span(args.get(1), args.get(2), s.len())?;
    // TODO: experimental
    // https://doc.rust-lang.org/std/primitive.char.html#associatedconstant.MAX_LEN_UTF8
    let mut buf = [0u8; 4];
    Ok(Value::bytevector_mut(
        s.as_ref()
            .chars()
            .skip(span.start)
            .take(span.len())
            .flat_map(|ch| ch.encode_utf8(&mut buf).as_bytes().to_vec()),
    ))
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
    super::bind_intrinsic(env, "integer->char", 1..1, char_from_integer);
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

fn char_from_integer(args: &[Value], _env: &Frame) -> EvalResult {
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
    Ok(Value::Boolean(match first(args) {
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
    let k = super::second(args);
    try_sub_list(first(args), try_val_to_index(k, SECOND_ARG_LABEL)?, k).cloned()
}

fn list_get(args: &[Value], _env: &Frame) -> EvalResult {
    let k = super::second(args);
    let subl = try_sub_list(first(args), try_val_to_index(k, SECOND_ARG_LABEL)?, k)?;
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

    super::bind_intrinsic(env, "make-string", 1..2, make_string);
    super::bind_intrinsic(env, "string", 0..MAX_ARITY, string);

    super::bind_intrinsic(env, "string-length", 1..1, string_length);
    super::bind_intrinsic(env, "string-ref", 2..2, string_get);
    super::bind_intrinsic(env, "string-set!", 3..3, string_set);

    super::bind_intrinsic(env, "string=?", 0..MAX_ARITY, strings_eq);
    super::bind_intrinsic(env, "string<?", 0..MAX_ARITY, strings_lt);
    super::bind_intrinsic(env, "string<=?", 0..MAX_ARITY, strings_lte);
    super::bind_intrinsic(env, "string>?", 0..MAX_ARITY, strings_gt);
    super::bind_intrinsic(env, "string>=?", 0..MAX_ARITY, strings_gte);

    super::bind_intrinsic(env, "substring", 3..3, string_copy);
    super::bind_intrinsic(env, "string-append", 0..MAX_ARITY, string_append);
    super::bind_intrinsic(env, "string-copy", 1..3, string_copy);
    super::bind_intrinsic(env, "string-copy!", 3..5, string_copy_inline);
}

predicate!(is_string, Value::String(_) | Value::StringMut(_));

fn make_string(args: &[Value], _env: &Frame) -> EvalResult {
    coll_fill(
        args,
        char::MIN,
        |v| try_val_to_char(v, SECOND_ARG_LABEL),
        Value::strmut_from_chars,
    )
}

fn string(args: &[Value], _env: &Frame) -> EvalResult {
    coll_new(
        args,
        |(idx, v)| try_val_to_char(v, idx),
        Value::strmut_from_chars,
    )
}

fn string_length(args: &[Value], _env: &Frame) -> EvalResult {
    coll_length(first(args), TypeName::STRING, Value::as_refstr)
}

fn string_get(args: &[Value], _env: &Frame) -> EvalResult {
    coll_get(
        first(args),
        super::second(args),
        Value::as_refstr,
        TypeName::STRING,
        |s, u| s.chars().nth(u),
        Value::Character,
    )
}

fn string_set(args: &[Value], _env: &Frame) -> EvalResult {
    coll_set(
        first(args),
        super::second(args),
        super::third(args),
        TypeName::VECTOR,
        Value::as_refstr,
        Value::as_mutrefstr,
        |v| try_val_to_char(v, THIRD_ARG_LABEL),
        |mut s, idx, ch| {
            let mut chars = s.chars().collect::<Vec<_>>();
            let c = chars.get_mut(idx).expect("expected valid index");
            *c = ch;
            s.replace_range(.., &chars.into_iter().collect::<String>());
        },
    )
}

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

fn string_copy(args: &[Value], _env: &Frame) -> EvalResult {
    coll_copy(
        first(args),
        args.get(1),
        args.get(2),
        TypeName::STRING,
        Value::as_refstr,
        |s: &str, span| Value::strmut_from_chars(s.chars().skip(span.start).take(span.len())),
    )
}

fn string_copy_inline(args: &[Value], _env: &Frame) -> EvalResult {
    let to = first(args);
    let tolen = to
        .as_refstr()
        .ok_or_else(|| invalid_target(TypeName::STRING, to))?
        .len();
    let at = super::second(args);
    let atidx = try_val_to_index(&at, SECOND_ARG_LABEL)?;
    if tolen < atidx {
        return Err(Condition::value_error("target index out of range", &at).into());
    }
    let from = super::third(args);
    let source = from.as_refstr().ok_or_else(|| {
        Exception::from(Condition::arg_error(
            THIRD_ARG_LABEL,
            TypeName::STRING,
            from,
        ))
    })?;
    let span = try_coll_span(args.get(3), args.get(4), source.len())?;
    if tolen - atidx < span.len() {
        return Err(Condition::bi_value_error(
            "source span too large for target range",
            &Value::cons(
                Value::Number(Number::from_usize(span.start)),
                Value::Number(Number::from_usize(span.end)),
            ),
            &Value::cons(
                Value::Number(Number::from_usize(atidx)),
                Value::Number(Number::from_usize(tolen)),
            ),
        )
        .into());
    }
    if let Value::StringMut(target) = to {
        let updated = {
            let replace = source.as_ref().chars().skip(span.start).take(span.len());
            let trg = target.borrow();
            let start = trg.chars().take(atidx);
            let rest = trg.chars().skip(atidx + span.len());
            start.chain(replace).chain(rest).collect::<String>()
        };
        mem::drop(source);
        target.borrow_mut().replace_range(.., &updated);
        Ok(Value::Unspecified)
    } else {
        Err(Condition::literal_mut_error(to).into())
    }
}

fn string_append(args: &[Value], _env: &Frame) -> EvalResult {
    coll_append(
        args,
        Value::as_refstr,
        TypeName::STRING,
        |strs: &[StrRef]| Value::strmut_from_chars(strs.iter().flat_map(|s| s.as_ref().chars())),
    )
}

//
// Symbols
//

fn load_symbol(env: &Frame) {
    super::bind_intrinsic(env, "symbol?", 1..1, is_symbol);
    super::bind_intrinsic(env, "symbol=?", 0..MAX_ARITY, symbols_eq);

    super::bind_intrinsic(env, "symbol->string", 1..1, symbol_to_string);
    super::bind_intrinsic(env, "string->symbol", 1..1, symbol_from_string);
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

fn symbol_from_string(args: &[Value], env: &Frame) -> EvalResult {
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

    super::bind_intrinsic(env, "make-vector", 1..2, make_vector);
    super::bind_intrinsic(env, "vector", 0..MAX_ARITY, vector);

    super::bind_intrinsic(env, "vector-length", 1..1, vector_length);
    super::bind_intrinsic(env, "vector-ref", 2..2, vector_get);
    super::bind_intrinsic(env, "vector-set!", 3..3, vector_set);

    super::bind_intrinsic(env, "vector-copy", 1..3, vector_copy);
    super::bind_intrinsic(env, "vector-append", 0..MAX_ARITY, vector_append);
}

predicate!(is_vector, Value::Vector(_) | Value::VectorMut(_));

fn make_vector(args: &[Value], _env: &Frame) -> EvalResult {
    coll_fill(
        args,
        Value::Unspecified,
        |v| Ok(v.clone()),
        Value::vector_mut,
    )
}

fn vector(args: &[Value], _env: &Frame) -> EvalResult {
    coll_new(args, |(_, v)| Ok(v.clone()), Value::vector_mut)
}

fn vector_length(args: &[Value], _env: &Frame) -> EvalResult {
    coll_length(first(args), TypeName::VECTOR, Value::as_refvec)
}

fn vector_get(args: &[Value], _env: &Frame) -> EvalResult {
    coll_get(
        first(args),
        super::second(args),
        Value::as_refvec,
        TypeName::VECTOR,
        |v, u| v.get(u).cloned(),
        convert::identity,
    )
}

fn vector_set(args: &[Value], _env: &Frame) -> EvalResult {
    coll_set(
        first(args),
        super::second(args),
        super::third(args),
        TypeName::VECTOR,
        Value::as_refvec,
        Value::as_mutrefvec,
        |v| Ok(v.clone()),
        |mut v, idx, item| v[idx] = item,
    )
}

fn vector_copy(args: &[Value], _env: &Frame) -> EvalResult {
    coll_copy(
        first(args),
        args.get(1),
        args.get(2),
        TypeName::VECTOR,
        Value::as_refvec,
        |vals: &[Value], span| Value::vector_mut(vals[span].into_iter().cloned()),
    )
}

fn vector_append(args: &[Value], _env: &Frame) -> EvalResult {
    coll_append(
        args,
        Value::as_refvec,
        TypeName::VECTOR,
        |vecs: &[VecRef]| Value::vector_mut(vecs.iter().flat_map(VecRef::as_ref).cloned()),
    )
}

//
// Helpers
//

num_convert!(
    try_val_to_index,
    usize,
    NumericError::UsizeConversionInvalidRange
);
num_convert!(
    try_val_to_byte,
    u8,
    NumericError::ByteConversionInvalidRange
);

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

fn try_val_to_char(arg: &Value, lbl: impl Display) -> Result<char, Exception> {
    if let Value::Character(c) = arg {
        Ok(*c)
    } else {
        Err(Condition::arg_error(lbl, TypeName::CHAR, arg).into())
    }
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

fn try_sub_list<'a>(lst: &'a Value, idx: usize, k: &Value) -> Result<&'a Value, Exception> {
    if idx == 0 {
        Ok(lst)
    } else if let Value::Pair(Some(p)) = lst {
        try_sub_list(&p.cdr, idx - 1, k)
    } else if let Value::Pair(None) = lst {
        Err(Condition::index_error(k).into())
    } else {
        Err(invalid_target(TypeName::PAIR, lst))
    }
}

fn coll_new<T>(
    args: &[Value],
    map: impl FnMut((usize, &Value)) -> Result<T, Exception>,
    ctor: impl FnOnce(Vec<T>) -> Value,
) -> EvalResult {
    Ok(ctor(
        args.iter()
            .enumerate()
            .map(map)
            .collect::<Result<Vec<_>, _>>()?,
    ))
}

fn coll_fill<T: Clone>(
    args: &[Value],
    default: T,
    map: impl FnOnce(&Value) -> Result<T, Exception>,
    ctor: impl FnOnce(RepeatN<T>) -> Value,
) -> EvalResult {
    Ok(ctor(iter::repeat_n(
        args.get(1).map_or(Ok(default), map)?,
        try_val_to_index(first(args), FIRST_ARG_LABEL)?,
    )))
}

fn coll_length<'a, T: ?Sized + 'a, M: AsRef<T> + 'a>(
    arg: &'a Value,
    expected_type: impl Display,
    collref: impl FnOnce(&'a Value) -> Option<CollRef<'a, T, M>>,
) -> EvalResult
where
    CollRef<'a, T, M>: CollSized,
{
    let c = collref(arg).ok_or_else(|| invalid_target(expected_type, arg))?;
    Ok(Value::Number(Number::from_usize(c.len())))
}

fn coll_get<T: ?Sized, M: AsRef<T>, U>(
    arg: &Value,
    k: &Value,
    collref: impl FnOnce(&Value) -> Option<CollRef<'_, T, M>>,
    expected_type: impl Display,
    get: impl FnOnce(&T, usize) -> Option<U>,
    map: impl FnOnce(U) -> Value,
) -> EvalResult {
    let c = collref(arg).ok_or_else(|| invalid_target(expected_type, arg))?;
    get(c.as_ref(), try_val_to_index(k, SECOND_ARG_LABEL)?).map_or_else(
        || Err(Condition::index_error(k).into()),
        |item| Ok(map(item)),
    )
}

fn coll_set<'a, T: ?Sized + 'a, M: AsRef<T> + 'a, U>(
    arg: &'a Value,
    k: &'a Value,
    val: &'a Value,
    expected_type: impl Display,
    collref: impl Fn(&'a Value) -> Option<CollRef<'a, T, M>>,
    mutcollref: impl Fn(&'a Value) -> Option<RefMut<'a, M>>,
    try_item: impl FnOnce(&'a Value) -> Result<U, Exception>,
    set: impl FnOnce(RefMut<'a, M>, usize, U),
) -> EvalResult
where
    CollRef<'a, T, M>: CollSized,
{
    let clen = collref(arg)
        .ok_or_else(|| invalid_target(expected_type, arg))?
        .len();
    mutcollref(arg).map_or_else(
        || Err(Condition::literal_mut_error(arg).into()),
        |c| {
            let idx = try_val_to_index(k, SECOND_ARG_LABEL)?;
            if idx < clen {
                let item = try_item(val)?;
                set(c, idx, item);
                Ok(Value::Unspecified)
            } else {
                Err(Condition::index_error(k).into())
            }
        },
    )
}

fn coll_copy<'a, T: ?Sized + 'a, M: AsRef<T> + 'a>(
    arg: &'a Value,
    start: Option<&'a Value>,
    end: Option<&'a Value>,
    expected_type: impl Display,
    collref: impl FnOnce(&'a Value) -> Option<CollRef<'a, T, M>>,
    copy: impl FnOnce(&T, Range<usize>) -> Value,
) -> EvalResult
where
    CollRef<'a, T, M>: CollSized,
{
    let coll = collref(arg).ok_or_else(|| invalid_target(expected_type, arg))?;
    Ok(copy(coll.as_ref(), try_coll_span(start, end, coll.len())?))
}

fn coll_append<T: ?Sized, M: AsRef<T>>(
    args: &[Value],
    collref: impl Fn(&Value) -> Option<CollRef<'_, T, M>>,
    expected_type: impl Display + Clone,
    copy: impl FnOnce(&[CollRef<'_, T, M>]) -> Value,
) -> EvalResult {
    let coll_refs = args
        .iter()
        .enumerate()
        .map(|(idx, v)| {
            collref(v)
                .ok_or_else(|| Exception::from(Condition::arg_error(idx, expected_type.clone(), v)))
        })
        .collect::<Result<Vec<_>, _>>()?;
    Ok(copy(&coll_refs))
}

fn try_coll_span(
    start: Option<&Value>,
    end: Option<&Value>,
    clen: usize,
) -> Result<Range<usize>, Exception> {
    let sidx = start.map_or(Ok(usize::MIN), |v| try_val_to_index(v, SECOND_ARG_LABEL))?;
    let eidx = end.map_or(Ok(clen), |v| try_val_to_index(v, THIRD_ARG_LABEL))?;
    if clen < eidx {
        Err(Condition::index_error(end.unwrap()).into())
    } else if eidx < sidx {
        Err(if let Some(v) = end {
            Condition::bi_value_error("start greater than end", start.unwrap(), v).into()
        } else {
            Condition::index_error(start.unwrap()).into()
        })
    } else {
        Ok(sidx..eidx)
    }
}
