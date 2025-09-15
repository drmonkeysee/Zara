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
            it.try_fold((true, first), |(acc, prev), (idx, val)| {
                if let $kind(next) = val {
                    Ok((acc && $pred(prev, next), next))
                } else {
                    Err(Condition::arg_error(idx, $valname, val).into())
                }
            })
            .map(|(b, _)| Value::Boolean(b))
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
    eval::{EvalResult, Frame, IntrinsicFn, MAX_ARITY},
    number::{Integer, Number, NumericError, NumericTypeName, Real},
    string::{Symbol, unicode::UnicodeError},
    value::{
        BvRef, CollRef, CollSized, Condition, InvalidList, Pair, PairSized, StrRef, TypeName,
        ValItem, Value, VecRef,
    },
};
use std::{
    cell::RefMut,
    convert,
    fmt::Display,
    iter::{self, RepeatN, Skip, Take},
    mem,
    ops::Range,
    str::Chars,
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

    super::bind_intrinsic(env, "make-bytevector", 1..2, bytevector_make);
    super::bind_intrinsic(env, "bytevector", 0..MAX_ARITY, bytevector);

    super::bind_intrinsic(env, "bytevector-length", 1..1, bytevector_length);
    super::bind_intrinsic(env, "bytevector-u8-ref", 2..2, bytevector_get);
    super::bind_intrinsic(env, "bytevector-u8-set!", 3..3, bytevector_set);

    super::bind_intrinsic(env, "bytevector-copy", 1..3, bytevector_copy);
    super::bind_intrinsic(env, "bytevector-copy!", 3..5, bytevector_copy_inline);
    super::bind_intrinsic(env, "bytevector-append", 0..MAX_ARITY, bytevector_append);

    super::bind_intrinsic(env, "utf8->string", 1..3, bytevector_to_string);
    super::bind_intrinsic(env, "string->utf8", 1..3, bytevector_from_string);
}

predicate!(
    is_bytevector,
    Value::ByteVector(_) | Value::ByteVectorMut(_)
);

fn bytevector_make(args: &[Value], _env: &Frame) -> EvalResult {
    coll_make(
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
        |item| Value::real(i64::from(item)),
    )
}

fn bytevector_set(args: &[Value], _env: &Frame) -> EvalResult {
    coll_set(
        first(args),
        super::second(args),
        super::third(args),
        TypeName::VECTOR,
        (Value::as_refbv, Value::as_mutrefbv),
        |v| try_val_to_byte(v, THIRD_ARG_LABEL),
        |mut v, idx, item| v[idx] = item,
    )
}

fn bytevector_copy(args: &[Value], _env: &Frame) -> EvalResult {
    coll_copy(
        first(args),
        args.get(1)..args.get(2),
        TypeName::BYTEVECTOR,
        Value::as_refbv,
        |bytes: &[u8], span| Value::bytevector_mut(bytes[span].iter().copied()),
    )
}

fn bytevector_copy_inline(args: &[Value], _env: &Frame) -> EvalResult {
    coll_copy_inline(
        first(args),
        super::second(args),
        super::third(args),
        args.get(3)..args.get(4),
        TypeName::BYTEVECTOR,
        (Value::as_refbv, Value::as_mutrefbv),
        |to, from, mut target, atidx, span| {
            if to.is(from) {
                target.copy_within(span, atidx);
            } else {
                let src = from.as_refbv().expect("expected bytevector argument");
                target[atidx..(atidx + span.len())].copy_from_slice(&src.as_ref()[span]);
            }
        },
    )
}

fn bytevector_append(args: &[Value], _env: &Frame) -> EvalResult {
    coll_append(
        args,
        Value::as_refbv,
        TypeName::BYTEVECTOR,
        |bvs: &[BvRef]| Value::bytevector_mut(bvs.iter().flat_map(BvRef::as_ref).copied()),
    )
}

fn bytevector_to_string(args: &[Value], _env: &Frame) -> EvalResult {
    let arg = first(args);
    let bv = arg
        .as_refbv()
        .ok_or_else(|| invalid_target(TypeName::BYTEVECTOR, arg))?;
    let clen = bv.len();
    let span = try_coll_span(args.get(1)..args.get(2), clen)?;
    String::from_utf8(bv.as_ref()[span].to_vec()).map_or_else(
        |e| {
            let err = e.utf8_error();
            let start = err.valid_up_to();
            let end = err.error_len().map_or(clen, |len| start + len);
            Err(Condition::bi_value_error(
                "invalid UTF-8 byte sequence",
                &Value::bytevector_mut(bv.as_ref()[start..end].iter().copied()),
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

fn bytevector_from_string(args: &[Value], _env: &Frame) -> EvalResult {
    str_to_coll(first(args), args.get(1)..args.get(2), |chars| {
        // TODO: experimental
        // https://doc.rust-lang.org/std/primitive.char.html#associatedconstant.MAX_LEN_UTF8
        let mut buf = [0u8; 4];
        Value::bytevector_mut(chars.flat_map(|ch| ch.encode_utf8(&mut buf).as_bytes().to_vec()))
    })
}

//
// Characters
//

fn load_char(env: &Frame) {
    super::bind_intrinsic(env, "char?", 1..1, is_char);

    super::bind_intrinsic(env, "char=?", 0..MAX_ARITY, chars_eq);
    super::bind_intrinsic(env, "char<?", 0..MAX_ARITY, chars_lt);
    super::bind_intrinsic(env, "char>?", 0..MAX_ARITY, chars_gt);
    super::bind_intrinsic(env, "char<=?", 0..MAX_ARITY, chars_lte);
    super::bind_intrinsic(env, "char>=?", 0..MAX_ARITY, chars_gte);

    super::bind_intrinsic(env, "char->integer", 1..1, char_to_integer);
    super::bind_intrinsic(env, "integer->char", 1..1, char_from_integer);
}

predicate!(is_char, Value::Character(_));
seq_predicate!(chars_eq, Value::Character, TypeName::CHAR, char::eq);
seq_predicate!(chars_lt, Value::Character, TypeName::CHAR, char::lt);
seq_predicate!(chars_gt, Value::Character, TypeName::CHAR, char::gt);
seq_predicate!(chars_lte, Value::Character, TypeName::CHAR, char::le);
seq_predicate!(chars_gte, Value::Character, TypeName::CHAR, char::ge);

fn char_to_integer(args: &[Value], _env: &Frame) -> EvalResult {
    let arg = first(args);
    if let Value::Character(c) = arg {
        let n = u32::from(*c);
        Ok(Value::real(i64::from(n)))
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
        Ok(Value::string(c.message()))
    } else {
        Err(invalid_target(TypeName::ERROR, arg))
    }
}

fn error_irritants(args: &[Value], _env: &Frame) -> EvalResult {
    let arg = first(args);
    if let Value::Error(c) = arg {
        Ok(c.irritants().map_or(Value::Null, Value::clone))
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
    real_op(first(args), |r| Ok(Value::real(r.clone().into_abs())))
}

fn get_numerator(args: &[Value], _env: &Frame) -> EvalResult {
    let arg = first(args);
    rational_op(arg, |r| {
        r.clone().try_into_numerator().map_or_else(
            |err| Err(Condition::value_error(err, arg).into()),
            |r| Ok(Value::real(r)),
        )
    })
}

fn get_denominator(args: &[Value], _env: &Frame) -> EvalResult {
    let arg = first(args);
    rational_op(arg, |r| {
        r.clone().try_into_denominator().map_or_else(
            |err| Err(Condition::value_error(err, arg).into()),
            |r| Ok(Value::real(r)),
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

    super::bind_intrinsic(env, "cons", 2..2, cons);
    super::bind_intrinsic(env, "car", 1..1, car);
    super::bind_intrinsic(env, "cdr", 1..1, cdr);
    super::bind_intrinsic(env, "set-car!", 2..2, set_car);
    super::bind_intrinsic(env, "set-cdr!", 2..2, set_cdr);

    super::bind_intrinsic(env, "caar", 1..1, caar);
    super::bind_intrinsic(env, "cadr", 1..1, cadr);
    super::bind_intrinsic(env, "cdar", 1..1, cdar);
    super::bind_intrinsic(env, "cddr", 1..1, cddr);

    super::bind_intrinsic(env, "null?", 1..1, is_null);
    super::bind_intrinsic(env, "list?", 1..1, is_list);

    super::bind_intrinsic(env, "make-list", 1..2, list_make);
    super::bind_intrinsic(env, "list", 0..MAX_ARITY, list);

    super::bind_intrinsic(env, "length", 1..1, list_length);
    super::bind_intrinsic(env, "append", 0..MAX_ARITY, list_append);
    super::bind_intrinsic(env, "reverse", 1..1, list_reverse);

    super::bind_intrinsic(env, "list-tail", 2..2, list_tail);
    super::bind_intrinsic(env, "list-ref", 2..2, list_get);
    super::bind_intrinsic(env, "list-set!", 3..3, list_set);

    super::bind_intrinsic(env, "memq", 2..2, member_eq);
    super::bind_intrinsic(env, "memv", 2..2, member_eqv);
    super::bind_intrinsic(env, "member", 2..3, member_equal);
    super::bind_intrinsic(env, "assq", 2..2, assoc_eq);
    super::bind_intrinsic(env, "assv", 2..2, assoc_eqv);
    super::bind_intrinsic(env, "assoc", 2..3, assoc_equal);

    super::bind_intrinsic(env, "list-copy", 1..1, list_copy);
}

predicate!(is_pair, Value::Pair(_) | Value::PairMut(_));
predicate!(is_null, Value::Null);
cadr_func!(car, a);
cadr_func!(cdr, d);
cadr_func!(caar, a, a);
cadr_func!(cadr, a, d);
cadr_func!(cdar, d, a);
cadr_func!(cddr, d, d);

#[allow(clippy::unnecessary_wraps, reason = "infallible intrinsic")]
fn cons(args: &[Value], _env: &Frame) -> EvalResult {
    Ok(Value::cons_mut(
        first(args).clone(),
        super::second(args).clone(),
    ))
}

fn set_car(args: &[Value], _env: &Frame) -> EvalResult {
    pair_set(first(args), super::second(args), |mut p, v| {
        p.car = v.clone();
    })
}

fn set_cdr(args: &[Value], _env: &Frame) -> EvalResult {
    pair_set(first(args), super::second(args), |mut p, v| {
        p.cdr = v.clone();
    })
}

#[allow(clippy::unnecessary_wraps, reason = "infallible intrinsic")]
fn is_list(args: &[Value], _env: &Frame) -> EvalResult {
    let arg = first(args);
    Ok(Value::Boolean(if let Value::Null = arg {
        true
    } else if let Some(p) = arg.as_refpair() {
        p.as_ref().is_list()
    } else {
        false
    }))
}

fn list_make(args: &[Value], _env: &Frame) -> EvalResult {
    coll_make(args, Value::Unspecified, val_identity, Value::list_mut)
}

fn list(args: &[Value], _env: &Frame) -> EvalResult {
    coll_new(args, |(_, v)| val_identity(v), Value::list_mut)
}

fn list_length(args: &[Value], _env: &Frame) -> EvalResult {
    let arg = first(args);
    if let Value::Null = arg {
        Ok(Value::real(0))
    } else if let Some(p) = arg.as_refpair() {
        p.len().map_or_else(
            |err| {
                Err(match err {
                    InvalidList::Cycle => Condition::circular_list(arg),
                    InvalidList::Improper => Condition::arg_type_error(
                        FIRST_ARG_LABEL,
                        TypeName::LIST,
                        TypeName::IMPLIST,
                        arg,
                    ),
                }
                .into())
            },
            |len| Ok(Value::Number(Number::from_usize(len))),
        )
    } else {
        Err(invalid_target(TypeName::LIST, arg))
    }
}

// TODO: circular lists => error?
fn list_append(args: &[Value], _env: &Frame) -> EvalResult {
    let mut acc = Vec::new();
    for arg in args.iter().take(args.len().max(1) - 1) {
        try_list_acc(arg, &mut acc)?;
    }
    let last = args.last().cloned().unwrap_or(Value::Null);
    Ok(if acc.is_empty() {
        last
    } else {
        acc.push(last);
        Value::list_cons_mut(acc)
    })
}

fn list_reverse(args: &[Value], _env: &Frame) -> EvalResult {
    let arg = first(args);
    arg.iter()
        .enumerate()
        .try_fold(Value::Null, |head, (i, item)| match item {
            ValItem::Cycle(_) => Err(Condition::circular_list(arg).into()),
            ValItem::Element(v) => {
                if let Value::Null = v {
                    Ok(head)
                } else if let Some(p) = v.as_refpair() {
                    Ok(Value::cons_mut(p.as_ref().car.clone(), head))
                } else {
                    Err(Condition::arg_error(i, TypeName::LIST, &v).into())
                }
            }
        })
}

fn list_tail(args: &[Value], _env: &Frame) -> EvalResult {
    let k = super::second(args);
    let ith = try_val_to_index(k, SECOND_ARG_LABEL)?;
    first(args)
        .iter()
        .enumerate()
        .find_map(|(i, item)| {
            let v = item.into_value();
            if i == ith {
                Some(Ok(v))
            } else if !matches!(v, Value::Null | Value::Pair(_) | Value::PairMut(_)) {
                Some(Err(v))
            } else {
                None
            }
        })
        .ok_or_else(|| Exception::from(Condition::index_error(k)))?
        .map_err(|e| invalid_target(TypeName::PAIR, &e))
}

fn list_get(args: &[Value], env: &Frame) -> EvalResult {
    let subl = list_tail(args, env)?;
    if let Value::Null = subl {
        Err(Condition::index_error(super::second(args)).into())
    } else {
        pcar(&subl)
    }
}

fn list_set(args: &[Value], env: &Frame) -> EvalResult {
    let p = list_tail(args, env)?;
    if let Value::Null = p {
        Err(Condition::index_error(super::second(args)).into())
    } else {
        pair_set(&p, super::third(args), |mut p, v| p.car = v.clone())
    }
}

fn member_eq(_args: &[Value], _env: &Frame) -> EvalResult {
    todo!();
}

fn member_eqv(_args: &[Value], _env: &Frame) -> EvalResult {
    todo!();
}

fn member_equal(_args: &[Value], _env: &Frame) -> EvalResult {
    todo!();
}

fn assoc_eq(_args: &[Value], _env: &Frame) -> EvalResult {
    todo!();
}

fn assoc_eqv(_args: &[Value], _env: &Frame) -> EvalResult {
    todo!();
}

fn assoc_equal(_args: &[Value], _env: &Frame) -> EvalResult {
    todo!();
}

// TODO: circular lists => error
fn list_copy(args: &[Value], _env: &Frame) -> EvalResult {
    let arg = first(args);
    let mut acc = Vec::new();
    list_cons_acc(arg, &mut acc);
    Ok(if acc.is_empty() {
        arg.clone()
    } else {
        Value::list_cons_mut(acc)
    })
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

    super::bind_intrinsic(env, "make-string", 1..2, string_make);
    super::bind_intrinsic(env, "string", 0..MAX_ARITY, string);

    super::bind_intrinsic(env, "string-length", 1..1, string_length);
    super::bind_intrinsic(env, "string-ref", 2..2, string_get);
    super::bind_intrinsic(env, "string-set!", 3..3, string_set);

    super::bind_intrinsic(env, "string=?", 0..MAX_ARITY, strings_eq);
    super::bind_intrinsic(env, "string<?", 0..MAX_ARITY, strings_lt);
    super::bind_intrinsic(env, "string>?", 0..MAX_ARITY, strings_gt);
    super::bind_intrinsic(env, "string<=?", 0..MAX_ARITY, strings_lte);
    super::bind_intrinsic(env, "string>=?", 0..MAX_ARITY, strings_gte);

    super::bind_intrinsic(env, "substring", 3..3, string_copy);
    super::bind_intrinsic(env, "string-append", 0..MAX_ARITY, string_append);

    super::bind_intrinsic(env, "string->list", 1..3, string_to_list);
    super::bind_intrinsic(env, "list->string", 1..1, string_from_list);

    super::bind_intrinsic(env, "string-copy", 1..3, string_copy);
    super::bind_intrinsic(env, "string-copy!", 3..5, string_copy_inline);
    super::bind_intrinsic(env, "string-fill!", 2..4, string_fill);
}

predicate!(is_string, Value::String(_) | Value::StringMut(_));

fn string_make(args: &[Value], _env: &Frame) -> EvalResult {
    coll_make(
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
        (Value::as_refstr, Value::as_mutrefstr),
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

fn strings_gt(args: &[Value], _env: &Frame) -> EvalResult {
    strs_predicate(args, str::gt)
}

fn strings_lte(args: &[Value], _env: &Frame) -> EvalResult {
    strs_predicate(args, str::le)
}

fn strings_gte(args: &[Value], _env: &Frame) -> EvalResult {
    strs_predicate(args, str::ge)
}

fn string_append(args: &[Value], _env: &Frame) -> EvalResult {
    coll_append(
        args,
        Value::as_refstr,
        TypeName::STRING,
        |strs: &[StrRef]| Value::strmut_from_chars(strs.iter().flat_map(|s| s.as_ref().chars())),
    )
}

fn string_to_list(args: &[Value], _env: &Frame) -> EvalResult {
    str_to_coll(first(args), args.get(1)..args.get(2), |chars| {
        Value::list_mut(chars.map(Value::Character).collect::<Vec<_>>())
    })
}

fn string_from_list(args: &[Value], env: &Frame) -> EvalResult {
    string(&try_list_to_vec(first(args))?, env)
}

fn string_copy(args: &[Value], _env: &Frame) -> EvalResult {
    coll_copy(
        first(args),
        args.get(1)..args.get(2),
        TypeName::STRING,
        Value::as_refstr,
        |s: &str, span| Value::strmut_from_chars(s.chars().skip(span.start).take(span.len())),
    )
}

fn string_copy_inline(args: &[Value], _env: &Frame) -> EvalResult {
    coll_copy_inline(
        first(args),
        super::second(args),
        super::third(args),
        args.get(3)..args.get(4),
        TypeName::STRING,
        (Value::as_refstr, Value::as_mutrefstr),
        |to, from, mut target, atidx, span| {
            let updated = if to.is(from) {
                build_str_copy(&target, &target, atidx, span)
            } else {
                build_str_copy(
                    from.as_refstr().expect("expected string argument").as_ref(),
                    &target,
                    atidx,
                    span,
                )
            };
            target.replace_range(.., &updated);
        },
    )
}

fn string_fill(args: &[Value], _env: &Frame) -> EvalResult {
    coll_fill(
        first(args),
        super::second(args),
        args.get(2)..args.get(3),
        TypeName::STRING,
        (Value::as_refstr, Value::as_mutrefstr),
        |v| try_val_to_char(v, SECOND_ARG_LABEL),
        |mut target, fill, span| {
            let updated = build_str_fill(iter::repeat_n(fill, span.len()), &target, span);
            target.replace_range(.., &updated);
        },
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

    super::bind_intrinsic(env, "make-vector", 1..2, vector_make);
    super::bind_intrinsic(env, "vector", 0..MAX_ARITY, vector);

    super::bind_intrinsic(env, "vector-length", 1..1, vector_length);
    super::bind_intrinsic(env, "vector-ref", 2..2, vector_get);
    super::bind_intrinsic(env, "vector-set!", 3..3, vector_set);

    super::bind_intrinsic(env, "vector->list", 1..3, vector_to_list);
    super::bind_intrinsic(env, "list->vector", 1..1, vector_from_list);
    super::bind_intrinsic(env, "vector->string", 1..3, vector_to_string);
    super::bind_intrinsic(env, "string->vector", 1..3, vector_from_string);

    super::bind_intrinsic(env, "vector-copy", 1..3, vector_copy);
    super::bind_intrinsic(env, "vector-copy!", 3..5, vector_copy_inline);
    super::bind_intrinsic(env, "vector-append", 0..MAX_ARITY, vector_append);
    super::bind_intrinsic(env, "vector-fill!", 2..4, vector_fill);
}

predicate!(is_vector, Value::Vector(_) | Value::VectorMut(_));

fn vector_make(args: &[Value], _env: &Frame) -> EvalResult {
    coll_make(args, Value::Unspecified, val_identity, Value::vector_mut)
}

fn vector(args: &[Value], _env: &Frame) -> EvalResult {
    coll_new(args, |(_, v)| val_identity(v), Value::vector_mut)
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
        (Value::as_refvec, Value::as_mutrefvec),
        val_identity,
        |mut v, idx, item| v[idx] = item,
    )
}

fn vector_to_list(args: &[Value], env: &Frame) -> EvalResult {
    vec_to_coll(first(args), args.get(1)..args.get(2), env, list)
}

fn vector_from_list(args: &[Value], env: &Frame) -> EvalResult {
    vector(&try_list_to_vec(first(args))?, env)
}

fn vector_to_string(args: &[Value], env: &Frame) -> EvalResult {
    vec_to_coll(first(args), args.get(1)..args.get(2), env, string)
}

fn vector_from_string(args: &[Value], _env: &Frame) -> EvalResult {
    str_to_coll(first(args), args.get(1)..args.get(2), |chars| {
        Value::vector_mut(chars.map(Value::Character))
    })
}

fn vector_copy(args: &[Value], _env: &Frame) -> EvalResult {
    coll_copy(
        first(args),
        args.get(1)..args.get(2),
        TypeName::VECTOR,
        Value::as_refvec,
        |vals: &[Value], span| Value::vector_mut(vals[span].iter().cloned()),
    )
}

fn vector_copy_inline(args: &[Value], _env: &Frame) -> EvalResult {
    coll_copy_inline(
        first(args),
        super::second(args),
        super::third(args),
        args.get(3)..args.get(4),
        TypeName::VECTOR,
        (Value::as_refvec, Value::as_mutrefvec),
        |to, from, mut target, atidx, span| {
            let range = atidx..(atidx + span.len());
            if to.is(from) {
                if range.start <= span.start {
                    reflexive_vector_copy(range.into_iter().zip(span), &mut target);
                } else {
                    reflexive_vector_copy(range.into_iter().zip(span).rev(), &mut target);
                }
            } else {
                let src = from.as_refvec().expect("expected vector argument");
                target[range].clone_from_slice(&src.as_ref()[span]);
            }
        },
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

fn vector_fill(args: &[Value], _env: &Frame) -> EvalResult {
    coll_fill(
        first(args),
        super::second(args),
        args.get(2)..args.get(3),
        TypeName::VECTOR,
        (Value::as_refvec, Value::as_mutrefvec),
        val_identity,
        |mut target, fill, span| {
            for i in span {
                target[i] = fill.clone();
            }
        },
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

fn pair_set(arg: &Value, val: &Value, set: impl FnOnce(RefMut<'_, Pair>, &Value)) -> EvalResult {
    match arg {
        Value::Pair(_) => Err(Condition::literal_mut_error(arg).into()),
        Value::PairMut(p) => {
            set(p.borrow_mut(), val);
            Ok(Value::Unspecified)
        }
        _ => Err(invalid_target(TypeName::PAIR, arg)),
    }
}

fn try_list_to_vec(val: &Value) -> Result<Vec<Value>, Exception> {
    let mut acc = Vec::new();
    try_list_acc(val, &mut acc)?;
    Ok(acc)
}

fn try_list_acc(curr: &Value, acc: &mut Vec<Value>) -> Result<(), Exception> {
    if let Value::Null = curr {
        Ok(())
    } else if let Some(p) = curr.as_refpair() {
        acc.push(p.as_ref().car.clone());
        try_list_acc(&p.as_ref().cdr, acc)
    } else {
        Err(Condition::arg_error(acc.len(), TypeName::LIST, curr).into())
    }
}

fn list_cons_acc(curr: &Value, acc: &mut Vec<Value>) {
    if let Some(p) = curr.as_refpair() {
        let pair = p.as_ref();
        if pair.cdr.as_refpair().is_some() {
            acc.push(pair.car.clone());
            return list_cons_acc(&pair.cdr, acc);
        }
    } else if acc.is_empty() && !matches!(curr, Value::Null) {
        // NOTE: curr was not a pair so don't accumulate anything
        return;
    }
    acc.push(curr.clone());
}

fn try_val_to_char(arg: &Value, lbl: impl Display) -> Result<char, Exception> {
    if let Value::Character(c) = arg {
        Ok(*c)
    } else {
        Err(Condition::arg_error(lbl, TypeName::CHAR, arg).into())
    }
}

fn strs_predicate(args: &[Value], pred: impl Fn(&str, &str) -> bool) -> EvalResult {
    args.iter()
        .enumerate()
        .try_fold((true, None), |(acc, prev), (idx, val)| {
            let b = val
                .as_refstr()
                .ok_or_else(|| Exception::from(Condition::arg_error(idx, TypeName::STRING, val)))?;
            let a = match prev {
                None => return Ok((acc, Some(val))),
                Some(v) => v.as_refstr().expect("expected string from prev"),
            };
            Ok((acc && pred(a.as_ref(), b.as_ref()), Some(val)))
        })
        .map(|(b, _)| Value::Boolean(b))
}

fn build_str_copy(from: &str, target: &str, at: usize, span: Range<usize>) -> String {
    build_str_fill(
        from.chars().skip(span.start).take(span.len()),
        target,
        at..(at + span.len()),
    )
}

fn build_str_fill(
    replace: impl IntoIterator<Item = char>,
    target: &str,
    skip: Range<usize>,
) -> String {
    let start = target.chars().take(skip.start);
    let rest = target.chars().skip(skip.end);
    start.chain(replace).chain(rest).collect::<String>()
}

fn str_to_coll(
    arg: &Value,
    span: Range<Option<&Value>>,
    ctor: impl FnOnce(Take<Skip<Chars<'_>>>) -> Value,
) -> EvalResult {
    let s = arg
        .as_refstr()
        .ok_or_else(|| invalid_target(TypeName::STRING, arg))?;
    let span = try_coll_span(span, s.len())?;
    Ok(ctor(s.as_ref().chars().skip(span.start).take(span.len())))
}

fn vec_to_coll(
    arg: &Value,
    span: Range<Option<&Value>>,
    env: &Frame,
    ctor: IntrinsicFn,
) -> EvalResult {
    let v = arg
        .as_refvec()
        .ok_or_else(|| invalid_target(TypeName::VECTOR, arg))?;
    let span = try_coll_span(span, v.len())?;
    ctor(&v.as_ref()[span], env)
}

fn reflexive_vector_copy(ranges: impl IntoIterator<Item = (usize, usize)>, v: &mut [Value]) {
    for (t, s) in ranges {
        v[t] = v[s].clone();
    }
}

#[allow(clippy::unnecessary_wraps, reason = "infallible interface")]
fn val_identity(v: &Value) -> EvalResult {
    Ok(v.clone())
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

fn coll_make<T: Clone>(
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
    collref: impl FnOnce(&Value) -> Option<CollRef<'_, T, M>>,
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
    op: impl FnOnce(&T, usize) -> Option<U>,
    map: impl FnOnce(U) -> Value,
) -> EvalResult {
    let c = collref(arg).ok_or_else(|| invalid_target(expected_type, arg))?;
    op(c.as_ref(), try_val_to_index(k, SECOND_ARG_LABEL)?).map_or_else(
        || Err(Condition::index_error(k).into()),
        |item| Ok(map(item)),
    )
}

fn coll_set<'a, T: ?Sized + 'a, M: AsRef<T> + 'a, U>(
    arg: &'a Value,
    k: &Value,
    val: &Value,
    expected_type: impl Display,
    collcnv: (
        impl FnOnce(&Value) -> Option<CollRef<'_, T, M>>,
        impl FnOnce(&Value) -> Option<RefMut<'_, M>>,
    ),
    try_item: impl FnOnce(&Value) -> Result<U, Exception>,
    op: impl FnOnce(RefMut<'_, M>, usize, U),
) -> EvalResult
where
    CollRef<'a, T, M>: CollSized,
{
    let clen = collcnv.0(arg)
        .ok_or_else(|| invalid_target(expected_type, arg))?
        .len();
    collcnv.1(arg).map_or_else(
        || Err(Condition::literal_mut_error(arg).into()),
        |c| {
            let idx = try_val_to_index(k, SECOND_ARG_LABEL)?;
            if idx < clen {
                let item = try_item(val)?;
                op(c, idx, item);
                Ok(Value::Unspecified)
            } else {
                Err(Condition::index_error(k).into())
            }
        },
    )
}

fn coll_copy<'a, T: ?Sized + 'a, M: AsRef<T> + 'a>(
    arg: &'a Value,
    span: Range<Option<&Value>>,
    expected_type: impl Display,
    collref: impl FnOnce(&Value) -> Option<CollRef<'_, T, M>>,
    op: impl FnOnce(&T, Range<usize>) -> Value,
) -> EvalResult
where
    CollRef<'a, T, M>: CollSized,
{
    let coll = collref(arg).ok_or_else(|| invalid_target(expected_type, arg))?;
    Ok(op(coll.as_ref(), try_coll_span(span, coll.len())?))
}

fn coll_copy_inline<'a, T: ?Sized + 'a, M: AsRef<T> + 'a>(
    to: &'a Value,
    at: &Value,
    from: &'a Value,
    span: Range<Option<&Value>>,
    expected_type: impl Display + Clone,
    collcnv: (
        impl Fn(&Value) -> Option<CollRef<'_, T, M>>,
        impl FnOnce(&Value) -> Option<RefMut<'_, M>>,
    ),
    op: impl FnOnce(&Value, &Value, RefMut<'_, M>, usize, Range<usize>),
) -> EvalResult
where
    CollRef<'a, T, M>: CollSized,
{
    let tolen = collcnv.0(to)
        .ok_or_else(|| invalid_target(expected_type.clone(), to))?
        .len();
    let atidx = try_val_to_index(at, SECOND_ARG_LABEL)?;
    if tolen < atidx {
        return Err(Condition::value_error("target index out of range", at).into());
    }
    let source = collcnv.0(from).ok_or_else(|| {
        Exception::from(Condition::arg_error(THIRD_ARG_LABEL, expected_type, from))
    })?;
    let span = try_coll_span(span, source.len())?;
    if span.is_empty() {
        return Ok(Value::Unspecified);
    }
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
    mem::drop(source);
    collcnv.1(to).map_or_else(
        || Err(Condition::literal_mut_error(to).into()),
        |target| {
            op(to, from, target, atidx, span);
            Ok(Value::Unspecified)
        },
    )
}

fn coll_append<T: ?Sized, M: AsRef<T>>(
    args: &[Value],
    collref: impl Fn(&Value) -> Option<CollRef<'_, T, M>>,
    expected_type: impl Display + Clone,
    op: impl FnOnce(&[CollRef<'_, T, M>]) -> Value,
) -> EvalResult {
    let coll_refs = args
        .iter()
        .enumerate()
        .map(|(idx, v)| {
            collref(v)
                .ok_or_else(|| Exception::from(Condition::arg_error(idx, expected_type.clone(), v)))
        })
        .collect::<Result<Vec<_>, _>>()?;
    Ok(op(&coll_refs))
}

fn coll_fill<'a, T: ?Sized + 'a, M: AsRef<T> + 'a, U>(
    arg: &'a Value,
    fill: &Value,
    span: Range<Option<&Value>>,
    expected_type: impl Display,
    collcnv: (
        impl FnOnce(&Value) -> Option<CollRef<'_, T, M>>,
        impl FnOnce(&Value) -> Option<RefMut<'_, M>>,
    ),
    try_item: impl FnOnce(&Value) -> Result<U, Exception>,
    op: impl FnOnce(RefMut<'_, M>, U, Range<usize>),
) -> EvalResult
where
    CollRef<'a, T, M>: CollSized,
{
    let clen = collcnv.0(arg)
        .ok_or_else(|| invalid_target(expected_type, arg))?
        .len();
    let fill = try_item(fill)?;
    let span = try_coll_span(span, clen)?;
    if span.is_empty() {
        return Ok(Value::Unspecified);
    }
    collcnv.1(arg).map_or_else(
        || Err(Condition::literal_mut_error(arg).into()),
        |target| {
            op(target, fill, span);
            Ok(Value::Unspecified)
        },
    )
}

fn try_coll_span(span: Range<Option<&Value>>, clen: usize) -> Result<Range<usize>, Exception> {
    let (start, end) = (span.start, span.end);
    let sidx = start.map_or(Ok(usize::MIN), |v| try_val_to_index(v, SECOND_ARG_LABEL))?;
    let eidx = end.map_or(Ok(clen), |v| try_val_to_index(v, THIRD_ARG_LABEL))?;
    if clen < eidx {
        Err(Condition::index_error(end.unwrap()).into())
    } else if eidx < sidx {
        Err(if let Some(v) = end {
            Condition::value_error(
                "start greater than end",
                &Value::cons(start.unwrap().clone(), v.clone()),
            )
            .into()
        } else {
            Condition::index_error(start.unwrap()).into()
        })
    } else {
        Ok(sidx..eidx)
    }
}
