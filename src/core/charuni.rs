// (scheme char)
use super::{first, invalid_target};
use crate::{
    eval::{EvalResult, Frame, MAX_ARITY},
    value::{TypeName, Value},
};

pub(super) fn load(env: &Frame) {
    super::bind_intrinsic(env, "char-ci=?", 0..MAX_ARITY, chars_cf_eq);
    super::bind_intrinsic(env, "char-ci<?", 0..MAX_ARITY, chars_cf_lt);
    super::bind_intrinsic(env, "char-ci<=?", 0..MAX_ARITY, chars_cf_lte);
    super::bind_intrinsic(env, "char-ci>?", 0..MAX_ARITY, chars_cf_gt);
    super::bind_intrinsic(env, "char-ci>=?", 0..MAX_ARITY, chars_cf_gte);

    super::bind_intrinsic(env, "char-alphabetic?", 1..1, is_alphabetic);
    super::bind_intrinsic(env, "char-numeric?", 1..1, is_numeric);
    super::bind_intrinsic(env, "char-whitespace?", 1..1, is_whitespace);
    super::bind_intrinsic(env, "char-upper-case?", 1..1, is_uppercase);
    super::bind_intrinsic(env, "char-lower-case?", 1..1, is_lowercase);

    super::bind_intrinsic(env, "digit-value", 1..1, to_digit);

    super::bind_intrinsic(env, "char-upcase", 1..1, char_upper);
    super::bind_intrinsic(env, "char-downcase", 1..1, char_lower);
    super::bind_intrinsic(env, "char-foldcase", 1..1, char_fold);

    super::bind_intrinsic(env, "string-ci=?", 0..MAX_ARITY, strings_cf_eq);
    super::bind_intrinsic(env, "string-ci<?", 0..MAX_ARITY, strings_cf_lt);
    super::bind_intrinsic(env, "string-ci<=?", 0..MAX_ARITY, strings_cf_lte);
    super::bind_intrinsic(env, "string-ci>?", 0..MAX_ARITY, strings_cf_gt);
    super::bind_intrinsic(env, "string-ci>=?", 0..MAX_ARITY, strings_cf_gte);

    super::bind_intrinsic(env, "string-upcase", 1..1, string_upper);
    super::bind_intrinsic(env, "string-downcase", 1..1, string_lower);
    super::bind_intrinsic(env, "string-foldcase", 1..1, string_fold);
}

try_predicate!(
    is_alphabetic,
    Value::Character,
    TypeName::CHAR,
    |c: &char| c.is_alphabetic()
);
try_predicate!(
    is_numeric,
    Value::Character,
    TypeName::CHAR,
    |_c: &char| todo!("is_numeric is too broad, need only Nd")
);
try_predicate!(
    is_whitespace,
    Value::Character,
    TypeName::CHAR,
    |c: &char| c.is_whitespace()
);
try_predicate!(
    is_uppercase,
    Value::Character,
    TypeName::CHAR,
    |c: &char| c.is_uppercase()
);
try_predicate!(
    is_lowercase,
    Value::Character,
    TypeName::CHAR,
    |c: &char| c.is_lowercase()
);

fn chars_cf_eq(_args: &[Value], _env: &Frame) -> EvalResult {
    todo!("need char_fold");
}

fn chars_cf_lt(_args: &[Value], _env: &Frame) -> EvalResult {
    todo!("need char_fold");
}

fn chars_cf_lte(_args: &[Value], _env: &Frame) -> EvalResult {
    todo!("need char_fold");
}

fn chars_cf_gt(_args: &[Value], _env: &Frame) -> EvalResult {
    todo!("need char_fold");
}

fn chars_cf_gte(_args: &[Value], _env: &Frame) -> EvalResult {
    todo!("need char_fold");
}

fn to_digit(_args: &[Value], _env: &Frame) -> EvalResult {
    todo!("need is_numeric");
}

fn char_upper(args: &[Value], _env: &Frame) -> EvalResult {
    char_case(first(args), char::to_uppercase)
}

fn char_lower(args: &[Value], _env: &Frame) -> EvalResult {
    char_case(first(args), char::to_lowercase)
}

fn char_fold(_args: &[Value], _env: &Frame) -> EvalResult {
    todo!("implement unicode case-folding");
}

fn strings_cf_eq(_args: &[Value], _env: &Frame) -> EvalResult {
    todo!("need string_fold");
}

fn strings_cf_lt(_args: &[Value], _env: &Frame) -> EvalResult {
    todo!("need string_fold");
}

fn strings_cf_lte(_args: &[Value], _env: &Frame) -> EvalResult {
    todo!("need string_fold");
}

fn strings_cf_gt(_args: &[Value], _env: &Frame) -> EvalResult {
    todo!("need string_fold");
}

fn strings_cf_gte(_args: &[Value], _env: &Frame) -> EvalResult {
    todo!("need string_fold");
}

fn string_upper(args: &[Value], _env: &Frame) -> EvalResult {
    string_case(first(args), str::to_uppercase)
}

fn string_lower(args: &[Value], _env: &Frame) -> EvalResult {
    string_case(first(args), str::to_lowercase)
}

fn string_fold(_args: &[Value], _env: &Frame) -> EvalResult {
    todo!("implement unicode case-folding");
}

fn char_case<I: ExactSizeIterator<Item = char>>(
    arg: &Value,
    case: impl FnOnce(char) -> I,
) -> EvalResult {
    if let Value::Character(c) = arg {
        let mut it = case(*c);
        Ok(if it.len() == 1 {
            Value::Character(it.next().unwrap())
        } else {
            arg.clone()
        })
    } else {
        Err(invalid_target(TypeName::CHAR, arg))
    }
}

fn string_case(arg: &Value, case: impl FnOnce(&str) -> String) -> EvalResult {
    let s = arg
        .as_refstr()
        .ok_or_else(|| invalid_target(TypeName::STRING, arg))?;
    Ok(Value::string_mut(case(s.as_ref())))
}
