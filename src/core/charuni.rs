// (scheme char)
use super::FIRST_ARG_LABEL;
use crate::{
    eval::{Binding, EvalResult, Frame},
    value::{Condition, TypeName, Value},
};

pub(super) fn load(scope: &mut Binding) {
    super::bind_intrinsic(scope, "char-alphabetic?", 1..1, is_alphabetic);
    super::bind_intrinsic(scope, "char-numeric?", 1..1, is_numeric);
    super::bind_intrinsic(scope, "char-whitespace?", 1..1, is_whitespace);
    super::bind_intrinsic(scope, "char-upper-case?", 1..1, is_uppercase);
    super::bind_intrinsic(scope, "char-lower-case?", 1..1, is_lowercase);

    super::bind_intrinsic(scope, "char-upcase", 1..1, char_upper);
    super::bind_intrinsic(scope, "char-downcase", 1..1, char_lower);

    super::bind_intrinsic(scope, "string-upcase", 1..1, string_upper);
    super::bind_intrinsic(scope, "string-downcase", 1..1, string_lower);
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

fn char_upper(args: &[Value], _env: &mut Frame) -> EvalResult {
    char_case(args.first().unwrap(), char::to_uppercase)
}

fn char_lower(args: &[Value], _env: &mut Frame) -> EvalResult {
    char_case(args.first().unwrap(), char::to_lowercase)
}

fn string_upper(args: &[Value], _env: &mut Frame) -> EvalResult {
    string_case(args.first().unwrap(), str::to_uppercase)
}

fn string_lower(args: &[Value], _env: &mut Frame) -> EvalResult {
    string_case(args.first().unwrap(), str::to_lowercase)
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
        invalid_target!(TypeName::CHAR, arg)
    }
}

fn string_case(arg: &Value, _case: impl FnOnce(&str) -> String) -> EvalResult {
    if let Value::String(_s) = arg {
        todo!("need to return a mutable string")
    } else {
        invalid_target!(TypeName::STRING, arg)
    }
}
