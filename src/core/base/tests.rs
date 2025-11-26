use super::*;
use crate::{
    Exception,
    testutil::{TestEnv, err_or_fail, extract_or_fail, ok_or_fail},
};
use std::rc::Rc;

#[test]
fn all_boolean_empty() {
    let args = [];
    let env = TestEnv::default();

    let r = booleans_eq(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert!(matches!(v, Value::Boolean(true)));
}

#[test]
fn all_boolean_single() {
    let cases = [[Value::Boolean(true)], [Value::Boolean(false)]];
    for case in cases {
        let env = TestEnv::default();

        let r = booleans_eq(&case, &env.new_frame());

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
    let env = TestEnv::default();

    let r = booleans_eq(&args, &env.new_frame());

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
    let env = TestEnv::default();

    let r = booleans_eq(&args, &env.new_frame());

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
    let env = TestEnv::default();

    let r = booleans_eq(&args, &env.new_frame());

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
    let env = TestEnv::default();

    let r = booleans_eq(&args, &env.new_frame());

    let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
    assert_eq!(
        err.to_string(),
        "#<value-error \"invalid type for arg `1` - expected: boolean, got: string\" (\"foo\")>"
    );
}

#[test]
fn all_boolean_bails_on_first_invalid_param() {
    let args = [
        Value::Boolean(false),
        Value::string("foo"),
        Value::Boolean(false),
        Value::Null,
    ];
    let env = TestEnv::default();

    let r = booleans_eq(&args, &env.new_frame());

    let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
    assert_eq!(
        err.to_string(),
        "#<value-error \"invalid type for arg `1` - expected: boolean, got: string\" (\"foo\")>"
    );
}

#[test]
fn all_symbols_empty() {
    let args = [];
    let env = TestEnv::default();

    let r = symbols_eq(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert!(matches!(v, Value::Boolean(true)));
}

#[test]
fn all_symbols_single() {
    let env = TestEnv::default();
    let args = [Value::Symbol(env.symbols.get("a"))];

    let r = symbols_eq(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert!(matches!(v, Value::Boolean(true)));
}

#[test]
fn all_symbols_equal() {
    let env = TestEnv::default();
    let name = env.symbols.get("a");
    let args = [
        Value::Symbol(name.clone()),
        Value::Symbol(name.clone()),
        Value::Symbol(name.clone()),
    ];

    let r = symbols_eq(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert!(matches!(v, Value::Boolean(true)));
}

#[test]
fn all_symbols_mixed() {
    let env = TestEnv::default();
    let (a, b) = (env.symbols.get("a"), env.symbols.get("b"));
    let args = [
        Value::Symbol(a.clone()),
        Value::Symbol(b.clone()),
        Value::Symbol(a.clone()),
    ];

    let r = symbols_eq(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert!(matches!(v, Value::Boolean(false)));
}

#[test]
fn all_symbols_interned_even_if_from_distinct_pointers() {
    let env = TestEnv::default();
    let args = [
        Value::Symbol(env.symbols.get(Rc::new("a").as_ref())),
        Value::Symbol(env.symbols.get(Rc::new("a").as_ref())),
        Value::Symbol(env.symbols.get(Rc::new("a").as_ref())),
    ];

    let r = symbols_eq(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert!(matches!(v, Value::Boolean(true)));
}

#[test]
fn all_symbols_invalid_param() {
    let env = TestEnv::default();
    let name = env.symbols.get("a");
    let args = [
        Value::Symbol(name.clone()),
        Value::string(name.as_rc()),
        Value::Symbol(name.clone()),
    ];

    let r = symbols_eq(&args, &env.new_frame());

    let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
    assert_eq!(
        err.to_string(),
        "#<value-error \"invalid type for arg `1` - expected: symbol, got: string\" (\"a\")>"
    );
}

#[test]
fn all_chars_empty() {
    let args = [];
    let env = TestEnv::default();

    let r = chars_eq(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert!(matches!(v, Value::Boolean(true)));
}

#[test]
fn all_chars_single() {
    let args = [Value::Character('a')];
    let env = TestEnv::default();

    let r = chars_eq(&args, &env.new_frame());

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
    let env = TestEnv::default();

    let r = chars_eq(&args, &env.new_frame());

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
    let env = TestEnv::default();

    let r = chars_eq(&args, &env.new_frame());

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
    let env = TestEnv::default();

    let r = chars_eq(&args, &env.new_frame());

    let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
    assert_eq!(
        err.to_string(),
        "#<value-error \"invalid type for arg `1` - expected: character, got: string\" (\"a\")>"
    );
}

#[test]
fn all_chars_lt() {
    let args = [
        Value::Character('a'),
        Value::Character('b'),
        Value::Character('c'),
    ];
    let env = TestEnv::default();

    let r = chars_lt(&args, &env.new_frame());

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
    let env = TestEnv::default();

    let r = chars_lt(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert!(matches!(v, Value::Boolean(false)));
}

#[test]
fn int_to_char() {
    let args = [Value::real(0x41)];
    let env = TestEnv::default();

    let r = char_from_integer(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert!(matches!(v, Value::Character('A')));
}

#[test]
fn int_to_char_invalid_arg() {
    let env = TestEnv::default();
    let args = [Value::Symbol(env.symbols.get("a"))];

    let r = char_from_integer(&args, &env.new_frame());

    let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
    assert_eq!(
        err.to_string(),
        "#<value-error \"invalid type for arg `0` - expected: integer, got: symbol\" (a)>"
    );
}

#[test]
fn int_to_char_invalid_range() {
    let args = [Value::real(-4)];
    let env = TestEnv::default();

    let r = char_from_integer(&args, &env.new_frame());

    let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
    assert_eq!(
        err.to_string(),
        "#<value-error \"unicode code point out of ranges [#x0, #xd7ff], [#xe000, #x10ffff] ([0, 55295], [57344, 1114111])\" (-4)>"
    );
}

#[test]
fn int_to_char_not_a_code_point() {
    let args = [Value::real(0xdff0)];
    let env = TestEnv::default();

    let r = char_from_integer(&args, &env.new_frame());

    let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
    assert_eq!(
        err.to_string(),
        "#<value-error \"unicode code point out of ranges [#x0, #xd7ff], [#xe000, #x10ffff] ([0, 55295], [57344, 1114111])\" (57328)>"
    );
}
