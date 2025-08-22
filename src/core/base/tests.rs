use super::*;
use crate::{
    testutil::{TestEnv, err_or_fail, extract_or_fail, ok_or_fail, some_or_fail},
    value::zlist,
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
    let env = TestEnv::default();

    let r = booleans_eq(&args, &env.new_frame());

    let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
    assert_eq!(
        err.to_string(),
        "#<env-error \"invalid type for arg `1` - expected: boolean, got: string\" (\"foo\")>"
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
        "#<env-error \"invalid type for arg `1` - expected: symbol, got: string\" (\"a\")>"
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
fn all_strings_empty() {
    let args = [];
    let env = TestEnv::default();

    let r = strings_eq(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert!(matches!(v, Value::Boolean(true)));
}

#[test]
fn all_strings_single() {
    let args = [Value::string("foo")];
    let env = TestEnv::default();

    let r = strings_eq(&args, &env.new_frame());

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
    let env = TestEnv::default();

    let r = strings_eq(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert!(matches!(v, Value::Boolean(true)));
}

#[test]
fn all_strings_equal_with_mutable() {
    let args = [
        Value::string("foo"),
        Value::string_mut("foo"),
        Value::string("foo"),
    ];
    let env = TestEnv::default();

    let r = strings_eq(&args, &env.new_frame());

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
    let env = TestEnv::default();

    let r = strings_eq(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert!(matches!(v, Value::Boolean(false)));
}

#[test]
fn all_strings_invalid_param() {
    let env = TestEnv::default();
    let args = [
        Value::string("foo"),
        Value::Symbol(env.symbols.get("foo")),
        Value::string("foo"),
    ];

    let r = strings_eq(&args, &env.new_frame());

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
    let env = TestEnv::default();

    let r = strings_lt(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert!(matches!(v, Value::Boolean(true)));
}

#[test]
fn all_strings_lt_with_mutable() {
    let args = [
        Value::string("abc"),
        Value::string_mut("def"),
        Value::string("ghi"),
    ];
    let env = TestEnv::default();

    let r = strings_lt(&args, &env.new_frame());

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
    let env = TestEnv::default();

    let r = strings_lt(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert!(matches!(v, Value::Boolean(false)));
}

#[test]
fn string_len() {
    let args = [Value::string("abc")];
    let env = TestEnv::default();

    let r = string_length(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v.to_string(), "3");
}

#[test]
fn string_len_unicode() {
    let args = [Value::string("a🦀c")];
    let env = TestEnv::default();

    let r = string_length(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v.to_string(), "3");
}

#[test]
fn string_mutable_predicate() {
    let args = [Value::string_mut("abc")];
    let env = TestEnv::default();

    let r = is_string(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert!(matches!(v, Value::Boolean(true)));
}

#[test]
fn string_mutable_length() {
    let args = [Value::string_mut("abc")];
    let env = TestEnv::default();

    let r = string_length(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v.to_string(), "3");
}

#[test]
fn string_mutable_get_ref() {
    let args = [Value::string_mut("abc"), Value::Number(Number::real(1))];
    let env = TestEnv::default();

    let r = string_get(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert!(matches!(v, Value::Character('b')));
}

#[test]
fn string_set_val() {
    let args = [
        Value::string_mut("abc"),
        Value::Number(Number::real(1)),
        Value::Character('X'),
    ];
    let env = TestEnv::default();

    let r = string_set(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert!(matches!(v, Value::Unspecified));
    assert_eq!(args[0].to_string(), "\"aXc\"");
}

#[test]
fn string_set_val_unicode() {
    let args = [
        Value::string_mut("a🦀c"),
        Value::Number(Number::real(1)),
        Value::Character('X'),
    ];
    let env = TestEnv::default();

    let r = string_set(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert!(matches!(v, Value::Unspecified));
    assert_eq!(args[0].to_string(), "\"aXc\"");
}

#[test]
fn string_set_val_after_unicode() {
    let args = [
        Value::string_mut("a🦀c"),
        Value::Number(Number::real(2)),
        Value::Character('X'),
    ];
    let env = TestEnv::default();

    let r = string_set(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert!(matches!(v, Value::Unspecified));
    assert_eq!(args[0].to_string(), "\"a🦀X\"");
}

#[test]
fn string_set_val_unicode_out_of_range() {
    let args = [
        Value::string_mut("a🦀c"),
        Value::Number(Number::real(5)),
        Value::Character('X'),
    ];
    let env = TestEnv::default();

    let r = string_set(&args, &env.new_frame());

    // NOTE: verify this generates a scheme condition signal rather than a rust panic
    let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
    assert_eq!(err.to_string(), "#<env-error \"index out of range\" (5)>");
}

#[test]
fn string_copy_unicode_out_of_range() {
    let args = [
        Value::string("a🦀c"),
        Value::Number(Number::real(0)),
        Value::Number(Number::real(5)),
    ];
    let env = TestEnv::default();

    let r = string_copy(&args, &env.new_frame());

    // NOTE: verify this generates a scheme condition signal rather than
    // running off the end of an iterator.
    let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
    assert_eq!(err.to_string(), "#<env-error \"index out of range\" (5)>");
}

#[test]
fn string_to_bytes() {
    let args = [Value::string("abc")];
    let env = TestEnv::default();

    let r = bytevector_from_str(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v.to_string(), "#u8(97 98 99)");
}

#[test]
fn unicode_to_bytes() {
    let args = [Value::string("a🦀c")];
    let env = TestEnv::default();

    let r = bytevector_from_str(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v.to_string(), "#u8(97 240 159 166 128 99)");
}

#[test]
fn unicode_to_bytes_fenceposts() {
    let args = [
        Value::string("a🦀c"),
        Value::Number(Number::real(1)),
        Value::Number(Number::real(2)),
    ];
    let env = TestEnv::default();

    let r = bytevector_from_str(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v.to_string(), "#u8(240 159 166 128)");
}

#[test]
fn is_even_integer() {
    let args = [Value::Number(Number::real(4))];
    let env = TestEnv::default();

    let r = is_even(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert!(matches!(v, Value::Boolean(true)));

    let r = is_odd(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert!(matches!(v, Value::Boolean(false)));
}

#[test]
fn is_even_float_with_no_frac() {
    let args = [Value::Number(Number::real(4.0))];
    let env = TestEnv::default();

    let r = is_even(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert!(matches!(v, Value::Boolean(true)));

    let r = is_odd(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert!(matches!(v, Value::Boolean(false)));
}

#[test]
fn is_even_float_with_frac() {
    let args = [Value::Number(Number::real(4.2))];
    let env = TestEnv::default();

    let r = is_even(&args, &env.new_frame());

    let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
    assert_eq!(
        err.to_string(),
        "#<env-error \"expected exact integer, got: 4.2\" (4.2)>"
    );

    let r = is_odd(&args, &env.new_frame());

    let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
    assert_eq!(
        err.to_string(),
        "#<env-error \"expected exact integer, got: 4.2\" (4.2)>"
    );
}

#[test]
fn is_odd_integer() {
    let args = [Value::Number(Number::real(3))];
    let env = TestEnv::default();

    let r = is_even(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert!(matches!(v, Value::Boolean(false)));

    let r = is_odd(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert!(matches!(v, Value::Boolean(true)));
}

#[test]
fn is_odd_float_with_no_frac() {
    let args = [Value::Number(Number::real(3.0))];
    let env = TestEnv::default();

    let r = is_even(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert!(matches!(v, Value::Boolean(false)));

    let r = is_odd(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert!(matches!(v, Value::Boolean(true)));
}

#[test]
fn is_odd_float_with_frac() {
    let args = [Value::Number(Number::real(3.2))];
    let env = TestEnv::default();

    let r = is_even(&args, &env.new_frame());

    let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
    assert_eq!(
        err.to_string(),
        "#<env-error \"expected exact integer, got: 3.2\" (3.2)>"
    );

    let r = is_odd(&args, &env.new_frame());

    let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
    assert_eq!(
        err.to_string(),
        "#<env-error \"expected exact integer, got: 3.2\" (3.2)>"
    );
}

#[test]
fn list_tail_normal_list() {
    let env = TestEnv::default();
    let args = [
        zlist![
            Value::Symbol(env.symbols.get("a")),
            Value::Symbol(env.symbols.get("b")),
            Value::Symbol(env.symbols.get("c"))
        ],
        Value::Number(Number::real(1)),
    ];

    let r = list_tail(&args, &env.new_frame());

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
    let env = TestEnv::default();

    let r = list_tail(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert!(matches!(v, Value::Pair(None)));
}

#[test]
fn list_tail_index_to_empty_list() {
    let env = TestEnv::default();
    let args = [
        zlist![
            Value::Symbol(env.symbols.get("a")),
            Value::Symbol(env.symbols.get("b")),
            Value::Symbol(env.symbols.get("c"))
        ],
        Value::Number(Number::real(3)),
    ];

    let r = list_tail(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert!(matches!(v, Value::Pair(None)));
}

#[test]
fn list_tail_non_list() {
    let env = TestEnv::default();
    let args = [
        Value::Symbol(env.symbols.get("a")),
        Value::Number(Number::real(0)),
    ];

    let r = list_tail(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert!(matches!(v, Value::Symbol(s) if s.as_ref() == "a"));
}

#[test]
fn list_tail_end_of_improper_list() {
    let env = TestEnv::default();
    let args = [
        Value::cons(
            Value::Symbol(env.symbols.get("a")),
            Value::cons(
                Value::Symbol(env.symbols.get("b")),
                Value::Symbol(env.symbols.get("c")),
            ),
        ),
        Value::Number(Number::real(2)),
    ];

    let r = list_tail(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert!(matches!(v, Value::Symbol(s) if s.as_ref() == "c"));
}

#[test]
fn list_tail_index_out_of_range() {
    let env = TestEnv::default();
    let args = [
        zlist![
            Value::Symbol(env.symbols.get("a")),
            Value::Symbol(env.symbols.get("b")),
            Value::Symbol(env.symbols.get("c"))
        ],
        Value::Number(Number::real(4)),
    ];

    let r = list_tail(&args, &env.new_frame());

    let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
    assert_eq!(err.to_string(), "#<env-error \"index out of range\" (4)>");
}

#[test]
fn list_tail_non_list_out_of_range() {
    let env = TestEnv::default();
    let args = [
        Value::Symbol(env.symbols.get("a")),
        Value::Number(Number::real(1)),
    ];

    let r = list_tail(&args, &env.new_frame());

    let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
    assert_eq!(
        err.to_string(),
        "#<env-error \"invalid type for arg `0` - expected: pair, got: symbol\" (a)>"
    );
}

#[test]
fn list_tail_improper_list_out_of_range() {
    let env = TestEnv::default();
    let args = [
        Value::cons(
            Value::Symbol(env.symbols.get("a")),
            Value::cons(
                Value::Symbol(env.symbols.get("b")),
                Value::Symbol(env.symbols.get("c")),
            ),
        ),
        Value::Number(Number::real(3)),
    ];

    let r = list_tail(&args, &env.new_frame());

    let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
    assert_eq!(
        err.to_string(),
        "#<env-error \"invalid type for arg `0` - expected: pair, got: symbol\" (c)>"
    );
}

#[test]
fn list_tail_wrong_index_type() {
    let args = [Value::null(), Value::string("foo")];
    let env = TestEnv::default();

    let r = list_tail(&args, &env.new_frame());

    let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
    assert_eq!(
        err.to_string(),
        "#<env-error \"invalid type for arg `1` - expected: integer, got: string\" (\"foo\")>"
    );
}

#[test]
fn list_tail_invalid_index_type() {
    let args = [Value::null(), Value::Number(Number::real(4.2))];
    let env = TestEnv::default();

    let r = list_tail(&args, &env.new_frame());

    let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
    assert_eq!(
        err.to_string(),
        "#<env-error \"invalid type for arg `1` - expected: integer, got: floating-point\" (4.2)>"
    );
}

#[test]
fn list_tail_index_invalid_range() {
    let args = [Value::null(), Value::Number(Number::real(-4))];
    let env = TestEnv::default();

    let r = list_tail(&args, &env.new_frame());

    let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
    assert_eq!(
        err.to_string(),
        "#<env-error \"integer literal out of range: [0, 18446744073709551615]\" (-4)>"
    );
}

#[test]
fn list_ref_normal_list() {
    let env = TestEnv::default();
    let args = [
        zlist![
            Value::Symbol(env.symbols.get("a")),
            Value::Symbol(env.symbols.get("b")),
            Value::Symbol(env.symbols.get("c"))
        ],
        Value::Number(Number::real(1)),
    ];

    let r = list_get(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert!(matches!(v, Value::Symbol(s) if s.as_ref() == "b"));
}

#[test]
fn list_ref_empty_list() {
    let args = [Value::null(), Value::Number(Number::real(0))];
    let env = TestEnv::default();

    let r = list_get(&args, &env.new_frame());

    let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
    assert_eq!(err.to_string(), "#<env-error \"index out of range\" (0)>");
}

#[test]
fn list_ref_non_list() {
    let env = TestEnv::default();
    let args = [
        Value::Symbol(env.symbols.get("a")),
        Value::Number(Number::real(0)),
    ];

    let r = list_get(&args, &env.new_frame());

    let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
    assert_eq!(
        err.to_string(),
        "#<env-error \"invalid type for arg `0` - expected: pair, got: symbol\" (a)>"
    );
}

#[test]
fn list_ref_improper_list_item() {
    let env = TestEnv::default();
    let args = [
        Value::cons(
            Value::Symbol(env.symbols.get("a")),
            Value::cons(
                Value::Symbol(env.symbols.get("b")),
                Value::Symbol(env.symbols.get("c")),
            ),
        ),
        Value::Number(Number::real(1)),
    ];

    let r = list_get(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert!(matches!(v, Value::Symbol(s) if s.as_ref() == "b"));
}

#[test]
fn list_ref_end_of_improper_list() {
    let env = TestEnv::default();
    let args = [
        Value::cons(
            Value::Symbol(env.symbols.get("a")),
            Value::cons(
                Value::Symbol(env.symbols.get("b")),
                Value::Symbol(env.symbols.get("c")),
            ),
        ),
        Value::Number(Number::real(2)),
    ];

    let r = list_get(&args, &env.new_frame());

    let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
    assert_eq!(
        err.to_string(),
        "#<env-error \"invalid type for arg `0` - expected: pair, got: symbol\" (c)>"
    );
}

#[test]
fn list_ref_index_out_of_range() {
    let env = TestEnv::default();
    let args = [
        zlist![
            Value::Symbol(env.symbols.get("a")),
            Value::Symbol(env.symbols.get("b")),
            Value::Symbol(env.symbols.get("c"))
        ],
        Value::Number(Number::real(4)),
    ];

    let r = list_get(&args, &env.new_frame());

    let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
    assert_eq!(err.to_string(), "#<env-error \"index out of range\" (4)>");
}

#[test]
fn list_ref_improper_list_out_of_range() {
    let env = TestEnv::default();
    let args = [
        Value::cons(
            Value::Symbol(env.symbols.get("a")),
            Value::cons(
                Value::Symbol(env.symbols.get("b")),
                Value::Symbol(env.symbols.get("c")),
            ),
        ),
        Value::Number(Number::real(3)),
    ];

    let r = list_get(&args, &env.new_frame());

    let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
    assert_eq!(
        err.to_string(),
        "#<env-error \"invalid type for arg `0` - expected: pair, got: symbol\" (c)>"
    );
}

#[test]
fn int_to_char() {
    let args = [Value::Number(Number::real(0x41))];
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
        "#<env-error \"invalid type for arg `0` - expected: integer, got: symbol\" (a)>"
    );
}

#[test]
fn int_to_char_invalid_range() {
    let args = [Value::Number(Number::real(-4))];
    let env = TestEnv::default();

    let r = char_from_integer(&args, &env.new_frame());

    let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
    assert_eq!(
        err.to_string(),
        "#<env-error \"unicode code point out of ranges [#x0, #xD7FF], [#xE000, #x10FFFF] ([0, 55295], [57344, 1114111])\" (-4)>"
    );
}

#[test]
fn int_to_char_not_a_code_point() {
    let args = [Value::Number(Number::real(0xdff0))];
    let env = TestEnv::default();

    let r = char_from_integer(&args, &env.new_frame());

    let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
    assert_eq!(
        err.to_string(),
        "#<env-error \"unicode code point out of ranges [#x0, #xD7FF], [#xE000, #x10FFFF] ([0, 55295], [57344, 1114111])\" (57328)>"
    );
}

#[test]
fn vector_mutable_predicate() {
    let args = [Value::vector_mut([
        Value::Character('A'),
        Value::Character('B'),
        Value::Character('C'),
    ])];
    let env = TestEnv::default();

    let r = is_vector(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert!(matches!(v, Value::Boolean(true)));
}

#[test]
fn vector_mutable_length() {
    let args = [
        Value::vector_mut([
            Value::Character('A'),
            Value::Character('B'),
            Value::Character('C'),
        ]),
        Value::Number(Number::real(1)),
    ];
    let env = TestEnv::default();

    let r = vector_length(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v.to_string(), "3");
}

#[test]
fn vector_mutable_get_ref() {
    let args = [
        Value::vector_mut([
            Value::Character('A'),
            Value::Character('B'),
            Value::Character('C'),
        ]),
        Value::Number(Number::real(1)),
    ];
    let env = TestEnv::default();

    let r = vector_get(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert!(matches!(v, Value::Character('B')));
}

#[test]
fn vector_get_ref() {
    let args = [
        Value::vector([
            Value::Character('A'),
            Value::Character('B'),
            Value::Character('C'),
        ]),
        Value::Number(Number::real(1)),
    ];
    let env = TestEnv::default();

    let r = vector_get(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert!(matches!(v, Value::Character('B')));
}

#[test]
fn vector_get_idx_out_of_bounds() {
    let args = [
        Value::vector([
            Value::Character('A'),
            Value::Character('B'),
            Value::Character('C'),
        ]),
        Value::Number(Number::real(4)),
    ];
    let env = TestEnv::default();

    let r = vector_get(&args, &env.new_frame());

    let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
    assert_eq!(err.to_string(), "#<env-error \"index out of range\" (4)>");
}

#[test]
fn vector_get_idx_out_of_range() {
    let args = [
        Value::vector([
            Value::Character('A'),
            Value::Character('B'),
            Value::Character('C'),
        ]),
        Value::Number(Number::real(-2)),
    ];
    let env = TestEnv::default();

    let r = vector_get(&args, &env.new_frame());

    let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
    assert_eq!(
        err.to_string(),
        "#<env-error \"integer literal out of range: [0, 18446744073709551615]\" (-2)>"
    );
}

#[test]
fn vector_get_idx_malformed() {
    let args = [
        Value::vector([
            Value::Character('A'),
            Value::Character('B'),
            Value::Character('C'),
        ]),
        Value::Number(Number::real(4.2)),
    ];
    let env = TestEnv::default();

    let r = vector_get(&args, &env.new_frame());

    let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
    assert_eq!(
        err.to_string(),
        "#<env-error \"invalid type for arg `1` - expected: integer, got: floating-point\" (4.2)>"
    );
}

#[test]
fn vector_get_idx_invalid() {
    let args = [
        Value::vector([
            Value::Character('A'),
            Value::Character('B'),
            Value::Character('C'),
        ]),
        Value::string("an idx"),
    ];
    let env = TestEnv::default();

    let r = vector_get(&args, &env.new_frame());

    let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
    assert_eq!(
        err.to_string(),
        "#<env-error \"invalid type for arg `1` - expected: integer, got: string\" (\"an idx\")>"
    );
}

#[test]
fn vector_set_val() {
    let args = [
        Value::vector_mut([
            Value::Character('A'),
            Value::Character('B'),
            Value::Character('C'),
        ]),
        Value::Number(Number::real(1)),
        Value::Number(Number::real(25)),
    ];
    let env = TestEnv::default();

    let r = vector_set(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert!(matches!(v, Value::Unspecified));
    assert_eq!(args[0].to_string(), "#(#\\A 25 #\\C)");
}

#[test]
fn bytevector_mutable_predicate() {
    let args = [Value::bytevector_mut([1, 2, 3])];
    let env = TestEnv::default();

    let r = is_bytevector(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert!(matches!(v, Value::Boolean(true)));
}

#[test]
fn bytevector_mutable_length() {
    let args = [Value::bytevector_mut([8, 9, 10])];
    let env = TestEnv::default();

    let r = bytevector_length(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v.to_string(), "3");
}

#[test]
fn bytevector_mutable_get_ref() {
    let args = [
        Value::bytevector_mut([1, 2, 3]),
        Value::Number(Number::real(1)),
    ];
    let env = TestEnv::default();

    let r = bytevector_get(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v.to_string(), "2");
}

#[test]
fn bytevector_set_val() {
    let args = [
        Value::bytevector_mut([1, 2, 3]),
        Value::Number(Number::real(1)),
        Value::Number(Number::real(25)),
    ];
    let env = TestEnv::default();

    let r = bytevector_set(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert!(matches!(v, Value::Unspecified));
    assert_eq!(args[0].to_string(), "#u8(1 25 3)");
}

#[test]
fn bytevector_set_val_out_of_range() {
    let args = [
        Value::bytevector_mut([1, 2, 3]),
        Value::Number(Number::real(1)),
        Value::Number(Number::real(400)),
    ];
    let env = TestEnv::default();

    let r = bytevector_set(&args, &env.new_frame());

    let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
    assert_eq!(
        err.to_string(),
        "#<env-error \"integer literal out of range: [0, 255]\" (400)>"
    );
}

#[test]
fn bytevector_set_val_invalid() {
    let args = [
        Value::bytevector_mut([1, 2, 3]),
        Value::Number(Number::real(1)),
        Value::string("a byte"),
    ];
    let env = TestEnv::default();

    let r = bytevector_set(&args, &env.new_frame());

    let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
    assert_eq!(
        err.to_string(),
        "#<env-error \"invalid type for arg `2` - expected: integer, got: string\" (\"a byte\")>"
    );
}

#[test]
fn bytevector_set_val_immutable() {
    let args = [
        Value::ByteVector([1, 2, 3].into()),
        Value::Number(Number::real(1)),
        Value::Number(Number::real(25)),
    ];
    let env = TestEnv::default();

    let r = bytevector_set(&args, &env.new_frame());

    let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
    assert_eq!(
        err.to_string(),
        "#<env-error \"cannot modify literal value\" (#u8(1 2 3))>"
    );
}

#[test]
fn bytevector_copy_all() {
    let args = [Value::ByteVector([1, 2, 3, 4].into())];
    let env = TestEnv::default();

    let r = bytevector_copy(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v.to_string(), "#u8(1 2 3 4)");
}

#[test]
fn bytevector_copy_start() {
    let args = [
        Value::ByteVector([1, 2, 3, 4].into()),
        Value::Number(Number::real(1)),
    ];
    let env = TestEnv::default();

    let r = bytevector_copy(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v.to_string(), "#u8(2 3 4)");
}

#[test]
fn bytevector_copy_start_end() {
    let args = [
        Value::ByteVector([1, 2, 3, 4].into()),
        Value::Number(Number::real(1)),
        Value::Number(Number::real(3)),
    ];
    let env = TestEnv::default();

    let r = bytevector_copy(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v.to_string(), "#u8(2 3)");
}

#[test]
fn bytevector_copy_nothing() {
    let args = [
        Value::ByteVector([1, 2, 3, 4].into()),
        Value::Number(Number::real(1)),
        Value::Number(Number::real(1)),
    ];
    let env = TestEnv::default();

    let r = bytevector_copy(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v.to_string(), "#u8()");
}

#[test]
fn bytevector_copy_start_gt_end() {
    let args = [
        Value::ByteVector([1, 2, 3, 4].into()),
        Value::Number(Number::real(3)),
        Value::Number(Number::real(1)),
    ];
    let env = TestEnv::default();

    let r = bytevector_copy(&args, &env.new_frame());

    let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
    assert_eq!(
        err.to_string(),
        "#<env-error \"start greater than end\" (3 1)>"
    );
}

#[test]
fn bytevector_copy_end_too_large() {
    let args = [
        Value::ByteVector([1, 2, 3, 4].into()),
        Value::Number(Number::real(3)),
        Value::Number(Number::real(5)),
    ];
    let env = TestEnv::default();

    let r = bytevector_copy(&args, &env.new_frame());

    let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
    assert_eq!(err.to_string(), "#<env-error \"index out of range\" (5)>");
}

#[test]
fn bytevector_copy_start_too_large() {
    let args = [
        Value::ByteVector([1, 2, 3, 4].into()),
        Value::Number(Number::real(6)),
    ];
    let env = TestEnv::default();

    let r = bytevector_copy(&args, &env.new_frame());

    let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
    assert_eq!(err.to_string(), "#<env-error \"index out of range\" (6)>");
}

#[test]
fn bytes_to_str() {
    let args = [Value::ByteVector([0x63, 0x62, 0x61].into())];
    let env = TestEnv::default();

    let r = bytevector_to_str(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v.to_string(), "\"cba\"");
}

#[test]
fn bytes_to_unicode() {
    let args = [Value::ByteVector(
        [0x63, 0x62, 0xf0, 0x9f, 0xa6, 0x80].into(),
    )];
    let env = TestEnv::default();

    let r = bytevector_to_str(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v.to_string(), "\"cb🦀\"");
}

#[test]
fn bytes_to_unicode_invalid_sequence() {
    let args = [Value::ByteVector([0x63, 0x62, 0x9f, 0xa6, 0x80].into())];
    let env = TestEnv::default();

    let r = bytevector_to_str(&args, &env.new_frame());

    let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
    assert_eq!(
        err.to_string(),
        "#<env-error \"invalid UTF-8 byte sequence\" (#u8(159) 2 3)>"
    );
}

#[test]
fn bytes_to_unicode_truncated_sequence() {
    let args = [Value::ByteVector([0x63, 0x62, 0xf0, 0x9f, 0xa6].into())];
    let env = TestEnv::default();

    let r = bytevector_to_str(&args, &env.new_frame());

    let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
    assert_eq!(
        err.to_string(),
        "#<env-error \"invalid UTF-8 byte sequence\" (#u8(240 159 166) 2 5)>"
    );
}

#[test]
fn bytes_to_unicode_invalid_sequence_excluded() {
    let args = [
        Value::ByteVector([0x63, 0x62, 0x9f, 0xa6, 0x80].into()),
        Value::Number(Number::real(0)),
        Value::Number(Number::real(2)),
    ];
    let env = TestEnv::default();

    let r = bytevector_to_str(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v.to_string(), "\"cb\"");
}
