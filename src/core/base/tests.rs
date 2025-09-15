use super::*;
use crate::{
    testutil::{TestEnv, err_or_fail, extract_or_fail, ok_or_fail, some_or_fail, zlist_mut},
    value::zlist,
};
use std::rc::Rc;

fn make_circular_list(env: &TestEnv) -> (Value, Value, Value) {
    // (a b . #0=(c d e f . #0#))
    let lst = zlist_mut![
        Value::Symbol(env.symbols.get("a")),
        Value::Symbol(env.symbols.get("b")),
        Value::Symbol(env.symbols.get("c")),
        Value::Symbol(env.symbols.get("d")),
        Value::Symbol(env.symbols.get("e")),
        Value::Symbol(env.symbols.get("f")),
    ];
    let loop_cdr = ok_or_fail!(pcdr(&ok_or_fail!(pcdr(&lst))));
    let loop_cons = ok_or_fail!(pcdr(&ok_or_fail!(pcdr(&ok_or_fail!(pcdr(&loop_cdr))))));
    ok_or_fail!(set_cdr(
        &[loop_cons.clone(), loop_cdr.clone()],
        &env.new_frame()
    ));
    assert!(loop_cdr.is(&some_or_fail!(loop_cons.as_refpair()).as_ref().cdr));
    (lst.clone(), loop_cdr.clone(), loop_cons.clone())
}

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
        Value::Null,
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
    let args = [Value::string_mut("abc"), Value::real(1)];
    let env = TestEnv::default();

    let r = string_get(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert!(matches!(v, Value::Character('b')));
}

#[test]
fn string_set_val() {
    let args = [
        Value::string_mut("abc"),
        Value::real(1),
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
        Value::real(1),
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
        Value::real(2),
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
        Value::real(5),
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
    let args = [Value::string("a🦀c"), Value::real(0), Value::real(5)];
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

    let r = bytevector_from_string(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v.to_string(), "#u8(97 98 99)");
}

#[test]
fn unicode_to_bytes() {
    let args = [Value::string("a🦀c")];
    let env = TestEnv::default();

    let r = bytevector_from_string(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v.to_string(), "#u8(97 240 159 166 128 99)");
}

#[test]
fn unicode_to_bytes_fenceposts() {
    let args = [Value::string("a🦀c"), Value::real(1), Value::real(2)];
    let env = TestEnv::default();

    let r = bytevector_from_string(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v.to_string(), "#u8(240 159 166 128)");
}

#[test]
fn is_even_integer() {
    let args = [Value::real(4)];
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
    let args = [Value::real(4.0)];
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
    let args = [Value::real(4.2)];
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
    let args = [Value::real(3)];
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
    let args = [Value::real(3.0)];
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
    let args = [Value::real(3.2)];
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
fn pair_mut_predicate() {
    let env = TestEnv::default();
    let args = [Value::cons_mut(
        Value::Symbol(env.symbols.get("a")),
        Value::Symbol(env.symbols.get("b")),
    )];

    let r = is_pair(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert!(matches!(v, Value::Boolean(true)));
}

#[test]
fn set_circular_list() {
    let env = TestEnv::default();
    let pair = Value::cons_mut(
        Value::Symbol(env.symbols.get("a")),
        Value::Symbol(env.symbols.get("b")),
    );
    let args = [pair.clone(), pair.clone()];

    let r = set_cdr(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert!(matches!(v, Value::Unspecified));
    let lst = extract_or_fail!(&pair, Value::PairMut);
    let cdr = &lst.borrow().cdr;
    assert!(pair.is(cdr));
}

#[test]
fn list_mut_predicate() {
    let env = TestEnv::default();
    let args = [zlist_mut![
        Value::Symbol(env.symbols.get("a")),
        Value::Symbol(env.symbols.get("b")),
        Value::Symbol(env.symbols.get("c"))
    ]];

    let r = is_list(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert!(matches!(v, Value::Boolean(true)));
}

#[test]
fn list_circular_predicate() {
    let env = TestEnv::default();
    let (lst, loop_head, loop_tail) = make_circular_list(&env);
    let args = [lst.clone()];

    let r = is_list(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert!(matches!(v, Value::Boolean(false)));

    let args = [lst.clone()];

    let r = is_pair(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert!(matches!(v, Value::Boolean(true)));

    let args = [loop_head.clone()];

    let r = is_pair(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert!(matches!(v, Value::Boolean(true)));

    let args = [loop_tail.clone()];

    let r = is_pair(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert!(matches!(v, Value::Boolean(true)));
}

#[test]
fn list_circular_length() {
    let env = TestEnv::default();
    let (lst, _, _) = make_circular_list(&env);
    let args = [lst.clone()];

    let r = list_length(&args, &env.new_frame());

    let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
    assert_eq!(
        err.to_string(),
        "#<env-error \"circular list encountered\" ((a b . #0=(c d e f . #0#)))>"
    );
}

#[test]
fn list_append_no_args() {
    let env = TestEnv::default();
    let args = [];

    let r = list_append(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert!(matches!(v, Value::Null));
}

#[test]
fn list_append_empty() {
    let env = TestEnv::default();
    let args = [zlist![]];

    let r = list_append(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert!(matches!(v, Value::Null));
}

#[test]
fn list_append_all_empties() {
    let env = TestEnv::default();
    let args = [zlist![], zlist![], zlist![]];

    let r = list_append(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert!(matches!(v, Value::Null));
}

#[test]
fn list_append_single() {
    let env = TestEnv::default();
    let args = [Value::Symbol(env.symbols.get("a"))];

    let r = list_append(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert!(matches!(v, Value::Symbol(s) if s.as_ref() == "a"));
}

#[test]
fn list_append_empties_with_single() {
    let env = TestEnv::default();
    let args = [zlist![], zlist![], Value::Symbol(env.symbols.get("a"))];

    let r = list_append(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert!(matches!(v, Value::Symbol(s) if s.as_ref() == "a"));
}

#[test]
fn list_append_single_list() {
    let env = TestEnv::default();
    let arg = zlist![
        Value::Symbol(env.symbols.get("a")),
        Value::Symbol(env.symbols.get("b")),
        Value::Symbol(env.symbols.get("c")),
    ];
    let args = [arg.clone()];

    let r = list_append(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert!(v.is(&arg));
}

#[test]
fn list_append_multiple_lists() {
    let env = TestEnv::default();
    let last = zlist![
        Value::Symbol(env.symbols.get("x")),
        Value::Symbol(env.symbols.get("y")),
        Value::Symbol(env.symbols.get("z")),
    ];
    let args = [
        zlist![
            Value::Symbol(env.symbols.get("a")),
            Value::Symbol(env.symbols.get("b")),
            Value::Symbol(env.symbols.get("c")),
        ],
        zlist![Value::real(1), Value::real(2), Value::real(3),],
        last.clone(),
    ];

    let r = list_append(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert!(matches!(v, Value::PairMut(_)));
    assert_eq!(v.to_string(), "(a b c 1 2 3 x y z)");
    let sub = ok_or_fail!(list_tail(
        &[v, Value::Number(Number::from_usize(6))],
        &env.new_frame()
    ));
    assert!(sub.is(&last));
}

#[test]
fn list_append_improper_lists() {
    let env = TestEnv::default();
    let last = Value::Symbol(env.symbols.get("x"));
    let args = [
        zlist![
            Value::Symbol(env.symbols.get("a")),
            Value::Symbol(env.symbols.get("b")),
            Value::Symbol(env.symbols.get("c")),
        ],
        zlist![Value::real(1), Value::real(2), Value::real(3),],
        last.clone(),
    ];

    let r = list_append(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert!(matches!(v, Value::PairMut(_)));
    assert_eq!(v.to_string(), "(a b c 1 2 3 . x)");
    let sub = ok_or_fail!(list_tail(
        &[v, Value::Number(Number::from_usize(6))],
        &env.new_frame()
    ));
    assert!(sub.is(&last));
}

#[test]
fn list_append_invalid_arg() {
    let env = TestEnv::default();
    let last = zlist![
        Value::Symbol(env.symbols.get("x")),
        Value::Symbol(env.symbols.get("y")),
        Value::Symbol(env.symbols.get("z")),
    ];
    let args = [
        zlist![
            Value::Symbol(env.symbols.get("a")),
            Value::Symbol(env.symbols.get("b")),
            Value::Symbol(env.symbols.get("c")),
        ],
        Value::real(1),
        last.clone(),
    ];

    let r = list_append(&args, &env.new_frame());

    let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
    assert_eq!(
        err.to_string(),
        "#<env-error \"invalid type for arg `3` - expected: list, got: number\" (1)>"
    );
}

#[test]
fn list_reverse_empty() {
    let env = TestEnv::default();
    let args = [zlist![]];

    let r = list_reverse(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert!(matches!(v, Value::Null));
}

#[test]
fn list_reverse_single_item() {
    let env = TestEnv::default();
    let args = [zlist![Value::Symbol(env.symbols.get("a"))]];

    let r = list_reverse(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert!(matches!(v, Value::PairMut(_)));
    assert_eq!(v.to_string(), "(a)");
}

#[test]
fn list_reverse_items() {
    let env = TestEnv::default();
    let args = [zlist![
        Value::Symbol(env.symbols.get("a")),
        Value::Symbol(env.symbols.get("b")),
        Value::Symbol(env.symbols.get("c")),
    ]];

    let r = list_reverse(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert!(matches!(v, Value::PairMut(_)));
    assert_eq!(v.to_string(), "(c b a)");
}

#[test]
fn list_reverse_pair() {
    let env = TestEnv::default();
    let args = [Value::cons(
        Value::Symbol(env.symbols.get("a")),
        Value::Symbol(env.symbols.get("b")),
    )];

    let r = list_reverse(&args, &env.new_frame());

    let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
    assert_eq!(
        err.to_string(),
        "#<env-error \"invalid type for arg `1` - expected: list, got: symbol\" (b)>"
    );
}

#[test]
fn list_reverse_improper_list() {
    let env = TestEnv::default();
    let args = [Value::cons(
        Value::Symbol(env.symbols.get("a")),
        Value::cons(
            Value::Symbol(env.symbols.get("b")),
            Value::Symbol(env.symbols.get("c")),
        ),
    )];

    let r = list_reverse(&args, &env.new_frame());

    let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
    assert_eq!(
        err.to_string(),
        "#<env-error \"invalid type for arg `2` - expected: list, got: symbol\" (c)>"
    );
}

#[test]
fn list_reverse_circular_list() {
    let env = TestEnv::default();
    let (lst, _, _) = make_circular_list(&env);
    let args = [lst.clone()];

    let r = list_reverse(&args, &env.new_frame());

    let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
    assert_eq!(
        err.to_string(),
        "#<env-error \"circular list encountered\" ((a b . #0=(c d e f . #0#)))>"
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
        Value::real(1),
    ];

    let r = list_tail(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    let second_item = extract_or_fail!(
        &extract_or_fail!(&args[0], Value::Pair).as_ref().cdr,
        Value::Pair
    );
    assert!(Rc::ptr_eq(&extract_or_fail!(v, Value::Pair), second_item));
}

#[test]
fn list_tail_mutable_list() {
    let env = TestEnv::default();
    let args = [
        zlist_mut![
            Value::Symbol(env.symbols.get("a")),
            Value::Symbol(env.symbols.get("b")),
            Value::Symbol(env.symbols.get("c"))
        ],
        Value::real(1),
    ];

    let r = list_tail(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    let vcdr = &extract_or_fail!(&args[0], Value::PairMut)
        .as_ref()
        .borrow()
        .cdr;
    let second_item = extract_or_fail!(vcdr, Value::PairMut);
    assert!(Rc::ptr_eq(
        &extract_or_fail!(v, Value::PairMut),
        second_item
    ));
}

#[test]
fn list_tail_empty_list() {
    let args = [zlist![], Value::real(0)];
    let env = TestEnv::default();

    let r = list_tail(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert!(matches!(v, Value::Null));
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
        Value::real(3),
    ];

    let r = list_tail(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert!(matches!(v, Value::Null));
}

#[test]
fn list_tail_non_list() {
    let env = TestEnv::default();
    let args = [Value::Symbol(env.symbols.get("a")), Value::real(0)];

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
        Value::real(2),
    ];

    let r = list_tail(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert!(matches!(v, Value::Symbol(s) if s.as_ref() == "c"));
}

#[test]
fn list_tail_end_of_mutable_improper_list() {
    let env = TestEnv::default();
    let args = [
        Value::cons_mut(
            Value::Symbol(env.symbols.get("a")),
            Value::cons_mut(
                Value::Symbol(env.symbols.get("b")),
                Value::Symbol(env.symbols.get("c")),
            ),
        ),
        Value::real(2),
    ];

    let r = list_tail(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert!(matches!(v, Value::Symbol(s) if s.as_ref() == "c"));
}

#[test]
fn list_tail_circular_list() {
    let env = TestEnv::default();
    let (lst, loop_head, loop_tail) = make_circular_list(&env);

    let r = list_tail(&[lst.clone(), Value::real(6)], &env.new_frame());

    let v = ok_or_fail!(r);
    assert!(loop_head.is(&v));

    let r = list_tail(&[lst.clone(), Value::real(10)], &env.new_frame());

    let v = ok_or_fail!(r);
    assert!(loop_head.is(&v));

    let r = list_tail(&[lst.clone(), Value::real(11)], &env.new_frame());

    let v = ok_or_fail!(r);
    assert!(!loop_head.is(&v));

    let r = list_tail(&[lst.clone(), Value::real(13)], &env.new_frame());

    let v = ok_or_fail!(r);
    assert!(loop_tail.is(&v));
}

#[test]
fn list_tail_circular_list_many_loops() {
    let env = TestEnv::default();
    let (lst, loop_head, _) = make_circular_list(&env);
    // NOTE: reach the head of the cycle, then loop a million times;
    // if implemented recursively this will stack overflow.
    let args = [lst.clone(), Value::real(100002)];

    let r = list_tail(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert!(loop_head.is(&v));
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
        Value::real(4),
    ];

    let r = list_tail(&args, &env.new_frame());

    let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
    assert_eq!(err.to_string(), "#<env-error \"index out of range\" (4)>");
}

#[test]
fn list_tail_non_list_out_of_range() {
    let env = TestEnv::default();
    let args = [Value::Symbol(env.symbols.get("a")), Value::real(1)];

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
        Value::real(3),
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
    let args = [zlist![], Value::string("foo")];
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
    let args = [zlist![], Value::real(4.2)];
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
    let args = [zlist![], Value::real(-4)];
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
        Value::real(1),
    ];

    let r = list_get(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert!(matches!(v, Value::Symbol(s) if s.as_ref() == "b"));
}

#[test]
fn list_ref_normal_mutable_list() {
    let env = TestEnv::default();
    let args = [
        zlist_mut![
            Value::Symbol(env.symbols.get("a")),
            Value::Symbol(env.symbols.get("b")),
            Value::Symbol(env.symbols.get("c"))
        ],
        Value::real(1),
    ];

    let r = list_get(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert!(matches!(v, Value::Symbol(s) if s.as_ref() == "b"));
}

#[test]
fn list_ref_empty_list() {
    let args = [zlist![], Value::real(0)];
    let env = TestEnv::default();

    let r = list_get(&args, &env.new_frame());

    let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
    assert_eq!(err.to_string(), "#<env-error \"index out of range\" (0)>");
}

#[test]
fn list_ref_circular_list() {
    let env = TestEnv::default();
    let (lst, _, _) = make_circular_list(&env);

    let r = list_get(&[lst.clone(), Value::real(10)], &env.new_frame());

    let v = ok_or_fail!(r);
    assert!(matches!(v, Value::Symbol(s) if s.as_ref() == "c"));

    let r = list_get(&[lst.clone(), Value::real(11)], &env.new_frame());

    let v = ok_or_fail!(r);
    assert!(matches!(v, Value::Symbol(s) if s.as_ref() == "d"));

    let r = list_get(&[lst.clone(), Value::real(13)], &env.new_frame());

    let v = ok_or_fail!(r);
    assert!(matches!(v, Value::Symbol(s) if s.as_ref() == "f"));
}

#[test]
fn list_ref_non_list() {
    let env = TestEnv::default();
    let args = [Value::Symbol(env.symbols.get("a")), Value::real(0)];

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
        Value::real(1),
    ];

    let r = list_get(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert!(matches!(v, Value::Symbol(s) if s.as_ref() == "b"));
}

#[test]
fn list_ref_mutable_improper_list_item() {
    let env = TestEnv::default();
    let args = [
        Value::cons_mut(
            Value::Symbol(env.symbols.get("a")),
            Value::cons_mut(
                Value::Symbol(env.symbols.get("b")),
                Value::Symbol(env.symbols.get("c")),
            ),
        ),
        Value::real(1),
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
        Value::real(2),
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
        Value::real(4),
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
        Value::real(3),
    ];

    let r = list_get(&args, &env.new_frame());

    let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
    assert_eq!(
        err.to_string(),
        "#<env-error \"invalid type for arg `0` - expected: pair, got: symbol\" (c)>"
    );
}

#[test]
fn list_set_value() {
    let env = TestEnv::default();
    let lst = zlist_mut![
        Value::Symbol(env.symbols.get("a")),
        Value::Symbol(env.symbols.get("b")),
        Value::Symbol(env.symbols.get("c"))
    ];
    let args = [
        lst.clone(),
        Value::real(1),
        Value::Symbol(env.symbols.get("z")),
    ];

    let r = list_set(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert!(matches!(v, Value::Unspecified));
    assert_eq!(lst.to_string(), "(a z c)");
}

#[test]
fn list_set_value_replace_pair() {
    let env = TestEnv::default();
    let lst = zlist_mut![
        Value::Symbol(env.symbols.get("a")),
        Value::Symbol(env.symbols.get("b")),
        Value::cons(
            Value::Symbol(env.symbols.get("c")),
            Value::Symbol(env.symbols.get("d"))
        ),
    ];
    let args = [
        lst.clone(),
        Value::real(2),
        Value::Symbol(env.symbols.get("z")),
    ];

    let r = list_set(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert!(matches!(v, Value::Unspecified));
    assert_eq!(lst.to_string(), "(a b z)");
}

#[test]
fn list_set_value_improper_list() {
    let env = TestEnv::default();
    let lst = Value::cons_mut(
        Value::Symbol(env.symbols.get("a")),
        Value::cons_mut(
            Value::Symbol(env.symbols.get("b")),
            Value::cons_mut(
                Value::Symbol(env.symbols.get("c")),
                Value::Symbol(env.symbols.get("d")),
            ),
        ),
    );
    let args = [
        lst.clone(),
        Value::real(2),
        Value::Symbol(env.symbols.get("z")),
    ];

    let r = list_set(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert!(matches!(v, Value::Unspecified));
    assert_eq!(lst.to_string(), "(a b z . d)");
}

#[test]
fn list_set_circular_list() {
    let env = TestEnv::default();
    let (lst, list_head, _) = make_circular_list(&env);
    let args = [lst, Value::real(10), Value::Symbol(env.symbols.get("z"))];

    let r = list_set(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert!(matches!(v, Value::Unspecified));
    assert!(
        matches!(&some_or_fail!(list_head.as_refpair()).as_ref().car, Value::Symbol(s) if s.as_ref() == "z")
    );
}

#[test]
fn list_set_out_of_range() {
    let env = TestEnv::default();
    let args = [
        zlist_mut![
            Value::Symbol(env.symbols.get("a")),
            Value::Symbol(env.symbols.get("b")),
            Value::Symbol(env.symbols.get("c"))
        ],
        Value::real(3),
        Value::Symbol(env.symbols.get("z")),
    ];

    let r = list_get(&args, &env.new_frame());

    let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
    assert_eq!(err.to_string(), "#<env-error \"index out of range\" (3)>");
}

#[test]
fn list_copy_null() {
    let env = TestEnv::default();
    let lst = zlist![];
    let args = [lst.clone()];

    let r = list_copy(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert!(v.is(&lst));
}

#[test]
fn list_copy_non_list() {
    let env = TestEnv::default();
    let lst = Value::Symbol(env.symbols.get("a"));
    let args = [lst.clone()];

    let r = list_copy(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert!(v.is(&lst));
}

#[test]
fn list_copy_list() {
    let env = TestEnv::default();
    let lst = zlist![
        Value::Symbol(env.symbols.get("a")),
        Value::Symbol(env.symbols.get("b")),
        Value::Symbol(env.symbols.get("c"))
    ];
    let args = [lst.clone()];

    let r = list_copy(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert!(!v.is(&lst));
    assert!(matches!(v, Value::PairMut(_)));
    assert_eq!(v.to_string(), "(a b c)");
}

#[test]
fn list_copy_improper_list() {
    let env = TestEnv::default();
    let cons = Value::cons(
        Value::Symbol(env.symbols.get("c")),
        Value::Symbol(env.symbols.get("d")),
    );
    let lst = Value::cons(
        Value::Symbol(env.symbols.get("a")),
        Value::cons(Value::Symbol(env.symbols.get("b")), cons.clone()),
    );
    let args = [lst.clone()];

    let r = list_copy(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert!(!v.is(&lst));
    assert!(matches!(v, Value::PairMut(_)));
    assert_eq!(v.to_string(), "(a b c . d)");
    let vcdr = &extract_or_fail!(&v, Value::PairMut).as_ref().borrow().cdr;
    let last = &extract_or_fail!(vcdr, Value::PairMut).as_ref().borrow().cdr;
    assert!(last.is(&cons));
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
        "#<env-error \"invalid type for arg `0` - expected: integer, got: symbol\" (a)>"
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
        "#<env-error \"unicode code point out of ranges [#x0, #xD7FF], [#xE000, #x10FFFF] ([0, 55295], [57344, 1114111])\" (-4)>"
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
        Value::real(1),
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
        Value::real(1),
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
        Value::real(1),
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
        Value::real(4),
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
        Value::real(-2),
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
        Value::real(4.2),
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
        Value::real(1),
        Value::real(25),
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
    let args = [Value::bytevector_mut([1, 2, 3]), Value::real(1)];
    let env = TestEnv::default();

    let r = bytevector_get(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v.to_string(), "2");
}

#[test]
fn bytevector_set_val() {
    let args = [
        Value::bytevector_mut([1, 2, 3]),
        Value::real(1),
        Value::real(25),
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
        Value::real(1),
        Value::real(400),
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
        Value::real(1),
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
        Value::real(1),
        Value::real(25),
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
    let args = [Value::ByteVector([1, 2, 3, 4].into()), Value::real(1)];
    let env = TestEnv::default();

    let r = bytevector_copy(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v.to_string(), "#u8(2 3 4)");
}

#[test]
fn bytevector_copy_start_end() {
    let args = [
        Value::ByteVector([1, 2, 3, 4].into()),
        Value::real(1),
        Value::real(3),
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
        Value::real(1),
        Value::real(1),
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
        Value::real(3),
        Value::real(1),
    ];
    let env = TestEnv::default();

    let r = bytevector_copy(&args, &env.new_frame());

    let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
    assert_eq!(
        err.to_string(),
        "#<env-error \"start greater than end\" ((3 . 1))>"
    );
}

#[test]
fn bytevector_copy_end_too_large() {
    let args = [
        Value::ByteVector([1, 2, 3, 4].into()),
        Value::real(3),
        Value::real(5),
    ];
    let env = TestEnv::default();

    let r = bytevector_copy(&args, &env.new_frame());

    let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
    assert_eq!(err.to_string(), "#<env-error \"index out of range\" (5)>");
}

#[test]
fn bytevector_copy_start_too_large() {
    let args = [Value::ByteVector([1, 2, 3, 4].into()), Value::real(6)];
    let env = TestEnv::default();

    let r = bytevector_copy(&args, &env.new_frame());

    let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
    assert_eq!(err.to_string(), "#<env-error \"index out of range\" (6)>");
}

#[test]
fn bytes_to_str() {
    let args = [Value::ByteVector([0x63, 0x62, 0x61].into())];
    let env = TestEnv::default();

    let r = bytevector_to_string(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v.to_string(), "\"cba\"");
}

#[test]
fn bytes_to_unicode() {
    let args = [Value::ByteVector(
        [0x63, 0x62, 0xf0, 0x9f, 0xa6, 0x80].into(),
    )];
    let env = TestEnv::default();

    let r = bytevector_to_string(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v.to_string(), "\"cb🦀\"");
}

#[test]
fn bytes_to_unicode_invalid_sequence() {
    let args = [Value::ByteVector([0x63, 0x62, 0x9f, 0xa6, 0x80].into())];
    let env = TestEnv::default();

    let r = bytevector_to_string(&args, &env.new_frame());

    let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
    assert_eq!(
        err.to_string(),
        "#<env-error \"invalid UTF-8 byte sequence\" (#u8(159) (2 . 3))>"
    );
}

#[test]
fn bytes_to_unicode_truncated_sequence() {
    let args = [Value::ByteVector([0x63, 0x62, 0xf0, 0x9f, 0xa6].into())];
    let env = TestEnv::default();

    let r = bytevector_to_string(&args, &env.new_frame());

    let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
    assert_eq!(
        err.to_string(),
        "#<env-error \"invalid UTF-8 byte sequence\" (#u8(240 159 166) (2 . 5))>"
    );
}

#[test]
fn bytes_to_unicode_invalid_sequence_excluded() {
    let args = [
        Value::ByteVector([0x63, 0x62, 0x9f, 0xa6, 0x80].into()),
        Value::real(0),
        Value::real(2),
    ];
    let env = TestEnv::default();

    let r = bytevector_to_string(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v.to_string(), "\"cb\"");
}

#[test]
fn bytevector_copy_into_equal() {
    let args = [
        Value::bytevector_mut([1, 2, 3, 4, 5]),
        Value::real(0),
        Value::ByteVector([6, 7, 8, 9, 10].into()),
    ];
    let env = TestEnv::default();

    let r = bytevector_copy_inline(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v, Value::Unspecified);
    assert_eq!(args[0].to_string(), "#u8(6 7 8 9 10)");
    assert!(!args[0].is(&args[2]));
}

#[test]
fn bytevector_copy_into_larger() {
    let args = [
        Value::bytevector_mut([1, 2, 3, 4, 5]),
        Value::real(1),
        Value::ByteVector([6, 7, 8].into()),
    ];
    let env = TestEnv::default();

    let r = bytevector_copy_inline(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v, Value::Unspecified);
    assert_eq!(args[0].to_string(), "#u8(1 6 7 8 5)");
}

#[test]
fn bytevector_copy_into_smaller() {
    let args = [
        Value::bytevector_mut([1, 2, 3]),
        Value::real(1),
        Value::ByteVector([6, 7, 8, 9, 10].into()),
        Value::real(3),
        Value::real(5),
    ];
    let env = TestEnv::default();

    let r = bytevector_copy_inline(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v, Value::Unspecified);
    assert_eq!(args[0].to_string(), "#u8(1 9 10)");
}

#[test]
fn bytevector_copy_from_empty() {
    let args = [
        Value::bytevector_mut([1, 2, 3]),
        Value::real(1),
        Value::ByteVector([].into()),
    ];
    let env = TestEnv::default();

    let r = bytevector_copy_inline(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v, Value::Unspecified);
    assert_eq!(args[0].to_string(), "#u8(1 2 3)");
}

#[test]
fn bytevector_copy_to_empty() {
    let args = [
        Value::bytevector_mut([]),
        Value::real(0),
        Value::ByteVector([6, 7, 8, 9, 10].into()),
        Value::real(3),
        Value::real(3),
    ];
    let env = TestEnv::default();

    let r = bytevector_copy_inline(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v, Value::Unspecified);
    assert_eq!(args[0].to_string(), "#u8()");
}

#[test]
fn bytevector_copy_invalid_at() {
    let args = [
        Value::bytevector_mut([1, 2, 3, 4, 5]),
        Value::real(10),
        Value::ByteVector([6, 7, 8, 9, 10].into()),
    ];
    let env = TestEnv::default();

    let r = bytevector_copy_inline(&args, &env.new_frame());

    let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
    assert_eq!(
        err.to_string(),
        "#<env-error \"target index out of range\" (10)>"
    );
}

#[test]
fn bytevector_copy_at_length_ok_if_span_is_empty() {
    let args = [
        Value::bytevector_mut([1, 2, 3, 4, 5]),
        Value::real(5),
        Value::ByteVector([6, 7, 8, 9, 10].into()),
        Value::real(0),
        Value::real(0),
    ];
    let env = TestEnv::default();

    let r = bytevector_copy_inline(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v, Value::Unspecified);
    assert_eq!(args[0].to_string(), "#u8(1 2 3 4 5)");
}

#[test]
fn bytevector_copy_accepts_immutable_if_span_is_empty() {
    let args = [
        Value::ByteVector([1, 2, 3, 4, 5].into()),
        Value::real(2),
        Value::ByteVector([6, 7, 8, 9, 10].into()),
        Value::real(0),
        Value::real(0),
    ];
    let env = TestEnv::default();

    let r = bytevector_copy_inline(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v, Value::Unspecified);
    assert_eq!(args[0].to_string(), "#u8(1 2 3 4 5)");
}

#[test]
fn bytevector_copy_at_length_err_if_span_is_not_empty() {
    let args = [
        Value::bytevector_mut([1, 2, 3, 4, 5]),
        Value::real(5),
        Value::ByteVector([6, 7, 8, 9, 10].into()),
    ];
    let env = TestEnv::default();

    let r = bytevector_copy_inline(&args, &env.new_frame());

    let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
    assert_eq!(
        err.to_string(),
        "#<env-error \"source span too large for target range\" ((0 . 5) (5 . 5))>"
    );
}

#[test]
fn bytevector_copy_immutable_target() {
    let args = [
        Value::ByteVector([1, 2, 3, 4, 5].into()),
        Value::real(0),
        Value::ByteVector([6, 7, 8, 9, 10].into()),
    ];
    let env = TestEnv::default();

    let r = bytevector_copy_inline(&args, &env.new_frame());

    let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
    assert_eq!(
        err.to_string(),
        "#<env-error \"cannot modify literal value\" (#u8(1 2 3 4 5))>"
    );
}

#[test]
fn bytevector_copy_too_much_into_smaller() {
    let args = [
        Value::bytevector_mut([1, 2, 3]),
        Value::real(1),
        Value::ByteVector([6, 7, 8, 9, 10].into()),
        Value::real(1),
        Value::real(5),
    ];
    let env = TestEnv::default();

    let r = bytevector_copy_inline(&args, &env.new_frame());

    let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
    assert_eq!(
        err.to_string(),
        "#<env-error \"source span too large for target range\" ((1 . 5) (1 . 3))>"
    );
}

#[test]
fn bytevector_copy_into_self() {
    let bv = Value::bytevector_mut([1, 2, 3, 4, 5]);
    let args = [bv.clone(), Value::real(0), bv];
    let env = TestEnv::default();

    let r = bytevector_copy_inline(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v, Value::Unspecified);
    assert_eq!(args[0].to_string(), "#u8(1 2 3 4 5)");
    assert!(args[0].is(&args[2]));
}

#[test]
fn bytevector_copy_part_into_self() {
    let bv = Value::bytevector_mut([1, 2, 3, 4, 5]);
    let args = [
        bv.clone(),
        Value::real(0),
        bv,
        Value::real(2),
        Value::real(4),
    ];
    let env = TestEnv::default();

    let r = bytevector_copy_inline(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v, Value::Unspecified);
    assert_eq!(args[0].to_string(), "#u8(3 4 3 4 5)");
    assert!(args[0].is(&args[2]));
}

#[test]
fn bytevector_copy_into_tail_overlap() {
    let bv = Value::bytevector_mut([1, 2, 3, 4, 5]);
    let args = [
        bv.clone(),
        Value::real(1),
        bv,
        Value::real(2),
        Value::real(5),
    ];
    let env = TestEnv::default();

    let r = bytevector_copy_inline(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v, Value::Unspecified);
    assert_eq!(args[0].to_string(), "#u8(1 3 4 5 5)");
    assert!(args[0].is(&args[2]));
}

#[test]
fn bytevector_copy_into_head_overlap() {
    let bv = Value::bytevector_mut([1, 2, 3, 4, 5]);
    let args = [
        bv.clone(),
        Value::real(2),
        bv,
        Value::real(0),
        Value::real(3),
    ];
    let env = TestEnv::default();

    let r = bytevector_copy_inline(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v, Value::Unspecified);
    assert_eq!(args[0].to_string(), "#u8(1 2 1 2 3)");
    assert!(args[0].is(&args[2]));
}

#[test]
fn string_copy_into_equal() {
    let args = [
        Value::string_mut("foobar"),
        Value::real(0),
        Value::string("beefee"),
    ];
    let env = TestEnv::default();

    let r = string_copy_inline(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v, Value::Unspecified);
    assert_eq!(args[0].to_string(), "\"beefee\"");
    assert!(!args[0].is(&args[2]));
}

#[test]
fn string_copy_into_larger() {
    let args = [
        Value::string_mut("foobar"),
        Value::real(1),
        Value::string("bee"),
    ];
    let env = TestEnv::default();

    let r = string_copy_inline(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v, Value::Unspecified);
    assert_eq!(args[0].to_string(), "\"fbeear\"");
}

#[test]
fn string_copy_into_smaller() {
    let args = [
        Value::string_mut("foobar"),
        Value::real(1),
        Value::string("beefee"),
        Value::real(3),
        Value::real(5),
    ];
    let env = TestEnv::default();

    let r = string_copy_inline(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v, Value::Unspecified);
    assert_eq!(args[0].to_string(), "\"ffebar\"");
}

#[test]
fn string_copy_from_unicode_into_ascii() {
    let args = [
        Value::string_mut("abc"),
        Value::real(1),
        Value::string("123🐝"),
        Value::real(3),
        Value::real(4),
    ];
    let env = TestEnv::default();

    let r = string_copy_inline(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v, Value::Unspecified);
    assert_eq!(args[0].to_string(), "\"a🐝c\"");
}

#[test]
fn string_copy_inline_ranges_are_accurate() {
    let args = [
        Value::string_mut("🦀"),
        Value::real(0),
        Value::string("crab"),
    ];
    let env = TestEnv::default();

    let r = string_copy_inline(&args, &env.new_frame());
    let err = extract_or_fail!(err_or_fail!(r), Exception::Signal);
    assert_eq!(
        err.to_string(),
        "#<env-error \"source span too large for target range\" ((0 . 4) (0 . 1))>"
    );
}

#[test]
fn string_copy_into_unicode() {
    let args = [
        Value::string_mut("foo🦀bar"),
        Value::real(4),
        Value::string("bee🐝fee"),
        Value::real(3),
        Value::real(4),
    ];
    let env = TestEnv::default();

    let r = string_copy_inline(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v, Value::Unspecified);
    assert_eq!(args[0].to_string(), "\"foo🦀🐝ar\"");
}

#[test]
fn string_copy_into_self() {
    let s = Value::string_mut("foobar");
    let args = [s.clone(), Value::real(0), s];
    let env = TestEnv::default();

    let r = string_copy_inline(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v, Value::Unspecified);
    assert_eq!(args[0].to_string(), "\"foobar\"");
    assert!(args[0].is(&args[2]));
}

#[test]
fn string_copy_part_into_self() {
    let s = Value::string_mut("foobar");
    let args = [s.clone(), Value::real(0), s, Value::real(2), Value::real(4)];
    let env = TestEnv::default();

    let r = string_copy_inline(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v, Value::Unspecified);
    assert_eq!(args[0].to_string(), "\"obobar\"");
    assert!(args[0].is(&args[2]));
}

#[test]
fn string_copy_tail_overlap_into_self() {
    let s = Value::string_mut("foobar");
    let args = [s.clone(), Value::real(1), s, Value::real(2), Value::real(5)];
    let env = TestEnv::default();

    let r = string_copy_inline(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v, Value::Unspecified);
    assert_eq!(args[0].to_string(), "\"fobaar\"");
    assert!(args[0].is(&args[2]));
}

#[test]
fn string_copy_head_overlap_into_self() {
    let s = Value::string_mut("foobar");
    let args = [s.clone(), Value::real(2), s, Value::real(0), Value::real(3)];
    let env = TestEnv::default();

    let r = string_copy_inline(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v, Value::Unspecified);
    assert_eq!(args[0].to_string(), "\"fofoor\"");
    assert!(args[0].is(&args[2]));
}

#[test]
fn vector_copy_into_equal() {
    let args = [
        Value::vector_mut([
            Value::real(1),
            Value::real(2),
            Value::string("foo"),
            Value::Boolean(false),
            Value::Character('a'),
        ]),
        Value::real(0),
        Value::vector([
            Value::real(3),
            Value::real(4),
            Value::string("bar"),
            Value::Boolean(true),
            Value::Character('z'),
        ]),
    ];
    let env = TestEnv::default();

    let r = vector_copy_inline(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v, Value::Unspecified);
    assert_eq!(args[0].to_string(), "#(3 4 \"bar\" #t #\\z)");
    assert!(!args[0].is(&args[2]));
}

#[test]
fn vector_copy_into_larger() {
    let args = [
        Value::vector_mut([
            Value::real(1),
            Value::real(2),
            Value::string("foo"),
            Value::Boolean(false),
            Value::Character('a'),
        ]),
        Value::real(1),
        Value::vector([Value::real(3), Value::real(4), Value::string("bar")]),
    ];
    let env = TestEnv::default();

    let r = vector_copy_inline(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v, Value::Unspecified);
    assert_eq!(args[0].to_string(), "#(1 3 4 \"bar\" #\\a)");
}

#[test]
fn vector_copy_into_smaller() {
    let args = [
        Value::vector_mut([Value::real(1), Value::real(2), Value::string("foo")]),
        Value::real(1),
        Value::vector([
            Value::real(3),
            Value::real(4),
            Value::string("bar"),
            Value::Boolean(true),
            Value::Character('z'),
        ]),
        Value::real(3),
        Value::real(5),
    ];
    let env = TestEnv::default();

    let r = vector_copy_inline(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v, Value::Unspecified);
    assert_eq!(args[0].to_string(), "#(1 #t #\\z)");
}

#[test]
fn vector_copy_into_self() {
    let v = Value::vector_mut([
        Value::real(1),
        Value::real(2),
        Value::string("foo"),
        Value::Boolean(false),
        Value::Character('a'),
    ]);
    let args = [v.clone(), Value::real(0), v];
    let env = TestEnv::default();

    let r = vector_copy_inline(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v, Value::Unspecified);
    assert_eq!(args[0].to_string(), "#(1 2 \"foo\" #f #\\a)");
    assert!(args[0].is(&args[2]));
}

#[test]
fn vector_copy_part_into_self() {
    let v = Value::vector_mut([
        Value::real(1),
        Value::real(2),
        Value::string("foo"),
        Value::Boolean(false),
        Value::Character('a'),
    ]);
    let args = [v.clone(), Value::real(0), v, Value::real(2), Value::real(4)];
    let env = TestEnv::default();

    let r = vector_copy_inline(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v, Value::Unspecified);
    assert_eq!(args[0].to_string(), "#(\"foo\" #f \"foo\" #f #\\a)");
    assert!(args[0].is(&args[2]));
}

#[test]
fn vector_copy_no_overlap_part_into_self() {
    let v = Value::vector_mut([
        Value::real(1),
        Value::real(2),
        Value::string("foo"),
        Value::Boolean(false),
        Value::Character('a'),
    ]);
    let args = [v.clone(), Value::real(0), v, Value::real(3), Value::real(5)];
    let env = TestEnv::default();

    let r = vector_copy_inline(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v, Value::Unspecified);
    assert_eq!(args[0].to_string(), "#(#f #\\a \"foo\" #f #\\a)");
    assert!(args[0].is(&args[2]));
}

#[test]
fn vector_copy_lower_part_into_self() {
    let v = Value::vector_mut([
        Value::real(1),
        Value::real(2),
        Value::string("foo"),
        Value::Boolean(false),
        Value::Character('a'),
    ]);
    let args = [v.clone(), Value::real(2), v, Value::real(0), Value::real(2)];
    let env = TestEnv::default();

    let r = vector_copy_inline(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v, Value::Unspecified);
    assert_eq!(args[0].to_string(), "#(1 2 1 2 #\\a)");
    assert!(args[0].is(&args[2]));
}

#[test]
fn vector_copy_no_overlap_lower_part_into_self() {
    let v = Value::vector_mut([
        Value::real(1),
        Value::real(2),
        Value::string("foo"),
        Value::Boolean(false),
        Value::Character('a'),
    ]);
    let args = [v.clone(), Value::real(3), v, Value::real(0), Value::real(2)];
    let env = TestEnv::default();

    let r = vector_copy_inline(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v, Value::Unspecified);
    assert_eq!(args[0].to_string(), "#(1 2 \"foo\" 1 2)");
    assert!(args[0].is(&args[2]));
}

#[test]
fn vector_copy_into_tail_overlap() {
    let v = Value::vector_mut([
        Value::real(1),
        Value::real(2),
        Value::string("foo"),
        Value::Boolean(false),
        Value::Character('a'),
    ]);
    let args = [v.clone(), Value::real(1), v, Value::real(2), Value::real(5)];
    let env = TestEnv::default();

    let r = vector_copy_inline(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v, Value::Unspecified);
    assert_eq!(args[0].to_string(), "#(1 \"foo\" #f #\\a #\\a)");
    assert!(args[0].is(&args[2]));
}

#[test]
fn vector_copy_into_head_overlap() {
    let v = Value::vector_mut([
        Value::real(1),
        Value::real(2),
        Value::string("foo"),
        Value::Boolean(false),
        Value::Character('a'),
    ]);
    let args = [v.clone(), Value::real(2), v, Value::real(0), Value::real(3)];
    let env = TestEnv::default();

    let r = vector_copy_inline(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v, Value::Unspecified);
    assert_eq!(args[0].to_string(), "#(1 2 1 2 \"foo\")");
    assert!(args[0].is(&args[2]));
}

#[test]
fn string_fill_all() {
    let args = [Value::string_mut("foobar"), Value::Character('x')];
    let env = TestEnv::default();

    let r = string_fill(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v, Value::Unspecified);
    assert_eq!(args[0].to_string(), "\"xxxxxx\"");
}

#[test]
fn string_fill_to_end() {
    let args = [
        Value::string_mut("foobar"),
        Value::Character('x'),
        Value::real(2),
    ];
    let env = TestEnv::default();

    let r = string_fill(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v, Value::Unspecified);
    assert_eq!(args[0].to_string(), "\"foxxxx\"");
}

#[test]
fn string_fill_partial() {
    let args = [
        Value::string_mut("foobar"),
        Value::Character('x'),
        Value::real(2),
        Value::real(5),
    ];
    let env = TestEnv::default();

    let r = string_fill(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v, Value::Unspecified);
    assert_eq!(args[0].to_string(), "\"foxxxr\"");
}

#[test]
fn string_fill_none() {
    let args = [
        Value::string_mut("foobar"),
        Value::Character('x'),
        Value::real(0),
        Value::real(0),
    ];
    let env = TestEnv::default();

    let r = string_fill(&args, &env.new_frame());

    let v = ok_or_fail!(r);
    assert_eq!(v, Value::Unspecified);
    assert_eq!(args[0].to_string(), "\"foobar\"");
}
