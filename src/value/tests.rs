use super::*;

mod display {
    use super::*;

    #[test]
    fn bytevector_display() {
        let v = Value::ByteVector([1, 2, 3].into());

        assert_eq!(v.to_string(), "#u8(1 2 3)");
    }

    #[test]
    fn empty_bytevector_display() {
        let v = Value::ByteVector([].into());

        assert_eq!(v.to_string(), "#u8()");
    }

    #[test]
    fn bytevector_typename() {
        let v = Value::ByteVector([].into());

        assert_eq!(v.as_typename().to_string(), "bytevector");
    }

    #[test]
    fn boolean_true() {
        let v = Value::Boolean(true);

        assert_eq!(v.to_string(), "#t");
    }

    #[test]
    fn boolean_false() {
        let v = Value::Boolean(false);

        assert_eq!(v.to_string(), "#f");
    }

    #[test]
    fn boolean_typename() {
        let v = Value::Boolean(false);

        assert_eq!(v.as_typename().to_string(), "boolean");
    }

    #[test]
    fn character_typename() {
        let v = Value::Character('a');

        assert_eq!(v.as_typename().to_string(), "character");
    }

    #[test]
    fn display_typename() {
        let v = Value::real(42);

        assert_eq!(v.as_typename().to_string(), "number");
    }

    #[test]
    fn string_typename() {
        let v = Value::string("foo");

        assert_eq!(v.as_typename().to_string(), "string");
    }

    #[test]
    fn symbol_typename() {
        let v = Value::symbol("foo");

        assert_eq!(v.as_typename().to_string(), "symbol");
    }

    #[test]
    fn pair_typename() {
        let v = Value::cons(Value::Boolean(true), Value::Character('a'));

        assert_eq!(v.as_typename().to_string(), "pair");
    }

    #[test]
    fn list_typename() {
        let v = zlist![Value::Boolean(true), Value::Character('a')];

        assert_eq!(v.as_typename().to_string(), "list");
    }

    #[test]
    fn empty_list_typename() {
        let v = zlist![];

        assert_eq!(v.as_typename().to_string(), "list");
    }

    #[test]
    fn empty_list_display() {
        let v = zlist![];

        assert_eq!(v.to_string(), "()")
    }

    #[test]
    fn unspecified_typename() {
        let v = Value::Unspecified;

        assert_eq!(v.as_typename().to_string(), "unspecified");
    }

    #[test]
    fn unspecified_display() {
        let v = Value::Unspecified;

        assert_eq!(v.to_string(), "#<unspecified>");
    }

    #[test]
    fn vector_typename() {
        let v = Value::vector([]);

        assert_eq!(v.as_typename().to_string(), "vector");
    }

    #[test]
    fn vector_display() {
        let v = Value::vector([
            Value::string("foo"),
            Value::symbol("a"),
            zlist![Value::Boolean(true), Value::Character('a')],
        ]);

        assert_eq!(v.to_string(), "#(\"foo\" a (#t #\\a))");
    }

    #[test]
    fn empty_vector_display() {
        let v = Value::vector([]);

        assert_eq!(v.to_string(), "#()");
    }

    #[test]
    fn procedure_typename() {
        let v = Value::Procedure(
            Procedure::intrinsic("foo", 0..0, |_, _| Ok(Value::Unspecified)).into(),
        );

        assert_eq!(v.as_typename().to_string(), "procedure");
    }

    #[test]
    fn procedure_display() {
        let v = Value::Procedure(
            Procedure::intrinsic("foo", 0..0, |_, _| Ok(Value::Unspecified)).into(),
        );

        assert_eq!(v.to_string(), "#<procedure foo>");
    }
}

mod character {
    use super::*;

    #[test]
    fn display_ascii() {
        let v = Value::Character('a');

        assert_eq!(v.to_string(), "#\\a");
    }

    #[test]
    fn display_extended_char() {
        let v = Value::Character('λ');

        assert_eq!(v.to_string(), "#\\λ");
    }

    #[test]
    fn display_emoji() {
        let v = Value::Character('🦀');

        assert_eq!(v.to_string(), "#\\🦀");
    }

    #[test]
    fn display_control_picture() {
        let v = Value::Character('\u{2401}');

        assert_eq!(v.to_string(), "#\\␁");
    }

    #[test]
    fn display_replacement_char() {
        let v = Value::Character('\u{fffd}');

        assert_eq!(v.to_string(), "#\\�");
    }

    #[test]
    fn display_one_digit_hex() {
        let v = Value::Character('\x0c');

        assert_eq!(v.to_string(), "#\\xc");
    }

    #[test]
    fn display_hex_uses_lowercase() {
        let v = Value::Character('\x0C');

        assert_eq!(v.to_string(), "#\\xc");
    }

    #[test]
    fn display_two_digit_hex() {
        let v = Value::Character('\x1d');

        assert_eq!(v.to_string(), "#\\x1d");
    }

    #[test]
    fn display_four_digit_hex() {
        let v = Value::Character('\u{fff9}');

        assert_eq!(v.to_string(), "#\\xfff9");
    }

    #[test]
    fn display_special_purpose_plane() {
        let v = Value::Character('\u{e0001}');

        assert_eq!(v.to_string(), "#\\xe0001");
    }

    #[test]
    fn display_private_use_plane() {
        let v = Value::Character('\u{100001}');

        assert_eq!(v.to_string(), "#\\x100001");
    }

    #[test]
    fn display_character_name() {
        check_character_list(&[
            ('\x07', "alarm"),
            ('\x08', "backspace"),
            ('\x7f', "delete"),
            ('\x1b', "escape"),
            ('\n', "newline"),
            ('\0', "null"),
            ('\r', "return"),
            (' ', "space"),
            ('\t', "tab"),
        ]);
    }

    #[test]
    fn display_string_escape_characters() {
        check_character_list(&[('"', "\""), ('\'', "'"), ('\\', "\\"), ('\n', "newline")]);
    }

    fn check_character_list(cases: &[(char, &str)]) {
        for &(inp, exp) in cases {
            let v = Value::Character(inp);

            assert_eq!(v.to_string(), format!("#\\{exp}"));
        }
    }
}

mod number {
    use super::*;
    use crate::testutil::ok_or_fail;

    #[test]
    fn display_int() {
        let v = Value::real(23);

        assert_eq!(v.to_string(), "23");
    }

    #[test]
    fn display_float() {
        let v = Value::real(234.23);

        assert_eq!(v.to_string(), "234.23");
    }

    #[test]
    fn display_rational() {
        let r = ok_or_fail!(Real::reduce(3, 4));
        let v = Value::real(r);

        assert_eq!(v.to_string(), "3/4");
    }

    #[test]
    fn display_complex() {
        let v = Value::Number(Number::complex(4, 5));

        assert_eq!(v.to_string(), "4+5i");
    }
}

mod string {
    use super::*;

    #[test]
    fn display_empty() {
        let v = Value::string("");

        assert_eq!(v.to_string(), "\"\"");
    }

    #[test]
    fn display_alphanumeric() {
        let v = Value::string("abc123!@#");

        assert_eq!(v.to_string(), "\"abc123!@#\"");
    }

    #[test]
    fn display_extended() {
        let v = Value::string("λ");

        assert_eq!(v.to_string(), "\"λ\"");
    }

    #[test]
    fn display_emoji() {
        let v = Value::string("🦀");

        assert_eq!(v.to_string(), "\"🦀\"");
    }

    #[test]
    fn display_control_picture() {
        let v = Value::string("\u{2401}");

        assert_eq!(v.to_string(), "\"␁\"");
    }

    #[test]
    fn display_replacement() {
        let v = Value::string("\u{fffd}");

        assert_eq!(v.to_string(), "\"�\"");
    }

    #[test]
    fn display_null() {
        let v = Value::string("\0");

        assert_eq!(v.to_string(), "\"\\x0;\"");
    }

    #[test]
    fn display_pipe() {
        let v = Value::string("|");

        assert_eq!(v.to_string(), "\"|\"");
    }

    #[test]
    fn display_one_digit_hex() {
        let v = Value::string("\x0c");

        assert_eq!(v.to_string(), "\"\\xc;\"");
    }

    #[test]
    fn display_hex_uses_lowercase() {
        let v = Value::string("\x0C");

        assert_eq!(v.to_string(), "\"\\xc;\"");
    }

    #[test]
    fn display_two_digit_hex() {
        let v = Value::string("\x1d");

        assert_eq!(v.to_string(), "\"\\x1d;\"");
    }

    #[test]
    fn display_four_digit_hex() {
        let v = Value::string("\u{fff9}");

        assert_eq!(v.to_string(), "\"\\xfff9;\"");
    }

    #[test]
    fn display_special_purpose_plane() {
        let v = Value::string("\u{e0001}");

        assert_eq!(v.to_string(), "\"\\xe0001;\"");
    }

    #[test]
    fn display_private_use_plane() {
        let v = Value::string("\u{100001}");

        assert_eq!(v.to_string(), "\"\\x100001;\"");
    }

    #[test]
    fn display_literal_endline() {
        let v = Value::string(
            "foo
bar",
        );

        assert_eq!(v.to_string(), "\"foo\\nbar\"");
    }

    #[test]
    fn display_escape_sequences() {
        check_escape_sequence(&[
            ("\x07", "\\a"),
            ("\x08", "\\b"),
            ("\t", "\\t"),
            ("\n", "\\n"),
            ("\r", "\\r"),
            ("\"", "\\\""),
            ("\\", "\\\\"),
        ]);
    }

    fn check_escape_sequence(cases: &[(&str, &str)]) {
        for &(inp, exp) in cases {
            let v = Value::string(inp);

            assert_eq!(v.to_string(), format!("\"{exp}\""));
        }
    }
}

// NOTE: most of these tests adapted from string module above
mod symbol {
    use super::*;

    #[test]
    fn simple() {
        let v = Value::symbol("foo");

        assert_eq!(v.to_string(), "foo");
    }

    #[test]
    fn empty() {
        let v = Value::symbol("");

        assert_eq!(v.to_string(), "||");
    }

    #[test]
    fn whitespace() {
        let v = Value::symbol("foo bar");

        assert_eq!(v.to_string(), "|foo bar|");
    }

    #[test]
    fn only_whitespace() {
        let v = Value::symbol("   ");

        assert_eq!(v.to_string(), "|   |");
    }

    #[test]
    fn alphanumeric() {
        let v = Value::symbol("abc123!@$^&");

        assert_eq!(v.to_string(), "abc123!@$^&");
    }

    #[test]
    fn special_lex_chars() {
        let cases = ['(', ')', '\'', '`', '#', '"', ';', '.', ','];
        for case in cases {
            let s = format!("abc{case}123");

            let v = Value::symbol(s.clone());

            let expected = if case == '.' { s } else { format!("|{s}|") };
            assert_eq!(v.to_string(), expected);
        }
    }

    #[test]
    fn starts_with_number() {
        let v = Value::symbol("123abc");

        assert_eq!(v.to_string(), "|123abc|");
    }

    #[test]
    fn starts_with_peculiar() {
        let cases = ['+', '-', '.'];
        for case in cases {
            let s = format!("{case}foo");

            let v = Value::symbol(s.clone());

            assert_eq!(v.to_string(), s);
        }
    }

    #[test]
    fn null() {
        let v = Value::symbol("\0");

        assert_eq!(v.to_string(), "|\\x0;|");
    }

    #[test]
    fn pipe() {
        let v = Value::symbol("|");

        assert_eq!(v.to_string(), "|\\||");
    }

    #[test]
    fn one_digit_hex() {
        let v = Value::symbol("\x0c");

        assert_eq!(v.to_string(), "|\\xc;|");
    }

    #[test]
    fn hex_uses_lowercase() {
        let v = Value::symbol("\x0C");

        assert_eq!(v.to_string(), "|\\xc;|");
    }

    #[test]
    fn two_digit_hex() {
        let v = Value::symbol("\x1d");

        assert_eq!(v.to_string(), "|\\x1d;|");
    }

    #[test]
    fn four_digit_hex() {
        let v = Value::symbol("\u{fff9}");

        assert_eq!(v.to_string(), "|\\xfff9;|");
    }

    #[test]
    fn special_purpose_plane() {
        let v = Value::symbol("\u{e0001}");

        assert_eq!(v.to_string(), "|\\xe0001;|");
    }

    #[test]
    fn private_use_plane() {
        let v = Value::symbol("\u{100001}");

        assert_eq!(v.to_string(), "|\\x100001;|");
    }

    #[test]
    fn literal_endline() {
        let v = Value::symbol(
            "foo
bar",
        );

        assert_eq!(v.to_string(), "|foo\\nbar|");
    }

    #[test]
    fn escape_sequences() {
        check_escape_sequence(&[
            ("\x07", "\\a"),
            ("\x08", "\\b"),
            ("\t", "\\t"),
            ("\n", "\\n"),
            ("\r", "\\r"),
            ("\"", "\""),
            ("\\", "\\\\"),
        ]);
    }

    fn check_escape_sequence(cases: &[(&str, &str)]) {
        for &(inp, exp) in cases {
            let v = Value::symbol(inp);

            assert_eq!(v.to_string(), format!("|{exp}|"));
        }
    }

    /***
     * TODO:
     * some of these test may no longer be verbatim after adding unicode support
     ***/

    #[test]
    fn extended() {
        let v = Value::symbol("λ");

        assert_eq!(v.to_string(), "|λ|");
    }

    #[test]
    fn emoji() {
        let v = Value::symbol("🦀");

        assert_eq!(v.to_string(), "|🦀|");
    }

    #[test]
    fn control_picture() {
        let v = Value::symbol("\u{2401}");

        assert_eq!(v.to_string(), "|␁|");
    }

    #[test]
    fn replacement() {
        let v = Value::symbol("\u{fffd}");

        assert_eq!(v.to_string(), "|�|");
    }
}

mod pair {
    use super::*;

    #[test]
    fn pair_is_not_list() {
        // (#t . #f)
        let p = Pair::cons(Value::Boolean(true), Value::Boolean(false));

        assert!(!p.is_list());
    }

    #[test]
    fn empty_cdr_is_list() {
        // (#t)
        let p = Pair::cons(Value::Boolean(true), Value::null());

        assert!(p.is_list());
    }

    #[test]
    fn nested_is_list() {
        // (1 2 3)
        let p = Pair::cons(
            Value::real(1),
            Value::cons(Value::real(2), Value::cons(Value::real(3), Value::null())),
        );

        assert!(p.is_list());
    }

    #[test]
    fn improper_list_is_not_list() {
        // (1 2 . 3)
        let p = Pair::cons(Value::real(1), Value::cons(Value::real(2), Value::real(3)));

        assert!(!p.is_list());
    }

    #[test]
    fn list_containing_pair_is_list() {
        // ((1 . 2) 3)
        let p = Pair::cons(
            Value::cons(Value::real(1), Value::real(2)),
            Value::cons(Value::real(3), Value::null()),
        );

        assert!(p.is_list());
    }

    #[test]
    fn list_containing_empty_list_is_list() {
        // (())
        let p = Pair::cons(Value::null(), Value::null());

        assert!(p.is_list());
    }

    #[test]
    fn pair_display() {
        let v = Value::cons(Value::Boolean(true), Value::Boolean(false));

        assert_eq!(v.to_string(), "(#t . #f)")
    }

    #[test]
    fn empty_cdr_display() {
        let v = Value::cons(Value::Boolean(true), Value::null());

        assert_eq!(v.to_string(), "(#t)")
    }

    #[test]
    fn list_display() {
        let v = Value::cons(
            Value::real(1),
            Value::cons(Value::real(2), Value::cons(Value::real(3), Value::null())),
        );

        assert_eq!(v.to_string(), "(1 2 3)")
    }

    #[test]
    fn improper_list_display() {
        let v = Value::cons(Value::real(1), Value::cons(Value::real(2), Value::real(3)));

        assert_eq!(v.to_string(), "(1 2 . 3)")
    }

    #[test]
    fn list_containing_pair_display() {
        let v = Value::cons(
            Value::cons(Value::real(1), Value::real(2)),
            Value::cons(Value::real(3), Value::null()),
        );

        assert_eq!(v.to_string(), "((1 . 2) 3)")
    }

    #[test]
    fn list_containing_list_display() {
        let v = Value::cons(
            Value::real(1),
            Value::cons(
                Value::cons(Value::real(2), Value::cons(Value::real(3), Value::null())),
                Value::null(),
            ),
        );

        assert_eq!(v.to_string(), "(1 (2 3))")
    }

    #[test]
    fn list_containing_empty_list_display() {
        let v = Value::cons(Value::null(), Value::null());

        assert_eq!(v.to_string(), "(())")
    }
}

mod list {
    use super::*;

    #[test]
    fn empty() {
        let lst = zlist![];

        assert_eq!(lst.to_string(), "()");
    }

    #[test]
    fn one() {
        let lst = zlist![Value::real(5)];

        assert_eq!(lst.to_string(), "(5)");
    }

    #[test]
    fn three() {
        let lst = zlist![Value::real(5), Value::symbol("a"), Value::Boolean(true)];

        assert_eq!(lst.to_string(), "(5 a #t)");
    }

    #[test]
    fn nested() {
        let lst = zlist![
            Value::real(5),
            zlist![Value::symbol("a"), Value::Boolean(true)],
        ];

        assert_eq!(lst.to_string(), "(5 (a #t))");
    }

    #[test]
    fn ctor_empty() {
        let lst = Value::list(vec![]);

        assert_eq!(lst.to_string(), "()");
    }

    #[test]
    fn ctor_vec() {
        let lst = Value::list(vec![
            Value::real(5),
            Value::symbol("a"),
            Value::Boolean(true),
        ]);

        assert_eq!(lst.to_string(), "(5 a #t)");
    }

    #[test]
    fn ctor_slice() {
        let lst = Value::list([Value::real(5), Value::symbol("a"), Value::Boolean(true)]);

        assert_eq!(lst.to_string(), "(5 a #t)");
    }

    #[test]
    fn improper_ctor_empty() {
        let lst = Value::improper_list(vec![]);

        assert_eq!(lst.to_string(), "()");
    }

    #[test]
    fn improper_ctor_single() {
        let lst = Value::improper_list(vec![Value::real(5)]);

        assert!(matches!(lst, Value::Number(_)));
        assert_eq!(lst.to_string(), "5");
    }

    #[test]
    fn improper_ctor_vec() {
        let lst = Value::improper_list(vec![
            Value::real(5),
            Value::symbol("a"),
            Value::Boolean(true),
        ]);

        assert_eq!(lst.to_string(), "(5 a . #t)");
    }

    #[test]
    fn improper_ctor_slice() {
        let lst = Value::improper_list([Value::real(5), Value::symbol("a"), Value::Boolean(true)]);

        assert_eq!(lst.to_string(), "(5 a . #t)");
    }
}

mod condition {
    use super::*;

    #[test]
    fn display_empty_condition() {
        let c = Condition {
            kind: ConditionKind::General,
            msg: "foo".into(),
            irritants: Value::Unspecified,
        };

        assert_eq!(c.to_string(), "#<exception \"foo\">");
    }

    #[test]
    fn display_list_irritants() {
        let c = Condition {
            kind: ConditionKind::General,
            msg: "foo".into(),
            irritants: zlist![Value::symbol("a"), Value::real(5)],
        };

        assert_eq!(c.to_string(), "#<exception \"foo\" (a 5)>");
    }

    #[test]
    fn display_value_irritant() {
        let c = Condition {
            kind: ConditionKind::General,
            msg: "foo".into(),
            irritants: Value::Boolean(true),
        };

        assert_eq!(c.to_string(), "#<exception \"foo\" #t>");
    }

    #[test]
    fn display_env_condition() {
        let c = Condition {
            kind: ConditionKind::Env,
            msg: "foo".into(),
            irritants: Value::Unspecified,
        };

        assert_eq!(c.to_string(), "#<env-error \"foo\">");
    }

    #[test]
    fn display_file_condition() {
        let c = Condition {
            kind: ConditionKind::File,
            msg: "foo".into(),
            irritants: Value::Unspecified,
        };

        assert_eq!(c.to_string(), "#<file-error \"foo\">");
    }

    #[test]
    fn display_read_condition() {
        let c = Condition {
            kind: ConditionKind::Read,
            msg: "foo".into(),
            irritants: Value::Unspecified,
        };

        assert_eq!(c.to_string(), "#<read-error \"foo\">");
    }

    #[test]
    fn display_system_condition() {
        let c = Condition {
            kind: ConditionKind::System,
            msg: "foo".into(),
            irritants: Value::Unspecified,
        };

        assert_eq!(c.to_string(), "#<sys-error \"foo\">");
    }
}

mod cloning {
    use super::*;
    use crate::testutil::extract_or_fail;

    #[test]
    fn clone_with_underlying_cloneable() {
        let v = Value::symbol("foo");

        let c = v.clone();

        assert!(Rc::ptr_eq(
            &extract_or_fail!(v, Value::Symbol),
            &extract_or_fail!(c, Value::Symbol)
        ));
    }

    #[test]
    fn clone_with_underlying_move_only() {
        let v = zlist![Value::Unspecified];
        dbg!(&v);

        let c = v.clone();

        assert!(Rc::ptr_eq(
            &extract_or_fail!(extract_or_fail!(v, Value::Pair), Some),
            &extract_or_fail!(extract_or_fail!(c, Value::Pair), Some)
        ));
    }

    #[test]
    fn clone_with_underlying_primitive_slice() {
        let v = Value::ByteVector([1, 2, 3].into());

        let c = v.clone();

        assert!(Rc::ptr_eq(
            &extract_or_fail!(v, Value::ByteVector),
            &extract_or_fail!(c, Value::ByteVector)
        ));
    }
}
