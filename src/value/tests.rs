use super::*;
use crate::{
    eval::Binding,
    string::SymbolTable,
    testutil::{empty_procedure_body, extract_or_fail, ok_or_fail, some_or_fail, zlist_mut},
};

mod display {
    use super::*;
    use crate::eval::Lambda;

    #[test]
    fn bytevector_display() {
        let v = Value::ByteVector([1, 2, 3].into());

        assert_eq!(v.as_datum().to_string(), "#u8(1 2 3)");
    }

    #[test]
    fn empty_bytevector_display() {
        let v = Value::ByteVector([].into());

        assert_eq!(v.as_datum().to_string(), "#u8()");
    }

    #[test]
    fn bytevectormut_display() {
        let v = Value::bytevector_mut([3, 2, 1]);

        assert_eq!(v.as_datum().to_string(), "#u8(3 2 1)");
    }

    #[test]
    fn bytevector_typename() {
        let v = Value::ByteVector([].into());

        assert_eq!(v.as_typename().to_string(), "bytevector");
    }

    #[test]
    fn bytevectormut_typename() {
        let v = Value::bytevector_mut([]);

        assert_eq!(v.as_typename().to_string(), "bytevector");
    }

    #[test]
    fn boolean_true() {
        let v = Value::Boolean(true);

        assert_eq!(v.as_datum().to_string(), "#t");
    }

    #[test]
    fn boolean_false() {
        let v = Value::Boolean(false);

        assert_eq!(v.as_datum().to_string(), "#f");
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
    fn eof_typename() {
        let v = Value::Eof;

        assert_eq!(v.as_typename().to_string(), "end-of-file");
    }

    #[test]
    fn eof_display() {
        let v = Value::Eof;

        assert_eq!(v.as_datum().to_string(), "#<eof>");
    }

    #[test]
    fn error_typename() {
        let v = Value::Error(Condition::system_error("foo").into());

        assert_eq!(v.as_typename().to_string(), "error condition");
    }

    #[test]
    fn number_typename() {
        let v = Value::real(42);

        assert_eq!(v.as_typename().to_string(), "number");
    }

    #[test]
    fn string_typename() {
        let v = Value::string("foo");

        assert_eq!(v.as_typename().to_string(), "string");
    }

    #[test]
    fn stringmut_typename() {
        let v = Value::string_mut("foo");

        assert_eq!(v.as_typename().to_string(), "string");
    }

    #[test]
    fn symbol_typename() {
        let sym = SymbolTable::default();

        let v = Value::Symbol(sym.get("foo"));

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
    fn pairmut_typename() {
        let v = Value::cons_mut(Value::Boolean(true), Value::Character('a'));

        assert_eq!(v.as_typename().to_string(), "pair");
    }

    #[test]
    fn listmut_typename() {
        let v = zlist_mut![Value::Boolean(true), Value::Character('a')];

        assert_eq!(v.as_typename().to_string(), "list");
    }

    #[test]
    fn empty_list_typename() {
        let v = zlist![];

        assert_eq!(v.as_typename().to_string(), "null");
    }

    #[test]
    fn empty_list_display() {
        let v = zlist![];

        assert_eq!(v.as_datum().to_string(), "()")
    }

    #[test]
    fn unspecified_typename() {
        let v = Value::Unspecified;

        assert_eq!(v.as_typename().to_string(), "unspecified");
    }

    #[test]
    fn unspecified_display() {
        let v = Value::Unspecified;

        assert_eq!(v.as_datum().to_string(), "#<unspecified>");
    }

    #[test]
    fn vector_typename() {
        let v = Value::vector([]);

        assert_eq!(v.as_typename().to_string(), "vector");
    }

    #[test]
    fn vector_display() {
        let sym = SymbolTable::default();
        let v = Value::vector([
            Value::string("foo"),
            Value::Symbol(sym.get("a")),
            zlist![Value::Boolean(true), Value::Character('a')],
        ]);

        assert_eq!(v.as_datum().to_string(), "#(\"foo\" a (#t #\\a))");
        assert_eq!(v.as_simple_datum().to_string(), v.as_datum().to_string());
        assert_eq!(v.as_shared_datum().to_string(), v.as_datum().to_string());
    }

    #[test]
    fn empty_vector_display() {
        let v = Value::vector([]);

        assert_eq!(v.as_datum().to_string(), "#()");
        assert_eq!(v.as_simple_datum().to_string(), v.as_datum().to_string());
        assert_eq!(v.as_shared_datum().to_string(), v.as_datum().to_string());
    }

    #[test]
    fn vectormut_typename() {
        let v = Value::vector_mut([]);

        assert_eq!(v.as_typename().to_string(), "vector");
    }

    #[test]
    fn vectormut_display() {
        let sym = SymbolTable::default();
        let v = Value::vector_mut([
            Value::string("foo"),
            Value::Symbol(sym.get("a")),
            zlist![Value::Boolean(true), Value::Character('a')],
        ]);

        assert_eq!(v.as_datum().to_string(), "#(\"foo\" a (#t #\\a))");
        assert_eq!(v.as_simple_datum().to_string(), v.as_datum().to_string());
        assert_eq!(v.as_shared_datum().to_string(), v.as_datum().to_string());
    }

    #[test]
    fn cyclic_vector_self_display() {
        // #0=#(1 2 #0#)
        let vec = RefCell::new(vec![Value::real(1), Value::real(2)]).into();
        let v = Value::VectorMut(Rc::clone(&vec));
        vec.borrow_mut().push(v.clone());

        assert_eq!(v.as_datum().to_string(), "#0=#(1 2 #0#)");
        assert_eq!(v.as_shared_datum().to_string(), v.as_datum().to_string());
    }

    #[test]
    fn partly_cyclic_vector_display() {
        // #(1 2 #0=#(3 4 #0#))
        let vec = RefCell::new(vec![Value::real(3), Value::real(4)]).into();
        let head = Value::VectorMut(Rc::clone(&vec));
        vec.borrow_mut().push(head.clone());
        let v = Value::vector([Value::real(1), Value::real(2), head.clone()]);

        assert_eq!(v.as_datum().to_string(), "#(1 2 #0=#(3 4 #0#))");
        assert_eq!(v.as_shared_datum().to_string(), v.as_datum().to_string());
    }

    #[test]
    fn multiple_cyclic_vector_display() {
        // #0=#(1 2 #0# 3 #0#)
        let vec = RefCell::new(vec![Value::real(1), Value::real(2)]).into();
        let v = Value::VectorMut(Rc::clone(&vec));
        vec.borrow_mut().push(v.clone());
        vec.borrow_mut().push(Value::real(3));
        vec.borrow_mut().push(v.clone());

        assert_eq!(v.as_datum().to_string(), "#0=#(1 2 #0# 3 #0#)");
        assert_eq!(v.as_shared_datum().to_string(), v.as_datum().to_string());
    }

    #[test]
    fn intrinsic_typename() {
        let sym = SymbolTable::default();
        let v = Value::Intrinsic(
            Intrinsic {
                arity: 0..0,
                def: |_, _| Ok(Value::Unspecified),
                name: sym.get("foo"),
            }
            .into(),
        );

        assert_eq!(v.as_typename().to_string(), "intrinsic");
    }

    #[test]
    fn intrinsic_display() {
        let sym = SymbolTable::default();
        let v = Value::Intrinsic(
            Intrinsic {
                arity: 0..0,
                def: |_, _| Ok(Value::Unspecified),
                name: sym.get("foo"),
            }
            .into(),
        );

        assert_eq!(v.as_datum().to_string(), "#<intrinsic foo>");
    }

    #[test]
    fn procedure_typename() {
        let lm = ok_or_fail!(Lambda::new([], None, empty_procedure_body()));
        let v = Value::procedure(Procedure::new(lm, Binding::default(), None));

        assert_eq!(v.as_typename().to_string(), "procedure");
    }

    #[test]
    fn procedure_display() {
        let sym = SymbolTable::default();
        let lm = ok_or_fail!(Lambda::new([], None, empty_procedure_body()));
        let v = Value::procedure(Procedure::new(lm, Binding::default(), Some(sym.get("foo"))));

        assert_eq!(v.as_datum().to_string(), "#<procedure foo>");
    }

    #[test]
    fn port_typename() {
        let v = Value::port_stdout(false);

        assert_eq!(v.as_typename().to_string(), "port");
    }

    #[test]
    fn port_display() {
        let v = Value::port_stdout(false);

        assert_eq!(v.as_datum().to_string(), "#<port stdout>");
    }
}

mod character {
    use super::*;

    #[test]
    fn display_ascii() {
        let v = Value::Character('a');

        assert_eq!(v.as_datum().to_string(), "#\\a");
    }

    #[test]
    fn display_extended_char() {
        let v = Value::Character('λ');

        assert_eq!(v.as_datum().to_string(), "#\\λ");
    }

    #[test]
    fn display_emoji() {
        let v = Value::Character('🦀');

        assert_eq!(v.as_datum().to_string(), "#\\🦀");
    }

    #[test]
    fn display_control_picture() {
        let v = Value::Character('\u{2401}');

        assert_eq!(v.as_datum().to_string(), "#\\␁");
    }

    #[test]
    fn display_replacement_char() {
        let v = Value::Character('\u{fffd}');

        assert_eq!(v.as_datum().to_string(), "#\\�");
    }

    #[test]
    fn display_one_digit_hex() {
        let v = Value::Character('\x0c');

        assert_eq!(v.as_datum().to_string(), "#\\xc");
    }

    #[test]
    fn display_hex_uses_lowercase() {
        let v = Value::Character('\x0C');

        assert_eq!(v.as_datum().to_string(), "#\\xc");
    }

    #[test]
    fn display_two_digit_hex() {
        let v = Value::Character('\x1d');

        assert_eq!(v.as_datum().to_string(), "#\\x1d");
    }

    #[test]
    fn display_four_digit_hex() {
        let v = Value::Character('\u{fff9}');

        assert_eq!(v.as_datum().to_string(), "#\\xfff9");
    }

    #[test]
    fn display_special_purpose_plane() {
        let v = Value::Character('\u{e0001}');

        assert_eq!(v.as_datum().to_string(), "#\\xe0001");
    }

    #[test]
    fn display_private_use_plane() {
        let v = Value::Character('\u{100001}');

        assert_eq!(v.as_datum().to_string(), "#\\x100001");
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

            assert_eq!(v.as_datum().to_string(), format!("#\\{exp}"));
        }
    }
}

mod number {
    use super::*;

    #[test]
    fn display_int() {
        let v = Value::real(23);

        assert_eq!(v.as_datum().to_string(), "23");
    }

    #[test]
    fn display_float() {
        let v = Value::real(234.23);

        assert_eq!(v.as_datum().to_string(), "234.23");
    }

    #[test]
    fn display_rational() {
        let r = ok_or_fail!(Real::reduce(3, 4));
        let v = Value::real(r);

        assert_eq!(v.as_datum().to_string(), "3/4");
    }

    #[test]
    fn display_complex() {
        let v = Value::Number(Number::complex(4, 5));

        assert_eq!(v.as_datum().to_string(), "4+5i");
    }
}

mod string {
    use super::*;

    #[test]
    fn display_empty() {
        let v = Value::string("");

        assert_eq!(v.as_datum().to_string(), "\"\"");
    }

    #[test]
    fn display_alphanumeric() {
        let v = Value::string("abc123!@#");

        assert_eq!(v.as_datum().to_string(), "\"abc123!@#\"");
    }

    #[test]
    fn display_extended() {
        let v = Value::string("λ");

        assert_eq!(v.as_datum().to_string(), "\"λ\"");
    }

    #[test]
    fn display_emoji() {
        let v = Value::string("🦀");

        assert_eq!(v.as_datum().to_string(), "\"🦀\"");
    }

    #[test]
    fn display_control_picture() {
        let v = Value::string("\u{2401}");

        assert_eq!(v.as_datum().to_string(), "\"␁\"");
    }

    #[test]
    fn display_replacement() {
        let v = Value::string("\u{fffd}");

        assert_eq!(v.as_datum().to_string(), "\"�\"");
    }

    #[test]
    fn display_null() {
        let v = Value::string("\0");

        assert_eq!(v.as_datum().to_string(), "\"\\x0;\"");
    }

    #[test]
    fn display_pipe() {
        let v = Value::string("|");

        assert_eq!(v.as_datum().to_string(), "\"|\"");
    }

    #[test]
    fn display_one_digit_hex() {
        let v = Value::string("\x0c");

        assert_eq!(v.as_datum().to_string(), "\"\\xc;\"");
    }

    #[test]
    fn display_hex_uses_lowercase() {
        let v = Value::string("\x0C");

        assert_eq!(v.as_datum().to_string(), "\"\\xc;\"");
    }

    #[test]
    fn display_two_digit_hex() {
        let v = Value::string("\x1d");

        assert_eq!(v.as_datum().to_string(), "\"\\x1d;\"");
    }

    #[test]
    fn display_four_digit_hex() {
        let v = Value::string("\u{fff9}");

        assert_eq!(v.as_datum().to_string(), "\"\\xfff9;\"");
    }

    #[test]
    fn display_special_purpose_plane() {
        let v = Value::string("\u{e0001}");

        assert_eq!(v.as_datum().to_string(), "\"\\xe0001;\"");
    }

    #[test]
    fn display_private_use_plane() {
        let v = Value::string("\u{100001}");

        assert_eq!(v.as_datum().to_string(), "\"\\x100001;\"");
    }

    #[test]
    fn display_literal_endline() {
        let v = Value::string(
            "foo
bar",
        );

        assert_eq!(v.as_datum().to_string(), "\"foo\\nbar\"");
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

    #[test]
    fn display_string_mut() {
        let v = Value::string_mut("abc123!@#");

        assert_eq!(v.as_datum().to_string(), "\"abc123!@#\"");
    }

    fn check_escape_sequence(cases: &[(&str, &str)]) {
        for &(inp, exp) in cases {
            let v = Value::string(inp);

            assert_eq!(v.as_datum().to_string(), format!("\"{exp}\""));
        }
    }
}

// NOTE: most of these tests adapted from string module above
mod symbol {
    use super::*;

    #[test]
    fn simple() {
        let sym = SymbolTable::default();
        let v = Value::Symbol(sym.get("foo"));

        assert_eq!(v.as_datum().to_string(), "foo");
    }

    #[test]
    fn empty() {
        let sym = SymbolTable::default();
        let v = Value::Symbol(sym.get(""));

        assert_eq!(v.as_datum().to_string(), "||");
    }

    #[test]
    fn whitespace() {
        let sym = SymbolTable::default();
        let v = Value::Symbol(sym.get("foo bar"));

        assert_eq!(v.as_datum().to_string(), "|foo bar|");
    }

    #[test]
    fn only_whitespace() {
        let sym = SymbolTable::default();
        let v = Value::Symbol(sym.get("   "));

        assert_eq!(v.as_datum().to_string(), "|   |");
    }

    #[test]
    fn alphanumeric() {
        let sym = SymbolTable::default();
        let v = Value::Symbol(sym.get("abc123!@$^&"));

        assert_eq!(v.as_datum().to_string(), "abc123!@$^&");
    }

    #[test]
    fn special_lex_chars() {
        let sym = SymbolTable::default();
        let cases = ['(', ')', '\'', '`', '#', '"', ';', '.', ','];
        for case in cases {
            let s = format!("abc{case}123");

            let v = Value::Symbol(sym.get(&s));

            let expected = if case == '.' { s } else { format!("|{s}|") };
            assert_eq!(v.as_datum().to_string(), expected);
        }
    }

    #[test]
    fn starts_with_number() {
        let sym = SymbolTable::default();
        let v = Value::Symbol(sym.get("123abc"));

        assert_eq!(v.as_datum().to_string(), "|123abc|");
    }

    #[test]
    fn starts_with_peculiar() {
        let sym = SymbolTable::default();
        let cases = ['+', '-', '.'];
        for case in cases {
            let s = format!("{case}foo");

            let v = Value::Symbol(sym.get(&s));

            assert_eq!(v.as_datum().to_string(), s);
        }
    }

    #[test]
    fn null() {
        let sym = SymbolTable::default();
        let v = Value::Symbol(sym.get("\0"));

        assert_eq!(v.as_datum().to_string(), "|\\x0;|");
    }

    #[test]
    fn pipe() {
        let sym = SymbolTable::default();
        let v = Value::Symbol(sym.get("|"));

        assert_eq!(v.as_datum().to_string(), "|\\||");
    }

    #[test]
    fn one_digit_hex() {
        let sym = SymbolTable::default();
        let v = Value::Symbol(sym.get("\x0c"));

        assert_eq!(v.as_datum().to_string(), "|\\xc;|");
    }

    #[test]
    fn hex_uses_lowercase() {
        let sym = SymbolTable::default();
        let v = Value::Symbol(sym.get("\x0C"));

        assert_eq!(v.as_datum().to_string(), "|\\xc;|");
    }

    #[test]
    fn two_digit_hex() {
        let sym = SymbolTable::default();
        let v = Value::Symbol(sym.get("\x1d"));

        assert_eq!(v.as_datum().to_string(), "|\\x1d;|");
    }

    #[test]
    fn four_digit_hex() {
        let sym = SymbolTable::default();
        let v = Value::Symbol(sym.get("\u{fff9}"));

        assert_eq!(v.as_datum().to_string(), "|\\xfff9;|");
    }

    #[test]
    fn special_purpose_plane() {
        let sym = SymbolTable::default();
        let v = Value::Symbol(sym.get("\u{e0001}"));

        assert_eq!(v.as_datum().to_string(), "|\\xe0001;|");
    }

    #[test]
    fn private_use_plane() {
        let sym = SymbolTable::default();
        let v = Value::Symbol(sym.get("\u{100001}"));

        assert_eq!(v.as_datum().to_string(), "|\\x100001;|");
    }

    #[test]
    fn literal_endline() {
        let sym = SymbolTable::default();
        let v = Value::Symbol(sym.get(
            "foo
bar",
        ));

        assert_eq!(v.as_datum().to_string(), "|foo\\nbar|");
    }

    #[test]
    fn escape_sequences() {
        let sym = SymbolTable::default();
        check_escape_sequence(
            &sym,
            &[
                ("\x07", "\\a"),
                ("\x08", "\\b"),
                ("\t", "\\t"),
                ("\n", "\\n"),
                ("\r", "\\r"),
                ("\"", "\""),
                ("\\", "\\\\"),
            ],
        );
    }

    fn check_escape_sequence(sym: &SymbolTable, cases: &[(&str, &str)]) {
        for &(inp, exp) in cases {
            let v = Value::Symbol(sym.get(inp));

            assert_eq!(v.as_datum().to_string(), format!("|{exp}|"));
        }
    }

    /***
     * TODO:
     * some of these test may no longer be verbatim after adding unicode support
     ***/

    #[test]
    fn extended() {
        let sym = SymbolTable::default();
        let v = Value::Symbol(sym.get("λ"));

        assert_eq!(v.as_datum().to_string(), "|λ|");
    }

    #[test]
    fn emoji() {
        let sym = SymbolTable::default();
        let v = Value::Symbol(sym.get("🦀"));

        assert_eq!(v.as_datum().to_string(), "|🦀|");
    }

    #[test]
    fn control_picture() {
        let sym = SymbolTable::default();
        let v = Value::Symbol(sym.get("\u{2401}"));

        assert_eq!(v.as_datum().to_string(), "|␁|");
    }

    #[test]
    fn replacement() {
        let sym = SymbolTable::default();
        let v = Value::Symbol(sym.get("\u{fffd}"));

        assert_eq!(v.as_datum().to_string(), "|�|");
    }
}

mod pair {
    use super::*;
    use crate::testutil::err_or_fail;

    #[test]
    fn pair_is_not_list() {
        // (#t . #f)
        let p = Pair {
            car: Value::Boolean(true),
            cdr: Value::Boolean(false),
        };

        assert!(!p.is_list());
    }

    #[test]
    fn empty_cdr_is_list() {
        // (#t)
        let p = Pair {
            car: Value::Boolean(true),
            cdr: Value::Null,
        };

        assert!(p.is_list());
    }

    #[test]
    fn nested_is_list() {
        // (1 2 3)
        let p = Pair {
            car: Value::real(1),
            cdr: Value::cons(Value::real(2), Value::cons(Value::real(3), Value::Null)),
        };

        assert!(p.is_list());
    }

    #[test]
    fn mutable_nested_is_list() {
        // (1 2 3)
        let p = Pair {
            car: Value::real(1),
            cdr: Value::cons_mut(Value::real(2), Value::cons_mut(Value::real(3), Value::Null)),
        };

        assert!(p.is_list());
    }

    #[test]
    fn improper_list_is_not_list() {
        // (1 2 . 3)
        let p = Pair {
            car: Value::real(1),
            cdr: Value::cons(Value::real(2), Value::real(3)),
        };

        assert!(!p.is_list());
    }

    #[test]
    fn mutable_improper_list_is_not_list() {
        // (1 2 . 3)
        let p = Pair {
            car: Value::real(1),
            cdr: Value::cons_mut(Value::real(2), Value::real(3)),
        };

        assert!(!p.is_list());
    }

    #[test]
    fn list_containing_pair_is_list() {
        // ((1 . 2) 3)
        let p = Pair {
            car: Value::cons(Value::real(1), Value::real(2)),
            cdr: Value::cons(Value::real(3), Value::Null),
        };

        assert!(p.is_list());
    }

    #[test]
    fn list_containing_empty_list_is_list() {
        // (())
        let p = Pair {
            car: Value::Null,
            cdr: Value::Null,
        };

        assert!(p.is_list());
    }

    #[test]
    fn pair_display() {
        let v = Value::cons(Value::Boolean(true), Value::Boolean(false));

        assert_eq!(v.as_datum().to_string(), "(#t . #f)");
        assert_eq!(v.as_simple_datum().to_string(), v.as_datum().to_string());
        assert_eq!(v.as_shared_datum().to_string(), v.as_datum().to_string());
    }

    #[test]
    fn empty_cdr_display() {
        let v = Value::cons(Value::Boolean(true), Value::Null);

        assert_eq!(v.as_datum().to_string(), "(#t)");
        assert_eq!(v.as_simple_datum().to_string(), v.as_datum().to_string());
        assert_eq!(v.as_shared_datum().to_string(), v.as_datum().to_string());
    }

    #[test]
    fn list_display() {
        let v = Value::cons(
            Value::real(1),
            Value::cons(Value::real(2), Value::cons(Value::real(3), Value::Null)),
        );

        assert_eq!(v.as_datum().to_string(), "(1 2 3)");
        assert_eq!(v.as_simple_datum().to_string(), v.as_datum().to_string());
        assert_eq!(v.as_shared_datum().to_string(), v.as_datum().to_string());
    }

    #[test]
    fn improper_list_display() {
        let v = Value::cons(Value::real(1), Value::cons(Value::real(2), Value::real(3)));

        assert_eq!(v.as_datum().to_string(), "(1 2 . 3)");
        assert_eq!(v.as_simple_datum().to_string(), v.as_datum().to_string());
        assert_eq!(v.as_shared_datum().to_string(), v.as_datum().to_string());
    }

    #[test]
    fn list_containing_pair_display() {
        let v = Value::cons(
            Value::cons(Value::real(1), Value::real(2)),
            Value::cons(Value::real(3), Value::Null),
        );

        assert_eq!(v.as_datum().to_string(), "((1 . 2) 3)");
        assert_eq!(v.as_simple_datum().to_string(), v.as_datum().to_string());
        assert_eq!(v.as_shared_datum().to_string(), v.as_datum().to_string());
    }

    #[test]
    fn list_containing_list_display() {
        let v = Value::cons(
            Value::real(1),
            Value::cons(
                Value::cons(Value::real(2), Value::cons(Value::real(3), Value::Null)),
                Value::Null,
            ),
        );

        assert_eq!(v.as_datum().to_string(), "(1 (2 3))");
        assert_eq!(v.as_simple_datum().to_string(), v.as_datum().to_string());
        assert_eq!(v.as_shared_datum().to_string(), v.as_datum().to_string());
    }

    #[test]
    fn list_containing_empty_list_display() {
        let v = Value::cons(Value::Null, Value::Null);

        assert_eq!(v.as_datum().to_string(), "(())");
        assert_eq!(v.as_simple_datum().to_string(), v.as_datum().to_string());
        assert_eq!(v.as_shared_datum().to_string(), v.as_datum().to_string());
    }

    #[test]
    fn pair_has_no_length() {
        // (#t . #f)
        let p = Pair {
            car: Value::Boolean(true),
            cdr: Value::Boolean(false),
        };

        assert!(matches!(err_or_fail!(p.len()), InvalidList::Improper));
    }

    #[test]
    fn list_length_one() {
        // (#t)
        let p = Pair {
            car: Value::Boolean(true),
            cdr: Value::Null,
        };

        let r = p.len();

        assert_eq!(ok_or_fail!(r), 1);
    }

    #[test]
    fn list_length() {
        // (1 2 3)
        let p = Pair {
            car: Value::real(1),
            cdr: Value::cons(Value::real(2), Value::cons(Value::real(3), Value::Null)),
        };

        let r = p.len();

        assert_eq!(ok_or_fail!(r), 3);
    }

    #[test]
    fn mutable_list_length() {
        // (1 2 3)
        let p = Pair {
            car: Value::real(1),
            cdr: Value::cons_mut(Value::real(2), Value::cons_mut(Value::real(3), Value::Null)),
        };

        let r = p.len();

        assert_eq!(ok_or_fail!(r), 3);
    }

    #[test]
    fn improper_list_has_no_length() {
        // (1 2 . 3)
        let p = Pair {
            car: Value::real(1),
            cdr: Value::cons(Value::real(2), Value::real(3)),
        };

        assert!(matches!(err_or_fail!(p.len()), InvalidList::Improper));
    }

    #[test]
    fn improper_mutable_list_has_no_length() {
        // (1 2 . 3)
        let p = Pair {
            car: Value::real(1),
            cdr: Value::cons_mut(Value::real(2), Value::real(3)),
        };

        assert!(matches!(err_or_fail!(p.len()), InvalidList::Improper));
    }

    #[test]
    fn circular_list_display() {
        // #0=(1 2 3 . #0#)
        let end = RefCell::new(Pair {
            car: Value::real(3),
            cdr: Value::Null,
        })
        .into();
        let p = Pair {
            car: Value::real(1),
            cdr: Value::cons(Value::real(2), Value::PairMut(Rc::clone(&end))),
        }
        .into();
        end.borrow_mut().cdr = Value::Pair(Rc::clone(&p));

        let v = Value::Pair(Rc::clone(&p));

        assert_eq!(v.as_datum().to_string(), "#0=(1 2 3 . #0#)");
        assert_eq!(v.as_shared_datum().to_string(), v.as_datum().to_string());
    }

    #[test]
    fn circular_list_is_not_list() {
        // #0=(1 2 3 . #0#)
        let end = RefCell::new(Pair {
            car: Value::real(3),
            cdr: Value::Null,
        })
        .into();
        let p = Pair {
            car: Value::real(1),
            cdr: Value::cons(Value::real(2), Value::PairMut(Rc::clone(&end))),
        }
        .into();
        end.borrow_mut().cdr = Value::Pair(Rc::clone(&p));

        assert!(!p.is_list());
    }

    #[test]
    fn circular_list_has_no_length() {
        // #0=(1 2 3 . #0#)
        let end = RefCell::new(Pair {
            car: Value::real(3),
            cdr: Value::Null,
        })
        .into();
        let p = Pair {
            car: Value::real(1),
            cdr: Value::cons(Value::real(2), Value::PairMut(Rc::clone(&end))),
        }
        .into();
        end.borrow_mut().cdr = Value::Pair(Rc::clone(&p));

        assert!(matches!(err_or_fail!(p.len()), InvalidList::Cycle));
    }

    #[test]
    fn list_containing_circular_list_is_valid_list_with_length() {
        // (9 8 #0=(1 2 3 . #0#))
        let end = RefCell::new(Pair {
            car: Value::real(3),
            cdr: Value::Null,
        })
        .into();
        let cyc = Pair {
            car: Value::real(1),
            cdr: Value::cons(Value::real(2), Value::PairMut(Rc::clone(&end))),
        }
        .into();
        end.borrow_mut().cdr = Value::Pair(Rc::clone(&cyc));
        let p = Pair {
            car: Value::real(9),
            cdr: Value::cons(Value::real(8), Value::cons(Value::Pair(cyc), Value::Null)),
        }
        .into();
        let v = Value::Pair(Rc::clone(&p));

        let r = p.len();

        assert_eq!(ok_or_fail!(r), 3);
        assert!(p.is_list());
        assert_eq!(v.as_datum().to_string(), "(9 8 #0=(1 2 3 . #0#))");
        assert_eq!(v.as_shared_datum().to_string(), v.as_datum().to_string());
    }

    #[test]
    fn partly_circular_list_display() {
        // (1 2 . #0=(3 4 5 . #0#))
        let end = RefCell::new(Pair {
            car: Value::real(5),
            cdr: Value::Null,
        })
        .into();
        let start = Pair {
            car: Value::real(3),
            cdr: Value::cons(Value::real(4), Value::PairMut(Rc::clone(&end))),
        }
        .into();
        end.borrow_mut().cdr = Value::Pair(Rc::clone(&start));
        let p = Pair {
            car: Value::real(1),
            cdr: Value::cons(Value::real(2), Value::Pair(Rc::clone(&start))),
        }
        .into();

        let v = Value::Pair(Rc::clone(&p));

        assert_eq!(v.as_datum().to_string(), "(1 2 . #0=(3 4 5 . #0#))");
        assert_eq!(v.as_shared_datum().to_string(), v.as_datum().to_string());
    }

    #[test]
    fn partly_circular_list_is_not_list() {
        // (1 2 . #0=(3 4 5 . #0#))
        let end = RefCell::new(Pair {
            car: Value::real(5),
            cdr: Value::Null,
        })
        .into();
        let start = Pair {
            car: Value::real(3),
            cdr: Value::cons(Value::real(4), Value::PairMut(Rc::clone(&end))),
        }
        .into();
        end.borrow_mut().cdr = Value::Pair(Rc::clone(&start));
        let p = Pair {
            car: Value::real(1),
            cdr: Value::cons(Value::real(2), Value::Pair(Rc::clone(&start))),
        };

        assert!(!p.is_list());
    }

    #[test]
    fn partly_circular_list_has_no_length() {
        // (1 2 . #0=(3 4 5 . #0#))
        let end = RefCell::new(Pair {
            car: Value::real(5),
            cdr: Value::Null,
        })
        .into();
        let start = Pair {
            car: Value::real(3),
            cdr: Value::cons(Value::real(4), Value::PairMut(Rc::clone(&end))),
        }
        .into();
        end.borrow_mut().cdr = Value::Pair(Rc::clone(&start));
        let p = Pair {
            car: Value::real(1),
            cdr: Value::cons(Value::real(2), Value::Pair(Rc::clone(&start))),
        };

        assert!(matches!(err_or_fail!(p.len()), InvalidList::Cycle));
    }

    #[test]
    fn cyclic_cdr_display() {
        let p = RefCell::new(Pair {
            car: Value::real(1),
            cdr: Value::real(2),
        })
        .into();
        let cons = Value::PairMut(Rc::clone(&p));
        p.borrow_mut().cdr = cons.clone();

        assert_eq!(cons.as_datum().to_string(), "#0=(1 . #0#)");
        assert_eq!(
            cons.as_shared_datum().to_string(),
            cons.as_datum().to_string()
        );
    }

    #[test]
    fn cyclic_car_display() {
        let p = RefCell::new(Pair {
            car: Value::real(1),
            cdr: Value::real(2),
        })
        .into();
        let cons = Value::PairMut(Rc::clone(&p));
        p.borrow_mut().car = cons.clone();

        assert_eq!(cons.as_datum().to_string(), "#0=(#0# . 2)");
        assert_eq!(
            cons.as_shared_datum().to_string(),
            cons.as_datum().to_string()
        );
    }

    #[test]
    fn cyclic_cons_display() {
        let p = RefCell::new(Pair {
            car: Value::real(1),
            cdr: Value::real(2),
        })
        .into();
        let cons = Value::PairMut(Rc::clone(&p));
        {
            let mut pmut = p.borrow_mut();
            pmut.car = cons.clone();
            pmut.cdr = cons.clone();
        }

        assert_eq!(cons.as_datum().to_string(), "#0=(#0# . #0#)");
        assert_eq!(
            cons.as_shared_datum().to_string(),
            cons.as_datum().to_string()
        );
    }

    #[test]
    fn same_cycle_reference_display() {
        let end = RefCell::new(Pair {
            car: Value::real(3),
            cdr: Value::Null,
        })
        .into();
        let p = Pair {
            car: Value::real(1),
            cdr: Value::cons(Value::real(2), Value::PairMut(Rc::clone(&end))),
        }
        .into();
        end.borrow_mut().cdr = Value::Pair(Rc::clone(&p));
        let cyc = Value::Pair(p);

        let lst = zlist![Value::real(10), Value::real(11), cyc.clone(), cyc];

        assert_eq!(lst.as_datum().to_string(), "(10 11 #0=(1 2 3 . #0#) #0#)");
        assert_eq!(
            lst.as_shared_datum().to_string(),
            lst.as_datum().to_string()
        );
    }

    #[test]
    fn multiple_same_cycle_reference_display() {
        let end = RefCell::new(Pair {
            car: Value::real(3),
            cdr: Value::Null,
        })
        .into();
        let p = Pair {
            car: Value::real(1),
            cdr: Value::cons(Value::real(2), Value::PairMut(Rc::clone(&end))),
        }
        .into();
        end.borrow_mut().cdr = Value::Pair(Rc::clone(&p));
        let cyc1 = Value::Pair(p);

        let end = RefCell::new(Pair {
            car: Value::real(6),
            cdr: Value::Null,
        })
        .into();
        let p = Pair {
            car: Value::real(4),
            cdr: Value::cons(Value::real(5), Value::PairMut(Rc::clone(&end))),
        }
        .into();
        end.borrow_mut().cdr = Value::Pair(Rc::clone(&p));
        let cyc2 = Value::Pair(p);

        let lst = zlist![
            Value::real(10),
            Value::real(11),
            cyc1.clone(),
            cyc1,
            cyc2.clone(),
            Value::real(12),
            cyc2,
        ];

        assert_eq!(
            lst.as_datum().to_string(),
            "(10 11 #0=(1 2 3 . #0#) #0# #1=(4 5 6 . #1#) 12 #1#)"
        );
        assert_eq!(
            lst.as_shared_datum().to_string(),
            lst.as_datum().to_string()
        );
    }
}

mod list_ctor {
    use super::*;

    #[test]
    fn empty() {
        let lst = zlist![];

        assert_eq!(lst.as_datum().to_string(), "()");
        assert_eq!(
            lst.as_simple_datum().to_string(),
            lst.as_datum().to_string()
        );
        assert_eq!(
            lst.as_shared_datum().to_string(),
            lst.as_datum().to_string()
        );
    }

    #[test]
    fn empty_mutable() {
        let lst = zlist_mut![];

        assert_eq!(lst.as_datum().to_string(), "()");
        assert_eq!(
            lst.as_simple_datum().to_string(),
            lst.as_datum().to_string()
        );
        assert_eq!(
            lst.as_shared_datum().to_string(),
            lst.as_datum().to_string()
        );
    }

    #[test]
    fn one() {
        let lst = zlist![Value::real(5)];

        assert_eq!(lst.as_datum().to_string(), "(5)");
        assert_eq!(
            lst.as_simple_datum().to_string(),
            lst.as_datum().to_string()
        );
        assert_eq!(
            lst.as_shared_datum().to_string(),
            lst.as_datum().to_string()
        );
    }

    #[test]
    fn three() {
        let sym = SymbolTable::default();
        let lst = zlist![
            Value::real(5),
            Value::Symbol(sym.get("a")),
            Value::Boolean(true)
        ];

        assert_eq!(lst.as_datum().to_string(), "(5 a #t)");
        assert_eq!(
            lst.as_simple_datum().to_string(),
            lst.as_datum().to_string()
        );
        assert_eq!(
            lst.as_shared_datum().to_string(),
            lst.as_datum().to_string()
        );
    }

    #[test]
    fn mutable() {
        let sym = SymbolTable::default();
        let lst = zlist_mut![
            Value::real(5),
            Value::Symbol(sym.get("a")),
            Value::Boolean(true)
        ];

        assert_eq!(lst.as_datum().to_string(), "(5 a #t)");
        assert_eq!(
            lst.as_simple_datum().to_string(),
            lst.as_datum().to_string()
        );
        assert_eq!(
            lst.as_shared_datum().to_string(),
            lst.as_datum().to_string()
        );
    }

    #[test]
    fn nested() {
        let sym = SymbolTable::default();
        let lst = zlist![
            Value::real(5),
            zlist![Value::Symbol(sym.get("a")), Value::Boolean(true)],
        ];

        assert_eq!(lst.as_datum().to_string(), "(5 (a #t))");
        assert_eq!(
            lst.as_simple_datum().to_string(),
            lst.as_datum().to_string()
        );
        assert_eq!(
            lst.as_shared_datum().to_string(),
            lst.as_datum().to_string()
        );
    }

    #[test]
    fn ctor_empty() {
        let lst = Value::list(vec![]);

        assert_eq!(lst.as_datum().to_string(), "()");
        assert_eq!(
            lst.as_simple_datum().to_string(),
            lst.as_datum().to_string()
        );
        assert_eq!(
            lst.as_shared_datum().to_string(),
            lst.as_datum().to_string()
        );
    }

    #[test]
    fn ctor_vec() {
        let sym = SymbolTable::default();
        let lst = Value::list(vec![
            Value::real(5),
            Value::Symbol(sym.get("a")),
            Value::Boolean(true),
        ]);

        assert_eq!(lst.as_datum().to_string(), "(5 a #t)");
        assert_eq!(
            lst.as_simple_datum().to_string(),
            lst.as_datum().to_string()
        );
        assert_eq!(
            lst.as_shared_datum().to_string(),
            lst.as_datum().to_string()
        );
    }

    #[test]
    fn ctor_slice() {
        let sym = SymbolTable::default();
        let lst = Value::list([
            Value::real(5),
            Value::Symbol(sym.get("a")),
            Value::Boolean(true),
        ]);

        assert_eq!(lst.as_datum().to_string(), "(5 a #t)");
        assert_eq!(
            lst.as_simple_datum().to_string(),
            lst.as_datum().to_string()
        );
        assert_eq!(
            lst.as_shared_datum().to_string(),
            lst.as_datum().to_string()
        );
    }

    #[test]
    fn improper_ctor_empty() {
        let lst = Value::list_cons(vec![]);

        assert_eq!(lst.as_datum().to_string(), "()");
        assert_eq!(
            lst.as_simple_datum().to_string(),
            lst.as_datum().to_string()
        );
        assert_eq!(
            lst.as_shared_datum().to_string(),
            lst.as_datum().to_string()
        );
    }

    #[test]
    fn improper_ctor_single() {
        let lst = Value::list_cons(vec![Value::real(5)]);

        assert!(matches!(lst, Value::Number(_)));
        assert_eq!(lst.as_datum().to_string(), "5");
        assert_eq!(
            lst.as_simple_datum().to_string(),
            lst.as_datum().to_string()
        );
        assert_eq!(
            lst.as_shared_datum().to_string(),
            lst.as_datum().to_string()
        );
    }

    #[test]
    fn improper_ctor_vec() {
        let sym = SymbolTable::default();
        let lst = Value::list_cons(vec![
            Value::real(5),
            Value::Symbol(sym.get("a")),
            Value::Boolean(true),
        ]);

        assert_eq!(lst.as_datum().to_string(), "(5 a . #t)");
        assert_eq!(
            lst.as_simple_datum().to_string(),
            lst.as_datum().to_string()
        );
        assert_eq!(
            lst.as_shared_datum().to_string(),
            lst.as_datum().to_string()
        );
    }

    #[test]
    fn improper_ctor_slice() {
        let sym = SymbolTable::default();
        let lst = Value::list_cons([
            Value::real(5),
            Value::Symbol(sym.get("a")),
            Value::Boolean(true),
        ]);

        assert_eq!(lst.as_datum().to_string(), "(5 a . #t)");
        assert_eq!(
            lst.as_simple_datum().to_string(),
            lst.as_datum().to_string()
        );
        assert_eq!(
            lst.as_shared_datum().to_string(),
            lst.as_datum().to_string()
        );
    }
}

mod cloning {
    use super::*;

    #[test]
    fn clone_with_underlying_cloneable() {
        let sym = SymbolTable::default();
        let v = Value::Symbol(sym.get("foo"));

        let c = v.clone();

        assert!(extract_or_fail!(v, Value::Symbol).is(&extract_or_fail!(c, Value::Symbol)));
    }

    #[test]
    fn clone_with_underlying_move_only() {
        let v = zlist![Value::Unspecified];

        let c = v.clone();

        assert!(Rc::ptr_eq(
            &extract_or_fail!(v, Value::Pair),
            &extract_or_fail!(c, Value::Pair),
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

mod equivalence {
    use super::*;

    #[test]
    fn always_the_same() {
        let cases = [
            (Value::Eof, Value::Eof),
            (Value::Null, Value::Null),
            (Value::Unspecified, Value::Unspecified),
        ];
        for (a, b) in cases {
            assert!(a.is(&b));
        }
    }

    #[test]
    fn never_the_same() {
        let cases = [
            (Value::Character('a'), Value::Character('a')),
            (Value::real(4), Value::real(4)),
        ];
        for (a, b) in cases {
            assert!(!a.is(&b));
        }
    }

    #[test]
    fn boolean_identity() {
        let cases = [
            (Value::Boolean(true), Value::Boolean(true), true),
            (Value::Boolean(true), Value::Boolean(false), false),
            (Value::Boolean(false), Value::Boolean(true), false),
            (Value::Boolean(false), Value::Boolean(false), true),
        ];
        for (a, b, expected) in cases {
            assert_eq!(a.is(&b), expected);
        }
    }

    #[test]
    fn diff_types_never_the_same() {
        let sym = SymbolTable::default();
        let cases = [
            (Value::Symbol(sym.get("foo")), Value::string("foo")),
            (Value::vector([]), Value::ByteVector([].into())),
        ];
        for (a, b) in cases {
            assert!(!a.is(&b));
        }
    }

    #[test]
    fn same_string_ptr_in_string_and_symbol_not_same() {
        let s = "foo".into();
        let sym = SymbolTable::default();
        let a = Value::string(Rc::clone(&s));
        let b = Value::Symbol(sym.get(s));

        assert!(!a.is(&b));
    }

    #[test]
    fn string_pointers_or_symbol_pointers_are_the_same() {
        let s = "foo".into();

        let a = Value::string(Rc::clone(&s));
        let b = Value::string(Rc::clone(&s));

        assert!(a.is(&b));

        let sym = SymbolTable::default();
        let a = Value::Symbol(sym.get(&s));
        let b = Value::Symbol(sym.get(&s));

        assert!(a.is(&b));
    }

    #[test]
    fn different_string_pointers_are_not_the_same() {
        let s = "foo";

        let a = Value::string(s);
        let b = Value::string(s);

        assert!(!a.is(&b));
    }

    #[test]
    fn symbols_must_be_interned() {
        let s = "foo";
        let sym = SymbolTable::default();

        let a = Value::Symbol(sym.get(s));
        let b = Value::Symbol(sym.get(s));

        assert!(a.is(&b));
    }

    #[test]
    fn identity_is_the_same() {
        let lst = zlist![Value::real(4), Value::real(5)];

        assert!(lst.is(&lst));
    }

    #[test]
    fn same_characters_are_equivalent() {
        let a = Value::Character('a');
        let b = Value::Character('a');

        assert!(a.is_eqv(&b));
    }

    #[test]
    fn diff_characters_are_not_equivalent() {
        let a = Value::Character('a');
        let b = Value::Character('b');

        assert!(!a.is_eqv(&b));
    }

    #[test]
    fn equal_bytevectors() {
        let a = Value::ByteVector([1, 2, 3].into());
        let b = Value::ByteVector([1, 2, 3].into());

        assert!(!a.is(&b));
        assert!(!a.is_eqv(&b));
        assert!(a == b);
    }

    #[test]
    fn unequal_bytevectors() {
        let a = Value::ByteVector([1, 2, 3].into());
        let b = Value::ByteVector([1, 2, 3, 4].into());

        assert!(!a.is(&b));
        assert!(!a.is_eqv(&b));
        assert!(a != b);
    }

    #[test]
    fn equal_mutable_bytevectors() {
        let a = Value::bytevector_mut([1, 2, 3]);
        let b = Value::bytevector_mut([1, 2, 3]);

        assert!(!a.is(&b));
        assert!(!a.is_eqv(&b));
        assert!(a == b);
    }

    #[test]
    fn unequal_mutable_bytevectors() {
        let a = Value::bytevector_mut([1, 2, 3]);
        let b = Value::bytevector_mut([1, 2, 3, 4]);

        assert!(!a.is(&b));
        assert!(!a.is_eqv(&b));
        assert!(a != b);
    }

    #[test]
    fn equal_mixed_bytevectors() {
        let a = Value::ByteVector([1, 2, 3].into());
        let b = Value::bytevector_mut([1, 2, 3]);

        assert!(!a.is(&b));
        assert!(!a.is_eqv(&b));
        assert!(a == b);

        assert!(!b.is(&a));
        assert!(!b.is_eqv(&a));
        assert!(b == a);
    }

    #[test]
    fn equal_pairs() {
        let a = Value::cons(Value::real(4), Value::Character('a'));
        let b = Value::cons(Value::real(4), Value::Character('a'));

        assert!(!a.is(&b));
        assert!(!a.is_eqv(&b));
        assert!(a == b);
    }

    #[test]
    fn equal_mutable_pairs() {
        let a = Value::cons_mut(Value::real(4), Value::Character('a'));
        let b = Value::cons_mut(Value::real(4), Value::Character('a'));

        assert!(!a.is(&b));
        assert!(!a.is_eqv(&b));
        assert!(a == b);
    }

    #[test]
    fn non_equal_pairs_if_elements_not_equivalent() {
        let a = Value::cons(Value::real(4), Value::Character('a'));
        let b = Value::cons(Value::real(4.0), Value::Character('a'));

        assert!(!a.is(&b));
        assert!(!a.is_eqv(&b));
        assert!(a != b);
    }

    #[test]
    fn equal_lists() {
        let a = zlist![Value::Boolean(true), Value::Character('b'), Value::real(3)];
        let b = zlist![Value::Boolean(true), Value::Character('b'), Value::real(3)];

        assert!(!a.is(&b));
        assert!(!a.is_eqv(&b));
        assert!(a == b);
    }

    #[test]
    fn equal_mutable_lists() {
        let a = zlist_mut![Value::Boolean(true), Value::Character('b'), Value::real(3)];
        let b = zlist_mut![Value::Boolean(true), Value::Character('b'), Value::real(3)];

        assert!(!a.is(&b));
        assert!(!a.is_eqv(&b));
        assert!(a == b);
    }

    #[test]
    fn equal_mixed_lists() {
        let a = zlist![Value::Boolean(true), Value::Character('b'), Value::real(3)];
        let b = zlist_mut![Value::Boolean(true), Value::Character('b'), Value::real(3)];

        assert!(!a.is(&b));
        assert!(!a.is_eqv(&b));
        assert!(a == b);
    }

    #[test]
    fn unequal_lists_if_one_is_longer() {
        let a = zlist![Value::Boolean(true), Value::Character('b'), Value::real(3)];
        let b = zlist![
            Value::Boolean(true),
            Value::Character('b'),
            Value::real(3),
            Value::Boolean(false),
        ];

        assert!(!a.is(&b));
        assert!(!a.is_eqv(&b));
        assert!(a != b);
    }

    #[test]
    fn unequal_mut_lists_if_one_is_longer() {
        let a = zlist_mut![Value::Boolean(true), Value::Character('b'), Value::real(3)];
        let b = zlist_mut![
            Value::Boolean(true),
            Value::Character('b'),
            Value::real(3),
            Value::Boolean(false),
        ];

        assert!(!a.is(&b));
        assert!(!a.is_eqv(&b));
        assert!(a != b);
    }

    #[test]
    fn equal_strings() {
        let a = Value::string("foo");
        let b = Value::string("foo");

        assert!(!a.is(&b));
        assert!(!a.is_eqv(&b));
        assert!(a == b);
    }

    #[test]
    fn equal_mut_strings() {
        let a = Value::string_mut("foo");
        let b = Value::string_mut("foo");

        assert!(!a.is(&b));
        assert!(!a.is_eqv(&b));
        assert!(a == b);
    }

    #[test]
    fn equal_mixed_strings() {
        let a = Value::string("foo");
        let b = Value::string_mut("foo");

        assert!(!a.is(&b));
        assert!(!a.is_eqv(&b));
        assert!(a == b);

        assert!(!b.is(&a));
        assert!(!b.is_eqv(&a));
        assert!(b == a);
    }

    #[test]
    fn strings_do_not_equal_symbols() {
        let sym = SymbolTable::default();
        let a = Value::string("foo");
        let b = Value::Symbol(sym.get("foo"));

        assert!(!a.is(&b));
        assert!(!a.is_eqv(&b));
        assert!(a != b);
    }

    #[test]
    fn strings_mut_do_not_equal_symbols() {
        let sym = SymbolTable::default();
        let a = Value::string_mut("foo");
        let b = Value::Symbol(sym.get("foo"));

        assert!(!a.is(&b));
        assert!(!a.is_eqv(&b));
        assert!(a != b);
    }

    #[test]
    fn equal_vectors() {
        let a = Value::vector([Value::Boolean(true), Value::Character('b'), Value::real(3)]);
        let b = Value::vector([Value::Boolean(true), Value::Character('b'), Value::real(3)]);

        assert!(!a.is(&b));
        assert!(!a.is_eqv(&b));
        assert!(a == b);
    }

    #[test]
    fn equal_mut_vectors() {
        let a = Value::vector_mut([Value::Boolean(true), Value::Character('b'), Value::real(3)]);
        let b = Value::vector_mut([Value::Boolean(true), Value::Character('b'), Value::real(3)]);

        assert!(!a.is(&b));
        assert!(!a.is_eqv(&b));
        assert!(a == b);
    }

    #[test]
    fn equal_mixed_vectors() {
        let a = Value::vector([Value::Boolean(true), Value::Character('b'), Value::real(3)]);
        let b = Value::vector_mut([Value::Boolean(true), Value::Character('b'), Value::real(3)]);

        assert!(!a.is(&b));
        assert!(!a.is_eqv(&b));
        assert!(a == b);

        assert!(!b.is(&a));
        assert!(!b.is_eqv(&a));
        assert!(b == a);
    }

    #[test]
    fn unequal_vectors() {
        let a = Value::vector([Value::Boolean(true), Value::Character('b'), Value::real(3)]);
        let b = Value::vector([
            Value::Boolean(true),
            Value::Character('b'),
            Value::real(3),
            Value::Boolean(false),
        ]);

        assert!(!a.is(&b));
        assert!(!a.is_eqv(&b));
        assert!(a != b);
    }

    #[test]
    fn unequal_vectors_with_unequivalent_items() {
        let a = Value::vector([Value::Boolean(true), Value::Character('b'), Value::real(3)]);
        let b = Value::vector([
            Value::Boolean(true),
            Value::Character('b'),
            Value::real(3.0),
            Value::Boolean(false),
        ]);

        assert!(!a.is(&b));
        assert!(!a.is_eqv(&b));
        assert!(a != b);
    }

    #[test]
    fn unequal_mut_vectors() {
        let a = Value::vector_mut([Value::Boolean(true), Value::Character('b'), Value::real(3)]);
        let b = Value::vector_mut([
            Value::Boolean(true),
            Value::Character('b'),
            Value::real(3),
            Value::Boolean(false),
        ]);

        assert!(!a.is(&b));
        assert!(!a.is_eqv(&b));
        assert!(a != b);
    }

    #[test]
    fn unequal_mut_vectors_with_unequivalent_items() {
        let a = Value::vector_mut([Value::Boolean(true), Value::Character('b'), Value::real(3)]);
        let b = Value::vector_mut([
            Value::Boolean(true),
            Value::Character('b'),
            Value::real(3.0),
            Value::Boolean(false),
        ]);

        assert!(!a.is(&b));
        assert!(!a.is_eqv(&b));
        assert!(a != b);
    }
}

mod iterator {
    use super::*;

    #[test]
    fn single_value() {
        let v = Value::real(5);
        let it = v.iter();

        let vec = it.collect::<Vec<_>>();

        assert_eq!(vec.len(), 1);
        assert!(matches!(
            &vec[0],
            Value::Number(n) if n.to_string() == "5"
        ));
    }

    #[test]
    fn pair() {
        let v = Value::cons(Value::real(5), Value::real(10));
        let it = v.iter();

        let vec = it.collect::<Vec<_>>();

        assert_eq!(vec.len(), 2);
        assert!(matches!(&vec[0], v @ Value::Pair(_) if v.as_datum().to_string() == "(5 . 10)"));
        assert!(matches!(&vec[1], Value::Number(n) if n.to_string() == "10"));
    }

    #[test]
    fn list() {
        let v = zlist![Value::real(5), Value::real(10)];
        let it = v.iter();

        let vec = it.collect::<Vec<_>>();

        assert_eq!(vec.len(), 3);
        assert!(matches!(&vec[0], v @ Value::Pair(_) if v.as_datum().to_string() == "(5 10)"));
        assert!(matches!(&vec[1], v @ Value::Pair(_) if v.as_datum().to_string() == "(10)"));
        assert!(matches!(&vec[2], Value::Null));
    }

    #[test]
    fn circular_list() {
        // #0=(1 2 3 . #0#)
        let end = RefCell::new(Pair {
            car: Value::real(3),
            cdr: Value::Null,
        })
        .into();
        let p = Pair {
            car: Value::real(1),
            cdr: Value::cons(Value::real(2), Value::PairMut(Rc::clone(&end))),
        }
        .into();
        end.borrow_mut().cdr = Value::Pair(Rc::clone(&p));
        let v = Value::Pair(p);
        let mut it = v.iter();

        let sublist = it.next();

        let lst = some_or_fail!(sublist);
        let p = extract_or_fail!(lst.clone(), Value::Pair);
        assert_eq!(p.car.as_datum().to_string(), "1");

        it.next();
        it.next();
        let sublist = it.next();

        let cyc = some_or_fail!(sublist);
        assert!(cyc.is(&lst));
    }

    #[test]
    fn sublist() {
        let v = zlist![
            Value::real(5),
            Value::real(10),
            Value::real(15),
            Value::real(20)
        ];
        let mut it = v.iter();

        let sublist = it.nth(2);

        let lst = some_or_fail!(sublist);
        assert_eq!(lst.as_datum().to_string(), "(15 20)");
    }

    #[test]
    fn full_sublist() {
        let v = zlist![
            Value::real(5),
            Value::real(10),
            Value::real(15),
            Value::real(20)
        ];
        let mut it = v.iter();

        let sublist = it.next();

        let lst = some_or_fail!(sublist);
        assert_eq!(lst.as_datum().to_string(), "(5 10 15 20)");
    }

    #[test]
    fn sublist_end() {
        let v = zlist![
            Value::real(5),
            Value::real(10),
            Value::real(15),
            Value::real(20)
        ];
        let mut it = v.iter();

        let sublist = it.nth(4);

        let lst = some_or_fail!(sublist);
        assert_eq!(lst.as_datum().to_string(), "()");
    }

    #[test]
    fn sublist_past_end() {
        let v = zlist![
            Value::real(5),
            Value::real(10),
            Value::real(15),
            Value::real(20)
        ];
        let mut it = v.iter();

        let sublist = it.nth(5);

        assert!(sublist.is_none());
    }

    #[test]
    fn improper_sublist_end() {
        let v = Value::cons(
            Value::real(5),
            Value::cons(
                Value::real(10),
                Value::cons(Value::real(15), Value::real(20)),
            ),
        );
        let mut it = v.iter();

        let sublist = it.nth(3);

        let lst = some_or_fail!(sublist);
        assert_eq!(lst.as_datum().to_string(), "20");
    }

    #[test]
    fn improper_sublist_past_end() {
        let v = Value::cons(
            Value::real(5),
            Value::cons(
                Value::real(10),
                Value::cons(Value::real(15), Value::real(20)),
            ),
        );
        let mut it = v.iter();

        let sublist = it.nth(4);

        assert!(sublist.is_none());
    }

    #[test]
    fn circular_sublist() {
        // #0=(1 2 3 . #0#)
        let end = RefCell::new(Pair {
            car: Value::real(3),
            cdr: Value::Null,
        })
        .into();
        let p = Pair {
            car: Value::real(1),
            cdr: Value::cons(Value::real(2), Value::PairMut(Rc::clone(&end))),
        }
        .into();
        end.borrow_mut().cdr = Value::Pair(Rc::clone(&p));
        let v = Value::Pair(p);
        let mut it = v.iter();

        let sublist = it.nth(7);

        let lst = some_or_fail!(sublist);
        let p = extract_or_fail!(lst, Value::Pair);
        assert_eq!(p.car.as_datum().to_string(), "2");
    }
}

mod traverse {
    use super::*;

    fn cycle_count(graph: &Traverse) -> usize {
        graph.visits.values().fold(usize::MIN, |mut acc, vs| {
            if vs.cycle {
                acc += 1
            };
            acc
        })
    }

    #[test]
    fn normal_list() {
        // (1 2 3)
        let v = zlist![Value::real(1), Value::real(2), Value::real(3)];

        let graph = Traverse::pair(v.as_refpair().unwrap().as_ref());

        assert_eq!(cycle_count(&graph), 0);
    }

    #[test]
    fn circular_list() {
        // #0=(1 2 3 . #0#)
        let end = RefCell::new(Pair {
            car: Value::real(3),
            cdr: Value::Null,
        })
        .into();
        let p = Pair {
            car: Value::real(1),
            cdr: Value::cons(Value::real(2), Value::PairMut(Rc::clone(&end))),
        }
        .into();
        end.borrow_mut().cdr = Value::Pair(Rc::clone(&p));

        let graph = Traverse::pair(&p);

        assert_eq!(cycle_count(&graph), 1);
    }

    #[test]
    fn partly_circular_list() {
        // (1 2 . #0=(3 4 5 . #0#))
        let end = RefCell::new(Pair {
            car: Value::real(5),
            cdr: Value::Null,
        })
        .into();
        let start = Pair {
            car: Value::real(3),
            cdr: Value::cons(Value::real(4), Value::PairMut(Rc::clone(&end))),
        }
        .into();
        end.borrow_mut().cdr = Value::Pair(Rc::clone(&start));
        let p = Pair {
            car: Value::real(1),
            cdr: Value::cons(Value::real(2), Value::Pair(Rc::clone(&start))),
        }
        .into();

        let graph = Traverse::pair(&p);

        assert_eq!(cycle_count(&graph), 1);
    }

    #[test]
    fn nested_circular_list() {
        // #0=(#1=(9 8 . #1#) 2 3 . #0#)
        let nested_end = RefCell::new(Pair {
            car: Value::real(8),
            cdr: Value::Null,
        })
        .into();
        let nested_head = Pair {
            car: Value::real(9),
            cdr: Value::PairMut(Rc::clone(&nested_end)),
        }
        .into();
        nested_end.borrow_mut().cdr = Value::Pair(Rc::clone(&nested_head));
        let end = RefCell::new(Pair {
            car: Value::real(3),
            cdr: Value::Null,
        })
        .into();
        let p = Pair {
            car: Value::Pair(Rc::clone(&nested_head)),
            cdr: Value::cons(Value::real(2), Value::PairMut(Rc::clone(&end))),
        }
        .into();
        end.borrow_mut().cdr = Value::Pair(Rc::clone(&p));

        let graph = Traverse::pair(&p);

        assert_eq!(cycle_count(&graph), 2);
        assert_eq!(some_or_fail!(graph.get(p.node_id())).label, 0);
        assert_eq!(some_or_fail!(graph.get(nested_head.node_id())).label, 1);
    }

    #[test]
    fn nested_circular_pair() {
        // #0=(#1=(9 8 . #1#) 2 3 . #0#)
        let nested_end = RefCell::new(Pair {
            car: Value::real(8),
            cdr: Value::Null,
        })
        .into();
        let nested_head = Pair {
            car: Value::real(9),
            cdr: Value::PairMut(Rc::clone(&nested_end)),
        }
        .into();
        nested_end.borrow_mut().cdr = Value::Pair(Rc::clone(&nested_head));
        let end = RefCell::new(Pair {
            car: Value::real(3),
            cdr: Value::Null,
        })
        .into();
        let p = Pair {
            car: Value::Pair(Rc::clone(&nested_head)),
            cdr: Value::cons(Value::real(2), Value::PairMut(Rc::clone(&end))),
        }
        .into();
        end.borrow_mut().cdr = Value::Pair(Rc::clone(&p));

        let graph = Traverse::pair(&p);

        assert_eq!(cycle_count(&graph), 2);
        assert_eq!(some_or_fail!(graph.get(p.node_id())).label, 0);
        assert_eq!(some_or_fail!(graph.get(nested_head.node_id())).label, 1);
    }

    #[test]
    fn contained_circular_list() {
        // a ==> #0=(1 2 3 9 #0# 7)
        // b ==> #0=(9 (1 2 3 . #0#) 7)
        let a_end = RefCell::new(Pair {
            car: Value::real(3),
            cdr: Value::Null,
        })
        .into();
        let a = Value::cons(
            Value::real(1),
            Value::cons(Value::real(2), Value::PairMut(Rc::clone(&a_end))),
        );
        let b = zlist![Value::real(9), a.clone(), Value::real(7)];
        a_end.borrow_mut().cdr = b.clone();

        let a_graph = Traverse::pair(a.as_refpair().unwrap().as_ref());
        let b_graph = Traverse::pair(b.as_refpair().unwrap().as_ref());

        assert_eq!(cycle_count(&a_graph), 1);
        assert_eq!(cycle_count(&b_graph), 1);
        assert_eq!(a.as_datum().to_string(), "#0=(1 2 3 9 #0# 7)");
        assert_eq!(a.as_shared_datum().to_string(), a.as_datum().to_string());
        assert_eq!(b.as_datum().to_string(), "#0=(9 (1 2 3 . #0#) 7)");
        assert_eq!(b.as_shared_datum().to_string(), b.as_datum().to_string());
    }

    #[test]
    fn simple_vector() {
        // #(1 2 3)
        let v = Value::vector([Value::real(1), Value::real(2), Value::real(3)]);

        let graph = Traverse::vec(v.as_refvec().unwrap().as_ref());

        assert_eq!(cycle_count(&graph), 0);
    }

    #[test]
    fn cyclic_vector() {
        // #0=#(1 2 #0#)
        let vec = RefCell::new(vec![Value::real(1), Value::real(2)]).into();
        let v = Value::VectorMut(Rc::clone(&vec));
        vec.borrow_mut().push(v.clone());

        let graph = Traverse::vec(&vec.borrow());

        assert_eq!(cycle_count(&graph), 1);
    }

    #[test]
    fn partly_cyclic_vector() {
        // #(1 2 #0=#(3 4 #0#))
        let vec = RefCell::new(vec![Value::real(3), Value::real(4)]).into();
        let head = Value::VectorMut(Rc::clone(&vec));
        vec.borrow_mut().push(head.clone());
        let v = Value::vector([Value::real(1), Value::real(2), head.clone()]);

        let graph = Traverse::vec(v.as_refvec().unwrap().as_ref());

        assert_eq!(cycle_count(&graph), 1);
    }

    #[test]
    fn multiple_cyclic_vector() {
        // #0=#(1 2 #0# 3 #0#)
        let vec = RefCell::new(vec![Value::real(1), Value::real(2)]).into();
        let v = Value::VectorMut(Rc::clone(&vec));
        vec.borrow_mut().push(v.clone());
        vec.borrow_mut().push(Value::real(3));
        vec.borrow_mut().push(v.clone());

        let graph = Traverse::vec(&vec.borrow());

        assert_eq!(cycle_count(&graph), 1);
    }

    #[test]
    fn nested_cyclic_vector() {
        // #0=#(1 2 #1=#(9 8 #1#) 3 #0#)
        let nested_vec = RefCell::new(vec![Value::real(9), Value::real(8)]).into();
        let nested_v = Value::VectorMut(Rc::clone(&nested_vec));
        nested_vec.borrow_mut().push(nested_v.clone());
        let vec = RefCell::new(vec![
            Value::real(1),
            Value::real(2),
            nested_v.clone(),
            Value::real(3),
        ])
        .into();
        let v = Value::VectorMut(Rc::clone(&vec));
        vec.borrow_mut().push(v.clone());

        let graph = Traverse::vec(&vec.borrow());

        assert_eq!(cycle_count(&graph), 2);
        assert_eq!(
            some_or_fail!(graph.get(vec.borrow().as_ptr().cast())).label,
            0
        );
        assert_eq!(
            some_or_fail!(graph.get(nested_vec.borrow().as_ptr().cast())).label,
            1
        );
    }

    #[test]
    fn self_nested_cyclic_vector() {
        // #0=#(1 2 #(3 4 #0#))
        let nested_vec = RefCell::new(vec![Value::real(3), Value::real(4)]).into();
        let nested_v = Value::VectorMut(Rc::clone(&nested_vec));
        let vec = RefCell::new(vec![Value::real(1), Value::real(2), nested_v.clone()]).into();
        let v = Value::VectorMut(Rc::clone(&vec));
        nested_vec.borrow_mut().push(v.clone());

        let graph = Traverse::vec(&vec.borrow());

        assert_eq!(cycle_count(&graph), 1);
    }

    #[test]
    fn mutually_nested_vectors() {
        // #0=#(1 #(9 #0# 7) 3)
        // #0=#(9 #(1 #0# 3) 7)
        let a_vec = RefCell::new(vec![Value::real(1), Value::Unspecified, Value::real(3)]).into();
        let b = Value::vector_mut(vec![
            Value::real(9),
            Value::VectorMut(Rc::clone(&a_vec)),
            Value::real(7),
        ]);
        a_vec.borrow_mut()[1] = b.clone();
        let a = Value::VectorMut(a_vec);

        let a_graph = Traverse::vec(a.as_refvec().unwrap().as_ref());
        let b_graph = Traverse::vec(b.as_refvec().unwrap().as_ref());

        assert_eq!(cycle_count(&a_graph), 1);
        assert_eq!(cycle_count(&b_graph), 1);
        assert_eq!(a.as_datum().to_string(), "#0=#(1 #(9 #0# 7) 3)");
        assert_eq!(a.as_shared_datum().to_string(), a.as_datum().to_string());
        assert_eq!(b.as_datum().to_string(), "#0=#(9 #(1 #0# 3) 7)");
        assert_eq!(b.as_shared_datum().to_string(), b.as_datum().to_string());
    }

    #[test]
    fn circular_list_with_cyclic_vector() {
        // #0=(#1=#(9 8 #1#) 2 3 . #0#)
        let nested_vec = RefCell::new(vec![Value::real(9), Value::real(8)]).into();
        let nested_v = Value::VectorMut(Rc::clone(&nested_vec));
        nested_vec.borrow_mut().push(nested_v.clone());
        let end = RefCell::new(Pair {
            car: Value::real(3),
            cdr: Value::Null,
        })
        .into();
        let p = Pair {
            car: nested_v.clone(),
            cdr: Value::cons(Value::real(2), Value::PairMut(Rc::clone(&end))),
        }
        .into();
        end.borrow_mut().cdr = Value::Pair(Rc::clone(&p));
        let v = Value::Pair(Rc::clone(&p));

        let graph = Traverse::pair(&p);

        assert_eq!(cycle_count(&graph), 2);
        assert_eq!(some_or_fail!(graph.get(p.node_id())).label, 0);
        assert_eq!(
            some_or_fail!(graph.get(nested_vec.borrow().as_ptr().cast())).label,
            1
        );
        assert_eq!(v.as_datum().to_string(), "#0=(#1=#(9 8 #1#) 2 3 . #0#)");
        assert_eq!(v.as_shared_datum().to_string(), v.as_datum().to_string());
    }

    #[test]
    fn list_ending_with_cyclic_vector() {
        // (#0=#(9 8 #1#) 2 3 . #0#)
        let nested_vec = RefCell::new(vec![Value::real(9), Value::real(8)]).into();
        let nested_v = Value::VectorMut(Rc::clone(&nested_vec));
        nested_vec.borrow_mut().push(nested_v.clone());
        let p = Pair {
            car: nested_v.clone(),
            cdr: Value::cons(
                Value::real(2),
                Value::cons(Value::real(3), nested_v.clone()),
            ),
        }
        .into();
        let v = Value::Pair(Rc::clone(&p));

        let graph = Traverse::pair(&p);

        assert_eq!(cycle_count(&graph), 1);
        assert_eq!(
            some_or_fail!(graph.get(nested_vec.borrow().as_ptr().cast())).label,
            0
        );
        assert_eq!(v.as_datum().to_string(), "(#0=#(9 8 #0#) 2 3 . #0#)");
        assert_eq!(v.as_shared_datum().to_string(), v.as_datum().to_string());
    }

    #[test]
    fn cyclic_vector_with_circular_list() {
        // #0=#(1 2 #1=(9 8 . #1#) 3 #0#)
        let nested_end = RefCell::new(Pair {
            car: Value::real(8),
            cdr: Value::Null,
        })
        .into();
        let nested_head = Pair {
            car: Value::real(9),
            cdr: Value::PairMut(Rc::clone(&nested_end)),
        }
        .into();
        nested_end.borrow_mut().cdr = Value::Pair(Rc::clone(&nested_head));
        let vec = RefCell::new(vec![
            Value::real(1),
            Value::real(2),
            Value::Pair(Rc::clone(&nested_head)),
            Value::real(3),
        ])
        .into();
        let v = Value::VectorMut(Rc::clone(&vec));
        vec.borrow_mut().push(v.clone());

        let graph = Traverse::vec(&vec.borrow());

        assert_eq!(cycle_count(&graph), 2);
        assert_eq!(
            some_or_fail!(graph.get(vec.borrow().as_ptr().cast())).label,
            0
        );
        assert_eq!(some_or_fail!(graph.get(nested_head.node_id())).label, 1);
        assert_eq!(v.as_datum().to_string(), "#0=#(1 2 #1=(9 8 . #1#) 3 #0#)");
        assert_eq!(v.as_shared_datum().to_string(), v.as_datum().to_string());
    }

    #[test]
    fn get_does_not_return_non_cycles() {
        // (1 2 . #0=(3 4 5 . #0#))
        let end = RefCell::new(Pair {
            car: Value::real(5),
            cdr: Value::Null,
        })
        .into();
        let start = Pair {
            car: Value::real(3),
            cdr: Value::cons(Value::real(4), Value::PairMut(Rc::clone(&end))),
        }
        .into();
        end.borrow_mut().cdr = Value::Pair(Rc::clone(&start));
        let p = Pair {
            car: Value::real(1),
            cdr: Value::cons(Value::real(2), Value::Pair(Rc::clone(&start))),
        }
        .into();

        let graph = Traverse::pair(&p);

        assert_eq!(cycle_count(&graph), 1);
        assert!(graph.contains(start.node_id()));
        assert!(!graph.contains(p.node_id()));
    }

    #[test]
    fn list_contains_duplicate_list() {
        // (1 2 (9 8) 3 (9 8) 4)
        let inner = zlist![Value::real(9), Value::real(8)];
        let val = zlist![
            Value::real(1),
            Value::real(2),
            inner.clone(),
            Value::real(3),
            inner,
            Value::real(4),
        ];

        let graph = Traverse::pair(val.as_refpair().unwrap().as_ref());

        assert_eq!(cycle_count(&graph), 0);
        assert_eq!(val.as_datum().to_string(), "(1 2 (9 8) 3 (9 8) 4)");
        assert_eq!(
            val.as_simple_datum().to_string(),
            val.as_datum().to_string()
        );
        assert_eq!(val.as_shared_datum().to_string(), "(1 2 #0=(9 8) 3 #0# 4)");
    }

    #[test]
    fn vector_contains_duplicate_vector() {
        // #(1 2 #(9 8) 3 #(9 8) 4)
        let inner = Value::vector([Value::real(9), Value::real(8)]);
        let val = Value::vector([
            Value::real(1),
            Value::real(2),
            inner.clone(),
            Value::real(3),
            inner,
            Value::real(4),
        ]);

        let graph = Traverse::vec(val.as_refvec().unwrap().as_ref());

        assert_eq!(cycle_count(&graph), 0);
        assert_eq!(val.as_datum().to_string(), "#(1 2 #(9 8) 3 #(9 8) 4)");
        assert_eq!(
            val.as_simple_datum().to_string(),
            val.as_datum().to_string()
        );
        assert_eq!(
            val.as_shared_datum().to_string(),
            "#(1 2 #0=#(9 8) 3 #0# 4)"
        );
    }

    #[test]
    fn list_contains_duplicate_vector() {
        // (1 2 #(9 8) 3 #(9 8) 4)
        let inner = Value::vector([Value::real(9), Value::real(8)]);
        let val = zlist![
            Value::real(1),
            Value::real(2),
            inner.clone(),
            Value::real(3),
            inner,
            Value::real(4),
        ];

        let graph = Traverse::pair(val.as_refpair().unwrap().as_ref());

        assert_eq!(cycle_count(&graph), 0);
        assert_eq!(val.as_datum().to_string(), "(1 2 #(9 8) 3 #(9 8) 4)");
        assert_eq!(
            val.as_simple_datum().to_string(),
            val.as_datum().to_string()
        );
        assert_eq!(val.as_shared_datum().to_string(), "(1 2 #0=#(9 8) 3 #0# 4)");
    }

    #[test]
    fn vector_contains_duplicate_list() {
        // #(1 2 (9 8) 3 (9 8) 4)
        let inner = zlist![Value::real(9), Value::real(8)];
        let val = Value::vector([
            Value::real(1),
            Value::real(2),
            inner.clone(),
            Value::real(3),
            inner,
            Value::real(4),
        ]);

        let graph = Traverse::vec(val.as_refvec().unwrap().as_ref());

        assert_eq!(cycle_count(&graph), 0);
        assert_eq!(val.as_datum().to_string(), "#(1 2 (9 8) 3 (9 8) 4)");
        assert_eq!(
            val.as_simple_datum().to_string(),
            val.as_datum().to_string()
        );
        assert_eq!(val.as_shared_datum().to_string(), "#(1 2 #0=(9 8) 3 #0# 4)");
    }
}
