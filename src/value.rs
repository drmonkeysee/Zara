use crate::{
    constant::Constant,
    lex::{DisplayTokenLines, TokenLine, TokenLinesMessage},
    string::SymbolDatum,
    syntax::Program,
};
use std::{
    fmt::{self, Display, Formatter},
    rc::Rc,
};

#[derive(Debug)]
pub(crate) enum Value {
    Ast(Program),
    ByteVector(Box<[u8]>),
    Constant(Constant),
    Pair(Option<Rc<Pair>>),
    // TODO: figure out symbol table
    Symbol(Box<str>),
    TokenList(Box<[TokenLine]>),
}

impl Value {
    fn null() -> Self {
        Self::Pair(None)
    }

    fn pair(p: impl Into<Rc<Pair>>) -> Self {
        Self::Pair(Some(p.into()))
    }

    pub(crate) fn as_datum(&self) -> Datum {
        Datum(self)
    }

    pub(crate) fn display_message(&self) -> ValueMessage {
        ValueMessage(self)
    }

    pub(crate) fn as_typename(&self) -> TypeName {
        TypeName(self)
    }
}

#[derive(Debug)]
pub(crate) struct Pair {
    car: Value,
    cdr: Rc<Value>,
}

impl Pair {
    fn cons(car: Value, cdr: impl Into<Rc<Value>>) -> Self {
        Self {
            car,
            cdr: cdr.into(),
        }
    }

    fn is_list(&self) -> bool {
        if let Value::Pair(p) = &*self.cdr {
            p.as_ref().is_none_or(|r| r.is_list())
        } else {
            false
        }
    }
}

impl Display for Pair {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.car.as_datum().fmt(f)?;
        if let Value::Pair(p) = &*self.cdr {
            if let Some(p) = p {
                write!(f, " {p}")?;
            }
            Ok(())
        } else {
            let d = self.cdr.as_datum();
            write!(f, " . {d}")
        }
    }
}

pub(crate) struct Datum<'a>(&'a Value);

impl Display for Datum<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self.0 {
            Value::Ast(prg) => write!(f, "{{{prg:?}}}"),
            Value::ByteVector(bv) => write!(
                f,
                "#u8({})",
                bv.iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(" ")
            ),
            Value::Constant(con) => con.as_datum().fmt(f),
            Value::Pair(None) => f.write_str("()"),
            Value::Pair(Some(p)) => write!(f, "({p})"),
            Value::Symbol(s) => SymbolDatum(s).fmt(f),
            Value::TokenList(lines) => DisplayTokenLines(lines).fmt(f),
        }
    }
}

pub(crate) struct ValueMessage<'a>(&'a Value);

impl Display for ValueMessage<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self.0 {
            Value::Ast(prg) => writeln!(f, "{prg:#?}"),
            Value::TokenList(lines) => TokenLinesMessage(lines).fmt(f),
            _ => Ok(()),
        }
    }
}

pub(crate) struct TypeName<'a>(&'a Value);

impl Display for TypeName<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.0 {
            Value::Ast(_) => f.write_str("abstract syntax tree"),
            Value::ByteVector(_) => f.write_str("bytevector"),
            Value::Constant(c) => c.as_typename().fmt(f),
            Value::Pair(None) => f.write_str("list"),
            Value::Pair(Some(p)) => f.write_str(if p.is_list() { "list" } else { "pair" }),
            Value::Symbol(_) => f.write_str("symbol"),
            Value::TokenList(_) => f.write_str("token list"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    mod display {
        use super::*;

        #[test]
        fn bytevector_datum() {
            let v = Value::ByteVector([1, 2, 3].into());

            assert_eq!(v.as_datum().to_string(), "#u8(1 2 3)");
        }

        #[test]
        fn empty_bytevector_datum() {
            let v = Value::ByteVector([].into());

            assert_eq!(v.as_datum().to_string(), "#u8()");
        }

        #[test]
        fn bytevector_typename() {
            let v = Value::ByteVector([].into());

            assert_eq!(v.as_typename().to_string(), "bytevector");
        }

        #[test]
        fn constant_datum() {
            let v = Value::Constant(Constant::Boolean(true));

            assert_eq!(v.as_datum().to_string(), "#t");
        }

        #[test]
        fn constant_typename() {
            let v = Value::Constant(Constant::Boolean(true));

            assert_eq!(v.as_typename().to_string(), "boolean");
        }

        #[test]
        fn symbol_typename() {
            let v = Value::Symbol("foo".into());

            assert_eq!(v.as_typename().to_string(), "symbol");
        }

        #[test]
        fn pair_typename() {
            let p = Pair::cons(
                Value::Constant(Constant::Boolean(true)),
                Value::Constant(Constant::Character('a')),
            );
            let v = Value::pair(p);

            assert_eq!(v.as_typename().to_string(), "pair");
        }

        #[test]
        fn list_typename() {
            let cdr = Value::pair(Pair::cons(
                Value::Constant(Constant::Character('a')),
                Value::null(),
            ));
            let v = Value::pair(Pair::cons(Value::Constant(Constant::Boolean(true)), cdr));

            assert_eq!(v.as_typename().to_string(), "list");
        }

        #[test]
        fn empty_list_typename() {
            let v = Value::null();

            assert_eq!(v.as_typename().to_string(), "list");
        }

        #[test]
        fn empty_list_display() {
            let v = Value::null();

            assert_eq!(v.as_datum().to_string(), "()")
        }
    }

    // NOTE: most of these tests adapted from constant::tests::string
    mod symbol {
        use super::*;

        #[test]
        fn simple() {
            let v = Value::Symbol("foo".into());

            assert_eq!(v.as_datum().to_string(), "foo");
        }

        #[test]
        fn empty() {
            let v = Value::Symbol("".into());

            assert_eq!(v.as_datum().to_string(), "||");
        }

        #[test]
        fn whitespace() {
            let v = Value::Symbol("foo bar".into());

            assert_eq!(v.as_datum().to_string(), "|foo bar|");
        }

        #[test]
        fn only_whitespace() {
            let v = Value::Symbol("   ".into());

            assert_eq!(v.as_datum().to_string(), "|   |");
        }

        #[test]
        fn alphanumeric() {
            let v = Value::Symbol("abc123!@$^&".into());

            assert_eq!(v.as_datum().to_string(), "abc123!@$^&");
        }

        #[test]
        fn special_lex_chars() {
            let cases = ['(', ')', '\'', '`', '#', '"', ';', '.', ','];
            for case in cases {
                let s = format!("abc{case}123");

                let v = Value::Symbol(s.clone().into());

                let expected = if case == '.' { s } else { format!("|{s}|") };
                assert_eq!(v.as_datum().to_string(), expected);
            }
        }

        #[test]
        fn starts_with_number() {
            let v = Value::Symbol("123abc".into());

            assert_eq!(v.as_datum().to_string(), "|123abc|");
        }

        #[test]
        fn null() {
            let v = Value::Symbol("\0".into());

            assert_eq!(v.as_datum().to_string(), "|\\x0;|");
        }

        #[test]
        fn pipe() {
            let v = Value::Symbol("|".into());

            assert_eq!(v.as_datum().to_string(), "|\\||");
        }

        #[test]
        fn one_digit_hex() {
            let v = Value::Symbol("\x0c".into());

            assert_eq!(v.as_datum().to_string(), "|\\xc;|");
        }

        #[test]
        fn hex_uses_lowercase() {
            let v = Value::Symbol("\x0C".into());

            assert_eq!(v.as_datum().to_string(), "|\\xc;|");
        }

        #[test]
        fn two_digit_hex() {
            let v = Value::Symbol("\x1d".into());

            assert_eq!(v.as_datum().to_string(), "|\\x1d;|");
        }

        #[test]
        fn four_digit_hex() {
            let v = Value::Symbol("\u{fff9}".into());

            assert_eq!(v.as_datum().to_string(), "|\\xfff9;|");
        }

        #[test]
        fn special_purpose_plane() {
            let v = Value::Symbol("\u{e0001}".into());

            assert_eq!(v.as_datum().to_string(), "|\\xe0001;|");
        }

        #[test]
        fn private_use_plane() {
            let v = Value::Symbol("\u{100001}".into());

            assert_eq!(v.as_datum().to_string(), "|\\x100001;|");
        }

        #[test]
        fn literal_endline() {
            let v = Value::Symbol(
                "foo
bar"
                .into(),
            );

            assert_eq!(v.as_datum().to_string(), "|foo\\nbar|");
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
                let v = Value::Symbol(inp.into());

                assert_eq!(v.as_datum().to_string(), format!("|{exp}|"));
            }
        }

        /***
         * TODO:
         * some of these test may no longer be verbatim after adding unicode support
         ***/

        #[test]
        fn extended() {
            let v = Value::Symbol("λ".into());

            assert_eq!(v.as_datum().to_string(), "|λ|");
        }

        #[test]
        fn emoji() {
            let v = Value::Symbol("🦀".into());

            assert_eq!(v.as_datum().to_string(), "|🦀|");
        }

        #[test]
        fn control_picture() {
            let v = Value::Symbol("\u{2401}".into());

            assert_eq!(v.as_datum().to_string(), "|␁|");
        }

        #[test]
        fn replacement() {
            let v = Value::Symbol("\u{fffd}".into());

            assert_eq!(v.as_datum().to_string(), "|�|");
        }
    }

    mod pair {
        use super::*;
        use crate::number::Number;

        #[test]
        fn pair_is_not_list() {
            // (#t . #f)
            let p = Pair::cons(
                Value::Constant(Constant::Boolean(true)),
                Value::Constant(Constant::Boolean(false)),
            );

            assert!(!p.is_list());
        }

        #[test]
        fn empty_cdr_is_list() {
            // (#t)
            let p = Pair::cons(Value::Constant(Constant::Boolean(true)), Value::null());

            assert!(p.is_list());
        }

        #[test]
        fn nested_is_list() {
            // (1 2 3)
            let p = Pair::cons(
                Value::Constant(Constant::Number(Number::real(1))),
                Value::pair(Pair::cons(
                    Value::Constant(Constant::Number(Number::real(2))),
                    Value::pair(Pair::cons(
                        Value::Constant(Constant::Number(Number::real(3))),
                        Value::null(),
                    )),
                )),
            );

            assert!(p.is_list());
        }

        #[test]
        fn improper_list_is_not_list() {
            // (1 2 . 3)
            let p = Pair::cons(
                Value::Constant(Constant::Number(Number::real(1))),
                Value::pair(Pair::cons(
                    Value::Constant(Constant::Number(Number::real(2))),
                    Value::Constant(Constant::Number(Number::real(3))),
                )),
            );

            assert!(!p.is_list());
        }

        #[test]
        fn list_containing_pair_is_list() {
            // ((1 . 2) 3)
            let p = Pair::cons(
                Value::pair(Pair::cons(
                    Value::Constant(Constant::Number(Number::real(1))),
                    Value::Constant(Constant::Number(Number::real(2))),
                )),
                Value::pair(Pair::cons(
                    Value::Constant(Constant::Number(Number::real(3))),
                    Value::null(),
                )),
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
            let p = Pair::cons(
                Value::Constant(Constant::Boolean(true)),
                Value::Constant(Constant::Boolean(false)),
            );
            let v = Value::pair(p);

            assert_eq!(v.as_datum().to_string(), "(#t . #f)")
        }

        #[test]
        fn empty_cdr_display() {
            let p = Pair::cons(Value::Constant(Constant::Boolean(true)), Value::null());
            let v = Value::pair(p);

            assert_eq!(v.as_datum().to_string(), "(#t)")
        }

        #[test]
        fn list_display() {
            let p = Pair::cons(
                Value::Constant(Constant::Number(Number::real(1))),
                Value::pair(Pair::cons(
                    Value::Constant(Constant::Number(Number::real(2))),
                    Value::pair(Pair::cons(
                        Value::Constant(Constant::Number(Number::real(3))),
                        Value::null(),
                    )),
                )),
            );
            let v = Value::pair(p);

            assert_eq!(v.as_datum().to_string(), "(1 2 3)")
        }

        #[test]
        fn improper_list_display() {
            let p = Pair::cons(
                Value::Constant(Constant::Number(Number::real(1))),
                Value::pair(Pair::cons(
                    Value::Constant(Constant::Number(Number::real(2))),
                    Value::Constant(Constant::Number(Number::real(3))),
                )),
            );
            let v = Value::pair(p);

            assert_eq!(v.as_datum().to_string(), "(1 2 . 3)")
        }

        #[test]
        fn list_containing_pair_display() {
            let p = Pair::cons(
                Value::pair(Pair::cons(
                    Value::Constant(Constant::Number(Number::real(1))),
                    Value::Constant(Constant::Number(Number::real(2))),
                )),
                Value::pair(Pair::cons(
                    Value::Constant(Constant::Number(Number::real(3))),
                    Value::null(),
                )),
            );
            let v = Value::pair(p);

            assert_eq!(v.as_datum().to_string(), "((1 . 2) 3)")
        }

        #[test]
        fn list_containing_list_display() {
            let p = Pair::cons(
                Value::Constant(Constant::Number(Number::real(1))),
                Value::pair(Pair::cons(
                    Value::pair(Pair::cons(
                        Value::Constant(Constant::Number(Number::real(2))),
                        Value::pair(Pair::cons(
                            Value::Constant(Constant::Number(Number::real(3))),
                            Value::null(),
                        )),
                    )),
                    Value::null(),
                )),
            );
            let v = Value::pair(p);

            assert_eq!(v.as_datum().to_string(), "(1 (2 3))")
        }

        #[test]
        fn list_containing_empty_list_display() {
            let p = Pair::cons(Value::null(), Value::null());
            let v = Value::pair(p);

            assert_eq!(v.as_datum().to_string(), "(())")
        }
    }
}
