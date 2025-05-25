macro_rules! zlist {
    () => {
        Value::null()
    };
    ($exp:expr $(, $exps:expr)* $(,)?) => {
        Value::pair(Pair::cons($exp, zlist![$($exps),*]))
    };
}

use crate::{
    constant::Constant,
    eval::Procedure,
    lex::{DisplayTokenLines, TokenLine, TokenLinesMessage},
    string::SymbolDatum,
    syntax::Program,
};
use std::{
    fmt::{self, Display, Formatter, Write},
    rc::Rc,
};
pub(crate) use zlist;

pub(crate) type ValueRef = Rc<Value>;
pub(crate) type ValueObj = Option<ValueRef>;

#[derive(Debug)]
pub(crate) enum Value {
    Ast(Program),
    ByteVector(Box<[u8]>),
    Constant(Constant),
    Pair(Option<Rc<Pair>>),
    Procedure(Box<Procedure>),
    // TODO: figure out symbol table
    Symbol(Box<str>),
    TokenList(Box<[TokenLine]>),
    Unspecified,
    // TODO: vector needs ref-cells for item equivalence and self-referencing
    Vector(Box<[Value]>),
}

impl Value {
    pub(crate) fn null() -> Self {
        Self::Pair(None)
    }

    pub(crate) fn pair(p: impl Into<Rc<Pair>>) -> Self {
        Self::Pair(Some(p.into()))
    }

    pub(crate) fn list<I>(items: I) -> Self
    where
        I: IntoIterator<Item = Self>,
        <I as IntoIterator>::IntoIter: DoubleEndedIterator,
    {
        items
            .into_iter()
            .rev()
            .fold(zlist![], |head, item| Self::pair(Pair::cons(item, head)))
    }

    pub(crate) fn improper_list<I>(items: I) -> Self
    where
        I: IntoIterator<Item = Self>,
        <I as IntoIterator>::IntoIter: DoubleEndedIterator,
    {
        items
            .into_iter()
            .rev()
            .reduce(|head, item| Self::pair(Pair::cons(item, head)))
            .unwrap_or_else(Self::null)
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
    // TODO: i think this needs to be a value ref, e.g. (cdr foo)
    car: Value,
    cdr: ValueRef,
}

impl Pair {
    #[allow(clippy::similar_names, reason = "lisp terms-of-art")]
    pub(crate) fn cons(car: Value, cdr: impl Into<ValueRef>) -> Self {
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

#[derive(Debug)]
pub(crate) struct Condition {
    kind: ConditionKind,
    irritants: ValueObj,
    msg: Box<str>,
}

impl Condition {
    pub(crate) fn binding(name: &str) -> Self {
        Self {
            kind: ConditionKind::Env,
            irritants: None,
            msg: format!("unbound variable: {name}").into(),
        }
    }

    pub(crate) fn as_datum(&self) -> ConditionDatum {
        ConditionDatum(self)
    }
}

pub(crate) struct Datum<'a>(&'a Value);

impl Display for Datum<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self.0 {
            Value::Ast(prg) => write!(f, "{{{prg:?}}}"),
            Value::ByteVector(bv) => write_seq("#u8", bv, ToString::to_string, f),
            Value::Constant(con) => con.as_datum().fmt(f),
            Value::Pair(None) => f.write_str("()"),
            Value::Pair(Some(p)) => write!(f, "({p})"),
            Value::Procedure(p) => p.fmt(f),
            Value::Symbol(s) => SymbolDatum(s).fmt(f),
            Value::TokenList(lines) => DisplayTokenLines(lines).fmt(f),
            Value::Unspecified => f.write_str("#<unspecified>"),
            Value::Vector(v) => write_seq("#", v, |v| v.as_datum().to_string(), f),
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
            Value::Procedure(_) => f.write_str("procedure"),
            Value::Symbol(_) => f.write_str("symbol"),
            Value::TokenList(_) => f.write_str("token list"),
            Value::Unspecified => f.write_str("unspecified"),
            Value::Vector(_) => f.write_str("vector"),
        }
    }
}

pub(crate) struct ConditionDatum<'a>(&'a Condition);

impl Display for ConditionDatum<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "#<{} \"{}\"", self.0.kind, self.0.msg)?;
        if let Some(v) = &self.0.irritants {
            write!(f, " {}", v.as_datum())?;
        }
        f.write_char('>')
    }
}

#[derive(Debug)]
enum ConditionKind {
    Env,
    File,
    General,
    Read,
}

impl Display for ConditionKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Env => f.write_str("env-error"),
            Self::File => f.write_str("file-error"),
            Self::General => f.write_str("exception"),
            Self::Read => f.write_str("read-error"),
        }
    }
}

fn write_seq<'a, T: 'a>(
    prefix: &str,
    seq: impl IntoIterator<Item = &'a T>,
    to_str: impl FnMut(&'a T) -> String,
    f: &mut Formatter<'_>,
) -> fmt::Result {
    write!(
        f,
        "{prefix}({})",
        seq.into_iter().map(to_str).collect::<Vec<_>>().join(" ")
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::number::Number;

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
            let v = zlist![
                Value::Constant(Constant::Boolean(true)),
                Value::Constant(Constant::Character('a')),
            ];

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

            assert_eq!(v.as_datum().to_string(), "()")
        }

        #[test]
        fn unspecified_typename() {
            let v = Value::Unspecified;

            assert_eq!(v.as_typename().to_string(), "unspecified");
        }

        #[test]
        fn unspecified_datum() {
            let v = Value::Unspecified;

            assert_eq!(v.as_datum().to_string(), "#<unspecified>");
        }

        #[test]
        fn vector_typename() {
            let v = Value::Vector([].into());

            assert_eq!(v.as_typename().to_string(), "vector");
        }

        #[test]
        fn vector_datum() {
            let v = Value::Vector(
                [
                    Value::Constant(Constant::String("foo".into())),
                    Value::Symbol("a".into()),
                    zlist![
                        Value::Constant(Constant::Boolean(true)),
                        Value::Constant(Constant::Character('a')),
                    ],
                ]
                .into(),
            );

            assert_eq!(v.as_datum().to_string(), "#(\"foo\" a (#t #\\a))");
        }

        #[test]
        fn empty_vector_datum() {
            let v = Value::Vector([].into());

            assert_eq!(v.as_datum().to_string(), "#()");
        }

        #[test]
        fn procedure_typename() {
            let v = Value::Procedure(Procedure::intrinsic("foo", 0..0, |_, _| None).into());

            assert_eq!(v.as_typename().to_string(), "procedure");
        }

        #[test]
        fn procedure_datum() {
            let v = Value::Procedure(Procedure::intrinsic("foo", 0..0, |_, _| None).into());

            assert_eq!(v.as_datum().to_string(), "#<procedure foo>");
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
        fn starts_with_peculiar() {
            let cases = ['+', '-', '.'];
            for case in cases {
                let s = format!("{case}foo");

                let v = Value::Symbol(s.clone().into());

                assert_eq!(v.as_datum().to_string(), s);
            }
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

    mod list {
        use super::*;

        #[test]
        fn empty() {
            let lst = zlist![];

            assert_eq!(lst.as_datum().to_string(), "()");
        }

        #[test]
        fn one() {
            let lst = zlist![Value::Constant(Constant::Number(Number::real(5)))];

            assert_eq!(lst.as_datum().to_string(), "(5)");
        }

        #[test]
        fn three() {
            let lst = zlist![
                Value::Constant(Constant::Number(Number::real(5))),
                Value::Symbol("a".into()),
                Value::Constant(Constant::Boolean(true)),
            ];

            assert_eq!(lst.as_datum().to_string(), "(5 a #t)");
        }

        #[test]
        fn nested() {
            let lst = zlist![
                Value::Constant(Constant::Number(Number::real(5))),
                zlist![
                    Value::Symbol("a".into()),
                    Value::Constant(Constant::Boolean(true)),
                ],
            ];

            assert_eq!(lst.as_datum().to_string(), "(5 (a #t))");
        }

        #[test]
        fn ctor_empty() {
            let lst = Value::list(vec![]);

            assert_eq!(lst.as_datum().to_string(), "()");
        }

        #[test]
        fn ctor_vec() {
            let lst = Value::list(vec![
                Value::Constant(Constant::Number(Number::real(5))),
                Value::Symbol("a".into()),
                Value::Constant(Constant::Boolean(true)),
            ]);

            assert_eq!(lst.as_datum().to_string(), "(5 a #t)");
        }

        #[test]
        fn ctor_slice() {
            let lst = Value::list([
                Value::Constant(Constant::Number(Number::real(5))),
                Value::Symbol("a".into()),
                Value::Constant(Constant::Boolean(true)),
            ]);

            assert_eq!(lst.as_datum().to_string(), "(5 a #t)");
        }

        #[test]
        fn improper_ctor_empty() {
            let lst = Value::improper_list(vec![]);

            assert_eq!(lst.as_datum().to_string(), "()");
        }

        #[test]
        fn improper_ctor_single() {
            let lst =
                Value::improper_list(vec![Value::Constant(Constant::Number(Number::real(5)))]);

            assert!(matches!(lst, Value::Constant(_)));
            assert_eq!(lst.as_datum().to_string(), "5");
        }

        #[test]
        fn improper_ctor_vec() {
            let lst = Value::improper_list(vec![
                Value::Constant(Constant::Number(Number::real(5))),
                Value::Symbol("a".into()),
                Value::Constant(Constant::Boolean(true)),
            ]);

            assert_eq!(lst.as_datum().to_string(), "(5 a . #t)");
        }

        #[test]
        fn improper_ctor_slice() {
            let lst = Value::improper_list([
                Value::Constant(Constant::Number(Number::real(5))),
                Value::Symbol("a".into()),
                Value::Constant(Constant::Boolean(true)),
            ]);

            assert_eq!(lst.as_datum().to_string(), "(5 a . #t)");
        }
    }

    mod condition {
        use super::*;

        #[test]
        fn display_empty_condition() {
            let c = Condition {
                kind: ConditionKind::General,
                msg: "foo".into(),
                irritants: None,
            };

            assert_eq!(c.as_datum().to_string(), "#<exception \"foo\">");
        }

        #[test]
        fn display_list_irritants() {
            let c = Condition {
                kind: ConditionKind::General,
                msg: "foo".into(),
                irritants: Some(
                    zlist![
                        Value::Symbol("a".into()),
                        Value::Constant(Constant::Number(Number::real(5)))
                    ]
                    .into(),
                ),
            };

            assert_eq!(c.as_datum().to_string(), "#<exception \"foo\" (a 5)>");
        }

        #[test]
        fn display_value_irritant() {
            let c = Condition {
                kind: ConditionKind::General,
                msg: "foo".into(),
                irritants: Some(Value::Constant(Constant::Boolean(true)).into()),
            };

            assert_eq!(c.as_datum().to_string(), "#<exception \"foo\" #t>");
        }

        #[test]
        fn display_env_condition() {
            let c = Condition {
                kind: ConditionKind::Env,
                msg: "foo".into(),
                irritants: None,
            };

            assert_eq!(c.as_datum().to_string(), "#<env-error \"foo\">");
        }

        #[test]
        fn display_file_condition() {
            let c = Condition {
                kind: ConditionKind::File,
                msg: "foo".into(),
                irritants: None,
            };

            assert_eq!(c.as_datum().to_string(), "#<file-error \"foo\">");
        }

        #[test]
        fn display_read_condition() {
            let c = Condition {
                kind: ConditionKind::Read,
                msg: "foo".into(),
                irritants: None,
            };

            assert_eq!(c.as_datum().to_string(), "#<read-error \"foo\">");
        }
    }
}
