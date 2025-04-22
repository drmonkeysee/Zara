use crate::number::Number;
use std::{
    cmp::Ordering,
    fmt::{self, Display, Formatter, Write},
};

#[derive(Debug)]
pub(crate) enum Constant {
    Boolean(bool),
    Character(char),
    Number(Number),
    String(Box<str>),
}

impl Constant {
    pub(crate) fn as_datum(&self) -> Datum {
        Datum(self)
    }

    pub(crate) fn as_token_descriptor(&self) -> TokenDescriptor {
        TokenDescriptor(self)
    }

    pub(crate) fn as_typename(&self) -> TypeName {
        TypeName(self)
    }
}

pub(crate) struct Datum<'a>(&'a Constant);

impl Display for Datum<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self.0 {
            Constant::Boolean(b) => write!(f, "#{}", if *b { 't' } else { 'f' }),
            Constant::Character(c) => write!(f, "#\\{}", CharDatum::new(*c)),
            Constant::Number(n) => n.as_datum().fmt(f),
            Constant::String(s) => StrDatum(s).fmt(f),
        }
    }
}

pub(crate) struct TokenDescriptor<'a>(&'a Constant);

impl Display for TokenDescriptor<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self.0 {
            Constant::Boolean(_) => f.write_str("BOOL"),
            Constant::Character(_) => f.write_str("CHAR"),
            Constant::Number(n) => write!(f, "NUM<{}>", n.as_token_descriptor()),
            Constant::String(_) => f.write_str("STR"),
        }
    }
}

pub(crate) struct TypeName<'a>(&'a Constant);

impl Display for TypeName<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.0 {
            Constant::Boolean(_) => f.write_str("boolean"),
            Constant::Character(_) => f.write_str("character"),
            Constant::Number(_) => f.write_str("number"),
            Constant::String(_) => f.write_str("string"),
        }
    }
}

enum CharDatum {
    Named(&'static str),
    Unnamed(char),
}

impl CharDatum {
    fn new(ch: char) -> Self {
        match ch {
            '\x07' => Self::Named("alarm"),
            '\x08' => Self::Named("backspace"),
            '\x7f' => Self::Named("delete"),
            '\x1b' => Self::Named("escape"),
            '\n' => Self::Named("newline"),
            '\0' => Self::Named("null"),
            '\r' => Self::Named("return"),
            ' ' => Self::Named("space"),
            '\t' => Self::Named("tab"),
            ch => Self::Unnamed(ch),
        }
    }
}

impl Display for CharDatum {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::Named(n) => f.write_str(n),
            Self::Unnamed(ch) => write_unnamed_char(*ch, f),
        }
    }
}

struct StrDatum<'a>(&'a str);

impl Display for StrDatum<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.write_char('"')?;
        for ch in self.0.chars() {
            match ch {
                // NOTE: Rust displays NUL directly, fooling DisplayableChar,
                // so handle as a special case here.
                '\x00' => f.write_str("\\x0;")?,
                '\x07' => f.write_str("\\a")?,
                '\x08' => f.write_str("\\b")?,
                '\n' => f.write_str("\\n")?,
                '\r' => f.write_str("\\r")?,
                '\t' => f.write_str("\\t")?,
                '"' => f.write_str("\\\"")?,
                '\\' => f.write_str("\\\\")?,
                _ => write_str_chr(ch, f)?,
            }
        }
        f.write_char('"')
    }
}

enum DisplayableChar {
    Char(char),
    Hex(u32),
}

fn write_str_chr(ch: char, f: &mut Formatter) -> fmt::Result {
    match char_to_displayable(ch) {
        DisplayableChar::Char(ch) => f.write_char(ch),
        DisplayableChar::Hex(hex) => write!(f, "\\x{hex:x};"),
    }
}

fn write_unnamed_char(ch: char, f: &mut Formatter) -> fmt::Result {
    match char_to_displayable(ch) {
        DisplayableChar::Char(ch) => f.write_char(ch),
        DisplayableChar::Hex(hex) => write!(f, "x{hex:x}"),
    }
}

fn char_to_displayable(ch: char) -> DisplayableChar {
    // NOTE: this is a little weird but there's no Unicode classification
    // exposed in Rust's stdlib to tell if a character has a dedicated glyph or
    // not, so check indirectly by seeing if the debug output starts with `\u`;
    // if so, we display the hex representation instead of the char constant.
    if ch.escape_debug().take(2).cmp(['\\', 'u']) == Ordering::Equal {
        DisplayableChar::Hex(ch as u32)
    } else {
        DisplayableChar::Char(ch)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    mod boolean {
        use super::*;

        #[test]
        fn bool_token() {
            let b = Constant::Boolean(false);

            assert_eq!(b.as_token_descriptor().to_string(), "BOOL");
        }

        #[test]
        fn display_true() {
            let b = Constant::Boolean(true);

            assert_eq!(b.as_datum().to_string(), "#t");
        }

        #[test]
        fn display_false() {
            let b = Constant::Boolean(false);

            assert_eq!(b.as_datum().to_string(), "#f");
        }

        #[test]
        fn display_typename() {
            let b = Constant::Boolean(false);

            assert_eq!(b.as_typename().to_string(), "boolean");
        }
    }

    mod character {
        use super::*;

        #[test]
        fn char_token() {
            let c = Constant::Character('a');

            assert_eq!(c.as_token_descriptor().to_string(), "CHAR");
        }

        #[test]
        fn display_ascii() {
            let c = Constant::Character('a');

            assert_eq!(c.as_datum().to_string(), "#\\a");
        }

        #[test]
        fn display_extended_char() {
            let c = Constant::Character('λ');

            assert_eq!(c.as_datum().to_string(), "#\\λ");
        }

        #[test]
        fn display_emoji() {
            let c = Constant::Character('🦀');

            assert_eq!(c.as_datum().to_string(), "#\\🦀");
        }

        #[test]
        fn display_control_picture() {
            let c = Constant::Character('\u{2401}');

            assert_eq!(c.as_datum().to_string(), "#\\␁");
        }

        #[test]
        fn display_replacement_char() {
            let c = Constant::Character('\u{fffd}');

            assert_eq!(c.as_datum().to_string(), "#\\�");
        }

        #[test]
        fn display_one_digit_hex() {
            let c = Constant::Character('\x0c');

            assert_eq!(c.as_datum().to_string(), "#\\xc");
        }

        #[test]
        fn display_hex_uses_lowercase() {
            let c = Constant::Character('\x0C');

            assert_eq!(c.as_datum().to_string(), "#\\xc");
        }

        #[test]
        fn display_two_digit_hex() {
            let c = Constant::Character('\x1d');

            assert_eq!(c.as_datum().to_string(), "#\\x1d");
        }

        #[test]
        fn display_four_digit_hex() {
            let c = Constant::Character('\u{fff9}');

            assert_eq!(c.as_datum().to_string(), "#\\xfff9");
        }

        #[test]
        fn display_special_purpose_plane() {
            let c = Constant::Character('\u{e0001}');

            assert_eq!(c.as_datum().to_string(), "#\\xe0001");
        }

        #[test]
        fn display_private_use_plane() {
            let c = Constant::Character('\u{100001}');

            assert_eq!(c.as_datum().to_string(), "#\\x100001");
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
                let c = Constant::Character(inp);

                assert_eq!(c.as_datum().to_string(), format!("#\\{exp}"));
            }
        }

        #[test]
        fn display_typename() {
            let c = Constant::Character('a');

            assert_eq!(c.as_typename().to_string(), "character");
        }
    }

    mod string {
        use super::*;

        #[test]
        fn string_token() {
            let s = Constant::String("foo".into());

            assert_eq!(s.as_token_descriptor().to_string(), "STR");
        }

        #[test]
        fn display_empty() {
            let s = Constant::String("".into());

            assert_eq!(s.as_datum().to_string(), "\"\"");
        }

        #[test]
        fn display_alphanumeric() {
            let s = Constant::String("abc123!@#".into());

            assert_eq!(s.as_datum().to_string(), "\"abc123!@#\"");
        }

        #[test]
        fn display_extended() {
            let s = Constant::String("λ".into());

            assert_eq!(s.as_datum().to_string(), "\"λ\"");
        }

        #[test]
        fn display_emoji() {
            let s = Constant::String("🦀".into());

            assert_eq!(s.as_datum().to_string(), "\"🦀\"");
        }

        #[test]
        fn display_control_picture() {
            let s = Constant::String("\u{2401}".into());

            assert_eq!(s.as_datum().to_string(), "\"␁\"");
        }

        #[test]
        fn display_replacement() {
            let s = Constant::String("\u{fffd}".into());

            assert_eq!(s.as_datum().to_string(), "\"�\"");
        }

        #[test]
        fn display_null() {
            let s = Constant::String("\0".into());

            assert_eq!(s.as_datum().to_string(), "\"\\x0;\"");
        }

        #[test]
        fn display_pipe() {
            let s = Constant::String("|".into());

            assert_eq!(s.as_datum().to_string(), "\"|\"");
        }

        #[test]
        fn display_one_digit_hex() {
            let s = Constant::String("\x0c".into());

            assert_eq!(s.as_datum().to_string(), "\"\\xc;\"");
        }

        #[test]
        fn display_hex_uses_lowercase() {
            let s = Constant::String("\x0C".into());

            assert_eq!(s.as_datum().to_string(), "\"\\xc;\"");
        }

        #[test]
        fn display_two_digit_hex() {
            let s = Constant::String("\x1d".into());

            assert_eq!(s.as_datum().to_string(), "\"\\x1d;\"");
        }

        #[test]
        fn display_four_digit_hex() {
            let s = Constant::String("\u{fff9}".into());

            assert_eq!(s.as_datum().to_string(), "\"\\xfff9;\"");
        }

        #[test]
        fn display_special_purpose_plane() {
            let s = Constant::String("\u{e0001}".into());

            assert_eq!(s.as_datum().to_string(), "\"\\xe0001;\"");
        }

        #[test]
        fn display_private_use_plane() {
            let s = Constant::String("\u{100001}".into());

            assert_eq!(s.as_datum().to_string(), "\"\\x100001;\"");
        }

        #[test]
        fn display_literal_endline() {
            let s = Constant::String(
                "foo
bar"
                .into(),
            );

            assert_eq!(s.as_datum().to_string(), "\"foo\\nbar\"");
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
                let s = Constant::String(inp.into());

                assert_eq!(s.as_datum().to_string(), format!("\"{exp}\""));
            }
        }

        #[test]
        fn display_typename() {
            let s = Constant::String("foo".into());

            assert_eq!(s.as_typename().to_string(), "string");
        }
    }

    mod number {
        use super::*;
        use crate::{number::Real, testutil::ok_or_fail};

        #[test]
        fn integer_token() {
            let n = Constant::Number(Number::real(42));

            assert_eq!(n.as_token_descriptor().to_string(), "NUM<INT>");
        }

        #[test]
        fn float_token() {
            let n = Constant::Number(Number::real(4.2));

            assert_eq!(n.as_token_descriptor().to_string(), "NUM<FLT>");
        }

        #[test]
        fn rational_token() {
            let r = ok_or_fail!(Real::reduce(4, 5));
            let n = Constant::Number(Number::Real(r));

            assert_eq!(n.as_token_descriptor().to_string(), "NUM<RAT>");
        }

        #[test]
        fn complex_token() {
            let n = Constant::Number(Number::complex(3, 5));

            assert_eq!(n.as_token_descriptor().to_string(), "NUM<CPX>");
        }

        #[test]
        fn display_int() {
            let n = Constant::Number(Number::real(23));

            assert_eq!(n.as_datum().to_string(), "23");
        }

        #[test]
        fn display_float() {
            let n = Constant::Number(Number::real(234.23));

            assert_eq!(n.as_datum().to_string(), "234.23");
        }

        #[test]
        fn display_rational() {
            let r = ok_or_fail!(Real::reduce(3, 4));
            let n = Constant::Number(Number::Real(r));

            assert_eq!(n.as_datum().to_string(), "3/4");
        }

        #[test]
        fn display_complex() {
            let n = Constant::Number(Number::complex(4, 5));

            assert_eq!(n.as_datum().to_string(), "4+5i");
        }

        #[test]
        fn display_typename() {
            let n = Constant::Number(Number::real(42));

            assert_eq!(n.as_typename().to_string(), "number");
        }
    }
}
