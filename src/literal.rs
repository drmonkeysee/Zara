use crate::number::Number;
use std::{
    cmp::Ordering,
    fmt::{Display, Formatter, Result, Write},
};

#[derive(Debug)]
pub(crate) enum Literal {
    Boolean(bool),
    Character(char),
    Number(Number),
    String(String), // TODO: can this be Box<str>
}

impl Literal {
    pub(crate) fn as_datum(&self) -> Datum {
        Datum(self)
    }

    pub(crate) fn as_token_descriptor(&self) -> TokenDescriptor {
        TokenDescriptor(self)
    }
}

pub(crate) struct Datum<'a>(&'a Literal);

impl Display for Datum<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self.0 {
            Literal::Boolean(b) => write!(f, "#{}", if *b { 't' } else { 'f' }),
            Literal::Character(c) => write!(f, "#\\{}", CharDatum::new(*c)),
            Literal::Number(n) => n.as_datum().fmt(f),
            Literal::String(s) => StrDatum(s).fmt(f),
        }
    }
}

pub(crate) struct TokenDescriptor<'a>(&'a Literal);

impl Display for TokenDescriptor<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self.0 {
            Literal::Boolean(_) => f.write_str("BOOL"),
            Literal::Character(_) => f.write_str("CHAR"),
            Literal::Number(n) => write!(f, "NUM<{}>", n.as_token_descriptor()),
            Literal::String(_) => f.write_str("STR"),
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
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::Named(n) => f.write_str(n),
            Self::Unnamed(ch) => write_unnamed_char(*ch, f),
        }
    }
}

struct StrDatum<'a>(&'a str);

impl Display for StrDatum<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
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

fn write_str_chr(ch: char, f: &mut Formatter<'_>) -> Result {
    match char_to_displayable(ch) {
        DisplayableChar::Char(ch) => f.write_char(ch),
        DisplayableChar::Hex(hex) => write!(f, "\\x{hex:x};"),
    }
}

fn write_unnamed_char(ch: char, f: &mut Formatter<'_>) -> Result {
    match char_to_displayable(ch) {
        DisplayableChar::Char(ch) => f.write_char(ch),
        DisplayableChar::Hex(hex) => write!(f, "x{hex:x}"),
    }
}

fn char_to_displayable(ch: char) -> DisplayableChar {
    // NOTE: this is a little weird but there's no Unicode classification
    // exposed in Rust's stdlib to tell if a character has a dedicated glyph or
    // not, so check indirectly by seeing if the debug output starts with `\u`;
    // if so, we display the hex representation instead of the char literal.
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
            let b = Literal::Boolean(false);

            assert_eq!(b.as_token_descriptor().to_string(), "BOOL");
        }

        #[test]
        fn display_true() {
            let b = Literal::Boolean(true);

            assert_eq!(b.as_datum().to_string(), "#t");
        }

        #[test]
        fn display_false() {
            let b = Literal::Boolean(false);

            assert_eq!(b.as_datum().to_string(), "#f");
        }
    }

    mod character {
        use super::*;

        #[test]
        fn char_token() {
            let c = Literal::Character('a');

            assert_eq!(c.as_token_descriptor().to_string(), "CHAR");
        }

        #[test]
        fn display_ascii() {
            let c = Literal::Character('a');

            assert_eq!(c.as_datum().to_string(), "#\\a");
        }

        #[test]
        fn display_extended_char() {
            let c = Literal::Character('λ');

            assert_eq!(c.as_datum().to_string(), "#\\λ");
        }

        #[test]
        fn display_emoji() {
            let c = Literal::Character('🦀');

            assert_eq!(c.as_datum().to_string(), "#\\🦀");
        }

        #[test]
        fn display_control_picture() {
            let c = Literal::Character('\u{2401}');

            assert_eq!(c.as_datum().to_string(), "#\\␁");
        }

        #[test]
        fn display_replacement_char() {
            let c = Literal::Character('\u{fffd}');

            assert_eq!(c.as_datum().to_string(), "#\\�");
        }

        #[test]
        fn display_one_digit_hex() {
            let c = Literal::Character('\x0c');

            assert_eq!(c.as_datum().to_string(), "#\\xc");
        }

        #[test]
        fn display_hex_uses_lowercase() {
            let c = Literal::Character('\x0C');

            assert_eq!(c.as_datum().to_string(), "#\\xc");
        }

        #[test]
        fn display_two_digit_hex() {
            let c = Literal::Character('\x1d');

            assert_eq!(c.as_datum().to_string(), "#\\x1d");
        }

        #[test]
        fn display_four_digit_hex() {
            let c = Literal::Character('\u{fff9}');

            assert_eq!(c.as_datum().to_string(), "#\\xfff9");
        }

        #[test]
        fn display_special_purpose_plane() {
            let c = Literal::Character('\u{e0001}');

            assert_eq!(c.as_datum().to_string(), "#\\xe0001");
        }

        #[test]
        fn display_private_use_plane() {
            let c = Literal::Character('\u{100001}');

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
                let c = Literal::Character(inp);

                assert_eq!(c.as_datum().to_string(), format!("#\\{exp}"));
            }
        }
    }

    mod string {
        use super::*;

        #[test]
        fn string_token() {
            let s = Literal::String("foo".to_owned());

            assert_eq!(s.as_token_descriptor().to_string(), "STR");
        }

        #[test]
        fn display_empty() {
            let s = Literal::String("".to_owned());

            assert_eq!(s.as_datum().to_string(), "\"\"");
        }

        #[test]
        fn display_alphanumeric() {
            let s = Literal::String("abc123!@#".to_owned());

            assert_eq!(s.as_datum().to_string(), "\"abc123!@#\"");
        }

        #[test]
        fn display_extended() {
            let s = Literal::String("λ".to_owned());

            assert_eq!(s.as_datum().to_string(), "\"λ\"");
        }

        #[test]
        fn display_emoji() {
            let s = Literal::String("🦀".to_owned());

            assert_eq!(s.as_datum().to_string(), "\"🦀\"");
        }

        #[test]
        fn display_control_picture() {
            let s = Literal::String("\u{2401}".to_owned());

            assert_eq!(s.as_datum().to_string(), "\"␁\"");
        }

        #[test]
        fn display_replacement() {
            let s = Literal::String("\u{fffd}".to_owned());

            assert_eq!(s.as_datum().to_string(), "\"�\"");
        }

        #[test]
        fn display_null() {
            let s = Literal::String("\0".to_owned());

            assert_eq!(s.as_datum().to_string(), "\"\\x0;\"");
        }

        #[test]
        fn display_pipe() {
            let s = Literal::String("|".to_owned());

            assert_eq!(s.as_datum().to_string(), "\"|\"");
        }

        #[test]
        fn display_one_digit_hex() {
            let s = Literal::String("\x0c".to_owned());

            assert_eq!(s.as_datum().to_string(), "\"\\xc;\"");
        }

        #[test]
        fn display_hex_uses_lowercase() {
            let s = Literal::String("\x0C".to_owned());

            assert_eq!(s.as_datum().to_string(), "\"\\xc;\"");
        }

        #[test]
        fn display_two_digit_hex() {
            let s = Literal::String("\x1d".to_owned());

            assert_eq!(s.as_datum().to_string(), "\"\\x1d;\"");
        }

        #[test]
        fn display_four_digit_hex() {
            let s = Literal::String("\u{fff9}".to_owned());

            assert_eq!(s.as_datum().to_string(), "\"\\xfff9;\"");
        }

        #[test]
        fn display_special_purpose_plane() {
            let s = Literal::String("\u{e0001}".to_owned());

            assert_eq!(s.as_datum().to_string(), "\"\\xe0001;\"");
        }

        #[test]
        fn display_private_use_plane() {
            let s = Literal::String("\u{100001}".to_owned());

            assert_eq!(s.as_datum().to_string(), "\"\\x100001;\"");
        }

        #[test]
        fn display_literal_endline() {
            let s = Literal::String(
                "foo
bar"
                .to_owned(),
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
                let s = Literal::String(inp.to_owned());

                assert_eq!(s.as_datum().to_string(), format!("\"{exp}\""));
            }
        }
    }

    mod number {
        use super::*;
        use crate::{number::Real, testutil::ok_or_fail};

        #[test]
        fn integer_token() {
            let n = Literal::Number(Number::real(42));

            assert_eq!(n.as_token_descriptor().to_string(), "NUM<INT>");
        }

        #[test]
        fn float_token() {
            let n = Literal::Number(Number::real(4.2));

            assert_eq!(n.as_token_descriptor().to_string(), "NUM<FLT>");
        }

        #[test]
        fn rational_token() {
            let r = ok_or_fail!(Real::reduce(4, 5));
            let n = Literal::Number(Number::Real(r));

            assert_eq!(n.as_token_descriptor().to_string(), "NUM<RAT>");
        }

        #[test]
        fn complex_token() {
            let n = Literal::Number(Number::complex(3, 5));

            assert_eq!(n.as_token_descriptor().to_string(), "NUM<CPX>");
        }

        #[test]
        fn display_int() {
            let n = Literal::Number(Number::real(23));

            assert_eq!(n.as_datum().to_string(), "23");
        }

        #[test]
        fn display_float() {
            let n = Literal::Number(Number::real(234.23));

            assert_eq!(n.as_datum().to_string(), "234.23");
        }

        #[test]
        fn display_rational() {
            let r = ok_or_fail!(Real::reduce(3, 4));
            let n = Literal::Number(Number::Real(r));

            assert_eq!(n.as_datum().to_string(), "3/4");
        }

        #[test]
        fn display_complex() {
            let n = Literal::Number(Number::complex(4, 5));

            assert_eq!(n.as_datum().to_string(), "4+5i");
        }
    }
}
