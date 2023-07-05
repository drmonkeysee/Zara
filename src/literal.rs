use std::{
    fmt::{Display, Error, Formatter},
    write,
};

#[derive(Debug)]
pub enum Literal {
    Boolean(bool),
    Character(char),
}

impl Display for Literal {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match self {
            Self::Boolean(b) => write!(f, "#{}", if *b { 't' } else { 'f' }),
            Self::Character(c) => display_char(*c, f),
        }
    }
}

fn display_char(ch: char, f: &mut Formatter<'_>) -> Result<(), Error> {
    write!(
        f,
        "{}",
        match ch {
            '\x07' => Some("alarm"),
            '\x08' => Some("backspace"),
            '\x7f' => Some("delete"),
            '\x1b' => Some("escape"),
            '\n' => Some("newline"),
            '\0' => Some("null"),
            '\r' => Some("return"),
            ' ' => Some("space"),
            '\t' => Some("tab"),
            _ => None,
        }
        .map_or_else(|| format!("#\\{ch}"), |n| format!("#\\{n}"))
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    mod boolean {
        use super::*;

        #[test]
        fn display_true() {
            let b = Literal::Boolean(true);

            assert_eq!(b.to_string(), "#t");
        }

        #[test]
        fn display_false() {
            let b = Literal::Boolean(false);

            assert_eq!(b.to_string(), "#f");
        }
    }

    mod character {
        use super::*;

        #[test]
        fn display_ascii() {
            let c = Literal::Character('a');

            assert_eq!(c.to_string(), "#\\a");
        }

        #[test]
        fn display_extended_char() {
            let c = Literal::Character('λ');

            assert_eq!(c.to_string(), "#\\λ");
        }

        #[test]
        fn display_emoji() {
            let c = Literal::Character('🦀');

            assert_eq!(c.to_string(), "#\\🦀");
        }

        #[test]
        fn display_one_digit_hex() {
            let c = Literal::Character('\x0c');

            assert_eq!(c.to_string(), "#\\xc");
        }

        #[test]
        fn display_two_digit_hex() {
            let c = Literal::Character('\x1d');

            assert_eq!(c.to_string(), "#\\x1d");
        }

        #[test]
        fn display_four_digit_hex() {
            let c = Literal::Character('\u{2401}');

            assert_eq!(c.to_string(), "#\\x2401");
        }

        #[test]
        fn display_special_purpose_plane() {
            let c = Literal::Character('\u{e0001}');

            assert_eq!(c.to_string(), "#\\xe0001");
        }

        #[test]
        fn display_private_use_plane() {
            let c = Literal::Character('\u{100001}');

            assert_eq!(c.to_string(), "#\\x100001");
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

                assert_eq!(c.to_string(), format!("#\\{exp}"));
            }
        }
    }
}
