use std::{
    cmp::Ordering,
    fmt::{Display, Formatter, Result},
};

#[derive(Debug)]
pub enum Literal {
    Boolean(bool),
    Character(char),
}

impl Display for Literal {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::Boolean(b) => write!(f, "#{}", if *b { 't' } else { 'f' }),
            Self::Character(c) => write!(f, "#\\{}", format_char(*c)),
        }
    }
}

fn format_char(ch: char) -> String {
    match ch {
        '\x07' => "alarm".to_owned(),
        '\x08' => "backspace".to_owned(),
        '\x7f' => "delete".to_owned(),
        '\x1b' => "escape".to_owned(),
        '\n' => "newline".to_owned(),
        '\0' => "null".to_owned(),
        '\r' => "return".to_owned(),
        ' ' => "space".to_owned(),
        '\t' => "tab".to_owned(),
        _ => format_unnamed_char(ch),
    }
}

fn format_unnamed_char(ch: char) -> String {
    // NOTE: this is a little weird but there's no Unicode classification
    // exposed in Rust's stdlib to tell if a character has a dedicated glyph or
    // not, so check indirectly by seeing if the debug output starts with `\u`;
    // if so, we display the hex representation instead of the char literal.
    if ch.escape_debug().take(2).cmp(['\\', 'u']) == Ordering::Equal {
        let ord = u32::from(ch);
        format!("x{ord:x}")
    } else {
        ch.to_string()
    }
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
        fn display_control_picture() {
            let c = Literal::Character('\u{2401}');

            assert_eq!(c.to_string(), "#\\␁");
        }

        #[test]
        fn display_replacement_char() {
            let c = Literal::Character('\u{fffd}');

            assert_eq!(c.to_string(), "#\\�");
        }

        #[test]
        fn display_one_digit_hex() {
            let c = Literal::Character('\x0c');

            assert_eq!(c.to_string(), "#\\xc");
        }

        #[test]
        fn display_hex_uses_lowercase() {
            let c = Literal::Character('\x0C');

            assert_eq!(c.to_string(), "#\\xc");
        }

        #[test]
        fn display_two_digit_hex() {
            let c = Literal::Character('\x1d');

            assert_eq!(c.to_string(), "#\\x1d");
        }

        #[test]
        fn display_four_digit_hex() {
            let c = Literal::Character('\u{fff9}');

            assert_eq!(c.to_string(), "#\\xfff9");
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
