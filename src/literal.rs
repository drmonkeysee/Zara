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
            Self::Character(c) => write!(f, "#\\{c}"),
        }
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
        fn display_character_name() {
            check_character_list(&[
                ('\u{7}', "alarm"),
                ('\u{8}', "backspace"),
                ('\u{7f}', "delete"),
                ('\u{1b}', "escape"),
                ('\n', "newline"),
                ('\0', "null"),
                ('\r', "return"),
                (' ', "space"),
                ('\t', "tab"),
            ]);
        }

        #[test]
        fn display_string_escape_characters() {
            check_character_list(&[('"', "\""), ('\'', "'"), ('\\', "\\"), ('\n', "")]);
        }

        fn check_character_list(cases: &[(char, &str)]) {
            for &(inp, exp) in cases {
                let c = Literal::Character(inp);

                assert_eq!(c.to_string(), format!("#\\{exp}"));
            }
        }
    }
}
