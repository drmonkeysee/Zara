use std::fmt::{self, Display, Formatter};

// NOTE: set of Unicode code points are in the
// ranges 0x0 to 0xD7FF and 0xE000 to 0x10FFFF, inclusive.
const MIN: char = char::MIN;
const LOW_MAX: char = '\u{d7ff}';
const HI_MIN: char = '\u{e000}';
const MAX: char = char::MAX;

pub(crate) enum UnicodeError {
    CodePointOutOfRange,
}

impl Display for UnicodeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::CodePointOutOfRange => write!(
                f,
                "unicode code point out of ranges [#x{0:X}, #x{1:X}], [#x{2:X}, #x{3:X}] ([{0}, {1}], [{2}, {3}])",
                MIN as u32, LOW_MAX as u32, HI_MIN as u32, MAX as u32
            ),
        }
    }
}

pub(crate) fn utf8_char_len(byte: u8) -> Option<usize> {
    match byte >> 4 {
        0b1111u8 => Some(4),
        0b1110u8 => Some(3),
        0b1100u8 | 0b1101u8 => Some(2),
        0b0u8..0b1000u8 => Some(1),
        _ => None,
    }
}

pub(crate) fn char_from_utf8(bytes: &[u8]) -> Result<char, UnicodeError> {
    todo!();
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::testutil::some_or_fail;

    #[test]
    fn char_len_for_zero() {
        let ch = 0b0000_0000u8;

        let len = utf8_char_len(ch);

        assert_eq!(some_or_fail!(len), 1);
    }

    #[test]
    fn char_len_for_max_ascii() {
        let ch = 0b0111_1111u8;

        let len = utf8_char_len(ch);

        assert_eq!(some_or_fail!(len), 1);
    }

    #[test]
    fn char_len_for_continuation() {
        let ch = 0b1000_0000u8;

        let len = utf8_char_len(ch);

        assert!(len.is_none());
    }

    #[test]
    fn char_len_for_two_bytes() {
        let ch = 0b1100_0000u8;

        let len = utf8_char_len(ch);

        assert_eq!(some_or_fail!(len), 2);
    }

    #[test]
    fn char_len_for_two_bytes_with_codepoint_bits() {
        let ch = 0b1101_0000u8;

        let len = utf8_char_len(ch);

        assert_eq!(some_or_fail!(len), 2);
    }

    #[test]
    fn char_len_for_three_bytes() {
        let ch = 0b1110_0000u8;

        let len = utf8_char_len(ch);

        assert_eq!(some_or_fail!(len), 3);
    }

    #[test]
    fn char_len_for_four_bytes() {
        let ch = 0b1111_0000u8;

        let len = utf8_char_len(ch);

        assert_eq!(some_or_fail!(len), 4);
    }
}
