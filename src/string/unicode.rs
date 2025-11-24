use std::fmt::{self, Display, Formatter};

// NOTE: set of Unicode code points are in the
// ranges 0x0 to 0xD7FF and 0xE000 to 0x10FFFF, inclusive.
const MIN: char = char::MIN;
const LOW_MAX: char = '\u{d7ff}';
const HI_MIN: char = '\u{e000}';
const MAX: char = char::MAX;
// TODO: experimental https://doc.rust-lang.org/std/primitive.char.html#associatedconstant.MAX_LEN_UTF8
const MAX_UTF8_BYTES: usize = 4;

#[derive(Debug)]
pub(crate) enum UnicodeError {
    ByteSequenceEmpty,
    ByteSequenceInvalid([u8; MAX_UTF8_BYTES]),
    ByteSequenceTooLong(usize),
    CodePointOutOfRange,
    PrefixInvalid(u8),
}

impl Display for UnicodeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::ByteSequenceEmpty => f.write_str("empty utf-8 sequence"),
            Self::ByteSequenceInvalid(seq) => write_invalid_seq(seq, f),
            Self::ByteSequenceTooLong(len) => write!(
                f,
                "utf-8 sequence length out of range: [1, {MAX_UTF8_BYTES}], {len}"
            ),
            Self::CodePointOutOfRange => write!(
                f,
                "unicode code point out of ranges [#x{0:x}, #x{1:x}], [#x{2:x}, #x{3:x}] ([{0}, {1}], [{2}, {3}])",
                MIN as u32, LOW_MAX as u32, HI_MIN as u32, MAX as u32
            ),
            Self::PrefixInvalid(p) => write!(f, "invalid utf-8 prefix: #x{p:x}"),
        }
    }
}

pub(crate) fn utf8_char_len(prefix: u8) -> Result<usize, UnicodeError> {
    #[allow(clippy::cast_possible_truncation)]
    const MAX_UTF8_COUNT: u32 = MAX_UTF8_BYTES as u32;

    match prefix.leading_ones() {
        c @ 2..=MAX_UTF8_COUNT => Ok(c.try_into().expect("expected count within u8 range")),
        0 => Ok(1),
        _ => Err(UnicodeError::PrefixInvalid(prefix)),
    }
}

pub(crate) fn char_from_utf8(seq: &[u8]) -> Result<char, UnicodeError> {
    let ch = match seq.len() {
        0 => return Err(UnicodeError::ByteSequenceEmpty),
        1..=MAX_UTF8_BYTES => str::from_utf8(seq).ok().and_then(|s| s.chars().next()),
        len => return Err(UnicodeError::ByteSequenceTooLong(len)),
    };
    ch.ok_or_else(|| {
        let mut err = [0; MAX_UTF8_BYTES];
        err[0..seq.len()].copy_from_slice(seq);
        UnicodeError::ByteSequenceInvalid(err)
    })
}

fn write_invalid_seq(seq: &[u8], f: &mut Formatter<'_>) -> fmt::Result {
    write!(
        f,
        "invalid utf-8 sequence: [{}]",
        seq.iter()
            .rev()
            .skip_while(|b| **b == 0x0)
            .collect::<Vec<_>>()
            .into_iter()
            .rev()
            .map(|b| format!("#x{b:x}"))
            .collect::<Vec<_>>()
            .join(", ")
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::testutil::{err_or_fail, ok_or_fail};

    #[test]
    fn display_invalid_seq() {
        let err = UnicodeError::ByteSequenceInvalid([0x11, 0x22, 0xaa, 0xbb]);

        assert_eq!(
            err.to_string(),
            "invalid utf-8 sequence: [#x11, #x22, #xaa, #xbb]"
        )
    }

    #[test]
    fn display_invalid_seq_with_trailing_zeros() {
        let err = UnicodeError::ByteSequenceInvalid([0x11, 0x22, 0x0, 0x0]);

        assert_eq!(err.to_string(), "invalid utf-8 sequence: [#x11, #x22]")
    }

    #[test]
    fn display_invalid_seq_with_internal_zeros() {
        let err = UnicodeError::ByteSequenceInvalid([0x11, 0x0, 0x0, 0xbb]);

        assert_eq!(
            err.to_string(),
            "invalid utf-8 sequence: [#x11, #x0, #x0, #xbb]"
        )
    }

    #[test]
    fn display_invalid_seq_with_mix_of_zeros() {
        let err = UnicodeError::ByteSequenceInvalid([0x0, 0x22, 0xaa, 0x0]);

        assert_eq!(err.to_string(), "invalid utf-8 sequence: [#x0, #x22, #xaa]")
    }

    #[test]
    fn char_len_for_zero() {
        let ch = 0b0000_0000u8;

        let len = utf8_char_len(ch);

        assert_eq!(ok_or_fail!(len), 1);
    }

    #[test]
    fn char_len_for_max_ascii() {
        let ch = 0b0111_1111u8;

        let len = utf8_char_len(ch);

        assert_eq!(ok_or_fail!(len), 1);
    }

    #[test]
    fn char_len_for_continuation() {
        let ch = 0b1000_0000u8;

        let len = utf8_char_len(ch);

        assert!(matches!(
            err_or_fail!(len),
            UnicodeError::PrefixInvalid(0x80)
        ));
    }

    #[test]
    fn char_len_for_two_bytes() {
        let ch = 0b1100_0000u8;

        let len = utf8_char_len(ch);

        assert_eq!(ok_or_fail!(len), 2);
    }

    #[test]
    fn char_len_for_two_bytes_with_codepoint_bits() {
        let ch = 0b1101_0000u8;

        let len = utf8_char_len(ch);

        assert_eq!(ok_or_fail!(len), 2);
    }

    #[test]
    fn char_len_for_three_bytes() {
        let ch = 0b1110_0000u8;

        let len = utf8_char_len(ch);

        assert_eq!(ok_or_fail!(len), 3);
    }

    #[test]
    fn char_len_for_four_bytes() {
        let ch = 0b1111_0000u8;

        let len = utf8_char_len(ch);

        assert_eq!(ok_or_fail!(len), 4);
    }

    #[test]
    fn char_from_utf8_empty() {
        let seq = [];

        let r = char_from_utf8(&seq);

        assert!(matches!(err_or_fail!(r), UnicodeError::ByteSequenceEmpty));
    }

    #[test]
    fn char_from_utf8_too_long() {
        let seq = [1, 2, 3, 4, 5];

        let r = char_from_utf8(&seq);

        assert!(matches!(
            err_or_fail!(r),
            UnicodeError::ByteSequenceTooLong(5)
        ));
    }

    #[test]
    fn char_from_utf8_single_byte() {
        let seq = [0x4d];

        let r = char_from_utf8(&seq);

        assert_eq!(ok_or_fail!(r), 'M');
    }

    #[test]
    fn char_from_utf8_invalid_single_byte() {
        let seq = [0b1011_0111];

        let r = char_from_utf8(&seq);

        assert!(
            matches!(err_or_fail!(r), UnicodeError::ByteSequenceInvalid(sq) if sq == [0xb7, 0x0, 0x0, 0x0])
        );
    }

    #[test]
    fn char_from_utf8_double_byte() {
        let seq = [0xc3, 0xa9];

        let r = char_from_utf8(&seq);

        assert_eq!(ok_or_fail!(r), 'é');
    }

    #[test]
    fn char_from_utf8_truncated_double_byte() {
        let seq = [0xc3];

        let r = char_from_utf8(&seq);

        assert!(
            matches!(err_or_fail!(r), UnicodeError::ByteSequenceInvalid(sq) if sq == [0xc3, 0x0, 0x0, 0x0])
        );
    }

    #[test]
    fn char_from_utf8_triple_byte() {
        let seq = [0xe2, 0x88, 0xab];

        let r = char_from_utf8(&seq);

        assert_eq!(ok_or_fail!(r), '∫');
    }

    #[test]
    fn char_from_utf8_quadruple_byte() {
        let seq = [0xf0, 0x9f, 0xa6, 0x80];

        let r = char_from_utf8(&seq);

        assert_eq!(ok_or_fail!(r), '🦀');
    }
}
