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
