pub(crate) mod identifier;
use std::{
    cmp::Ordering,
    fmt::{self, Display, Formatter, Write},
};

pub(crate) enum CharDatum {
    Named(&'static str),
    Unnamed(char),
}

impl CharDatum {
    pub(crate) fn new(ch: char) -> Self {
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

pub(crate) struct StrDatum<'a>(pub(crate) &'a str);

impl Display for StrDatum<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.write_char('"')?;
        for ch in self.0.chars() {
            write_text_char(ch, f)?;
        }
        f.write_char('"')
    }
}

pub(crate) struct SymbolDatum<'a>(pub(crate) &'a str);

impl Display for SymbolDatum<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for ch in self.0.chars() {
            write_text_char(ch, f)?;
        }
        Ok(())
    }
}

enum DisplayableChar {
    Char(char),
    Hex(u32),
}

fn write_text_char(ch: char, f: &mut Formatter) -> fmt::Result {
    match ch {
        // NOTE: Rust displays NUL directly, fooling DisplayableChar,
        // so handle as a special case here.
        '\x00' => f.write_str("\\x0;"),
        '\x07' => f.write_str("\\a"),
        '\x08' => f.write_str("\\b"),
        '\n' => f.write_str("\\n"),
        '\r' => f.write_str("\\r"),
        '\t' => f.write_str("\\t"),
        '"' => f.write_str("\\\""),
        '\\' => f.write_str("\\\\"),
        _ => write_str_chr(ch, f),
    }
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
