pub(crate) mod identifier;
use std::{
    cell::Cell,
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
            write_str_char(ch, f)?;
        }
        f.write_char('"')
    }
}

pub(crate) struct SymbolDatum<'a>(pub(crate) &'a str);

impl Display for SymbolDatum<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let c = SymbolConverter::new(self.0);
        let (output, verbatim) = c.into_string();
        if verbatim {
            write!(f, "|{output}|")
        } else {
            f.write_str(&output)
        }
    }
}

enum DisplayableChar {
    Char(char),
    Hex(u32),
}

struct SymbolConverter<'a> {
    name: &'a str,
    verbatim: Cell<bool>,
}

impl<'a> SymbolConverter<'a> {
    fn new(name: &'a str) -> Self {
        Self {
            name,
            verbatim: false.into(),
        }
    }

    fn into_string(self) -> (String, bool) {
        (self.to_string(), self.verbatim.get())
    }
}

impl Display for SymbolConverter<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let mut chars = self.name.chars();
        match chars.next() {
            None => self.verbatim.set(true), // NOTE: empty string
            Some(ch) => {
                if !identifier::is_initial(ch) && !identifier::is_peculiar_initial(ch) {
                    self.verbatim.set(true);
                }
                write_symbol_char(ch, f)?;
                for ch in chars {
                    if !identifier::is_standard(ch) {
                        self.verbatim.set(true);
                    }
                    write_symbol_char(ch, f)?;
                }
            }
        }
        Ok(())
    }
}

fn write_str_char(ch: char, f: &mut Formatter) -> fmt::Result {
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
        _ => write_literal_str_char(ch, f),
    }
}

fn write_literal_str_char(ch: char, f: &mut Formatter) -> fmt::Result {
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

fn write_symbol_char(ch: char, f: &mut Formatter<'_>) -> fmt::Result {
    match ch {
        // NOTE: verbatim delimiter
        '|' => f.write_str("\\|"),
        // NOTE: unlike for string, " is not a delimiter and should not be escaped
        '"' => f.write_char(ch),
        _ => write_str_char(ch, f),
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
