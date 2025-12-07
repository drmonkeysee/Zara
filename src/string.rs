pub(crate) mod identifier;
pub(crate) mod unicode;

use std::{
    borrow::Borrow,
    cell::{Cell, RefCell},
    cmp::Ordering,
    collections::HashSet,
    fmt::{self, Display, Formatter, Write},
    ops::Deref,
    rc::Rc,
};

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub(crate) struct Symbol(Rc<str>);

impl Symbol {
    fn new(name: impl AsRef<str>) -> Self {
        Self(name.as_ref().into())
    }

    pub(crate) fn is(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }

    pub(crate) fn as_rc(&self) -> Rc<str> {
        Rc::clone(&self.0)
    }

    pub(crate) fn as_datum(&self) -> SymbolDatum<'_> {
        SymbolDatum(self)
    }
}

impl AsRef<str> for Symbol {
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}

impl Borrow<str> for Symbol {
    fn borrow(&self) -> &str {
        self.0.borrow()
    }
}

impl Deref for Symbol {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Default)]
pub(crate) struct SymbolTable(RefCell<HashSet<Symbol>>);

impl SymbolTable {
    pub(crate) fn get(&self, name: impl AsRef<str>) -> Symbol {
        let name = name.as_ref();
        if let Some(s) = self.0.borrow().get(name) {
            s.clone()
        } else {
            self.0.borrow_mut().insert(Symbol::new(name));
            self.get(name)
        }
    }

    pub(crate) fn sorted_symbols(&self) -> Vec<Symbol> {
        let mut vec = self
            .0
            .borrow()
            .iter()
            .map(Symbol::clone)
            .collect::<Vec<_>>();
        vec.sort();
        vec
    }
}

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

pub(crate) fn is_whitespace(ch: char) -> bool {
    // TODO: support unicode whitespace
    ch.is_ascii_whitespace()
}

pub(crate) fn is_delimiter(ch: char) -> bool {
    ch == '(' || is_token_boundary(ch)
}

pub(crate) fn is_token_boundary(ch: char) -> bool {
    match ch {
        '"' | '#' | '\'' | ')' | ',' | ';' | '`' | '|' => true,
        _ if is_whitespace(ch) => true,
        _ => false,
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

    fn accept_deref(_s: &str) {
        /* do nothing, test fails by not compiling */
    }

    #[test]
    fn symbol_as_ref() {
        let s = Symbol::new("foo");

        assert_eq!(s.as_ref(), "foo");
    }

    #[test]
    fn symbol_deref() {
        let s = Symbol::new("foo");

        accept_deref(&s);
    }

    #[test]
    fn symbol_clone() {
        let a = Symbol::new("foo");
        let b = a.clone();

        assert!(Rc::ptr_eq(&a.0, &b.0));
        assert!(a.is(&b));
    }

    #[test]
    fn symbol_copy_by_ref() {
        let a = Symbol::new("foo");
        let b = Symbol::new(&a);

        assert!(!Rc::ptr_eq(&a.0, &b.0));
        assert!(!a.is(&b));
    }

    #[test]
    fn symbol_as_rc() {
        let a = Symbol::new("foo");
        let b = a.as_rc();

        assert!(Rc::ptr_eq(&a.0, &b));
    }

    #[test]
    fn symbol_display() {
        let s = Symbol::new("foo");

        assert_eq!(s.as_datum().to_string(), "foo");
    }

    #[test]
    fn same_symbols() {
        let s = SymbolTable::default();

        let a = s.get("foo");
        let b = s.get("foo");

        assert!(a.is(&b));
    }

    #[test]
    fn different_symbols() {
        let s = SymbolTable::default();

        let a = s.get("foo");
        let b = s.get("bar");

        assert!(!a.is(&b));
    }

    #[test]
    fn get_refs_empty() {
        let s = SymbolTable::default();

        let all = s.sorted_symbols();

        assert!(all.is_empty());
    }

    #[test]
    fn get_refs_single() {
        let s = SymbolTable::default();
        s.get("foo");

        let all = s.sorted_symbols();

        let vec = all.iter().map(Symbol::as_ref).collect::<Vec<_>>();
        assert_eq!(vec, ["foo"]);
    }

    #[test]
    fn get_refs_alphabetical() {
        let s = SymbolTable::default();
        s.get("foo");
        s.get("bar");
        s.get("baz");

        let all = s.sorted_symbols();

        let vec = all.iter().map(Symbol::as_ref).collect::<Vec<_>>();
        assert_eq!(vec, ["bar", "baz", "foo"]);
    }

    #[test]
    fn delimiter_chars() {
        let chars = [
            '"', '(', ')', ';', '|', '\'', '`', ',', '#', ' ', '\t', '\r', '\n',
        ];

        for ch in chars {
            assert!(
                is_delimiter(ch),
                "Expected {} to be a delimiter",
                ch.escape_default()
            );
        }
    }

    #[test]
    fn non_delimiter_chars() {
        let chars = ['.', ':', 'a', '@', '+', '-', '\\', '/', '1'];

        for ch in chars {
            assert!(!is_delimiter(ch), "Expected {ch} to not be a delimiter");
        }
    }

    #[test]
    fn token_boundary_chars() {
        let chars = [
            '"', ')', ';', '|', '\'', '`', ',', '#', ' ', '\t', '\r', '\n',
        ];

        for ch in chars {
            assert!(
                is_token_boundary(ch),
                "Expected {} to be a token boundary",
                ch.escape_default()
            );
        }
    }

    #[test]
    fn is_token_boundary_does_not_include_lparen() {
        assert!(!is_token_boundary('('));
    }
}
