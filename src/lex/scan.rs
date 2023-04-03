use std::{iter::Peekable, ops::Range, str::CharIndices};

pub type ScanIndex = (usize, char);

pub struct Scanner<'a> {
    textline: &'a str,
    chars: CharIter<'a>,
}

impl<'a> Scanner<'a> {
    pub fn new(textline: &'a str) -> Self {
        Scanner {
            textline,
            chars: textline.char_indices().peekable(),
        }
    }

    pub fn eat(&mut self) -> Option<ScanIndex> {
        self.chars.next()
    }

    pub fn pos(&mut self) -> usize {
        if let Some(&(idx, _)) = self.peek() {
            idx
        } else {
            self.textline.len()
        }
    }

    pub fn scan_to_delimiter(&mut self) -> usize {
        while let Some(&(idx, ch)) = self.peek() {
            if is_delimiter(ch) {
                return idx;
            }
            self.eat();
        }
        self.textline.len()
    }

    pub fn lexeme(&self, range: Range<usize>) -> &str {
        self.textline.get(range).unwrap_or_default()
    }

    fn peek(&mut self) -> Option<&(usize, char)> {
        self.chars.peek()
    }
}

type CharIter<'a> = Peekable<CharIndices<'a>>;

fn is_delimiter(ch: char) -> bool {
    match ch {
        '"' | '(' | ')' | ';' | '|' => true,
        _ if ch.is_ascii_whitespace() => true,
        _ => false,
    }
}
