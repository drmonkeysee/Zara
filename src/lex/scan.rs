use std::{iter::Peekable, ops::Range, str::CharIndices};

pub type ScanIndex = (usize, char);

pub struct Scanner<'a> {
    line: &'a str,
    iterator: CharIter<'a>,
}

impl<'a> Scanner<'a> {
    pub fn new(line: &'a str) -> Self {
        Scanner {
            line,
            iterator: line.char_indices().peekable(),
        }
    }

    pub fn eat(&mut self) -> Option<ScanIndex> {
        self.iterator.next()
    }

    pub fn pos(&mut self) -> usize {
        if let Some(&(idx, _)) = self.peek() {
            idx
        } else {
            self.line.len()
        }
    }

    pub fn scan_to_delimiter(&mut self) -> usize {
        while let Some(&(idx, ch)) = self.peek() {
            if is_delimiter(ch) {
                return idx;
            }
            self.eat();
        }
        self.line.len()
    }

    pub fn lexeme(&self, range: Range<usize>) -> &str {
        self.line.get(range).unwrap_or_default()
    }

    fn peek(&mut self) -> Option<&(usize, char)> {
        self.iterator.peek()
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
