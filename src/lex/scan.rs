use std::{iter::Peekable, ops::Range, str::CharIndices};

pub type ScanItem = (usize, char);
type CharIter<'a> = Peekable<CharIndices<'a>>;

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

    pub fn next_char(&mut self) -> Option<ScanItem> {
        loop {
            let item = self.chars.next();
            match item {
                Some((_, ch)) => {
                    if !ch.is_ascii_whitespace() {
                        return item;
                    }
                }
                None => return item,
            }
        }
    }

    pub fn pos(&mut self) -> usize {
        if let Some(&(idx, _)) = self.peek() {
            idx
        } else {
            self.textline.len()
        }
    }

    pub fn until_delimiter(&mut self) -> usize {
        while let Some(&(idx, ch)) = self.peek() {
            if is_delimiter(ch) {
                return idx;
            }
            self.next();
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

impl<'a> Iterator for Scanner<'a> {
    type Item = ScanItem;

    fn next(&mut self) -> Option<Self::Item> {
        self.chars.next()
    }
}

fn is_delimiter(ch: char) -> bool {
    match ch {
        '"' | '(' | ')' | ';' | '|' => true,
        _ if ch.is_ascii_whitespace() => true,
        _ => false,
    }
}
