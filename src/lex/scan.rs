use std::{iter::Peekable, ops::Range, str::CharIndices};

pub type ScanItem = (usize, char);

pub struct Scanner<'a> {
    textline: &'a str,
    chars: Peekable<CharIndices<'a>>,
}

struct PeekWhile<'a, P> {
    inner: &'a mut Peekable<CharIndices<'a>>,
    predicate: P,
}

impl<'a, P: Fn(&ScanItem) -> bool> PeekWhile<'a, P> {
    fn peek(self) -> Option<&'a ScanItem> {
        /*loop {
            let item = self.inner.peek();
            let p = &self.predicate;
            if p(item?) {
                self.inner.next();
            } else {
                break;
            }
        }
        self.inner.peek()*/

        while let Some(item) = self.inner.peek() {
            if (self.predicate)(item) {
                self.inner.next();
            } else {
                break;
            }
        }
        self.inner.peek()
    }
}

trait PeekableSkip<'a, P> {
    fn peek_while(&'a mut self, predicate: P) -> PeekWhile<'a, P>;
}

impl<'a, P: Fn(&ScanItem) -> bool> PeekableSkip<'a, P> for Peekable<CharIndices<'a>> {
    fn peek_while(&'a mut self, predicate: P) -> PeekWhile<'a, P> {
        PeekWhile {
            inner: self,
            predicate,
        }
    }
}

impl<'a> Scanner<'a> {
    pub fn new(textline: &'a str) -> Self {
        Self {
            textline,
            chars: textline.char_indices().peekable(),
        }
    }

    pub fn eat(&mut self) -> Option<ScanItem> {
        self.chars.next()
    }

    pub fn next_char(&mut self) -> Option<ScanItem> {
        self.chars
            .by_ref()
            .skip_while(|&(_, ch)| ch.is_ascii_whitespace())
            .next()
    }

    pub fn until_delimiter(&mut self) -> usize {
        /*let end = self.end();
        self.chars
            .peek_while(|&(_, ch)| !is_delimiter(ch))
            .peek()
            .map_or(end, |&(idx, _)| idx)*/
        while let Some(&(idx, ch)) = self.chars.peek() {
            if is_delimiter(ch) {
                return idx;
            }
            self.eat();
        }
        self.end()
        /*let end = self.end();
        // TODO: can i create peek_while e.g. (#t) will skip right paren right
        // because peekable() on skip_while consumes next() on chars while throwing away local peekable
        // so next call to peek on chars advances again.
        let mut binding = self
            .chars
            .by_ref()
            .peek_while(|&(_, ch)| !is_delimiter(ch))
            .peek();
        let blah = binding.peek();
        println!("delimiter peek {:?}", blah);
        blah.map_or(end, |&(idx, _)| idx)*/
    }

    pub fn lexeme(&self, range: Range<usize>) -> &str {
        self.textline.get(range).unwrap_or_default()
    }

    fn end(&self) -> usize {
        self.textline.len()
    }
}

fn is_delimiter(ch: char) -> bool {
    match ch {
        '"' | '(' | ')' | ';' | '|' => true,
        _ if ch.is_ascii_whitespace() => true,
        _ => false,
    }
}
