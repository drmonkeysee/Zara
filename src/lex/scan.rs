use std::{iter::Peekable, ops::Range, str::CharIndices};

pub(crate) type ScanItem = (usize, char);

pub(crate) struct Scanner<'a> {
    textline: &'a str,
    chars: ScanChars<'a>,
}

impl<'a> Scanner<'a> {
    pub(crate) fn new(textline: &'a str) -> Self {
        Self {
            textline,
            chars: textline.char_indices().peekable(),
        }
    }

    pub(crate) fn eat(&mut self) -> Option<ScanItem> {
        self.chars.next()
    }

    pub(crate) fn next_char(&mut self) -> Option<ScanItem> {
        self.chars
            .by_ref()
            .skip_while(|&(_, ch)| ch.is_ascii_whitespace())
            .next()
    }

    pub(crate) fn until_delimiter(&mut self) -> usize {
        let end = self.end();
        self.chars
            .peek_while(|&(_, ch)| !is_delimiter(ch))
            .peek()
            .map_or(end, |&(idx, _)| idx)
    }

    pub(crate) fn lexeme(&self, range: Range<usize>) -> &str {
        self.textline.get(range).unwrap_or_default()
    }

    fn end(&self) -> usize {
        self.textline.len()
    }
}

type ScanChars<'a> = Peekable<CharIndices<'a>>;

struct PeekWhile<'it, 'ch, P> {
    inner: &'it mut ScanChars<'ch>,
    predicate: P,
}

impl<'it, 'ch, P: Fn(&ScanItem) -> bool> PeekWhile<'it, 'ch, P> {
    fn peek(self) -> Option<&'it ScanItem> {
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
    fn peek_while(&mut self, predicate: P) -> PeekWhile<'_, 'a, P>;
}

impl<'a, P: Fn(&ScanItem) -> bool> PeekableSkip<'a, P> for ScanChars<'a> {
    fn peek_while(&mut self, predicate: P) -> PeekWhile<'_, 'a, P> {
        PeekWhile {
            inner: self,
            predicate,
        }
    }
}

fn is_delimiter(ch: char) -> bool {
    match ch {
        '"' | '(' | ')' | ';' | '|' => true,
        _ if ch.is_ascii_whitespace() => true,
        _ => false,
    }
}
