#[cfg(test)]
mod tests;

use crate::txt::TxtSpan;
use std::{iter::Peekable, str::CharIndices};

pub(super) type ScanItem<'txt> = <CharIndices<'txt> as Iterator>::Item;

pub(super) struct Scanner<'txt> {
    textline: &'txt str,
    chars: ScanChars<'txt>,
}

impl<'txt> Scanner<'txt> {
    pub(super) fn new(textline: &'txt str) -> Self {
        Self {
            textline,
            chars: textline.char_indices().peekable(),
        }
    }

    pub(super) fn next_token(&mut self) -> Option<ScanItem<'_>> {
        self.chars.find(not_whitespace)
    }

    pub(super) fn next_if_not_delimiter(&mut self) -> Option<ScanItem<'_>> {
        self.chars.next_if(not_delimiter)
    }

    pub(super) fn next_if_not_token_boundary(&mut self) -> Option<ScanItem<'_>> {
        self.chars.next_if(not_token_boundary)
    }

    pub(super) fn char(&mut self) -> Option<char> {
        self.next().map(into_char)
    }

    pub(super) fn char_if_eq(&mut self, ch: char) -> Option<usize> {
        self.chars.next_if(eq_char(ch)).map(into_idx)
    }

    pub(super) fn char_if_not_delimiter(&mut self) -> Option<char> {
        self.next_if_not_delimiter().map(into_char)
    }

    pub(super) fn char_if_not_token_boundary(&mut self) -> Option<char> {
        self.next_if_not_token_boundary().map(into_char)
    }

    pub(super) fn find_any_char(&mut self, chars: &[char]) -> Option<ScanItem<'_>> {
        self.chars.find(|(_, ch)| chars.contains(ch))
    }

    pub(super) fn skip_whitespace(&mut self) -> usize {
        let end = self.end();
        self.chars.next_until(not_whitespace).map_or(end, get_idx)
    }

    pub(super) fn rest_of_token(&mut self) -> &'txt str {
        let cur = self.pos();
        let end = self.end_of_token();
        self.lexeme(cur..end)
    }

    pub(super) fn lexeme(&self, range: TxtSpan) -> &'txt str {
        self.textline.get(range).unwrap_or_default()
    }

    pub(super) fn current_lexeme_at(&mut self, start: usize) -> &'txt str {
        let curr = self.pos();
        self.lexeme(start..curr)
    }

    pub(super) fn end_of_token(&mut self) -> usize {
        let end = self.end();
        self.chars.next_until(delimiter).map_or(end, get_idx)
    }

    pub(super) fn end_of_word(&mut self) -> usize {
        let end = self.end();
        self.chars.next_until(word_boundary).map_or(end, get_idx)
    }

    pub(super) fn end_of_line(&mut self) -> usize {
        #[allow(
            clippy::double_ended_iterator_last,
            reason = "iterator consumed intentionally"
        )]
        self.chars.by_ref().last();
        self.end()
    }

    pub(super) fn pos(&mut self) -> usize {
        let end = self.end();
        self.chars.peek().map_or(end, get_idx)
    }

    pub(super) fn consumed(&mut self) -> bool {
        self.pos() == self.end()
    }

    fn end(&self) -> usize {
        self.textline.len()
    }
}

impl<'txt> Iterator for Scanner<'txt> {
    type Item = ScanItem<'txt>;

    fn next(&mut self) -> Option<Self::Item> {
        self.chars.next()
    }
}

type ScanChars<'txt> = Peekable<CharIndices<'txt>>;

trait PeekableExt<P> {
    fn next_until(&mut self, predicate: P) -> Option<&ScanItem<'_>>;
}

impl<P: Fn(&ScanItem) -> bool> PeekableExt<P> for ScanChars<'_> {
    fn next_until(&mut self, predicate: P) -> Option<&ScanItem<'_>> {
        while self.next_if(|item| !predicate(item)).is_some() { /* consume iterator */ }
        self.peek()
    }
}

fn get_idx(&(idx, _): &ScanItem) -> usize {
    idx
}

fn into_idx((idx, _): ScanItem) -> usize {
    idx
}

fn into_char((_, ch): ScanItem) -> char {
    ch
}

fn eq_char(ch: char) -> impl FnOnce(&ScanItem) -> bool {
    move |&(_, c)| c == ch
}

fn delimiter(&(_, ch): &ScanItem) -> bool {
    is_delimiter(ch)
}

fn word_boundary(&(_, ch): &ScanItem) -> bool {
    ch == '\\' || is_delimiter(ch)
}

fn not_delimiter(item: &ScanItem) -> bool {
    !delimiter(item)
}

fn not_token_boundary(&(_, ch): &ScanItem) -> bool {
    !is_token_boundary(ch)
}

fn not_whitespace(&(_, ch): &ScanItem) -> bool {
    !ch.is_ascii_whitespace()
}

fn is_delimiter(ch: char) -> bool {
    ch == '(' || is_token_boundary(ch)
}

fn is_token_boundary(ch: char) -> bool {
    match ch {
        '"' | '#' | '\'' | ')' | ',' | ';' | '`' | '|' => true,
        _ if ch.is_ascii_whitespace() => true,
        _ => false,
    }
}
