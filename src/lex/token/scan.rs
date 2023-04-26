use std::{iter::Peekable, ops::Range, str::CharIndices};

pub(super) type ScanItem<'a> = <CharIndices<'a> as Iterator>::Item;

pub(super) struct Scanner<'a> {
    textline: &'a str,
    chars: ScanChars<'a>,
}

impl<'a> Scanner<'a> {
    pub(super) fn new(textline: &'a str) -> Self {
        Self {
            textline,
            chars: textline.char_indices().peekable(),
        }
    }

    pub(super) fn char(&mut self) -> Option<ScanItem> {
        self.chars.next()
    }

    pub(super) fn skip_whitespace(&mut self) -> Option<ScanItem> {
        self.chars
            .by_ref()
            .find(|&(_, ch)| !ch.is_ascii_whitespace())
    }

    pub(super) fn non_delimiter(&mut self) -> Option<ScanItem> {
        self.chars.next_if(non_delimiter)
    }

    pub(super) fn hashcode_non_delimiter(&mut self) -> Option<ScanItem> {
        self.chars.next_if(non_hashcode_delimiter)
    }

    pub(super) fn end_of_token(&mut self) -> usize {
        let end = self.end();
        self.chars
            .peek_while(non_delimiter)
            .peek()
            .map_or(end, |&(idx, _)| idx)
    }

    pub(super) fn lexeme(&self, range: Range<usize>) -> &str {
        self.textline.get(range).unwrap_or_default()
    }

    fn end(&self) -> usize {
        self.textline.len()
    }
}

type ScanChars<'a> = Peekable<CharIndices<'a>>;

struct PeekWhile<'me, 'str, P> {
    inner: &'me mut ScanChars<'str>,
    predicate: P,
}

impl<'me, 'str, P: Fn(&ScanItem) -> bool> PeekWhile<'me, 'str, P> {
    fn peek(self) -> Option<&'me ScanItem<'str>> {
        while self.inner.next_if(&self.predicate).is_some() { /* consume iterator */ }
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

fn non_delimiter(item: &ScanItem) -> bool {
    !is_delimiter(item.1)
}

fn non_hashcode_delimiter(item: &ScanItem) -> bool {
    !is_hashcode_delimiter(item.1)
}

fn is_delimiter(ch: char) -> bool {
    match ch {
        '"' | '(' | ')' | ';' | '|' => true,
        _ if ch.is_ascii_whitespace() => true,
        _ => false,
    }
}

fn is_hashcode_delimiter(ch: char) -> bool {
    ch != '(' && is_delimiter(ch)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn delimiter_chars() {
        let chars = ['"', '(', ')', ';', '|', ' ', '\t', '\r', '\n'];

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
        let chars = ['\'', '#', '.', ',', ':', 'a', '@', '+', '-', '\\', '/', '1'];

        for ch in chars {
            assert!(!is_delimiter(ch), "Expected {ch} to not be a delimiter");
        }
    }

    #[test]
    fn hashcode_delimiter_chars() {
        let chars = ['"', ')', ';', '|', ' ', '\t', '\r', '\n'];

        for ch in chars {
            assert!(
                is_hashcode_delimiter(ch),
                "Expected {} to be a hashcode delimiter",
                ch.escape_default()
            );
        }
    }

    #[test]
    fn is_hashcode_delimiter_does_not_include_lparen() {
        assert!(!is_hashcode_delimiter('('));
    }

    mod scanner {
        use super::*;

        #[test]
        fn next_empty_string() {
            let mut s = Scanner::new("");

            let r = s.char();

            assert!(r.is_none());
        }

        #[test]
        fn next_first_char() {
            let mut s = Scanner::new("abc");

            let r = s.char();

            assert!(r.is_some());
            assert_eq!(r.unwrap(), (0, 'a'));
        }

        #[test]
        fn next_first_whitespace() {
            let mut s = Scanner::new(" abc");

            let r = s.char();

            assert!(r.is_some());
            assert_eq!(r.unwrap(), (0, ' '));
        }

        #[test]
        fn next_first_utf8_char() {
            let mut s = Scanner::new("🦀bc");

            let r = s.char();

            assert!(r.is_some());
            assert_eq!(r.unwrap(), (0, '🦀'));
        }

        #[test]
        fn skip_whitespace_empty_string() {
            let mut s = Scanner::new("");

            let r = s.skip_whitespace();

            assert!(r.is_none());
        }

        #[test]
        fn skip_whitespace_first_char() {
            let mut s = Scanner::new("xyz");

            let r = s.skip_whitespace();

            assert!(r.is_some());
            assert_eq!(r.unwrap(), (0, 'x'));
        }

        #[test]
        fn skip_whitespace_skips_whitespace() {
            let mut s = Scanner::new("   \t  \r\n  xyz");

            let r = s.skip_whitespace();

            assert!(r.is_some());
            assert_eq!(r.unwrap(), (10, 'x'));
        }

        #[test]
        fn advances_properly_after_skip_whitespace_finds_first_char() {
            let mut s = Scanner::new("   \t  \r\n  xyz");

            let r = s.skip_whitespace();

            assert!(r.is_some());
            assert_eq!(r.unwrap(), (10, 'x'));

            let r = s.char();

            assert!(r.is_some());
            assert_eq!(r.unwrap(), (11, 'y'));
        }

        #[test]
        fn skip_whitespace_all_whitespace() {
            let mut s = Scanner::new("   \t  \r\n");

            let r = s.skip_whitespace();

            assert!(r.is_none());
        }

        #[test]
        fn non_delimiter_empty_string() {
            let mut s = Scanner::new("");

            let r = s.non_delimiter();

            assert!(r.is_none());
        }

        #[test]
        fn non_delimiter_if_delimiter_at_start() {
            let mut s = Scanner::new("(abc");

            let r = s.non_delimiter();

            assert!(r.is_none());
        }

        #[test]
        fn non_delimiter_at_start() {
            let mut s = Scanner::new("abc)");

            let r = s.non_delimiter();

            assert!(r.is_some());
            assert_eq!(r.unwrap(), (0, 'a'));
        }

        #[test]
        fn non_delimiter_advances_properly_after_delimiter() {
            let mut s = Scanner::new("ab)c");

            let r = s.non_delimiter();
            assert!(r.is_some());
            assert_eq!(r.unwrap(), (0, 'a'));

            let r = s.non_delimiter();
            assert!(r.is_some());
            assert_eq!(r.unwrap(), (1, 'b'));

            let r = s.non_delimiter();
            assert!(r.is_none());
            let r = s.non_delimiter();
            assert!(r.is_none());
            let r = s.non_delimiter();
            assert!(r.is_none());

            let r = s.char();
            assert!(r.is_some());
            assert_eq!(r.unwrap(), (2, ')'));
        }

        #[test]
        fn hashcode_non_delimiter_rparen() {
            let mut s = Scanner::new(")abc");

            let r = s.hashcode_non_delimiter();

            assert!(r.is_none());
        }

        #[test]
        fn hashcode_non_delimiter_lparen() {
            let mut s = Scanner::new("(abc");

            let r = s.hashcode_non_delimiter();

            assert!(r.is_some());
            assert_eq!(r.unwrap(), (0, '('));
        }

        #[test]
        fn empty_string_ends_token() {
            let mut s = Scanner::new("");

            let r = s.end_of_token();

            assert_eq!(r, 0);
        }

        #[test]
        fn string_termination_ends_token() {
            let mut s = Scanner::new("abc");

            let r = s.end_of_token();

            assert_eq!(r, 3);
        }

        #[test]
        fn delimiter_ends_token() {
            let mut s = Scanner::new("abcd)xyz");

            let r = s.end_of_token();

            assert_eq!(r, 4);
        }

        #[test]
        fn next_item_is_delimiter() {
            let mut s = Scanner::new("abcd)xyz");

            assert_eq!(s.end_of_token(), 4);

            let r = s.char();

            assert!(r.is_some());
            assert_eq!(r.unwrap(), (4, ')'));
        }

        #[test]
        fn stops_at_delimiter_until_advance() {
            let mut s = Scanner::new("abcd)xyz");

            assert_eq!(s.end_of_token(), 4);
            assert_eq!(s.end_of_token(), 4);
            assert_eq!(s.end_of_token(), 4);

            s.char();

            assert_eq!(s.end_of_token(), 8);
        }

        #[test]
        fn stops_at_immediately_following_delimiter() {
            let mut s = Scanner::new("abcd);xyz");

            assert_eq!(s.end_of_token(), 4);

            s.char();

            assert_eq!(s.end_of_token(), 5);
        }

        #[test]
        fn lexeme_empty_string() {
            let s = Scanner::new("");

            let r = s.lexeme(0..1);

            assert_eq!(r, "");
        }

        #[test]
        fn lexeme_empty_range() {
            let s = Scanner::new("abcdxyz");

            let r = s.lexeme(0..0);

            assert_eq!(r, "");
        }

        #[test]
        fn one_char_lexeme() {
            let s = Scanner::new("abcdxyz");

            let r = s.lexeme(0..1);

            assert_eq!(r, "a");
        }

        #[test]
        fn multi_char_lexeme() {
            let s = Scanner::new("abcdxyz");

            let r = s.lexeme(0..5);

            assert_eq!(r, "abcdx");
        }

        #[test]
        fn substr_lexeme() {
            let s = Scanner::new("abcdxyz");

            let r = s.lexeme(2..6);

            assert_eq!(r, "cdxy");
        }

        #[test]
        fn lexeme_at_end() {
            let s = Scanner::new("abcdxyz");

            let r = s.lexeme(3..7);

            assert_eq!(r, "dxyz");
        }

        #[test]
        fn lexeme_whole_str() {
            let s = Scanner::new("abcdxyz");

            let r = s.lexeme(0..7);

            assert_eq!(r, "abcdxyz");
        }

        #[test]
        fn lexeme_past_end() {
            let s = Scanner::new("abcdxyz");

            let r = s.lexeme(3..10);

            assert_eq!(r, "");
        }

        #[test]
        fn lexeme_backwards_range() {
            let s = Scanner::new("abcdxyz");

            let r = s.lexeme(5..2);

            assert_eq!(r, "");
        }
    }

    mod peek_while {
        use super::*;

        #[test]
        fn empty_string() {
            let mut it = "".char_indices().peekable();

            let r = it.peek_while(|_| false).peek();

            assert!(r.is_none());
        }

        #[test]
        fn while_false() {
            let mut it = "abc".char_indices().peekable();

            let r = it.peek_while(|_| false).peek();

            assert!(r.is_some());
            assert_eq!(*r.unwrap(), (0, 'a'));
        }

        #[test]
        fn while_true() {
            let mut it = "abc".char_indices().peekable();

            let r = it.peek_while(|_| true).peek();

            assert!(r.is_none());
        }

        #[test]
        fn false_predicate() {
            let mut it = "123abc456".char_indices().peekable();

            let r = it.peek_while(|&(_, ch)| ch.is_whitespace()).peek();

            assert!(r.is_some());
            assert_eq!(*r.unwrap(), (0, '1'));
        }

        #[test]
        fn true_predicate() {
            let mut it = "123abc456".char_indices().peekable();

            let r = it.peek_while(|&(_, ch)| ch.is_alphanumeric()).peek();

            assert!(r.is_none());
        }

        #[test]
        fn ch_predicate() {
            let mut it = "123abc456".char_indices().peekable();

            let r = it.peek_while(|&(_, ch)| ch.is_ascii_digit()).peek();

            assert!(r.is_some());
            assert_eq!(*r.unwrap(), (3, 'a'));
        }

        #[test]
        fn index_predicate() {
            let mut it = "123abc456".char_indices().peekable();

            let r = it.peek_while(|&(idx, _)| idx < 4).peek();

            assert!(r.is_some());
            assert_eq!(*r.unwrap(), (4, 'b'));
        }
    }
}
