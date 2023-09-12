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

    pub(super) fn next_token(&mut self) -> Option<ScanItem> {
        self.chars.find(|item| !item.1.is_ascii_whitespace())
    }

    pub(super) fn char(&mut self) -> Option<char> {
        self.chars.next().map(to_char)
    }

    pub(super) fn char_if_eq(&mut self, ch: char) -> Option<usize> {
        self.chars.next_if(eq_char(ch)).map(to_idx)
    }

    pub(super) fn char_if_not_delimiter(&mut self) -> Option<char> {
        self.chars.next_if(not_delimiter).map(to_char)
    }

    pub(super) fn char_if_not_token_boundary(&mut self) -> Option<char> {
        self.chars.next_if(not_token_boundary).map(to_char)
    }

    pub(super) fn find_char(&mut self, ch: char) -> Option<usize> {
        self.chars.find(eq_char(ch)).map(to_idx)
    }

    pub(super) fn find_any_char(&mut self, chars: &[char]) -> Option<ScanItem> {
        self.chars.find(|item| chars.contains(&item.1))
    }

    pub(super) fn rest_of_token(&mut self) -> &str {
        let cur = self.pos();
        let end = self.end_of_token();
        self.lexeme(cur..end)
    }

    pub(super) fn lexeme(&self, range: Range<usize>) -> &str {
        self.textline.get(range).unwrap_or_default()
    }

    pub(super) fn end_of_token(&mut self) -> usize {
        let end = self.end();
        self.chars.next_until(delimiter).map_or(end, get_idx)
    }

    pub(super) fn end_of_line(&mut self) -> usize {
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

type ScanChars<'a> = Peekable<CharIndices<'a>>;

trait PeekableExt<P> {
    fn next_until(&mut self, predicate: P) -> Option<&ScanItem>;
}

impl<P: Fn(&ScanItem) -> bool> PeekableExt<P> for ScanChars<'_> {
    fn next_until(&mut self, predicate: P) -> Option<&ScanItem> {
        while self.next_if(|item| !predicate(item)).is_some() { /* consume iterator */ }
        self.peek()
    }
}

fn get_idx(item: &ScanItem) -> usize {
    item.0
}

fn to_idx(item: ScanItem) -> usize {
    item.0
}

fn to_char(item: ScanItem) -> char {
    item.1
}

fn eq_char(ch: char) -> impl FnMut(&ScanItem) -> bool {
    move |item| item.1 == ch
}

fn delimiter(item: &ScanItem) -> bool {
    is_delimiter(item.1)
}

fn not_delimiter(item: &ScanItem) -> bool {
    !delimiter(item)
}

fn not_token_boundary(item: &ScanItem) -> bool {
    !is_token_boundary(item.1)
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

#[cfg(test)]
mod tests {
    use super::*;

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
            assert_eq!(r.unwrap(), 'a');
        }

        #[test]
        fn next_first_whitespace() {
            let mut s = Scanner::new(" abc");

            let r = s.char();

            assert!(r.is_some());
            assert_eq!(r.unwrap(), ' ');
        }

        #[test]
        fn next_first_utf8_char() {
            let mut s = Scanner::new("🦀bc");

            let r = s.char();

            assert!(r.is_some());
            assert_eq!(r.unwrap(), '🦀');
        }

        #[test]
        fn next_char_eq() {
            let mut s = Scanner::new("abc");

            let r = s.char_if_eq('a');

            assert!(r.is_some());
            assert_eq!(r.unwrap(), 0);

            let r = s.char();

            assert!(r.is_some());
            assert_eq!(r.unwrap(), 'b');
        }

        #[test]
        fn next_char_not_eq() {
            let mut s = Scanner::new("abc");

            let r = s.char_if_eq('b');

            assert!(r.is_none());

            let r = s.char();

            assert!(r.is_some());
            assert_eq!(r.unwrap(), 'a');
        }

        #[test]
        fn find_char_empty_string() {
            let mut s = Scanner::new("");

            let r = s.find_char('a');

            assert!(r.is_none());
        }

        #[test]
        fn find_char_start_of_string() {
            let mut s = Scanner::new("abc");

            let r = s.find_char('a');

            assert!(r.is_some());
            assert_eq!(r.unwrap(), 0);

            let r = s.char();

            assert!(r.is_some());
            assert_eq!(r.unwrap(), 'b');
        }

        #[test]
        fn find_char_in_string() {
            let mut s = Scanner::new("abc");

            let r = s.find_char('b');

            assert!(r.is_some());
            assert_eq!(r.unwrap(), 1);

            let r = s.char();

            assert!(r.is_some());
            assert_eq!(r.unwrap(), 'c');
        }

        #[test]
        fn find_char_not_in_string() {
            let mut s = Scanner::new("abc");

            let r = s.find_char('d');

            assert!(r.is_none());

            let r = s.char();

            assert!(r.is_none());
        }

        #[test]
        fn find_in_string() {
            let mut s = Scanner::new("abc");

            let r = s.find_any_char(&['z', 'b', 't']);

            assert!(r.is_some());
            assert_eq!(r.unwrap(), (1, 'b'));

            let r = s.char();

            assert!(r.is_some());
            assert_eq!(r.unwrap(), 'c');
        }

        #[test]
        fn find_not_in_string() {
            let mut s = Scanner::new("abc");

            let r = s.find_any_char(&['z', 'd', 't']);

            assert!(r.is_none());

            let r = s.char();

            assert!(r.is_none());
        }

        #[test]
        fn next_token_empty_string() {
            let mut s = Scanner::new("");

            let r = s.next_token();

            assert!(r.is_none());
        }

        #[test]
        fn next_token_first_char() {
            let mut s = Scanner::new("xyz");

            let r = s.next_token();

            assert!(r.is_some());
            assert_eq!(r.unwrap(), (0, 'x'));
        }

        #[test]
        fn next_token_skips_whitespace() {
            let mut s = Scanner::new("   \t  \r\n  xyz");

            let r = s.next_token();

            assert!(r.is_some());
            assert_eq!(r.unwrap(), (10, 'x'));
        }

        #[test]
        fn advances_properly_after_next_token_finds_first_char() {
            let mut s = Scanner::new("   \t  \r\n  xyz");

            let r = s.next_token();

            assert!(r.is_some());
            assert_eq!(r.unwrap(), (10, 'x'));

            let r = s.char();

            assert!(r.is_some());
            assert_eq!(r.unwrap(), 'y');
        }

        #[test]
        fn next_token_all_whitespace() {
            let mut s = Scanner::new("   \t  \r\n");

            let r = s.next_token();

            assert!(r.is_none());
        }

        #[test]
        fn char_if_not_delimiter_empty_string() {
            let mut s = Scanner::new("");

            let r = s.char_if_not_delimiter();

            assert!(r.is_none());
        }

        #[test]
        fn char_if_not_delimiter_if_delimiter_at_start() {
            let mut s = Scanner::new("(abc");

            let r = s.char_if_not_delimiter();

            assert!(r.is_none());
        }

        #[test]
        fn char_if_not_delimiter_at_start() {
            let mut s = Scanner::new("abc)");

            let r = s.char_if_not_delimiter();

            assert!(r.is_some());
            assert_eq!(r.unwrap(), 'a');
        }

        #[test]
        fn char_if_not_delimiter_advances_properly_after_delimiter() {
            let mut s = Scanner::new("ab)c");

            let r = s.char_if_not_delimiter();
            assert!(r.is_some());
            assert_eq!(r.unwrap(), 'a');

            let r = s.char_if_not_delimiter();
            assert!(r.is_some());
            assert_eq!(r.unwrap(), 'b');

            let r = s.char_if_not_delimiter();
            assert!(r.is_none());
            let r = s.char_if_not_delimiter();
            assert!(r.is_none());
            let r = s.char_if_not_delimiter();
            assert!(r.is_none());

            let r = s.char();
            assert!(r.is_some());
            assert_eq!(r.unwrap(), ')');
        }

        #[test]
        fn char_if_not_token_boundary_rparen() {
            let mut s = Scanner::new(")abc");

            let r = s.char_if_not_token_boundary();

            assert!(r.is_none());
        }

        #[test]
        fn char_if_not_token_boundary_lparen() {
            let mut s = Scanner::new("(abc");

            let r = s.char_if_not_token_boundary();

            assert!(r.is_some());
            assert_eq!(r.unwrap(), '(');
        }

        #[test]
        fn empty_string_end_of_line() {
            let mut s = Scanner::new("");

            let r = s.end_of_line();

            assert_eq!(r, 0);
        }

        #[test]
        fn end_of_line() {
            let mut s = Scanner::new("scanner input is always one line");

            let r = s.end_of_line();

            assert_eq!(r, 32);
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
            assert_eq!(r.unwrap(), ')');
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
        fn rest_of_token_empty_string() {
            let mut s = Scanner::new("");

            let r = s.rest_of_token();

            assert_eq!(r, "");
        }

        #[test]
        fn rest_of_token_from_start() {
            let mut s = Scanner::new("abc");

            let r = s.rest_of_token();

            assert_eq!(r, "abc");
        }

        #[test]
        fn rest_of_token_to_whitespace() {
            let mut s = Scanner::new("abc def");

            let r = s.rest_of_token();

            assert_eq!(r, "abc");
        }

        #[test]
        fn rest_of_token_to_delimiter() {
            let mut s = Scanner::new("abc(def)");

            let r = s.rest_of_token();

            assert_eq!(r, "abc");
        }

        #[test]
        fn rest_of_token_from_current_position() {
            let mut s = Scanner::new("abc(def)");

            s.char();
            let r = s.rest_of_token();

            assert_eq!(r, "bc");
        }

        #[test]
        fn rest_of_token_at_delimiter() {
            let mut s = Scanner::new("abc(def)");

            s.end_of_token();
            let r = s.rest_of_token();

            assert_eq!(r, "");
        }

        #[test]
        fn rest_of_token_past_delimiter() {
            let mut s = Scanner::new("abc(def)");

            s.end_of_token();
            s.char();
            let r = s.rest_of_token();

            assert_eq!(r, "def");
        }

        #[test]
        fn rest_of_token_end_of_string() {
            let mut s = Scanner::new("abc");

            s.end_of_token();
            let r = s.rest_of_token();

            assert_eq!(r, "");
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

        #[test]
        fn pos_at_begining() {
            let mut s = Scanner::new("abcdxyz");

            let r = s.pos();

            assert_eq!(r, 0);
        }

        #[test]
        fn pos_after_scanning() {
            let mut s = Scanner::new("abcdxyz");

            s.char();
            s.char();
            s.char();
            let r = s.pos();

            assert_eq!(r, 3);
        }

        #[test]
        fn pos_at_end() {
            let mut s = Scanner::new("abcdxyz");

            s.end_of_token();
            let r = s.pos();

            assert_eq!(r, 7);
        }

        #[test]
        fn pos_for_empty_string() {
            let mut s = Scanner::new("");

            let r = s.pos();

            assert_eq!(r, 0);
        }

        #[test]
        fn consumed_empty_string() {
            let mut s = Scanner::new("");

            assert!(s.consumed());
        }

        #[test]
        fn consumed_before_scan() {
            let mut s = Scanner::new("abc");

            assert!(!s.consumed());
        }

        #[test]
        fn consumed_partial_scan() {
            let mut s = Scanner::new("abc");

            s.char();

            assert!(!s.consumed());
        }

        #[test]
        fn consumed_full_scan() {
            let mut s = Scanner::new("abc");

            s.end_of_line();

            assert!(s.consumed());
        }
    }
}
