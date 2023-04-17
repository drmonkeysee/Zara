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

    pub(super) fn advance(&mut self) -> Option<ScanItem> {
        self.chars.next()
    }

    pub(super) fn next_char(&mut self) -> Option<ScanItem> {
        self.chars
            .by_ref()
            .skip_while(|&(_, ch)| ch.is_ascii_whitespace())
            .next()
    }

    pub(super) fn until_delimiter(&mut self) -> usize {
        let end = self.end();
        self.chars
            .peek_while(|&(_, ch)| !is_delimiter(ch))
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

struct PeekWhile<'it, 'ch, P> {
    inner: &'it mut ScanChars<'ch>,
    predicate: P,
}

impl<'it, 'ch, P: Fn(&ScanItem) -> bool> PeekWhile<'it, 'ch, P> {
    fn peek(self) -> Option<&'it ScanItem<'ch>> {
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

#[cfg(test)]
mod tests {
    use super::*;

    mod advance {
        use super::*;

        #[test]
        fn empty_string() {
            let mut s = Scanner::new("");

            assert!(s.advance().is_none());
        }

        #[test]
        fn gets_first_char() {
            let mut s = Scanner::new("abc");

            let r = s.advance();

            assert!(r.is_some());
            assert_eq!(r.unwrap(), (0, 'a'));
        }

        #[test]
        fn gets_first_whitespace() {
            let mut s = Scanner::new(" abc");

            let r = s.advance();

            assert!(r.is_some());
            assert_eq!(r.unwrap(), (0, ' '));
        }

        #[test]
        fn gets_first_utf8_char() {
            let mut s = Scanner::new("🦀bc");

            let r = s.advance();

            assert!(r.is_some());
            assert_eq!(r.unwrap(), (0, '🦀'));
        }
    }

    mod next_char {
        use super::*;

        #[test]
        fn empty_string() {
            let mut s = Scanner::new("");

            assert!(s.next_char().is_none());
        }

        #[test]
        fn gets_first_char() {
            let mut s = Scanner::new("xyz");

            let r = s.next_char();

            assert!(r.is_some());
            assert_eq!(r.unwrap(), (0, 'x'));
        }

        #[test]
        fn skips_whitespace() {
            let mut s = Scanner::new("   \t  \r\n  xyz");

            let r = s.next_char();

            assert!(r.is_some());
            assert_eq!(r.unwrap(), (10, 'x'));
        }

        #[test]
        fn advances_properly_after_finding_first_char() {
            let mut s = Scanner::new("   \t  \r\n  xyz");

            let r = s.next_char();

            assert!(r.is_some());
            assert_eq!(r.unwrap(), (10, 'x'));

            let r = s.advance();

            assert!(r.is_some());
            assert_eq!(r.unwrap(), (11, 'y'));
        }

        #[test]
        fn all_whitespace() {
            let mut s = Scanner::new("   \t  \r\n");

            assert!(s.next_char().is_none());
        }
    }

    mod until_delimiter {
        use super::*;

        #[test]
        fn empty_string() {
            let mut s = Scanner::new("");

            assert_eq!(s.until_delimiter(), 0);
        }

        #[test]
        fn no_delimiter() {
            let mut s = Scanner::new("abc");

            assert_eq!(s.until_delimiter(), 3);
        }

        #[test]
        fn stops_at_delimiter() {
            let mut s = Scanner::new("abcd)xyz");

            assert_eq!(s.until_delimiter(), 4);
        }

        #[test]
        fn next_advance_is_delimiter() {
            let mut s = Scanner::new("abcd)xyz");

            assert_eq!(s.until_delimiter(), 4);

            let r = s.advance();

            assert!(r.is_some());
            assert_eq!(r.unwrap(), (4, ')'));
        }

        #[test]
        fn stops_at_delimiter_until_advance() {
            let mut s = Scanner::new("abcd)xyz");

            assert_eq!(s.until_delimiter(), 4);
            assert_eq!(s.until_delimiter(), 4);
            assert_eq!(s.until_delimiter(), 4);

            s.advance();

            assert_eq!(s.until_delimiter(), 8);
        }

        #[test]
        fn stops_at_immediately_following_delimiter() {
            let mut s = Scanner::new("abcd);xyz");

            assert_eq!(s.until_delimiter(), 4);

            s.advance();

            assert_eq!(s.until_delimiter(), 5);
        }
    }

    mod lexeme {
        use super::*;

        #[test]
        fn empty_string() {
            let s = Scanner::new("");

            assert_eq!(s.lexeme(0..1), "");
        }

        #[test]
        fn empty_lexeme() {
            let s = Scanner::new("abcdxyz");

            assert_eq!(s.lexeme(0..0), "");
        }

        #[test]
        fn one_char() {
            let s = Scanner::new("abcdxyz");

            assert_eq!(s.lexeme(0..1), "a");
        }

        #[test]
        fn multi_char() {
            let s = Scanner::new("abcdxyz");

            assert_eq!(s.lexeme(0..5), "abcdx");
        }

        #[test]
        fn substr() {
            let s = Scanner::new("abcdxyz");

            assert_eq!(s.lexeme(2..6), "cdxy");
        }

        #[test]
        fn at_end() {
            let s = Scanner::new("abcdxyz");

            assert_eq!(s.lexeme(3..7), "dxyz");
        }

        #[test]
        fn whole_str() {
            let s = Scanner::new("abcdxyz");

            assert_eq!(s.lexeme(0..7), "abcdxyz");
        }

        #[test]
        fn past_end() {
            let s = Scanner::new("abcdxyz");

            assert_eq!(s.lexeme(3..10), "");
        }

        #[test]
        fn backwards_range() {
            let s = Scanner::new("abcdxyz");

            assert_eq!(s.lexeme(5..2), "");
        }
    }
}
