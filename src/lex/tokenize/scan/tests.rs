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
    use crate::testutil::some_or_fail;

    #[test]
    fn next_empty_string() {
        let mut s = Scanner::new("");

        let r = s.char();

        assert!(r.is_none());
    }

    #[test]
    fn next_first_char() {
        let mut s = Scanner::new("abc");

        let r = some_or_fail!(s.char());

        assert_eq!(r, 'a');
    }

    #[test]
    fn next_first_whitespace() {
        let mut s = Scanner::new(" abc");

        let r = some_or_fail!(s.char());

        assert_eq!(r, ' ');
    }

    #[test]
    fn next_first_utf8_char() {
        let mut s = Scanner::new("🦀bc");

        let r = some_or_fail!(s.char());

        assert_eq!(r, '🦀');
    }

    #[test]
    fn next_char_eq() {
        let mut s = Scanner::new("abc");

        let r = some_or_fail!(s.char_if_eq('a'));

        assert_eq!(r, 0);

        let r = some_or_fail!(s.char());

        assert_eq!(r, 'b');
    }

    #[test]
    fn next_char_not_eq() {
        let mut s = Scanner::new("abc");

        let r = s.char_if_eq('b');

        assert!(r.is_none());

        let r = some_or_fail!(s.char());

        assert_eq!(r, 'a');
    }

    #[test]
    fn next_if_not_delimiter_empty_string() {
        let mut s = Scanner::new("");

        let r = s.next_if_not_delimiter();

        assert!(r.is_none());
    }

    #[test]
    fn next_if_not_delimiter_if_delimiter_at_start() {
        let mut s = Scanner::new("(abc");

        let r = s.next_if_not_delimiter();

        assert!(r.is_none());
    }

    #[test]
    fn next_if_not_delimiter_at_start() {
        let mut s = Scanner::new("abc)");

        let r = some_or_fail!(s.next_if_not_delimiter());

        assert_eq!(r, (0, 'a'));
    }

    #[test]
    fn next_if_not_token_boundary_rparen() {
        let mut s = Scanner::new(")abc");

        let r = s.next_if_not_token_boundary();

        assert!(r.is_none());
    }

    #[test]
    fn next_if_not_token_boundary_lparen() {
        let mut s = Scanner::new("(abc");

        let r = some_or_fail!(s.next_if_not_token_boundary());

        assert_eq!(r, (0, '('));
    }

    #[test]
    fn find_in_string() {
        let mut s = Scanner::new("abc");

        let r = some_or_fail!(s.find_any_char(&['z', 'b', 't']));

        assert_eq!(r, (1, 'b'));

        let r = some_or_fail!(s.char());

        assert_eq!(r, 'c');
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

        let r = some_or_fail!(s.next_token());

        assert_eq!(r, (0, 'x'));
    }

    #[test]
    fn next_token_skips_whitespace() {
        let mut s = Scanner::new("   \t  \r\n  xyz");

        let r = some_or_fail!(s.next_token());

        assert_eq!(r, (10, 'x'));
    }

    #[test]
    fn advances_properly_after_next_token_finds_first_char() {
        let mut s = Scanner::new("   \t  \r\n  xyz");

        let r = some_or_fail!(s.next_token());

        assert_eq!(r, (10, 'x'));

        let r = some_or_fail!(s.char());

        assert_eq!(r, 'y');
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

        let r = some_or_fail!(s.char_if_not_delimiter());

        assert_eq!(r, 'a');
    }

    #[test]
    fn char_if_not_delimiter_advances_properly_after_delimiter() {
        let mut s = Scanner::new("ab)c");

        let r = some_or_fail!(s.char_if_not_delimiter());
        assert_eq!(r, 'a');

        let r = some_or_fail!(s.char_if_not_delimiter());
        assert_eq!(r, 'b');

        let r = s.char_if_not_delimiter();
        assert!(r.is_none());
        let r = s.char_if_not_delimiter();
        assert!(r.is_none());
        let r = s.char_if_not_delimiter();
        assert!(r.is_none());

        let r = some_or_fail!(s.char());
        assert_eq!(r, ')');
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

        let r = some_or_fail!(s.char_if_not_token_boundary());

        assert_eq!(r, '(');
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

        let r = some_or_fail!(s.char());

        assert_eq!(r, ')');
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
    fn end_of_word_empty_string() {
        let mut s = Scanner::new("");

        let r = s.end_of_word();

        assert_eq!(r, 0);
    }

    #[test]
    fn end_of_word_uses_delimiter() {
        let mut s = Scanner::new("abcd)xyz");

        let r = s.end_of_word();

        assert_eq!(r, 4);
    }

    #[test]
    fn end_of_word_includes_backslash() {
        let mut s = Scanner::new("abcd\\xyz");

        let r = s.end_of_word();

        assert_eq!(r, 4);
    }

    #[test]
    fn skip_whitespace() {
        let mut s = Scanner::new("   abc");

        assert_eq!(s.skip_whitespace(), 3);

        let r = some_or_fail!(s.char());

        assert_eq!(r, 'a');
    }

    #[test]
    fn skip_whitespace_no_whitespace() {
        let mut s = Scanner::new("abc");

        assert_eq!(s.skip_whitespace(), 0);

        let r = some_or_fail!(s.char());

        assert_eq!(r, 'a');
    }

    #[test]
    fn skip_whitespace_all_whitespace() {
        let mut s = Scanner::new("      ");

        assert_eq!(s.skip_whitespace(), 6);

        let r = s.char();

        assert!(r.is_none());
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
    fn current_lexeme_at_empty_string() {
        let mut s = Scanner::new("");

        let r = s.current_lexeme_at(0);

        assert_eq!(r, "");
    }

    #[test]
    fn current_lexeme_at_start() {
        let mut s = Scanner::new("abcdxyz");

        let r = s.current_lexeme_at(0);

        assert_eq!(r, "");
    }

    #[test]
    fn current_lexeme_at_current_pos() {
        let mut s = Scanner::new("abcdxyz");
        s.next();
        s.next();
        s.next();

        let r = s.current_lexeme_at(0);

        assert_eq!(r, "abc");
    }

    #[test]
    fn current_lexeme_at_past_pos() {
        let mut s = Scanner::new("abcdxyz");
        s.next();
        s.next();
        s.next();

        let r = s.current_lexeme_at(4);

        assert_eq!(r, "");
    }

    #[test]
    fn current_lexeme_at_end() {
        let mut s = Scanner::new("abcdxyz");
        s.end_of_line();

        let r = s.current_lexeme_at(3);

        assert_eq!(r, "dxyz");
    }

    #[test]
    fn current_lexeme_at_past_end() {
        let mut s = Scanner::new("abcdxyz");
        s.next();
        s.next();
        s.next();

        let r = s.current_lexeme_at(40);

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
