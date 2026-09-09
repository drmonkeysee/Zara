use super::*;
use crate::testutil::{err_or_fail, ok_or_fail, some_or_fail};
use std::{convert::Infallible, iter::Peekable, str::Chars};

struct StrCursor<'a>(Peekable<Chars<'a>>);

impl<'a> StrCursor<'a> {
    fn new(s: &'a str) -> Self {
        Self(s.chars().peekable())
    }
}

impl CharCursor for StrCursor<'_> {
    type Error = Infallible;

    fn read_char(&mut self) -> CursorResult<Self::Error> {
        Ok(self.0.next())
    }

    fn peek_char(&mut self) -> CursorResult<Self::Error> {
        Ok(self.0.peek().copied())
    }
}

#[derive(Debug, PartialEq, Eq)]
struct BoomError;

struct FailingCursor<'a> {
    chars: Peekable<Chars<'a>>,
    fail_after: usize,
    read_count: usize,
}

impl<'a> FailingCursor<'a> {
    fn new(s: &'a str, fail_after: usize) -> Self {
        Self {
            chars: s.chars().peekable(),
            fail_after,
            read_count: 0,
        }
    }
}

impl CharCursor for FailingCursor<'_> {
    type Error = BoomError;

    fn read_char(&mut self) -> CursorResult<Self::Error> {
        if self.read_count >= self.fail_after {
            return Err(BoomError);
        }
        self.read_count += 1;
        Ok(self.chars.next())
    }

    fn peek_char(&mut self) -> CursorResult<Self::Error> {
        if self.read_count >= self.fail_after {
            return Err(BoomError);
        }
        Ok(self.chars.peek().copied())
    }
}

#[test]
fn empty_string() {
    let mut s = StrCursor::new("");

    let r = scan(&mut s);

    assert_eq!(ok_or_fail!(r), None);
}

#[test]
fn all_whitespace() {
    let mut s = StrCursor::new("  \t \n \r\n  ");

    let r = scan(&mut s);

    assert_eq!(ok_or_fail!(r), None);
}

#[test]
fn comment_without_trailing_newline() {
    let mut s = StrCursor::new(";this is a comment");

    let r = scan(&mut s);

    let v = some_or_fail!(ok_or_fail!(r));
    assert_eq!(v, ";this is a comment");

    let r = scan(&mut s);

    assert_eq!(ok_or_fail!(r), None);
}

#[test]
fn comment_with_trailing_newline() {
    let mut s = StrCursor::new(";this is a comment\n");

    let r = scan(&mut s);

    let v = some_or_fail!(ok_or_fail!(r));
    assert_eq!(v, ";this is a comment\n");

    let r = scan(&mut s);

    assert_eq!(ok_or_fail!(r), None);
}

#[test]
fn block_comment() {
    let mut s = StrCursor::new("#| this is a block\ncomment with\nmultiple lines |#");

    let r = scan(&mut s);

    assert_eq!(ok_or_fail!(r), None);
}

#[test]
fn nested_block_comment() {
    let mut s = StrCursor::new("#| this is a comment #| with a nested comment |# inside it |#");

    let r = scan(&mut s);

    assert_eq!(ok_or_fail!(r), None);
}

#[test]
fn unterminated_block_comment() {
    let mut s = StrCursor::new("#| this is a block\ncomment oops");

    let r = scan(&mut s);

    assert_eq!(ok_or_fail!(r), None);
}

#[test]
fn almost_terminated_block_comment() {
    let mut s = StrCursor::new("#| this is a block\ncomment oops |");

    let r = scan(&mut s);

    assert_eq!(ok_or_fail!(r), None);
}

#[test]
fn hash_prefixed_symbol_is_scanned_as_single_token() {
    let mut s = StrCursor::new("#z");

    let r = scan(&mut s);

    let v = some_or_fail!(ok_or_fail!(r));
    assert_eq!(v, "#z");

    let r = scan(&mut s);

    assert_eq!(ok_or_fail!(r), None);
}

#[test]
fn unterminated_pair_expression_is_scanned_as_balanced_parens() {
    let mut s = StrCursor::new("(a . )");

    let r = scan(&mut s);

    let v = some_or_fail!(ok_or_fail!(r));
    assert_eq!(v, "(a . )");

    let r = scan(&mut s);

    assert_eq!(ok_or_fail!(r), None);
}

#[test]
fn boolean() {
    let mut s = StrCursor::new("#f");

    let r = scan(&mut s);

    let v = some_or_fail!(ok_or_fail!(r));
    assert_eq!(v, "#f");

    let r = scan(&mut s);

    assert_eq!(ok_or_fail!(r), None);
}

#[test]
fn character() {
    let mut s = StrCursor::new("#\\a");

    let r = scan(&mut s);

    let v = some_or_fail!(ok_or_fail!(r));
    assert_eq!(v, "#\\a");

    let r = scan(&mut s);

    assert_eq!(ok_or_fail!(r), None);
}

#[test]
fn pair() {
    let input = "(a . b)";
    let mut s = StrCursor::new(input);

    let r = scan(&mut s);

    let v = some_or_fail!(ok_or_fail!(r));
    assert_eq!(v, input);

    let r = scan(&mut s);

    assert_eq!(ok_or_fail!(r), None);
}

#[test]
fn list() {
    let input = "(a 2 \"three\")";
    let mut s = StrCursor::new(input);

    let r = scan(&mut s);

    let v = some_or_fail!(ok_or_fail!(r));
    assert_eq!(v, input);

    let r = scan(&mut s);

    assert_eq!(ok_or_fail!(r), None);
}

#[test]
fn multiline_list() {
    let input = "(
            a
            2
            \"three\")";
    let mut s = StrCursor::new(input);

    let r = scan(&mut s);

    let v = some_or_fail!(ok_or_fail!(r));
    assert_eq!(v, input);

    let r = scan(&mut s);

    assert_eq!(ok_or_fail!(r), None);
}

#[test]
fn nested_list() {
    let input = "(a (1 2 3) b c)";
    let mut s = StrCursor::new(input);

    let r = scan(&mut s);

    let v = some_or_fail!(ok_or_fail!(r));
    assert_eq!(v, input);

    let r = scan(&mut s);

    assert_eq!(ok_or_fail!(r), None);
}

#[test]
fn quoted_element() {
    let mut s = StrCursor::new("'foo");

    let r = scan(&mut s);

    let v = some_or_fail!(ok_or_fail!(r));
    assert_eq!(v, "'foo");

    let r = scan(&mut s);

    assert_eq!(ok_or_fail!(r), None);
}

#[test]
fn syntax_list() {
    let input = "(if (< a b) 'less 'more)";
    let mut s = StrCursor::new(input);

    let r = scan(&mut s);

    let v = some_or_fail!(ok_or_fail!(r));
    assert_eq!(v, input);

    let r = scan(&mut s);

    assert_eq!(ok_or_fail!(r), None);
}

#[test]
fn null() {
    let mut s = StrCursor::new("()");

    let r = scan(&mut s);

    let v = some_or_fail!(ok_or_fail!(r));
    assert_eq!(v, "()");

    let r = scan(&mut s);

    assert_eq!(ok_or_fail!(r), None);
}

#[test]
fn number() {
    let mut s = StrCursor::new("12");

    let r = scan(&mut s);

    let v = some_or_fail!(ok_or_fail!(r));
    assert_eq!(v, "12");

    let r = scan(&mut s);

    assert_eq!(ok_or_fail!(r), None);
}

#[test]
fn multiple_numbers_separated_by_whitespace() {
    let mut s = StrCursor::new("+inf.0 -inf.0 +nan.0 -nan.0");

    let r = scan(&mut s);

    let v = some_or_fail!(ok_or_fail!(r));
    assert_eq!(v, "+inf.0");

    let r = scan(&mut s);

    let v = some_or_fail!(ok_or_fail!(r));
    assert_eq!(v, " -inf.0");

    let r = scan(&mut s);

    let v = some_or_fail!(ok_or_fail!(r));
    assert_eq!(v, " +nan.0");

    let r = scan(&mut s);

    let v = some_or_fail!(ok_or_fail!(r));
    assert_eq!(v, " -nan.0");

    let r = scan(&mut s);

    assert_eq!(ok_or_fail!(r), None);
}

#[test]
fn simple_string() {
    let input = "\"foo bar\"";
    let mut s = StrCursor::new(input);

    let r = scan(&mut s);

    let v = some_or_fail!(ok_or_fail!(r));
    assert_eq!(v, input);

    let r = scan(&mut s);

    assert_eq!(ok_or_fail!(r), None);
}

#[test]
fn multiline_string() {
    let input = "\"foo
bar
   baz\"";
    let mut s = StrCursor::new(input);

    let r = scan(&mut s);

    let v = some_or_fail!(ok_or_fail!(r));
    assert_eq!(v, input);

    let r = scan(&mut s);

    assert_eq!(ok_or_fail!(r), None);
}

#[test]
fn nested_string() {
    let input = "\"string with \\\"inner string\\\" inside it\"";
    let mut s = StrCursor::new(input);

    let r = scan(&mut s);

    let v = some_or_fail!(ok_or_fail!(r));
    assert_eq!(v, input);

    let r = scan(&mut s);

    assert_eq!(ok_or_fail!(r), None);
}

#[test]
fn nested_string_handles_nested_slash() {
    let input = "\"string with \\\"inner \\\\ string\\\" inside it\"";
    let mut s = StrCursor::new(input);

    let r = scan(&mut s);

    let v = some_or_fail!(ok_or_fail!(r));
    assert_eq!(v, input);

    let r = scan(&mut s);

    assert_eq!(ok_or_fail!(r), None);
}

#[test]
fn string_ends_with_slash_is_scanned_to_eof_without_error() {
    let input = "\"foo bar\\\"";
    let mut s = StrCursor::new(input);

    let r = scan(&mut s);

    let v = some_or_fail!(ok_or_fail!(r));
    assert_eq!(v, input);

    let r = scan(&mut s);

    assert_eq!(ok_or_fail!(r), None);
}

#[test]
fn symbol() {
    let mut s = StrCursor::new("foo");

    let r = scan(&mut s);

    let v = some_or_fail!(ok_or_fail!(r));
    assert_eq!(v, "foo");

    let r = scan(&mut s);

    assert_eq!(ok_or_fail!(r), None);
}

#[test]
fn verbose_symbol() {
    let input = "|foo bar baz|";
    let mut s = StrCursor::new(input);

    let r = scan(&mut s);

    let v = some_or_fail!(ok_or_fail!(r));
    assert_eq!(v, input);

    let r = scan(&mut s);

    assert_eq!(ok_or_fail!(r), None);
}

#[test]
fn multiline_verbose_symbol() {
    let input = "|foo
bar
   baz|";
    let mut s = StrCursor::new(input);

    let r = scan(&mut s);

    let v = some_or_fail!(ok_or_fail!(r));
    assert_eq!(v, input);

    let r = scan(&mut s);

    assert_eq!(ok_or_fail!(r), None);
}

#[test]
fn nested_verbose_symbol() {
    let input = "|symbol with \\|nested symbol\\| in it|";
    let mut s = StrCursor::new(input);

    let r = scan(&mut s);

    let v = some_or_fail!(ok_or_fail!(r));
    assert_eq!(v, input);

    let r = scan(&mut s);

    assert_eq!(ok_or_fail!(r), None);
}

#[test]
fn nested_verbose_symbol_handles_escaped_slash() {
    let input = "|symbol with \\|nested \\\\ symbol\\| in it|";
    let mut s = StrCursor::new(input);

    let r = scan(&mut s);

    let v = some_or_fail!(ok_or_fail!(r));
    assert_eq!(v, input);

    let r = scan(&mut s);

    assert_eq!(ok_or_fail!(r), None);
}

#[test]
fn verbose_symbol_ends_with_slash_is_scanned_to_eof_without_error() {
    let input = "|foo bar baz\\|";
    let mut s = StrCursor::new(input);

    let r = scan(&mut s);

    let v = some_or_fail!(ok_or_fail!(r));
    assert_eq!(v, input);

    let r = scan(&mut s);

    assert_eq!(ok_or_fail!(r), None);
}

#[test]
fn bytevector() {
    let input = "#u8(1 2 3)";
    let mut s = StrCursor::new(input);

    let r = scan(&mut s);

    let v = some_or_fail!(ok_or_fail!(r));
    assert_eq!(v, input);

    let r = scan(&mut s);

    assert_eq!(ok_or_fail!(r), None);
}

#[test]
fn multiline_bytevector() {
    let input = "#u8(
            1
            2
            3)";
    let mut s = StrCursor::new(input);

    let r = scan(&mut s);

    let v = some_or_fail!(ok_or_fail!(r));
    assert_eq!(v, input);

    let r = scan(&mut s);

    assert_eq!(ok_or_fail!(r), None);
}

#[test]
fn malformed_bytevector_missing_8_splits_into_hash_token_and_list() {
    let mut s = StrCursor::new("#u(1 2 3)");

    let r = scan(&mut s);

    let v = some_or_fail!(ok_or_fail!(r));
    assert_eq!(v, "#u");

    let r = scan(&mut s);

    let v = some_or_fail!(ok_or_fail!(r));
    assert_eq!(v, "(1 2 3)");

    let r = scan(&mut s);

    assert_eq!(ok_or_fail!(r), None);
}

#[test]
fn malformed_bytevector_bad_suffix_is_scanned_as_single_token() {
    let mut s = StrCursor::new("#u8x");

    let r = scan(&mut s);

    let v = some_or_fail!(ok_or_fail!(r));
    assert_eq!(v, "#u8x");

    let r = scan(&mut s);

    assert_eq!(ok_or_fail!(r), None);
}

#[test]
fn vector() {
    let input = "#(a 2 \"three\")";
    let mut s = StrCursor::new(input);

    let r = scan(&mut s);

    let v = some_or_fail!(ok_or_fail!(r));
    assert_eq!(v, input);

    let r = scan(&mut s);

    assert_eq!(ok_or_fail!(r), None);
}

#[test]
fn multiline_vector() {
    let input = "#(
            a
            2
            \"three\"
            )";
    let mut s = StrCursor::new(input);

    let r = scan(&mut s);

    let v = some_or_fail!(ok_or_fail!(r));
    assert_eq!(v, input);

    let r = scan(&mut s);

    assert_eq!(ok_or_fail!(r), None);
}

#[test]
fn nested_vector() {
    let input = "#(a #(1 2 3) b c)";
    let mut s = StrCursor::new(input);

    let r = scan(&mut s);

    let v = some_or_fail!(ok_or_fail!(r));
    assert_eq!(v, input);

    let r = scan(&mut s);

    assert_eq!(ok_or_fail!(r), None);
}

#[test]
fn list_in_vector() {
    let input = "#(a (1 2 3) b c)";
    let mut s = StrCursor::new(input);

    let r = scan(&mut s);

    let v = some_or_fail!(ok_or_fail!(r));
    assert_eq!(v, input);

    let r = scan(&mut s);

    assert_eq!(ok_or_fail!(r), None);
}

#[test]
fn vector_in_list() {
    let input = "(a #(1 2 3) b c)";
    let mut s = StrCursor::new(input);

    let r = scan(&mut s);

    let v = some_or_fail!(ok_or_fail!(r));
    assert_eq!(v, input);

    let r = scan(&mut s);

    assert_eq!(ok_or_fail!(r), None);
}

#[test]
fn simple_value_ignore_leading_trailing_whitespace() {
    let mut s = StrCursor::new("\t 12  \n");

    let r = scan(&mut s);

    let v = some_or_fail!(ok_or_fail!(r));
    assert_eq!(v, "\t 12");

    let r = scan(&mut s);

    assert_eq!(ok_or_fail!(r), None);
}

#[test]
fn simple_value_stops_at_space() {
    let mut s = StrCursor::new("12 #t");

    let r = scan(&mut s);

    let v = some_or_fail!(ok_or_fail!(r));
    assert_eq!(v, "12");

    let r = scan(&mut s);

    let v = some_or_fail!(ok_or_fail!(r));
    assert_eq!(v, " #t");

    let r = scan(&mut s);

    assert_eq!(ok_or_fail!(r), None);
}

#[test]
fn simple_value_stops_at_delimiter() {
    let mut s = StrCursor::new("12#t");

    let r = scan(&mut s);

    let v = some_or_fail!(ok_or_fail!(r));
    assert_eq!(v, "12");

    let r = scan(&mut s);

    let v = some_or_fail!(ok_or_fail!(r));
    assert_eq!(v, "#t");

    let r = scan(&mut s);

    assert_eq!(ok_or_fail!(r), None);
}

#[test]
fn datum_comment_is_scanned_together_with_its_datum() {
    let mut s = StrCursor::new("#;12 #t");

    let r = scan(&mut s);

    let v = some_or_fail!(ok_or_fail!(r));
    assert_eq!(v, "#;12");

    let r = scan(&mut s);

    let v = some_or_fail!(ok_or_fail!(r));
    assert_eq!(v, " #t");

    let r = scan(&mut s);

    assert_eq!(ok_or_fail!(r), None);
}

#[test]
fn datum_comment_and_its_target_are_not_stripped_from_enclosing_list() {
    let input = "(a #;b c)";
    let mut s = StrCursor::new(input);

    let r = scan(&mut s);

    let v = some_or_fail!(ok_or_fail!(r));
    assert_eq!(v, input);

    let r = scan(&mut s);

    assert_eq!(ok_or_fail!(r), None);
}

#[test]
fn just_datum_comment_is_scanned_as_single_token() {
    let mut s = StrCursor::new("#;12");

    let r = scan(&mut s);

    let v = some_or_fail!(ok_or_fail!(r));
    assert_eq!(v, "#;12");

    let r = scan(&mut s);

    assert_eq!(ok_or_fail!(r), None);
}

#[test]
fn hash_with_newline_stops_before_newline() {
    let mut s = StrCursor::new("#\n");

    let r = scan(&mut s);

    let v = some_or_fail!(ok_or_fail!(r));
    assert_eq!(v, "#");

    let r = scan(&mut s);

    assert_eq!(ok_or_fail!(r), None);
}

#[test]
fn hash_bang_directive_is_scanned_as_single_token() {
    let mut s = StrCursor::new("#!fold-case");

    let r = scan(&mut s);

    let v = some_or_fail!(ok_or_fail!(r));
    assert_eq!(v, "#!fold-case");

    let r = scan(&mut s);

    assert_eq!(ok_or_fail!(r), None);
}

#[test]
fn propagates_cursor_read_error() {
    let mut s = FailingCursor::new("abc", 1);

    let r = scan(&mut s);

    assert_eq!(err_or_fail!(r), BoomError);
}

#[test]
fn propagates_cursor_peek_error() {
    let mut s = FailingCursor::new("(abc", 1);

    let r = scan(&mut s);

    assert_eq!(err_or_fail!(r), BoomError);
}
