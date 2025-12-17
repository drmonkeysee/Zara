use super::*;
use crate::{
    number::{Number, Real},
    testutil::{TestEnv, err_or_fail, extract_or_fail, ok_or_fail, some_or_fail},
    value::port::{PortError, StringReader},
};

#[test]
fn empty_string() {
    let env = TestEnv::default();
    let f = env.new_frame();
    let mut s = StringReader::new("");

    let r = parse(&mut s, &f, "test-port");

    assert!(matches!(r, Ok(None)));
}

#[test]
fn all_whitespace() {
    let env = TestEnv::default();
    let f = env.new_frame();
    let mut s = StringReader::new("  \t \n \r\n  ");

    let r = parse(&mut s, &f, "test-port");

    assert!(matches!(r, Ok(None)));
}

#[test]
fn comment() {
    let env = TestEnv::default();
    let f = env.new_frame();
    let mut s = StringReader::new(";this is a comment");

    let r = parse(&mut s, &f, "test-port");

    assert!(matches!(r, Ok(None)));
}

#[test]
fn block_comment() {
    let env = TestEnv::default();
    let f = env.new_frame();
    let mut s = StringReader::new("#| this is a block\ncomment with\nmultiple lines |#");

    let r = parse(&mut s, &f, "test-port");

    assert!(matches!(r, Ok(None)));
}

#[test]
fn nested_block_comment() {
    let env = TestEnv::default();
    let f = env.new_frame();
    let mut s = StringReader::new("#| this is a comment #| with a nested comment |# inside it |#");

    let r = parse(&mut s, &f, "test-port");

    assert!(matches!(r, Ok(None)));
}

#[test]
fn unterminated_block_comment() {
    let env = TestEnv::default();
    let f = env.new_frame();
    let mut s = StringReader::new("#| this is a block\ncomment oops");

    let r = parse(&mut s, &f, "test-port");

    assert!(matches!(r, Ok(None)));
}

#[test]
fn almost_terminated_block_comment() {
    let env = TestEnv::default();
    let f = env.new_frame();
    let mut s = StringReader::new("#| this is a block\ncomment oops |");

    let r = parse(&mut s, &f, "test-port");

    assert!(matches!(r, Ok(None)));
}

#[test]
fn lexical_error() {
    let env = TestEnv::default();
    let f = env.new_frame();
    let mut s = StringReader::new("#z");

    let r = parse(&mut s, &f, "test-port");

    let err = err_or_fail!(r);
    let read_err = extract_or_fail!(err, PortError::Read);
    assert_eq!(read_err.to_string(), "fatal error: tokenization failure");
}

#[test]
fn syntax_error() {
    let env = TestEnv::default();
    let f = env.new_frame();
    let mut s = StringReader::new("(a . )");

    let r = parse(&mut s, &f, "test-port");

    let err = err_or_fail!(r);
    let read_err = extract_or_fail!(err, PortError::Read);
    assert_eq!(read_err.to_string(), "fatal error: invalid syntax");
}

#[test]
fn boolean() {
    let env = TestEnv::default();
    let f = env.new_frame();
    let mut s = StringReader::new("#f");

    let r = parse(&mut s, &f, "test-port");

    let v = some_or_fail!(ok_or_fail!(r));
    assert!(matches!(v, Value::Boolean(false)));

    let r = parse(&mut s, &f, "test-port");

    assert!(matches!(r, Ok(None)));
}

#[test]
fn character() {
    let env = TestEnv::default();
    let f = env.new_frame();
    let mut s = StringReader::new("#\\a");

    let r = parse(&mut s, &f, "test-port");

    let v = some_or_fail!(ok_or_fail!(r));
    assert!(matches!(v, Value::Character('a')));

    let r = parse(&mut s, &f, "test-port");

    assert!(matches!(r, Ok(None)));
}

#[test]
fn pair() {
    let env = TestEnv::default();
    let f = env.new_frame();
    let mut s = StringReader::new("(a . b)");

    let r = parse(&mut s, &f, "test-port");

    let v = some_or_fail!(ok_or_fail!(r));
    assert!(matches!(v, Value::Pair(_)));
    assert_eq!(v.as_datum().to_string(), "(a . b)");

    let r = parse(&mut s, &f, "test-port");

    assert!(matches!(r, Ok(None)));
}

#[test]
fn list() {
    let env = TestEnv::default();
    let f = env.new_frame();
    let mut s = StringReader::new("(a 2 \"three\")");

    let r = parse(&mut s, &f, "test-port");

    let v = some_or_fail!(ok_or_fail!(r));
    assert!(matches!(v, Value::Pair(_)));
    assert_eq!(v.as_datum().to_string(), "(a 2 \"three\")");

    let r = parse(&mut s, &f, "test-port");

    assert!(matches!(r, Ok(None)));
}

#[test]
fn multiline_list() {
    let env = TestEnv::default();
    let f = env.new_frame();
    let mut s = StringReader::new(
        "(
            a
            2
            \"three\")",
    );

    let r = parse(&mut s, &f, "test-port");

    let v = some_or_fail!(ok_or_fail!(r));
    assert!(matches!(v, Value::Pair(_)));
    assert_eq!(v.as_datum().to_string(), "(a 2 \"three\")");

    let r = parse(&mut s, &f, "test-port");

    assert!(matches!(r, Ok(None)));
}

#[test]
fn nested_list() {
    let env = TestEnv::default();
    let f = env.new_frame();
    let mut s = StringReader::new("(a (1 2 3) b c)");

    let r = parse(&mut s, &f, "test-port");

    let v = some_or_fail!(ok_or_fail!(r));
    assert!(matches!(v, Value::Pair(_)));
    assert_eq!(v.as_datum().to_string(), "(a (1 2 3) b c)");

    let r = parse(&mut s, &f, "test-port");

    assert!(matches!(r, Ok(None)));
}

#[test]
fn quoted_element() {
    let env = TestEnv::default();
    let f = env.new_frame();
    let mut s = StringReader::new("'foo");

    let r = parse(&mut s, &f, "test-port");

    let v = some_or_fail!(ok_or_fail!(r));
    assert!(matches!(v, Value::Pair(_)));
    assert_eq!(v.as_datum().to_string(), "(quote foo)");

    let r = parse(&mut s, &f, "test-port");

    assert!(matches!(r, Ok(None)));
}

#[test]
fn syntax_list() {
    let env = TestEnv::default();
    let f = env.new_frame();
    let mut s = StringReader::new("(if (< a b) 'less 'more)");

    let r = parse(&mut s, &f, "test-port");

    let v = some_or_fail!(ok_or_fail!(r));
    assert!(matches!(v, Value::Pair(_)));
    assert_eq!(
        v.as_datum().to_string(),
        "(if (< a b) (quote less) (quote more))"
    );

    let r = parse(&mut s, &f, "test-port");

    assert!(matches!(r, Ok(None)));
}

#[test]
fn null() {
    let env = TestEnv::default();
    let f = env.new_frame();
    let mut s = StringReader::new("()");

    let r = parse(&mut s, &f, "test-port");

    let v = some_or_fail!(ok_or_fail!(r));
    assert!(matches!(v, Value::Null));

    let r = parse(&mut s, &f, "test-port");

    assert!(matches!(r, Ok(None)));
}

#[test]
fn number() {
    let env = TestEnv::default();
    let f = env.new_frame();
    let mut s = StringReader::new("12");

    let r = parse(&mut s, &f, "test-port");

    let v = some_or_fail!(ok_or_fail!(r));
    assert!(matches!(v, Value::Number(r) if r.to_string() == "12"));

    let r = parse(&mut s, &f, "test-port");

    assert!(matches!(r, Ok(None)));
}

#[test]
fn inf_nan() {
    let env = TestEnv::default();
    let f = env.new_frame();
    let mut s = StringReader::new("+inf.0 -inf.0 +nan.0 -nan.0");

    let r = parse(&mut s, &f, "test-port");

    let v = some_or_fail!(ok_or_fail!(r));
    assert!(matches!(
        v,
        Value::Number(Number::Real(Real::Float(f64::INFINITY)))
    ));

    let r = parse(&mut s, &f, "test-port");

    let v = some_or_fail!(ok_or_fail!(r));
    assert!(matches!(
        v,
        Value::Number(Number::Real(Real::Float(f64::NEG_INFINITY)))
    ));

    let r = parse(&mut s, &f, "test-port");

    let v = some_or_fail!(ok_or_fail!(r));
    assert!(matches!(
        v,
        Value::Number(Number::Real(Real::Float(f))) if f.is_nan()
    ));

    let r = parse(&mut s, &f, "test-port");

    let v = some_or_fail!(ok_or_fail!(r));
    assert!(matches!(
        v,
        Value::Number(Number::Real(Real::Float(f))) if f.is_nan()
    ));

    let r = parse(&mut s, &f, "test-port");

    assert!(matches!(r, Ok(None)));
}

#[test]
fn complex() {
    let env = TestEnv::default();
    let f = env.new_frame();
    let mut s = StringReader::new("4+5i 3-2i +i -i +2i -3i 32@45");

    let r = parse(&mut s, &f, "test-port");

    let v = some_or_fail!(ok_or_fail!(r));
    assert!(matches!(
        v,
        Value::Number(n) if n.to_string() == "4+5i"
    ));

    let r = parse(&mut s, &f, "test-port");

    let v = some_or_fail!(ok_or_fail!(r));
    assert!(matches!(
        v,
        Value::Number(n) if n.to_string() == "3-2i"
    ));

    let r = parse(&mut s, &f, "test-port");

    let v = some_or_fail!(ok_or_fail!(r));
    assert!(matches!(
        v,
        Value::Number(n) if n.to_string() == "+i"
    ));

    let r = parse(&mut s, &f, "test-port");

    let v = some_or_fail!(ok_or_fail!(r));
    assert!(matches!(
        v,
        Value::Number(n) if n.to_string() == "-i"
    ));

    let r = parse(&mut s, &f, "test-port");

    let v = some_or_fail!(ok_or_fail!(r));
    assert!(matches!(
        v,
        Value::Number(n) if n.to_string() == "+2i"
    ));

    let r = parse(&mut s, &f, "test-port");

    let v = some_or_fail!(ok_or_fail!(r));
    assert!(matches!(
        v,
        Value::Number(n) if n.to_string() == "-3i"
    ));

    let r = parse(&mut s, &f, "test-port");

    let v = some_or_fail!(ok_or_fail!(r));
    dbg!(&v);
    assert!(matches!(
        v,
        Value::Number(n) if n.to_string() == "16.81030364216735+27.22891278509179i"
    ));

    let r = parse(&mut s, &f, "test-port");

    assert!(matches!(r, Ok(None)));
}

#[test]
fn simple_string() {
    let env = TestEnv::default();
    let f = env.new_frame();
    let mut s = StringReader::new("\"foo bar\"");

    let r = parse(&mut s, &f, "test-port");

    let v = some_or_fail!(ok_or_fail!(r));
    assert!(matches!(v, Value::String(s) if s.as_ref() == "foo bar"));

    let r = parse(&mut s, &f, "test-port");

    assert!(matches!(r, Ok(None)));
}

#[test]
fn multiline_string() {
    let env = TestEnv::default();
    let f = env.new_frame();
    let mut s = StringReader::new(
        "\"foo
bar
   baz\"",
    );

    let r = parse(&mut s, &f, "test-port");

    let v = some_or_fail!(ok_or_fail!(r));
    assert!(matches!(v, Value::String(s) if s.as_ref() == "foo\nbar\n   baz"));

    let r = parse(&mut s, &f, "test-port");

    assert!(matches!(r, Ok(None)));
}

#[test]
fn nested_string() {
    let env = TestEnv::default();
    let f = env.new_frame();
    let mut s = StringReader::new("\"string with \\\"inner string\\\" inside it\"");

    let r = parse(&mut s, &f, "test-port");

    let v = some_or_fail!(ok_or_fail!(r));
    assert!(
        matches!(v, Value::String(s) if s.as_ref() == "string with \"inner string\" inside it")
    );

    let r = parse(&mut s, &f, "test-port");

    assert!(matches!(r, Ok(None)));
}

#[test]
fn symbol() {
    let env = TestEnv::default();
    let f = env.new_frame();
    let mut s = StringReader::new("foo");

    let r = parse(&mut s, &f, "test-port");

    let v = some_or_fail!(ok_or_fail!(r));
    assert!(matches!(v, Value::Symbol(s) if s.as_ref() == "foo"));

    let r = parse(&mut s, &f, "test-port");

    assert!(matches!(r, Ok(None)));
}

#[test]
fn verbose_symbol() {
    let env = TestEnv::default();
    let f = env.new_frame();
    let mut s = StringReader::new("|foo bar baz|");

    let r = parse(&mut s, &f, "test-port");

    let v = some_or_fail!(ok_or_fail!(r));
    assert!(matches!(v, Value::Symbol(s) if s.as_ref() == "foo bar baz"));

    let r = parse(&mut s, &f, "test-port");

    assert!(matches!(r, Ok(None)));
}

#[test]
fn multiline_verbose_symbol() {
    let env = TestEnv::default();
    let f = env.new_frame();
    let mut s = StringReader::new(
        "|foo
bar
   baz|",
    );

    let r = parse(&mut s, &f, "test-port");

    let v = some_or_fail!(ok_or_fail!(r));
    assert!(matches!(v, Value::Symbol(s) if s.as_ref() == "foo\nbar\n   baz"));

    let r = parse(&mut s, &f, "test-port");

    assert!(matches!(r, Ok(None)));
}

#[test]
fn nested_verbose_symbol() {
    let env = TestEnv::default();
    let f = env.new_frame();
    let mut s = StringReader::new("|symbol with \\|nested symbol\\| in it|");

    let r = parse(&mut s, &f, "test-port");

    let v = some_or_fail!(ok_or_fail!(r));
    assert!(matches!(v, Value::Symbol(s) if s.as_ref() == "symbol with |nested symbol| in it"));

    let r = parse(&mut s, &f, "test-port");

    assert!(matches!(r, Ok(None)));
}

#[test]
fn bytevector() {
    let env = TestEnv::default();
    let f = env.new_frame();
    let mut s = StringReader::new("#u8(1 2 3)");

    let r = parse(&mut s, &f, "test-port");

    let v = some_or_fail!(ok_or_fail!(r));
    assert!(matches!(v, Value::ByteVector(_)));
    assert_eq!(v.as_datum().to_string(), "#u8(1 2 3)");

    let r = parse(&mut s, &f, "test-port");

    assert!(matches!(r, Ok(None)));
}

#[test]
fn multiline_bytevector() {
    let env = TestEnv::default();
    let f = env.new_frame();
    let mut s = StringReader::new(
        "#u8(
            1
            2
            3)",
    );

    let r = parse(&mut s, &f, "test-port");

    let v = some_or_fail!(ok_or_fail!(r));
    assert!(matches!(v, Value::ByteVector(_)));
    assert_eq!(v.as_datum().to_string(), "#u8(1 2 3)");

    let r = parse(&mut s, &f, "test-port");

    assert!(matches!(r, Ok(None)));
}

#[test]
fn malformed_bytevectors() {
    let env = TestEnv::default();
    let f = env.new_frame();
    let mut s = StringReader::new("#u(1 2 3)");

    let r = parse(&mut s, &f, "test-port");

    let err = err_or_fail!(r);
    let read_err = extract_or_fail!(err, PortError::Read);
    assert_eq!(read_err.to_string(), "fatal error: tokenization failure");

    let mut s = StringReader::new("#u8x");

    let r = parse(&mut s, &f, "test-port");

    let err = err_or_fail!(r);
    let read_err = extract_or_fail!(err, PortError::Read);
    assert_eq!(read_err.to_string(), "fatal error: tokenization failure");
}

#[test]
fn vector() {
    let env = TestEnv::default();
    let f = env.new_frame();
    let mut s = StringReader::new("#(a 2 \"three\")");

    let r = parse(&mut s, &f, "test-port");

    let v = some_or_fail!(ok_or_fail!(r));
    assert!(matches!(v, Value::Vector(_)));
    assert_eq!(v.as_datum().to_string(), "#(a 2 \"three\")");

    let r = parse(&mut s, &f, "test-port");

    assert!(matches!(r, Ok(None)));
}

#[test]
fn multiline_vector() {
    let env = TestEnv::default();
    let f = env.new_frame();
    let mut s = StringReader::new(
        "#(
            a
            2
            \"three\"
            )",
    );

    let r = parse(&mut s, &f, "test-port");

    let v = some_or_fail!(ok_or_fail!(r));
    assert!(matches!(v, Value::Vector(_)));
    assert_eq!(v.as_datum().to_string(), "#(a 2 \"three\")");

    let r = parse(&mut s, &f, "test-port");

    assert!(matches!(r, Ok(None)));
}

#[test]
fn nested_vector() {
    let env = TestEnv::default();
    let f = env.new_frame();
    let mut s = StringReader::new("#(a #(1 2 3) b c)");

    let r = parse(&mut s, &f, "test-port");

    let v = some_or_fail!(ok_or_fail!(r));
    assert!(matches!(v, Value::Vector(_)));
    assert_eq!(v.as_datum().to_string(), "#(a #(1 2 3) b c)");

    let r = parse(&mut s, &f, "test-port");

    assert!(matches!(r, Ok(None)));
}

#[test]
fn list_in_vector() {
    let env = TestEnv::default();
    let f = env.new_frame();
    let mut s = StringReader::new("#(a (1 2 3) b c)");

    let r = parse(&mut s, &f, "test-port");

    let v = some_or_fail!(ok_or_fail!(r));
    assert!(matches!(v, Value::Vector(_)));
    assert_eq!(v.as_datum().to_string(), "#(a (1 2 3) b c)");

    let r = parse(&mut s, &f, "test-port");

    assert!(matches!(r, Ok(None)));
}

#[test]
fn vector_in_list() {
    let env = TestEnv::default();
    let f = env.new_frame();
    let mut s = StringReader::new("(a #(1 2 3) b c)");

    let r = parse(&mut s, &f, "test-port");

    let v = some_or_fail!(ok_or_fail!(r));
    assert!(matches!(v, Value::Pair(_)));
    assert_eq!(v.as_datum().to_string(), "(a #(1 2 3) b c)");

    let r = parse(&mut s, &f, "test-port");

    assert!(matches!(r, Ok(None)));
}

#[test]
fn simple_value_ignore_leading_trailing_whitespace() {
    let env = TestEnv::default();
    let f = env.new_frame();
    let mut s = StringReader::new("\t 12  \n");

    let r = parse(&mut s, &f, "test-port");

    let v = some_or_fail!(ok_or_fail!(r));
    assert!(matches!(v, Value::Number(r) if r.to_string() == "12"));

    let r = parse(&mut s, &f, "test-port");

    assert!(matches!(r, Ok(None)));
}

#[test]
fn simple_value_stops_at_space() {
    let env = TestEnv::default();
    let f = env.new_frame();
    let mut s = StringReader::new("12 #t");

    let r = parse(&mut s, &f, "test-port");

    let v = some_or_fail!(ok_or_fail!(r));
    assert!(matches!(v, Value::Number(r) if r.to_string() == "12"));

    let r = parse(&mut s, &f, "test-port");

    let v = some_or_fail!(ok_or_fail!(r));
    assert!(matches!(v, Value::Boolean(true)));

    let r = parse(&mut s, &f, "test-port");

    assert!(matches!(r, Ok(None)));
}

#[test]
fn simple_value_stops_at_delimiter() {
    let env = TestEnv::default();
    let f = env.new_frame();
    let mut s = StringReader::new("12#t");

    let r = parse(&mut s, &f, "test-port");

    let v = some_or_fail!(ok_or_fail!(r));
    assert!(matches!(v, Value::Number(r) if r.to_string() == "12"));

    let r = parse(&mut s, &f, "test-port");

    let v = some_or_fail!(ok_or_fail!(r));
    assert!(matches!(v, Value::Boolean(true)));

    let r = parse(&mut s, &f, "test-port");

    assert!(matches!(r, Ok(None)));
}

#[test]
fn datum_comment_is_ignored() {
    let env = TestEnv::default();
    let f = env.new_frame();
    let mut s = StringReader::new("#;12 #t");

    let r = parse(&mut s, &f, "test-port");

    let v = some_or_fail!(ok_or_fail!(r));
    assert!(matches!(v, Value::Boolean(true)));

    let r = parse(&mut s, &f, "test-port");

    assert!(matches!(r, Ok(None)));
}

#[test]
fn datum_item_is_ignored() {
    let env = TestEnv::default();
    let f = env.new_frame();
    let mut s = StringReader::new("(a #;b c)");

    let r = parse(&mut s, &f, "test-port");

    let v = some_or_fail!(ok_or_fail!(r));
    assert!(matches!(v, Value::Pair(_)));
    assert_eq!(v.as_datum().to_string(), "(a c)");

    let r = parse(&mut s, &f, "test-port");

    assert!(matches!(r, Ok(None)));
}

#[test]
fn just_datum_comment_is_eof() {
    let env = TestEnv::default();
    let f = env.new_frame();
    let mut s = StringReader::new("#;12");

    let r = parse(&mut s, &f, "test-port");

    assert!(matches!(r, Ok(None)));
}

#[test]
#[ignore = "directive parsing not implemented yet"]
fn directive() {
    let env = TestEnv::default();
    let f = env.new_frame();
    let mut s = StringReader::new("#!fold-case");

    let r = parse(&mut s, &f, "test-port");
    dbg!(&r);
    assert!(matches!(r, Ok(None)));
}
