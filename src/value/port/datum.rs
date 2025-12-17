use super::{CharReader, PortBool, PortDatum, PortResult, Value};
use crate::{
    DataReader,
    eval::{Frame, Namespace},
    src::StringSource,
    string,
    syntax::ParserOutput,
};

pub(super) fn parse(r: &mut dyn CharReader, env: &Frame, label: impl Into<String>) -> PortDatum {
    let mut buf = String::new();
    let Some(end) = start_scan(r, &mut buf)? else {
        return Ok(None);
    };
    let mut src = StringSource::empty(label);
    let mut reader = DataReader::default();
    loop {
        end.scan(r, &mut buf)?;
        src.set(buf.split_off(0));
        match reader.read(&mut src, Namespace(env.new_child()))? {
            ParserOutput::Complete(seq) => {
                let v = seq
                    .eval(env)
                    .expect("read-datum evaluation should always result in a valid value");
                if let Value::Unspecified = v {
                    todo!("no value found, keep going");
                } else {
                    return Ok(Some(v));
                }
            }
            ParserOutput::Continuation => todo!("incomplete datum found, keep going"),
        }
    }
}

enum ScanEnd {
    Delimiter,
    DoubleQuote,
    Paren,
    Pipe,
}

impl ScanEnd {
    fn scan(&self, r: &mut dyn CharReader, buf: &mut String) -> PortResult {
        match self {
            Self::Delimiter => {
                while let Some(ch) = r.peek_char()? {
                    if string::is_delimiter(ch) {
                        break;
                    } else {
                        consume_char(r, buf)?;
                    }
                }
                Ok(())
            }
            Self::DoubleQuote => read_to(r, '"', buf),
            Self::Paren => read_to(r, ')', buf),
            Self::Pipe => read_to(r, '|', buf),
        }
    }

    fn consume_delimiter(&self, r: &mut dyn CharReader, buf: &mut String) -> PortBool {
        Ok(if let Self::Delimiter = self {
            if let Some(ch) = r.read_char()? {
                buf.push(ch);
                true
            } else {
                false
            }
        } else {
            true
        })
    }
}

fn start_scan(r: &mut dyn CharReader, buf: &mut String) -> PortResult<Option<ScanEnd>> {
    while let Some(ch) = r.read_char()? {
        buf.push(ch);
        match ch {
            '"' => return Ok(Some(ScanEnd::DoubleQuote)),
            '#' => {
                let r = classify_hash(r, buf)?;
                if r.is_some() {
                    return Ok(r);
                }
            }
            '(' => return Ok(Some(ScanEnd::Paren)),
            ';' => read_to(r, '\n', buf)?,
            '|' => return Ok(Some(ScanEnd::Pipe)),
            _ if string::is_whitespace(ch) => (),
            _ => return Ok(Some(ScanEnd::Delimiter)),
        }
    }
    Ok(None)
}

fn classify_hash(r: &mut dyn CharReader, buf: &mut String) -> PortResult<Option<ScanEnd>> {
    if let Some(ch) = r.peek_char()? {
        match ch {
            '(' => {
                // NOTE: vector
                consume_char(r, buf)?;
                return Ok(Some(ScanEnd::Paren));
            }
            ';' => {
                // NOTE: datum comment, keep going
                consume_char(r, buf)?;
            }
            'u' => {
                consume_char(r, buf)?;
                if let Some(ch) = r.peek_char()?
                    && ch == '8'
                {
                    consume_char(r, buf)?;
                    if let Some(ch) = r.peek_char()?
                        && ch == '('
                    {
                        // NOTE: bytevector
                        consume_char(r, buf)?;
                        return Ok(Some(ScanEnd::Paren));
                    }
                }
                return Ok(Some(ScanEnd::Delimiter));
            }
            '|' => {
                consume_char(r, buf)?;
                loop {
                    read_to(r, '|', buf)?;
                    match r.peek_char()? {
                        None => break,
                        Some(ch) => {
                            if ch == '#' {
                                // NOTE: block comment
                                consume_char(r, buf)?;
                                break;
                            }
                        }
                    }
                }
            }
            _ => (),
        }
    } else {
        return Ok(Some(ScanEnd::Delimiter));
    }
    Ok(None)
}

fn read_to(r: &mut dyn CharReader, sentinel: char, buf: &mut String) -> PortResult {
    while let Some(ch) = r.read_char()? {
        buf.push(ch);
        if ch == sentinel {
            break;
        }
    }
    Ok(())
}

fn consume_char(r: &mut dyn CharReader, buf: &mut String) -> PortResult {
    buf.push(r.read_char()?.expect("just-peeked char should be readable"));
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
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
}
