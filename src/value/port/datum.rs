use super::{CharReader, PortBool, PortDatum, PortResult};
use crate::{DataReader, eval::Frame, src::StringSource, string};

pub(super) fn parse(r: &mut dyn CharReader, env: &Frame, src: impl Into<String>) -> PortDatum {
    let mut buf = String::new();
    let Some(end) = start_scan(r, &mut buf)? else {
        return Ok(None);
    };
    let mut reader = DataReader::default();
    let mut src = StringSource::empty(src);
    loop {
        end.scan(r, &mut buf)?;
        src.set(buf.split_off(0));
        /*
        match lexer.tokenize(&mut src).unwrap() {
            LexerOutput::Complete(t) => {
                match parser.parse(t, Namespace(env.new_child())).unwrap() {
                    ParserOutput::Complete(_) => todo!(),
                    ParserOutput::Continuation => todo!(),
                }
            }
            LexerOutput::Continuation => {
                buf.clear();
                if !end.consume_delimiter(r, &mut buf)? {
                    todo!("continuation-to-read-error")
                }
            }
        }
        */
    }
    todo!();
}

enum ScanEnd {
    Character,
    DoubleQuote,
    Paren,
    Pipe,
}

impl ScanEnd {
    fn scan(&self, r: &mut dyn CharReader, buf: &mut String) -> PortResult {
        match self {
            Self::Character => {
                while let Some(ch) = r.peek_char()? {
                    if !string::is_delimiter(ch) {
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
        Ok(if let Self::Character = self {
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
            _ => return Ok(Some(ScanEnd::Character)),
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
                return Ok(Some(ScanEnd::Character));
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
        return Ok(Some(ScanEnd::Character));
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
    use crate::{testutil::TestEnv, value::port::StringReader};

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
}
