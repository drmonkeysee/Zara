#[cfg(test)]
mod tests;

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
