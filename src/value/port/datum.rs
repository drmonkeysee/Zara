#[cfg(test)]
mod tests;

use super::{CharReader, PortDatum, PortResult, Value};
use crate::{
    DataReader,
    eval::{Frame, Namespace},
    src::StringSource,
    string,
    syntax::ParserOutput,
};

pub(super) fn parse(r: &mut dyn CharReader, env: &Frame, label: impl Into<String>) -> PortDatum {
    let mut buf = String::new();
    let mut src = StringSource::empty(label);
    let mut reader = DataReader::default();
    loop {
        let Some(end) = start_scan(r, &mut buf)? else {
            return Ok(None);
        };
        end.scan(r, &mut buf)?;
        src.set(buf.split_off(0));
        match reader.read(&mut src, Namespace(env.new_child()))? {
            ParserOutput::Complete(seq) => {
                let v = seq
                    .eval(env)
                    .expect("read-datum evaluation should always result in a valid value");
                if !matches!(v, Value::Unspecified) {
                    return Ok(Some(v));
                }
            }
            ParserOutput::Continuation => {
                return match reader.unsupported_continuation() {
                    None => Ok(None),
                    Some(err) => Err(err.into()),
                };
            }
        }
    }
}

enum ScanEnd {
    Delimiter,
    Escapable(char),
    Paren(usize),
}

impl ScanEnd {
    fn scan(&self, r: &mut dyn CharReader, buf: &mut String) -> PortResult {
        match self {
            Self::Delimiter => scan_delimiter(r, buf),
            Self::Escapable(ch) => scan_escapable_delimiter(*ch, r, buf),
            Self::Paren(c) => scan_parens(*c, r, buf),
        }
    }
}

fn start_scan(r: &mut dyn CharReader, buf: &mut String) -> PortResult<Option<ScanEnd>> {
    while let Some(ch) = r.read_char()? {
        buf.push(ch);
        match ch {
            '"' | '|' => return Ok(Some(ScanEnd::Escapable(ch))),
            '#' => {
                let r = classify_hash(r, buf)?;
                if r.is_some() {
                    return Ok(r);
                }
            }
            '(' => return Ok(Some(ScanEnd::Paren(1))),
            ';' => read_to(r, '\n', buf)?,
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
                return Ok(Some(ScanEnd::Paren(1)));
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
                        return Ok(Some(ScanEnd::Paren(1)));
                    }
                }
                return Ok(Some(ScanEnd::Delimiter));
            }
            '|' => {
                consume_char(r, buf)?;
                loop {
                    read_to(r, '|', buf)?;
                    match r.read_char()? {
                        None => break,
                        Some(ch) => {
                            buf.push(ch);
                            if ch == '#' {
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

fn scan_delimiter(r: &mut dyn CharReader, buf: &mut String) -> PortResult {
    while let Some(ch) = r.peek_char()? {
        if string::is_delimiter(ch) {
            break;
        } else {
            consume_char(r, buf)?;
        }
    }
    Ok(())
}

fn scan_escapable_delimiter(
    delimiter: char,
    r: &mut dyn CharReader,
    buf: &mut String,
) -> PortResult {
    let mut esc = false;
    while let Some(ch) = r.read_char()? {
        buf.push(ch);
        match ch {
            '\\' => esc = !esc,
            _ if ch == delimiter => {
                if esc {
                    esc = false;
                } else {
                    break;
                }
            }
            _ => esc = false,
        }
    }
    Ok(())
}

fn scan_parens(mut c: usize, r: &mut dyn CharReader, buf: &mut String) -> PortResult {
    while let Some(ch) = r.read_char()? {
        buf.push(ch);
        match ch {
            '(' => c += 1,
            ')' => {
                c -= 1;
                if c == 0 {
                    break;
                }
            }
            _ => (),
        }
    }
    Ok(())
}

fn consume_char(r: &mut dyn CharReader, buf: &mut String) -> PortResult {
    buf.push(r.read_char()?.expect("just-peeked char should be readable"));
    Ok(())
}
