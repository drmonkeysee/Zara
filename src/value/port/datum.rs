#[cfg(test)]
mod tests;

use super::{CharReader, PortBool, PortDatum, PortResult, PortString, Value};
use crate::{
    DataReader,
    eval::{Frame, Namespace},
    src::StringSource,
    string,
    syntax::ParserOutput,
};

pub(super) fn parse(r: &mut dyn CharReader, env: &Frame, label: impl Into<String>) -> PortDatum {
    let mut src = StringSource::empty(label);
    let mut reader = DataReader::default();
    loop {
        let Some(buf) = datum_scan(r)? else {
            return Ok(None);
        };
        src.set(buf);
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

fn datum_scan(r: &mut dyn CharReader) -> PortString {
    let mut buf = String::new();
    while let Some(ch) = r.read_char()? {
        buf.push(ch);
        match ch {
            '"' | '|' => {
                scan_escapable_delimiter(ch, r, &mut buf)?;
                return Ok(Some(buf));
            }
            '#' => {
                if classify_hash(r, &mut buf)? {
                    return Ok(Some(buf));
                }
            }
            '(' => {
                scan_parens(1, r, &mut buf)?;
                return Ok(Some(buf));
            }
            ';' => scan_line(r, &mut buf)?,
            _ if string::is_whitespace(ch) => (),
            _ => {
                scan_delimiter(r, &mut buf)?;
                return Ok(Some(buf));
            }
        }
    }
    Ok(None)
}

fn classify_hash(r: &mut dyn CharReader, buf: &mut String) -> PortBool {
    if let Some(ch) = r.peek_char()? {
        match ch {
            '(' => {
                // NOTE: vector
                consume_char(r, buf)?;
                scan_parens(1, r, buf)?;
                return Ok(true);
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
                        scan_parens(1, r, buf)?;
                        return Ok(true);
                    }
                }
                scan_delimiter(r, buf)?;
                return Ok(true);
            }
            '|' => {
                consume_char(r, buf)?;
                scan_block_comment(1, r, buf)?;
            }
            _ => (),
        }
    } else {
        scan_delimiter(r, buf)?;
        return Ok(true);
    }
    Ok(false)
}

fn scan_line(r: &mut dyn CharReader, buf: &mut String) -> PortResult {
    while let Some(ch) = r.read_char()? {
        buf.push(ch);
        if ch == '\n' {
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
            ')' => c -= 1,
            _ => (),
        }
        if c == 0 {
            break;
        }
    }
    Ok(())
}

enum BlockDelimiter {
    None,
    Hash,
    Pipe,
}

fn scan_block_comment(mut c: usize, r: &mut dyn CharReader, buf: &mut String) -> PortResult {
    let mut d = BlockDelimiter::None;
    while let Some(ch) = r.read_char()? {
        buf.push(ch);
        match ch {
            '#' => {
                if let BlockDelimiter::Pipe = d {
                    c -= 1;
                }
                d = BlockDelimiter::Hash;
            }
            '|' => {
                if let BlockDelimiter::Hash = d {
                    c += 1;
                }
                d = BlockDelimiter::Pipe;
            }
            _ => d = BlockDelimiter::None,
        }
        if c == 0 {
            break;
        }
    }
    Ok(())
}

fn consume_char(r: &mut dyn CharReader, buf: &mut String) -> PortResult {
    buf.push(r.read_char()?.expect("just-peeked char should be readable"));
    Ok(())
}
