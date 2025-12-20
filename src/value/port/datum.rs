#[cfg(test)]
mod tests;

use super::{CharReader, PortDatum, PortResult, PortString, Value};
use crate::{
    DataReader,
    eval::{Frame, Namespace},
    src::StringSource,
    string,
    syntax::ParserOutput,
};
use std::ops::ControlFlow;

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

type ScanFlow = ControlFlow<()>;
type ScanResult = PortResult<ScanFlow>;

fn datum_scan(r: &mut dyn CharReader) -> PortString {
    let mut buf = String::new();
    while let Some(ch) = r.read_char()? {
        buf.push(ch);
        if let ScanFlow::Break(()) = match ch {
            '"' | '|' => scan_escapable_delimiter(ch, r, &mut buf)?,
            '#' => classify_hash(r, &mut buf)?,
            '(' => scan_parens(1, r, &mut buf)?,
            ';' => scan_line(r, &mut buf)?,
            _ if string::is_whitespace(ch) => ScanFlow::Continue(()),
            _ => scan_delimiter(r, &mut buf)?,
        } {
            return Ok(Some(buf));
        }
    }
    Ok(None)
}

fn classify_hash(r: &mut dyn CharReader, buf: &mut String) -> ScanResult {
    if let Some(ch) = r.peek_char()? {
        match ch {
            '(' => {
                // NOTE: vector
                consume_char(r, buf)?;
                return scan_parens(1, r, buf);
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
                        return scan_parens(1, r, buf);
                    }
                }
                return scan_delimiter(r, buf);
            }
            '|' => {
                consume_char(r, buf)?;
                scan_block_comment(1, r, buf)?;
            }
            _ => return scan_delimiter(r, buf),
        }
    } else {
        return Ok(ScanFlow::Break(()));
    }
    Ok(ScanFlow::Continue(()))
}

fn scan_line(r: &mut dyn CharReader, buf: &mut String) -> ScanResult {
    while let Some(ch) = r.read_char()? {
        buf.push(ch);
        if ch == '\n' {
            break;
        }
    }
    Ok(ScanFlow::Break(()))
}

fn scan_delimiter(r: &mut dyn CharReader, buf: &mut String) -> ScanResult {
    while let Some(ch) = r.peek_char()? {
        if string::is_delimiter(ch) {
            break;
        }
        consume_char(r, buf)?;
    }
    Ok(ScanFlow::Break(()))
}

fn scan_escapable_delimiter(
    delimiter: char,
    r: &mut dyn CharReader,
    buf: &mut String,
) -> ScanResult {
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
    Ok(ScanFlow::Break(()))
}

fn scan_parens(mut c: usize, r: &mut dyn CharReader, buf: &mut String) -> ScanResult {
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
    Ok(ScanFlow::Break(()))
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
