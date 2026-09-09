#[cfg(test)]
mod tests;

use crate::string;
use std::ops::ControlFlow;

pub(crate) type DatumScanResult<E> = Result<Option<String>, E>;
pub(crate) type CursorResult<E> = Result<Option<char>, E>;

pub(crate) trait CharCursor {
    type Error;
    fn read_char(&mut self) -> CursorResult<Self::Error>;
    fn peek_char(&mut self) -> CursorResult<Self::Error>;
}

pub(crate) fn scan<C: CharCursor + ?Sized>(cur: &mut C) -> DatumScanResult<C::Error> {
    let mut buf = String::new();
    while let Some(ch) = cur.read_char()? {
        buf.push(ch);
        if let ScanFlow::Break(()) = match ch {
            '"' | '|' => scan_escapable_delimiter(ch, cur, &mut buf)?,
            '#' => classify_hash(cur, &mut buf)?,
            '(' => scan_parens(1, cur, &mut buf)?,
            ';' => scan_comment(cur, &mut buf)?,
            _ if string::is_whitespace(ch) => ScanFlow::Continue(()),
            _ => scan_delimiter(cur, &mut buf)?,
        } {
            return Ok(Some(buf));
        }
    }
    Ok(None)
}

type ScanFlow = ControlFlow<()>;
type ScanResult<E, T = ScanFlow> = Result<T, E>;

fn classify_hash<C: CharCursor + ?Sized>(cur: &mut C, buf: &mut String) -> ScanResult<C::Error> {
    if let Some(ch) = cur.peek_char()? {
        match ch {
            '(' => {
                // vector
                consume_char(cur, buf)?;
                return scan_parens(1, cur, buf);
            }
            ';' => {
                // datum comment, keep going
                consume_char(cur, buf)?;
            }
            'u' => {
                consume_char(cur, buf)?;
                if let Some(ch) = cur.peek_char()?
                    && ch == '8'
                {
                    consume_char(cur, buf)?;
                    if let Some(ch) = cur.peek_char()?
                        && ch == '('
                    {
                        // bytevector
                        consume_char(cur, buf)?;
                        return scan_parens(1, cur, buf);
                    }
                }
                return scan_delimiter(cur, buf);
            }
            '|' => {
                consume_char(cur, buf)?;
                scan_block_comment(1, cur, buf)?;
            }
            _ => return scan_delimiter(cur, buf),
        }
    } else {
        return Ok(ScanFlow::Break(()));
    }
    Ok(ScanFlow::Continue(()))
}

fn scan_comment<C: CharCursor + ?Sized>(cur: &mut C, buf: &mut String) -> ScanResult<C::Error> {
    while let Some(ch) = cur.read_char()? {
        buf.push(ch);
        if ch == '\n' {
            break;
        }
    }
    Ok(ScanFlow::Continue(()))
}

fn scan_delimiter<C: CharCursor + ?Sized>(cur: &mut C, buf: &mut String) -> ScanResult<C::Error> {
    while let Some(ch) = cur.peek_char()? {
        if string::is_delimiter(ch) {
            break;
        }
        consume_char(cur, buf)?;
    }
    Ok(ScanFlow::Break(()))
}

fn scan_escapable_delimiter<C: CharCursor + ?Sized>(
    delimiter: char,
    cur: &mut C,
    buf: &mut String,
) -> ScanResult<C::Error> {
    let mut esc = false;
    while let Some(ch) = cur.read_char()? {
        buf.push(ch);
        match ch {
            '\\' => esc = !esc,
            _ if ch == delimiter && !esc => break,
            _ => esc = false,
        }
    }
    Ok(ScanFlow::Break(()))
}

fn scan_parens<C: CharCursor + ?Sized>(
    mut c: usize,
    cur: &mut C,
    buf: &mut String,
) -> ScanResult<C::Error> {
    while let Some(ch) = cur.read_char()? {
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

fn scan_block_comment<C: CharCursor + ?Sized>(
    mut c: usize,
    cur: &mut C,
    buf: &mut String,
) -> ScanResult<C::Error, ()> {
    let mut d = BlockDelimiter::None;
    while let Some(ch) = cur.read_char()? {
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

fn consume_char<C: CharCursor + ?Sized>(cur: &mut C, buf: &mut String) -> ScanResult<C::Error, ()> {
    buf.push(
        cur.read_char()?
            .expect("just-peeked char should be readable"),
    );
    Ok(())
}
