use super::{CharReader, PortDatum, PortResult};
use crate::{
    eval::{Frame, Namespace},
    lex::{Lexer, LexerOutput},
    string,
    syntax::{ExpressionTree, Parser, ParserOutput},
};

pub(super) fn parse(r: &mut dyn CharReader, env: &Frame) -> PortDatum {
    let mut buf = String::new();
    let Some(end) = start_scan(r, &mut buf)? else {
        return Ok(None);
    };
    end.scan(r, &mut buf)?;
    /*
    let lex = Lexer::default();
    let expr = ExpressionTree::default();
    match lex.tokenize(src)? {
        LexerOutput::Complete(tokens) => match expr.parse(tokens, Namespace(env.new_child()))? {
            ParserOutput::Complete(datum) => todo!(),
            ParserOutput::Continuation => todo!(),
        },
        LexerOutput::Continuation => todo!(),
    }
    */
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
