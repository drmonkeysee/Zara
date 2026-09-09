#[cfg(test)]
mod tests;

use super::{CharReader, PortDatum, PortError, Value};
use crate::{
    DataReader,
    eval::{Frame, Namespace},
    lex::datum::{self, CharCursor, CursorResult},
    src::StringSource,
    syntax::ParserOutput,
};

pub(super) fn parse(r: &mut dyn CharReader, env: &Frame, label: impl Into<String>) -> PortDatum {
    let mut src = StringSource::empty(label);
    let mut reader = DataReader::default();
    loop {
        let Some(buf) = datum::scan(r)? else {
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

impl<T: CharReader + ?Sized> CharCursor for T {
    type Error = PortError;

    fn read_char(&mut self) -> CursorResult<Self::Error> {
        self.read_char()
    }

    fn peek_char(&mut self) -> CursorResult<Self::Error> {
        self.peek_char()
    }
}
