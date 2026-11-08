use super::Radix;
use crate::{
    NumReader,
    eval::{EvalResult, Frame, Namespace},
    src::StringSource,
    syntax::ParserOutput,
    value::Value,
};

pub(crate) fn read(
    s: impl Into<String>,
    rx: Radix,
    env: &Frame,
    label: impl Into<String>,
) -> EvalResult {
    let mut s = s.into();
    // If no radix in current string prepend the requested radix; if s is a
    // valid numeric datum, prepending prefix keeps it valid, if it's invalid
    // prepending the radix won't make it worse. Note that prepending any radix
    // other than #d on an inexact literal is an invalid numeric datum.
    if !Radix::in_string(&s) {
        s.insert_str(0, rx.into_prefix());
    }
    let mut src = StringSource::new(s, label);
    let mut r = NumReader::default();
    match r.read(&mut src, Namespace(env.new_child())) {
        Err(_) | Ok(ParserOutput::Continuation) => Ok(Value::Boolean(false)),
        // NumReader guarantees if we got this far, eval will yield a number
        Ok(ParserOutput::Complete(seq)) => seq.eval(env),
    }
}
