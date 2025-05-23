use super::Frame;
use crate::{syntax::Program, value::Value};
use std::rc::Rc;

struct Procedure {
    arity: u8,
    body: Body,
    name: Box<str>,
}

enum Body {
    Intrinsic(fn(&[Rc<Value>], &Frame) -> Option<Rc<Value>>),
    // TODO: this likely has to be a 3rd thing: Body to exclude constructs
    // that can only appear at top-level program.
    Lambda(Box<Program> /*, TODO: need parameter names? */),
}
