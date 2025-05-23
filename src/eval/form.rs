use super::Frame;
use crate::{syntax::Program, value::Value};
use std::rc::Rc;

pub(crate) type IntrinsicFn = fn(&[Rc<Value>], &Frame) -> Option<Rc<Value>>;

#[derive(Debug)]
pub(crate) struct Procedure {
    arity: u8,
    body: Body,
    name: Box<str>,
}

impl Procedure {
    pub(crate) fn intrinsic(name: impl Into<Box<str>>, arity: u8, body: IntrinsicFn) -> Self {
        Self {
            arity,
            body: Body::Intrinsic(body),
            name: name.into(),
        }
    }
}

#[derive(Debug)]
enum Body {
    Intrinsic(IntrinsicFn),
    // TODO: this likely has to be a 3rd thing: Body to exclude constructs
    // that can only appear at top-level program.
    Lambda(Box<Program> /*, TODO: need parameter names? */),
}
