use super::Frame;
use crate::{
    syntax::Program,
    value::{Value, ValueRef},
};
use std::{
    fmt::{self, Display, Formatter},
    ops::Range,
    rc::Rc,
};

pub(crate) type IntrinsicFn = fn(&[Rc<Value>], &Frame) -> ValueRef;

#[derive(Debug)]
pub(crate) struct Procedure {
    arity: Range<u8>,
    body: Body,
    name: Box<str>,
}

impl Procedure {
    pub(crate) fn intrinsic(
        name: impl Into<Box<str>>,
        arity: Range<u8>,
        body: IntrinsicFn,
    ) -> Self {
        Self {
            arity,
            body: Body::Intrinsic(body),
            name: name.into(),
        }
    }

    pub(crate) fn as_datum(&self) -> Datum {
        Datum(self)
    }
}

#[derive(Debug)]
enum Body {
    Intrinsic(IntrinsicFn),
    // TODO: this likely has to be a 3rd thing: Body to exclude constructs
    // that can only appear at top-level program.
    Lambda(Box<Program> /*, TODO: need parameter names? */),
}

pub(crate) struct Datum<'a>(&'a Procedure);

impl Display for Datum<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "#<procedure {}>", self.0.name)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn intrinsic_datum_zero_arity() {
        let p = Procedure::intrinsic("foo", 0..0, |_, _| None);

        assert_eq!(p.as_datum().to_string(), "#<procedure foo>");
    }

    #[test]
    fn intrinsic_datum_single_arity() {
        let p = Procedure::intrinsic("foo", 1..1, |_, _| None);

        assert_eq!(p.as_datum().to_string(), "#<procedure foo (_)>");
    }

    #[test]
    fn intrinsic_datum_multi_arity() {
        let p = Procedure::intrinsic("foo", 3..3, |_, _| None);

        assert_eq!(p.as_datum().to_string(), "#<procedure foo (_ _ _)>");
    }

    #[test]
    fn intrinsic_datum_optional() {
        let p = Procedure::intrinsic("foo", 0..1, |_, _| None);

        assert_eq!(p.as_datum().to_string(), "#<procedure foo (?)>");
    }

    #[test]
    fn intrinsic_datum_multi_optional() {
        let p = Procedure::intrinsic("foo", 1..3, |_, _| None);

        assert_eq!(p.as_datum().to_string(), "#<procedure foo (_ ? ?)>");
    }

    #[test]
    fn intrinsic_datum_open_arity() {
        let p = Procedure::intrinsic("foo", 0..255, |_, _| None);

        assert_eq!(p.as_datum().to_string(), "#<procedure foo (…)>");
    }

    #[test]
    fn intrinsic_datum_required_params_with_open_arity() {
        let p = Procedure::intrinsic("foo", 2..255, |_, _| None);

        assert_eq!(p.as_datum().to_string(), "#<procedure foo (_ _ …)>");
    }
}
