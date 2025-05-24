use super::Frame;
use crate::{
    syntax::Program,
    value::{Value, ValueRef},
};
use std::{
    fmt::{self, Display, Formatter, Write},
    iter,
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
    Lambda(Program /*, TODO: need parameter names? */),
}

pub(crate) struct Datum<'a>(&'a Procedure);

impl Display for Datum<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "#<procedure {}", self.0.name)?;
        write_arity(&self.0.arity, f)?;
        f.write_char('>')
    }
}

fn write_arity(arity: &Range<u8>, f: &mut Formatter<'_>) -> fmt::Result {
    if arity.start == 0 && arity.len() == 0 {
        Ok(())
    } else {
        f.write_str(" (")?;
        let params = iter::repeat_n("_", arity.start.into()).chain(if arity.end == u8::MAX {
            iter::repeat_n("…", 1)
        } else {
            iter::repeat_n("?", arity.len())
        });
        f.write_str(&params.collect::<Vec<_>>().join(" "))?;
        f.write_char(')')
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
