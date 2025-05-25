use super::Frame;
use crate::{syntax::Program, value::ValueRef};
use std::{
    fmt::{self, Display, Formatter, Write},
    iter,
    ops::Range,
};

pub(crate) type IntrinsicFn = fn(&[ValueRef], &Frame) -> ValueRef;

#[derive(Debug)]
pub(crate) struct Procedure {
    body: Body,
    name: Box<str>,
}

impl Procedure {
    pub(crate) fn intrinsic(
        name: impl Into<Box<str>>,
        arity: Range<u8>,
        func: IntrinsicFn,
    ) -> Self {
        Self {
            body: Body::Intrinsic { arity, func },
            name: name.into(),
        }
    }
}

impl Display for Procedure {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "#<procedure {}", self.name)?;
        match &self.body {
            Body::Intrinsic { arity, .. } => write_arity(arity, f)?,
            Body::Lambda(_) => todo!("lambda proc datum"),
        }
        f.write_char('>')
    }
}

#[derive(Debug)]
enum Body {
    Intrinsic { arity: Range<u8>, func: IntrinsicFn },
    // TODO: this likely has to be a 3rd thing: Body to exclude constructs
    // that can only appear at top-level program.
    Lambda(Program /*, TODO: need parameter names? */),
}

fn write_arity(arity: &Range<u8>, f: &mut Formatter<'_>) -> fmt::Result {
    if arity.start == 0 && arity.is_empty() {
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
    use crate::value::Value;

    #[test]
    fn intrinsic_zero_arity() {
        let p = Procedure::intrinsic("foo", 0..0, |_, _| Value::Unspecified.into());

        assert_eq!(p.to_string(), "#<procedure foo>");
    }

    #[test]
    fn intrinsic_single_arity() {
        let p = Procedure::intrinsic("foo", 1..1, |_, _| Value::Unspecified.into());

        assert_eq!(p.to_string(), "#<procedure foo (_)>");
    }

    #[test]
    fn intrinsic_multi_arity() {
        let p = Procedure::intrinsic("foo", 3..3, |_, _| Value::Unspecified.into());

        assert_eq!(p.to_string(), "#<procedure foo (_ _ _)>");
    }

    #[test]
    fn intrinsic_optional() {
        let p = Procedure::intrinsic("foo", 0..1, |_, _| Value::Unspecified.into());

        assert_eq!(p.to_string(), "#<procedure foo (?)>");
    }

    #[test]
    fn intrinsic_multi_optional() {
        let p = Procedure::intrinsic("foo", 1..3, |_, _| Value::Unspecified.into());

        assert_eq!(p.to_string(), "#<procedure foo (_ ? ?)>");
    }

    #[test]
    fn intrinsic_open_arity() {
        let p = Procedure::intrinsic("foo", 0..255, |_, _| Value::Unspecified.into());

        assert_eq!(p.to_string(), "#<procedure foo (…)>");
    }

    #[test]
    fn intrinsic_required_params_with_open_arity() {
        let p = Procedure::intrinsic("foo", 2..255, |_, _| Value::Unspecified.into());

        assert_eq!(p.to_string(), "#<procedure foo (_ _ …)>");
    }
}
