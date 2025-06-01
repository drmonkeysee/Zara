use super::{EvalResult, Frame};
use crate::{syntax::Program, value::Value};
use std::{
    fmt::{self, Display, Formatter, Write},
    iter,
    ops::Range,
};

pub(crate) const MAX_ARITY: u8 = u8::MAX;

pub(crate) type IntrinsicFn = fn(&[Value], &mut Frame) -> EvalResult;
pub(crate) type Arity = Range<u8>;

#[derive(Debug)]
pub(crate) struct Procedure {
    arity: Arity,
    body: Body,
    name: Box<str>,
}

impl Procedure {
    pub(crate) fn intrinsic(name: impl Into<Box<str>>, arity: Arity, body: IntrinsicFn) -> Self {
        Self {
            arity,
            body: Body::Intrinsic(body),
            name: name.into(),
        }
    }

    pub(crate) fn name(&self) -> &str {
        &self.name
    }

    pub(crate) fn arity(&self) -> &Arity {
        &self.arity
    }

    pub(crate) fn matches_arity(&self, args_len: usize) -> bool {
        (self.arity.is_empty() && self.arity.start as usize == args_len)
            || (self.arity.start as usize <= args_len && args_len <= self.arity.len())
    }

    pub(crate) fn apply(&self, args: &[Value], env: &mut Frame) -> EvalResult {
        match self.body {
            Body::Intrinsic(func) => func(args, env),
            Body::Lambda(_) => todo!("lambda apply"),
        }
    }
}

impl Display for Procedure {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "#<procedure {}", self.name)?;
        write_arity(&self.arity, f)?;
        f.write_char('>')
    }
}

#[derive(Debug)]
enum Body {
    Intrinsic(IntrinsicFn),
    // TODO: this likely has to be a 3rd thing: Body to exclude constructs
    // that can only appear at top-level program.
    Lambda(Program /*, TODO: need parameter names? */),
}

fn write_arity(arity: &Arity, f: &mut Formatter<'_>) -> fmt::Result {
    if arity.start == 0 && arity.is_empty() {
        Ok(())
    } else {
        f.write_str(" (")?;
        let params = iter::repeat_n("_", arity.start.into()).chain(if arity.end == MAX_ARITY {
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
    use crate::{
        testutil::{TestEnv, ok_or_fail},
        value::Value,
    };

    #[test]
    fn intrinsic_zero_arity() {
        let p = Procedure::intrinsic("foo", 0..0, |_, _| Ok(Value::Unspecified));

        assert_eq!(p.to_string(), "#<procedure foo>");
    }

    #[test]
    fn intrinsic_single_arity() {
        let p = Procedure::intrinsic("foo", 1..1, |_, _| Ok(Value::Unspecified));

        assert_eq!(p.to_string(), "#<procedure foo (_)>");
    }

    #[test]
    fn intrinsic_multi_arity() {
        let p = Procedure::intrinsic("foo", 3..3, |_, _| Ok(Value::Unspecified));

        assert_eq!(p.to_string(), "#<procedure foo (_ _ _)>");
    }

    #[test]
    fn intrinsic_optional() {
        let p = Procedure::intrinsic("foo", 0..1, |_, _| Ok(Value::Unspecified));

        assert_eq!(p.to_string(), "#<procedure foo (?)>");
    }

    #[test]
    fn intrinsic_multi_optional() {
        let p = Procedure::intrinsic("foo", 1..3, |_, _| Ok(Value::Unspecified));

        assert_eq!(p.to_string(), "#<procedure foo (_ ? ?)>");
    }

    #[test]
    fn intrinsic_open_arity() {
        let p = Procedure::intrinsic("foo", 0..255, |_, _| Ok(Value::Unspecified));

        assert_eq!(p.to_string(), "#<procedure foo (…)>");
    }

    #[test]
    fn intrinsic_required_params_with_open_arity() {
        let p = Procedure::intrinsic("foo", 2..255, |_, _| Ok(Value::Unspecified));

        assert_eq!(p.to_string(), "#<procedure foo (_ _ …)>");
    }

    #[test]
    fn matches_zero_arity() {
        let p = Procedure::intrinsic("foo", 0..0, |_, _| Ok(Value::Unspecified));

        assert!(p.matches_arity(0));
    }

    #[test]
    fn matches_single_arity() {
        let p = Procedure::intrinsic("foo", 1..1, |_, _| Ok(Value::Unspecified));

        assert!(p.matches_arity(1));
    }

    #[test]
    fn matches_max_arity() {
        let p = Procedure::intrinsic("foo", 255..255, |_, _| Ok(Value::Unspecified));

        assert!(p.matches_arity(MAX_ARITY as usize));
    }

    #[test]
    fn matches_min_variable_arity() {
        let p = Procedure::intrinsic("foo", 0..3, |_, _| Ok(Value::Unspecified));

        assert!(p.matches_arity(0));
    }

    #[test]
    fn matches_max_variable_arity() {
        let p = Procedure::intrinsic("foo", 0..3, |_, _| Ok(Value::Unspecified));

        assert!(p.matches_arity(3));
    }

    #[test]
    fn matches_exceeds_variable_arity() {
        let p = Procedure::intrinsic("foo", 0..3, |_, _| Ok(Value::Unspecified));

        assert!(!p.matches_arity(4));
    }

    #[test]
    fn matches_less_than_min_variable_arity() {
        let p = Procedure::intrinsic("foo", 2..5, |_, _| Ok(Value::Unspecified));

        assert!(!p.matches_arity(1));
    }

    #[test]
    fn exceeds_max_arity() {
        let p = Procedure::intrinsic("foo", 255..255, |_, _| Ok(Value::Unspecified));

        assert!(!p.matches_arity(256));
    }

    #[test]
    fn apply_zero_arity() {
        let p = Procedure::intrinsic("foo", 0..0, |_, _| Ok(Value::string("bar")));
        let mut env = TestEnv::default();
        let mut f = env.new_frame();
        let r = p.apply(&[], &mut f);

        let v = ok_or_fail!(r);
        assert!(matches!(v, Value::String(s) if &*s == "bar"));
    }

    #[test]
    fn apply_single_arity() {
        let p = Procedure::intrinsic("foo", 1..1, |args, _| {
            let Value::String(s) = &args[0] else {
                unreachable!()
            };
            Ok(Value::string(format!("bar {s}")))
        });
        let mut env = TestEnv::default();
        let mut f = env.new_frame();
        let args = [Value::string("baz")];

        let r = p.apply(&args, &mut f);

        let v = ok_or_fail!(r);
        assert!(matches!(v, Value::String(s) if &*s == "bar baz"));
    }
}
