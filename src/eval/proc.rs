use super::{EvalResult, Frame};
use crate::{syntax::Program, value::Value};
use std::{
    fmt::{self, Display, Formatter, Write},
    iter,
    ops::Range,
    rc::Rc,
};

pub(crate) const MAX_ARITY: u8 = u8::MAX;

pub(crate) type IntrinsicFn = fn(&[Value], &mut Frame) -> EvalResult;
pub(crate) type Arity = Range<u8>;

#[derive(Debug)]
pub(crate) struct Procedure {
    arity: Arity,
    def: Definition,
    name: Option<Rc<str>>,
}

impl Procedure {
    pub(crate) fn intrinsic(name: Rc<str>, arity: Arity, def: IntrinsicFn) -> Self {
        Self {
            arity,
            def: Definition::Intrinsic(def),
            name: Some(name),
        }
    }

    pub(crate) fn lambda(
        named_args: &[Rc<str>],
        variadic_arg: Option<Rc<str>>,
        body: Program,
        name: Option<Rc<str>>,
    ) -> Self {
        todo!();
    }

    pub(crate) fn name(&self) -> Option<&str> {
        self.name.as_deref()
    }

    pub(crate) fn arity(&self) -> &Arity {
        &self.arity
    }

    pub(crate) fn matches_arity(&self, args_len: usize) -> bool {
        (self.arity.is_empty() && self.arity.start as usize == args_len)
            || (self.arity.start as usize <= args_len && args_len <= self.arity.len())
    }

    pub(crate) fn apply(&self, args: &[Value], env: &mut Frame) -> EvalResult {
        self.def.apply(args, env)
    }
}

impl Display for Procedure {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str("#<procedure")?;
        if let Some(n) = &self.name {
            write!(f, " {n}")?;
        }
        write_arity(&self.arity, f)?;
        f.write_char('>')
    }
}

#[derive(Debug)]
enum Formal {
    Parameter(Rc<str>),
    Rest(Rc<str>),
}

#[derive(Debug)]
enum Definition {
    Intrinsic(IntrinsicFn),
    // TODO: this likely has to be a 3rd thing: Body to exclude constructs
    // that can only appear at top-level program.
    #[allow(dead_code, reason = "not yet implemented")]
    Lambda(Box<[Formal]>, Program),
}

impl Definition {
    fn apply(&self, args: &[Value], env: &mut Frame) -> EvalResult {
        match self {
            Self::Intrinsic(func) => func(args, env),
            Self::Lambda(..) => todo!("lambda apply"),
        }
    }
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
    use crate::testutil::{TestEnv, ok_or_fail};

    #[test]
    fn intrinsic_zero_arity() {
        let p = Procedure::intrinsic("foo".into(), 0..0, |_, _| Ok(Value::Unspecified));

        assert_eq!(p.to_string(), "#<procedure foo>");
    }

    #[test]
    fn intrinsic_single_arity() {
        let p = Procedure::intrinsic("foo".into(), 1..1, |_, _| Ok(Value::Unspecified));

        assert_eq!(p.to_string(), "#<procedure foo (_)>");
    }

    #[test]
    fn intrinsic_multi_arity() {
        let p = Procedure::intrinsic("foo".into(), 3..3, |_, _| Ok(Value::Unspecified));

        assert_eq!(p.to_string(), "#<procedure foo (_ _ _)>");
    }

    #[test]
    fn intrinsic_optional() {
        let p = Procedure::intrinsic("foo".into(), 0..1, |_, _| Ok(Value::Unspecified));

        assert_eq!(p.to_string(), "#<procedure foo (?)>");
    }

    #[test]
    fn intrinsic_multi_optional() {
        let p = Procedure::intrinsic("foo".into(), 1..3, |_, _| Ok(Value::Unspecified));

        assert_eq!(p.to_string(), "#<procedure foo (_ ? ?)>");
    }

    #[test]
    fn intrinsic_open_arity() {
        let p = Procedure::intrinsic("foo".into(), 0..255, |_, _| Ok(Value::Unspecified));

        assert_eq!(p.to_string(), "#<procedure foo (…)>");
    }

    #[test]
    fn intrinsic_required_params_with_open_arity() {
        let p = Procedure::intrinsic("foo".into(), 2..255, |_, _| Ok(Value::Unspecified));

        assert_eq!(p.to_string(), "#<procedure foo (_ _ …)>");
    }

    #[test]
    fn lambda_zero_arity_no_name() {
        let p = Procedure::lambda(&[], None, Program::empty(), None);

        assert_eq!(p.to_string(), "#<procedure>");
    }

    #[test]
    fn lambda_zero_arity() {
        let p = Procedure::lambda(&[], None, Program::empty(), Some("bar".into()));

        assert_eq!(p.to_string(), "#<procedure bar>");
    }

    #[test]
    fn lambda_single_arity() {
        let p = Procedure::lambda(&["x".into()], None, Program::empty(), Some("bar".into()));

        assert_eq!(p.to_string(), "#<procedure bar (x)>");
    }

    #[test]
    fn lambda_multi_arity() {
        let p = Procedure::lambda(
            &["x".into(), "y".into(), "z".into()],
            None,
            Program::empty(),
            Some("bar".into()),
        );

        assert_eq!(p.to_string(), "#<procedure bar (x y z)>");
    }

    #[test]
    fn lambda_variadic_arity() {
        let p = Procedure::lambda(
            &[],
            Some("any".into()),
            Program::empty(),
            Some("bar".into()),
        );

        assert_eq!(p.to_string(), "#<procedure bar any…>");
    }

    #[test]
    fn lambda_rest_arity() {
        let p = Procedure::lambda(
            &["x".into(), "y".into(), "z".into()],
            Some("rest".into()),
            Program::empty(),
            Some("bar".into()),
        );

        assert_eq!(p.to_string(), "#<procedure bar (x y z rest…)>");
    }

    #[test]
    fn matches_zero_arity() {
        let p = Procedure::intrinsic("foo".into(), 0..0, |_, _| Ok(Value::Unspecified));

        assert!(p.matches_arity(0));
    }

    #[test]
    fn matches_single_arity() {
        let p = Procedure::intrinsic("foo".into(), 1..1, |_, _| Ok(Value::Unspecified));

        assert!(p.matches_arity(1));
    }

    #[test]
    fn matches_max_arity() {
        let p = Procedure::intrinsic("foo".into(), 255..255, |_, _| Ok(Value::Unspecified));

        assert!(p.matches_arity(MAX_ARITY as usize));
    }

    #[test]
    fn matches_min_variable_arity() {
        let p = Procedure::intrinsic("foo".into(), 0..3, |_, _| Ok(Value::Unspecified));

        assert!(p.matches_arity(0));
    }

    #[test]
    fn matches_max_variable_arity() {
        let p = Procedure::intrinsic("foo".into(), 0..3, |_, _| Ok(Value::Unspecified));

        assert!(p.matches_arity(3));
    }

    #[test]
    fn matches_exceeds_variable_arity() {
        let p = Procedure::intrinsic("foo".into(), 0..3, |_, _| Ok(Value::Unspecified));

        assert!(!p.matches_arity(4));
    }

    #[test]
    fn matches_less_than_min_variable_arity() {
        let p = Procedure::intrinsic("foo".into(), 2..5, |_, _| Ok(Value::Unspecified));

        assert!(!p.matches_arity(1));
    }

    #[test]
    fn exceeds_max_arity() {
        let p = Procedure::intrinsic("foo".into(), 255..255, |_, _| Ok(Value::Unspecified));

        assert!(!p.matches_arity(256));
    }

    #[test]
    fn lambda_matches_zero_arity() {
        let p = Procedure::lambda(&[], None, Program::empty(), Some("bar".into()));

        assert!(p.matches_arity(0));
    }

    #[test]
    fn lambda_matches_single_arity() {
        let p = Procedure::lambda(&["x".into()], None, Program::empty(), Some("bar".into()));

        assert!(p.matches_arity(1));
    }

    #[test]
    fn lambda_matches_multi_arity() {
        let p = Procedure::lambda(
            &["x".into(), "y".into(), "z".into()],
            None,
            Program::empty(),
            Some("bar".into()),
        );

        assert!(p.matches_arity(3));
        assert!(!p.matches_arity(2));
        assert!(!p.matches_arity(4));
    }

    #[test]
    fn lambda_matches_variadic_arity() {
        let p = Procedure::lambda(
            &[],
            Some("any".into()),
            Program::empty(),
            Some("bar".into()),
        );

        assert!(p.matches_arity(0));
        assert!(p.matches_arity(MAX_ARITY as usize));
        assert!(!p.matches_arity(256));
    }

    #[test]
    fn lambda_matches_rest_arity() {
        let p = Procedure::lambda(
            &["x".into(), "y".into()],
            Some("any".into()),
            Program::empty(),
            Some("bar".into()),
        );

        assert!(!p.matches_arity(0));
        assert!(!p.matches_arity(1));
        assert!(p.matches_arity(2));
        assert!(p.matches_arity(MAX_ARITY as usize));
        assert!(!p.matches_arity(256));
    }

    #[test]
    fn apply_zero_arity() {
        let p = Procedure::intrinsic("foo".into(), 0..0, |_, _| Ok(Value::string("bar")));
        let mut env = TestEnv::default();
        let mut f = env.new_frame();
        let r = p.apply(&[], &mut f);

        let v = ok_or_fail!(r);
        assert!(matches!(v, Value::String(s) if s.as_ref() == "bar"));
    }

    #[test]
    fn apply_single_arity() {
        let p = Procedure::intrinsic("foo".into(), 1..1, |args, _| {
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
        assert!(matches!(v, Value::String(s) if s.as_ref() == "bar baz"));
    }
}
