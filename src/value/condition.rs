use super::Value;
use crate::eval::{Arity, MAX_ARITY};
use std::fmt::{self, Display, Formatter, Write};

#[derive(Debug)]
pub(crate) struct Condition {
    kind: ConditionKind,
    irritants: Option<Value>,
    msg: Box<str>,
}

impl Condition {
    pub(crate) fn arg_error(name: &str, expected_type: &str, arg: &Value) -> Self {
        Self::arg_type_error(name, expected_type, arg.as_typename(), arg)
    }

    pub(crate) fn arg_type_error(
        name: &str,
        expected_type: &str,
        actual_type: impl Display,
        arg: &Value,
    ) -> Self {
        Self {
            kind: ConditionKind::Env,
            irritants: Some(zlist![arg.clone()]),
            msg: format!(
                "invalid type for arg `{name}` - expected: {expected_type}, got: {actual_type}",
            )
            .into(),
        }
    }

    pub(crate) fn arity_error(name: &str, expected: &Arity, actual: usize) -> Self {
        let expected = if actual > MAX_ARITY as usize {
            format!("args exceed max arity: {MAX_ARITY}")
        } else {
            format!(
                "expected{}: {}",
                if expected.is_empty() { "" } else { " at least" },
                expected.start
            )
        };
        Self {
            kind: ConditionKind::Env,
            irritants: None,
            msg: format!("{name} arity mismatch - {expected}, got: {actual}",).into(),
        }
    }

    pub(crate) fn bind_error(name: &str) -> Self {
        Self {
            kind: ConditionKind::Env,
            irritants: None,
            msg: format!("unbound variable: {name}").into(),
        }
    }

    pub(crate) fn index_error(idx: &Value) -> Self {
        Self {
            kind: ConditionKind::Env,
            irritants: Some(zlist![idx.clone()]),
            msg: "index out of range".into(),
        }
    }

    pub(crate) fn proc_error(typename: &str) -> Self {
        Self {
            kind: ConditionKind::Env,
            irritants: None,
            msg: format!("expected procedure, got: {typename}").into(),
        }
    }

    pub(crate) fn system_error(msg: impl Into<Box<str>>) -> Self {
        Self {
            kind: ConditionKind::System,
            irritants: None,
            msg: msg.into(),
        }
    }

    pub(crate) fn value_error(msg: impl Into<Box<str>>, val: &Value) -> Self {
        Self {
            kind: ConditionKind::Env,
            irritants: Some(zlist![val.clone()]),
            msg: msg.into(),
        }
    }

    pub(crate) fn is_file_err(&self) -> bool {
        matches!(self.kind, ConditionKind::File)
    }

    pub(crate) fn is_read_err(&self) -> bool {
        matches!(self.kind, ConditionKind::Read)
    }

    pub(crate) fn message(&self) -> &str {
        &self.msg
    }

    pub(crate) fn irritants(&self) -> Option<&Value> {
        self.irritants.as_ref()
    }
}

impl Display for Condition {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "#<{} \"{}\"", self.kind, self.msg)?;
        match &self.irritants {
            None => (),
            Some(v) => write!(f, " {v}")?,
        }
        f.write_char('>')
    }
}

#[derive(Debug)]
enum ConditionKind {
    Env,
    #[allow(dead_code, reason = "not yet implemented")]
    File,
    #[allow(dead_code, reason = "not yet implemented")]
    General,
    #[allow(dead_code, reason = "not yet implemented")]
    Read,
    System,
}

impl Display for ConditionKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Env => f.write_str("env-error"),
            Self::File => f.write_str("file-error"),
            Self::General => f.write_str("exception"),
            Self::Read => f.write_str("read-error"),
            Self::System => f.write_str("sys-error"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn display_empty_condition() {
        let c = Condition {
            kind: ConditionKind::General,
            msg: "foo".into(),
            irritants: None,
        };

        assert_eq!(c.to_string(), "#<exception \"foo\">");
    }

    #[test]
    fn display_list_irritants() {
        let c = Condition {
            kind: ConditionKind::General,
            msg: "foo".into(),
            irritants: Some(zlist![Value::symbol("a"), Value::real(5)]),
        };

        assert_eq!(c.to_string(), "#<exception \"foo\" (a 5)>");
    }

    #[test]
    fn display_value_irritant() {
        let c = Condition {
            kind: ConditionKind::General,
            msg: "foo".into(),
            irritants: Some(Value::Boolean(true)),
        };

        assert_eq!(c.to_string(), "#<exception \"foo\" #t>");
    }

    #[test]
    fn display_env_condition() {
        let c = Condition {
            kind: ConditionKind::Env,
            msg: "foo".into(),
            irritants: None,
        };

        assert_eq!(c.to_string(), "#<env-error \"foo\">");
    }

    #[test]
    fn display_file_condition() {
        let c = Condition {
            kind: ConditionKind::File,
            msg: "foo".into(),
            irritants: None,
        };

        assert_eq!(c.to_string(), "#<file-error \"foo\">");
    }

    #[test]
    fn display_read_condition() {
        let c = Condition {
            kind: ConditionKind::Read,
            msg: "foo".into(),
            irritants: None,
        };

        assert_eq!(c.to_string(), "#<read-error \"foo\">");
    }

    #[test]
    fn display_system_condition() {
        let c = Condition {
            kind: ConditionKind::System,
            msg: "foo".into(),
            irritants: None,
        };

        assert_eq!(c.to_string(), "#<sys-error \"foo\">");
    }
}
