use std::fmt::{self, Display, Formatter};

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) enum Port {
    Stderr,
    Stdin,
    Stdout,
}

impl Port {
    pub(crate) fn is_input(&self) -> bool {
        matches!(self, Self::Stdin)
    }

    pub(crate) fn is_output(&self) -> bool {
        matches!(self, Self::Stderr | Self::Stdout)
    }

    pub(crate) fn is_textual(&self) -> bool {
        true
    }

    pub(crate) fn is_binary(&self) -> bool {
        false
    }

    pub(crate) fn has_prop(&self, prop: &PortProp) -> bool {
        prop.matches(self)
    }

    pub(crate) fn is_open(&self) -> bool {
        true
    }
}

impl Display for Port {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Stderr => f.write_str("stderr"),
            Self::Stdin => f.write_str("stdin"),
            Self::Stdout => f.write_str("stdout"),
        }
    }
}

pub(crate) enum PortProp {
    Binary,
    Input,
    Output,
    Textual,
}

impl PortProp {
    fn matches(&self, p: &Port) -> bool {
        match self {
            Self::Binary => p.is_binary(),
            Self::Input => p.is_input(),
            Self::Output => p.is_output(),
            Self::Textual => p.is_textual(),
        }
    }
}

impl Display for PortProp {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Binary => f.write_str("binary"),
            Self::Input => f.write_str("input"),
            Self::Output => f.write_str("output"),
            Self::Textual => f.write_str("textual"),
        }?;
        f.write_str(" port")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn stderr_display() {
        let p = Port::Stderr;

        assert_eq!(p.to_string(), "stderr");
    }

    #[test]
    fn stdin_display() {
        let p = Port::Stdin;

        assert_eq!(p.to_string(), "stdin");
    }

    #[test]
    fn stdout_display() {
        let p = Port::Stdout;

        assert_eq!(p.to_string(), "stdout");
    }
}
