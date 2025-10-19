use std::fmt::{self, Display, Formatter};

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) enum Port {
    Stderr,
    Stdin,
    Stdout,
}

impl Port {
    fn is_input(&self) -> bool {
        todo!();
    }

    fn is_output(&self) -> bool {
        todo!();
    }

    fn is_textual(&self) -> bool {
        todo!();
    }

    fn is_binary(&self) -> bool {
        todo!();
    }

    fn is_open(&self) -> bool {
        todo!();
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
