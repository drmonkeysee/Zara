use crate::{args, args::Args, repl::Repl};
use rustyline::error::ReadlineError;
use std::result;

pub(crate) type Result = result::Result<(), CmdError>;

pub(crate) enum Cmd {
    Help(String),
    Repl(Args),
    Stdin(String),
    Version,
}

impl Cmd {
    pub(crate) fn execute(self) -> Result {
        match self {
            Self::Help(me) => {
                args::usage(me);
                Ok(())
            }
            Self::Repl(opts) => repl(opts),
            Self::Stdin(input) => run_stdin(input),
            Self::Version => {
                args::version();
                Ok(())
            }
        }
    }
}

impl From<Args> for Cmd {
    fn from(value: Args) -> Self {
        // NOTE: enforce command precedence here
        if value.help {
            Self::Help(value.me)
        } else if value.ver {
            Self::Version
        } else if let Some(stdin) = value.stdin {
            Self::Stdin(stdin)
        } else {
            Self::Repl(value)
        }
    }
}

#[derive(Debug)]
pub(crate) enum CmdError {
    Repl(ReadlineError),
}

impl From<ReadlineError> for CmdError {
    fn from(value: ReadlineError) -> Self {
        Self::Repl(value)
    }
}

fn repl(args: Args) -> Result {
    let mut r = Repl::new(args)?;
    r.run()?;
    Ok(())
}

fn run_stdin(input: String) -> Result {
    todo!()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn default_args() {
        let args: Args = Default::default();

        let result = args.into();

        assert!(matches!(
            result,
            Cmd::Repl(Args {
                help: false,
                me: _,
                stdin: None,
                tokens: false,
                ver: false,
            })
        ));
    }

    #[test]
    fn help() {
        let program = "foo/me";
        let args = Args {
            help: true,
            me: program.to_owned(),
            ..Default::default()
        };

        let result = args.into();

        assert!(matches!(
            result,
            Cmd::Help(me) if me == program
        ));
    }

    #[test]
    fn stdin() {
        let input = "stdin input";
        let args = Args {
            stdin: Some(input.to_owned()),
            ..Default::default()
        };

        let result = args.into();

        assert!(matches!(
            result,
            Cmd::Stdin(s) if s == input
        ));
    }

    #[test]
    fn version() {
        let args = Args {
            ver: true,
            ..Default::default()
        };

        let result = args.into();

        assert!(matches!(result, Cmd::Version));
    }
}
