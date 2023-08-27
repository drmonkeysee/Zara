use crate::{args, args::Args, repl::Repl, run, run::Opts};
use rustyline::error::ReadlineError;
use std::{
    error::Error,
    fmt,
    fmt::{Display, Formatter},
    path::PathBuf,
    result,
};

pub(crate) type Result = result::Result<(), CmdError>;

pub(crate) enum Cmd {
    File(Opts, PathBuf),
    Help(String),
    Repl(Opts),
    Stdin(Opts, String),
    Version,
}

impl Cmd {
    pub(crate) fn execute(self) -> Result {
        match self {
            Self::File(opts, prg) => {
                run::file(opts, prg)?;
                Ok(())
            }
            Self::Help(ref me) => {
                args::usage(me);
                Ok(())
            }
            Self::Repl(opts) => {
                let mut r = Repl::new(opts)?;
                r.run()?;
                Ok(())
            }
            Self::Stdin(opts, src) => {
                run::stdin(opts, src)?;
                Ok(())
            }
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
        } else {
            let opts = Opts::with_args(&value);
            if let Some(stdin) = value.stdin {
                Self::Stdin(opts, stdin)
            } else {
                Self::Repl(opts)
            }
        }
    }
}

#[derive(Debug)]
pub(crate) enum CmdError {
    Repl(ReadlineError),
    Run(zara::Error),
}

impl Display for CmdError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Repl(err) => err.fmt(f),
            Self::Run(err) => err.fmt(f),
        }
    }
}

impl Error for CmdError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        Some(match self {
            Self::Repl(err) => err,
            Self::Run(err) => err,
        })
    }
}

impl From<ReadlineError> for CmdError {
    fn from(value: ReadlineError) -> Self {
        Self::Repl(value)
    }
}

impl From<zara::Error> for CmdError {
    fn from(value: zara::Error) -> Self {
        Self::Run(value)
    }
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
            Cmd::Repl(Opts {
                ast_output: false,
                token_output: false,
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
            Cmd::Stdin(
                Opts {
                    ast_output: false,
                    token_output: false,
                },
                src,
            ) if src == input
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
