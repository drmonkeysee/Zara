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

#[derive(Debug)]
pub(crate) enum Cmd {
    File(Opts, PathBuf),
    Help(String),
    Prg(Opts, String),
    Repl(Opts),
    Stdin(Opts),
    Version,
}

impl Cmd {
    pub(crate) fn execute(self) -> Result {
        Ok(match self {
            Self::File(opts, prg) => {
                run::file(opts, prg)?;
            }
            Self::Help(ref me) => {
                args::usage(me);
            }
            Self::Prg(opts, prg) => {
                run::prg(opts, prg)?;
            }
            Self::Repl(opts) => {
                let mut r = Repl::new(opts)?;
                r.run()?;
            }
            Self::Stdin(opts) => {
                run::stdin(opts)?;
            }
            Self::Version => {
                args::version();
            }
        })
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
            if value.stdin {
                if let Some(prg) = value.prg {
                    Self::Prg(opts, prg)
                } else {
                    Self::Stdin(opts)
                }
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
    fn prg() {
        let input = "stdin input";
        let args = Args {
            prg: Some(input.to_owned()),
            stdin: true,
            ..Default::default()
        };

        let result = args.into();

        assert!(matches!(
            result,
            Cmd::Prg(
                Opts {
                    ast_output: false,
                    token_output: false,
                },
                prg,
            ) if prg == input
        ));
    }

    #[test]
    fn stdin() {
        let args = Args {
            stdin: true,
            ..Default::default()
        };

        let result = args.into();

        assert!(matches!(
            result,
            Cmd::Stdin(Opts {
                ast_output: false,
                token_output: false,
            })
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
