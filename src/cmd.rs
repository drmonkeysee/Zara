use crate::{
    args::{self, Args},
    run::{self, Opts, Result},
};

use std::path::PathBuf;

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
        match self {
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
                run::repl(opts)?;
            }
            Self::Stdin(opts) => {
                run::stdin(opts)?;
            }
            Self::Version => {
                args::version();
            }
        }
        Ok(())
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
