use crate::{
    args::{self, Args, Input},
    cli::Result,
    run::{self, Opts},
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
        eprintln!("Run Args: {:?}", value.runargs);
        // NOTE: enforce command precedence here
        if value.help {
            Self::Help(value.me)
        } else if value.ver {
            Self::Version
        } else {
            let opts = Opts::with_args(&value);
            match value.input {
                Input::File(p) => Self::File(opts, p),
                Input::Prg(p) => Self::Prg(opts, p),
                Input::Repl => Self::Repl(opts),
                Input::Stdin => Self::Stdin(opts),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::Path;

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
    fn file() {
        let input = "my/file.scm";
        let args = Args {
            input: Input::File(Path::new(input).to_path_buf()),
            ..Default::default()
        };

        let result = args.into();

        assert!(matches!(
            result,
            Cmd::File(
                Opts {
                    ast_output: false,
                    token_output: false,
                },
                pth,
            ) if pth.to_str().unwrap() == input
        ));
    }

    #[test]
    fn prg() {
        let input = "stdin input";
        let args = Args {
            input: Input::Prg(input.to_owned()),
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
            input: Input::Stdin,
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
