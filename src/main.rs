mod args;
mod cli;
mod repl;
mod run;

use self::{
    args::{Args, Input},
    cli::{Error, Result},
};
use std::{
    env,
    process::{ExitCode, Termination},
};

fn main() -> Exit {
    let args = args::parse(env::args());
    execute(args).into()
}

// NOTE: newtype to have more control over exit output rather than the
// annoying default behavior of Result printing the Debug representation.
struct Exit(Result);

impl Termination for Exit {
    fn report(self) -> ExitCode {
        self.0.map_or_else(fail, Termination::report)
    }
}

impl From<Result> for Exit {
    fn from(value: Result) -> Self {
        Self(value)
    }
}

fn execute(args: Args) -> Result {
    eprintln!("Run Args: {:?}", args.runargs);
    if args.help {
        args::usage(&args.me);
    } else if args.ver {
        args::version();
    } else {
        match args.input {
            Input::File(p) => run::file(args.mode, p)?,
            Input::Prg(p) => run::prg(args.mode, p)?,
            Input::Repl => run::repl(args.mode)?,
            Input::Stdin => run::stdin(args.mode)?,
        }
    }
    Ok(())
}

fn fail(err: Error) -> ExitCode {
    eprintln!("{err}");
    ExitCode::FAILURE
}
