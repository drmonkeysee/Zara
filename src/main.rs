mod args;
mod cli;
mod repl;
mod run;

use self::{
    args::{Args, Cmd, Input},
    cli::Result,
};
use std::{
    env,
    process::{ExitCode, Termination},
};

fn main() -> Exit {
    let args = Args::parse(env::args());
    execute(args).into()
}

// NOTE: newtype to have more control over exit output rather than the
// annoying default behavior of Result printing the Debug representation.
struct Exit(Result);

impl Termination for Exit {
    fn report(self) -> ExitCode {
        self.0.map_or_else(Termination::report, Termination::report)
    }
}

impl From<Result> for Exit {
    fn from(value: Result) -> Self {
        Self(value)
    }
}

fn execute(args: Args) -> Result {
    eprintln!("Run Args: {:?}", args.runargs);
    match args.cmd {
        Cmd::Help => args::usage(&args.me),
        Cmd::Run => return exec_run(args),
        Cmd::Version => args::version(),
    }
    Ok(ExitCode::SUCCESS)
}

fn exec_run(args: Args) -> Result {
    let (input, mode, runargs) = args.decompose();
    match input {
        Input::File(p) => run::file(mode, p, runargs),
        Input::Prg(p) => run::prg(mode, p, runargs),
        Input::Repl => run::repl(mode, runargs),
        Input::Stdin => run::stdin(mode, runargs),
    }
}
