mod args;
mod cli;
mod cmd;
mod repl;
mod run;

use self::{
    cli::{Error, Result},
    cmd::Cmd,
};
use std::{
    env,
    process::{ExitCode, Termination},
};

fn main() -> Exit {
    let cmd: Cmd = args::parse(env::args()).into();
    cmd.execute().into()
}

fn fail(err: Error) -> ExitCode {
    eprintln!("{err}");
    ExitCode::FAILURE
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
