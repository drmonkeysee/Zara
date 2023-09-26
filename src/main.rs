mod args;
mod cmd;
mod repl;
mod run;

use self::{
    cmd::Cmd,
    run::{Error, Result},
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
        self.0.map_or_else(fail, |u| u.report())
    }
}

impl From<Result> for Exit {
    fn from(value: Result) -> Self {
        Self(value)
    }
}
