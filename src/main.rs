mod args;
mod cmd;
mod repl;
mod run;

use self::cmd::{Cmd, Result};
use std::{
    env,
    process::{ExitCode, Termination},
};

fn main() -> Exit {
    let cmd: Cmd = args::parse(env::args()).into();
    cmd.execute().into()
}

// NOTE: newtype to have more control over exit output rather than the
// annoying default behavior of Result printing the Debug representation.
struct Exit(Result);

impl From<Result> for Exit {
    fn from(value: Result) -> Self {
        Self(value)
    }
}

impl Termination for Exit {
    fn report(self) -> ExitCode {
        self.0.map_or_else(
            |err| {
                eprintln!("{}", err);
                ExitCode::FAILURE
            },
            |u| u.report(),
        )
    }
}
