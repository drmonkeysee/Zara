mod args;
mod cmd;
mod repl;
mod run;

use self::cmd::{Cmd, Result};
use std::env;

fn main() -> Result {
    let cmd: Cmd = args::parse(env::args()).into();
    cmd.execute()
}
