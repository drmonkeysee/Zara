mod args;
mod repl;

use self::args::Result;
use std::env;

fn main() -> Result {
    let args = env::args();
    let cmd = args::parse(args);
    cmd.execute()
}
