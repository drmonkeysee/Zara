mod cmd;
mod repl;

use self::cmd::Result;
use std::env;

fn main() -> Result {
    cmd::parse(env::args()).execute()
}
