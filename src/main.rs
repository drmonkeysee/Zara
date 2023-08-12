mod args;
mod repl;

use self::{args::Parsed, repl::Repl};
use rustyline::Result;
use std::env;

fn main() -> Result<()> {
    match args::parse(env::args()) {
        Parsed::Command(cmd) => {
            cmd.execute();
            Ok(())
        }
        Parsed::Options(opts) => {
            let mut r = Repl::new(opts)?;
            r.run()
        }
    }
}
