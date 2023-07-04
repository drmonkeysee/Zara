mod args;

use self::args::{Opts, Parsed};
use rustyline::{DefaultEditor, Result};
use std::{env, println};

fn main() -> Result<()> {
    match args::parse(env::args()) {
        Parsed::Command(cmd) => {
            cmd.execute();
            Ok(())
        }
        Parsed::Options(opts) => repl(opts),
    }
}

fn repl(options: Opts) -> Result<()> {
    println!("{:?}", options);
    let mut ed = DefaultEditor::new()?;
    for readline in ed.iter("λ:> ") {
        zara::runline(readline?)
            .map_or_else(|err| println!("{:?}", err), |expr| println!("{}", expr));
    }
    eprintln!("Saw EOF");
    Ok(())
}
