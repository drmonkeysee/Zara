mod args;

use crate::args::{Opts, Parsed};
use rustyline::{DefaultEditor, Result};
use std::env;

fn main() -> Result<()> {
    match args::parse(env::args()) {
        Parsed::Command(cmd) => Ok(cmd.execute()),
        Parsed::Options(opts) => repl(&opts),
    }
}

fn repl(options: &Opts) -> Result<()> {
    println!("{:?}", options);
    let mut ed = DefaultEditor::new()?;
    for readline in ed.iter("λ:> ") {
        let result = zara::eval(readline?.as_str());
        println!("{result}");
    }
    eprintln!("Saw EOF");
    Ok(())
}
