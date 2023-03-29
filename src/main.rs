mod args;

use crate::args::Opts;
use rustyline::{DefaultEditor, Result};
use std::env;

fn main() -> Result<()> {
    if let Some(opts) = args::parse(env::args()) {
        repl(opts)
    } else {
        Ok(())
    }
}

fn repl(options: Opts) -> Result<()> {
    println!("{:?}", options);
    let mut ed = DefaultEditor::new()?;
    for readline in ed.iter("λ:> ") {
        let result = zara::eval(readline?.as_str());
        println!("{result}");
    }
    eprintln!("Saw EOF");
    Ok(())
}
