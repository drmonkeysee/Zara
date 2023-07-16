mod args;

use self::args::{Opts, Parsed};
use rustyline::{DefaultEditor, Result};
use std::env;
use zara::{Evaluation, Expression, InterpreterError};

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
    let mut cont = false;
    let mut ed = DefaultEditor::new()?;
    loop {
        let prompt = if cont {
            cont = false;
            "... "
        } else {
            "λ:> "
        };
        match ed.readline(prompt) {
            Ok(line) => match zara::runline(line) {
                Ok(eval) => match eval {
                    Evaluation::Expression(expr) => print_expr(expr),
                    Evaluation::Continuation => cont = true,
                },
                Err(err) => print_err(err),
            },
            Err(err) => {
                eprintln!("{:?}", err);
                break;
            }
        }
    }
    Ok(())
}

fn print_expr(expr: Expression) {
    if expr.has_repr() {
        println!("==> {}", expr)
    }
}

fn print_err(err: InterpreterError) {
    println!(
        "{}:{} ({})",
        "library", "line", "/full/path/to/lib/file.scm"
    );
    println!("\tthis is an invalid src line with multiple errors");
    println!("\t           ^^^^^^^               ^^^^^^^^");
    println!("{}:{:?}", "position1", err);
    println!("{}:{:?}", "position2", err);
}
