mod args;

use self::args::{Opts, Parsed};
use rustyline::{DefaultEditor, Result};
use std::env;
use zara::{Evaluation, Expression, InterpreterError};

const INPUT: &str = "λ:> ";
const CONT: &str = "... ";

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

struct Repl {
    editor: DefaultEditor,
    options: Opts,
    prompt: &'static str,
}

impl Repl {
    fn new(opts: Opts) -> Result<Self> {
        Ok(Self {
            editor: DefaultEditor::new()?,
            options: opts,
            prompt: INPUT,
        })
    }

    fn run(&mut self) -> Result<()> {
        println!("{:?}", self.options);
        loop {
            match self.editor.readline(self.prompt) {
                Ok(line) => match zara::runline(line) {
                    Ok(eval) => match eval {
                        Evaluation::Expression(expr) => self.print_expr(expr),
                        Evaluation::Continuation => self.prompt = CONT,
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

    fn print_expr(&mut self, expr: Expression) {
        if expr.has_repr() {
            println!("==> {}", expr)
        }
        self.prompt = INPUT;
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
