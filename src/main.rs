mod args;

use self::args::{Opts, Parsed};
use rustyline::{DefaultEditor, Result};
use std::env;
use zara::{Evaluation, Expression, Interpreter, InterpreterError};

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
    interpreter: Interpreter,
    options: Opts,
    prompt: &'static str,
    running: bool, // TODO: will be needed for repl quit
}

impl Repl {
    fn new(options: Opts) -> Result<Self> {
        Ok(Self {
            editor: DefaultEditor::new()?,
            interpreter: Interpreter::new(),
            options,
            prompt: INPUT,
            running: true,
        })
    }

    fn run(&mut self) -> Result<()> {
        println!("{:?}", self.options);
        while self.running {
            let line = self.editor.readline(self.prompt)?;
            self.runline(line);
        }
        Ok(())
    }

    fn runline(&mut self, line: String) {
        match self.interpreter.runline(line) {
            Ok(eval) => match eval {
                Evaluation::Continuation => self.prompt = CONT,
                Evaluation::Expression(expr) => self.print_expr(expr),
            },
            Err(err) => print_runerr(err),
        }
    }

    fn print_expr(&mut self, expr: Expression) {
        if expr.has_repr() {
            println!("==> {}", expr)
        }
        self.prompt = INPUT;
    }
}

fn print_runerr(err: InterpreterError) {
    println!(
        "{}:{} ({})",
        "library", "line", "/full/path/to/lib/file.scm"
    );
    println!("\tthis is an invalid src line with multiple errors");
    println!("\t           ^^^^^^^               ^^^^^^^^");
    println!("{}:{:?}", "position1", err);
    println!("{}:{:?}", "position2", err);
}
