mod args;

use self::args::{Opts, Parsed};
use rustyline::{DefaultEditor, Result};
use std::{env, rc::Rc};
use zara::{
    txt::{LineNumber, TextContext, TextLine, TextSource},
    Error, Evaluation, Expression, Interpreter,
};

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
    ctx: Rc<TextContext>,
    editor: DefaultEditor,
    interpreter: Interpreter,
    lineno: LineNumber,
    options: Opts,
    prompt: &'static str,
    running: bool, // TODO: will be needed for repl quit
}

impl Repl {
    fn new(options: Opts) -> Result<Self> {
        Ok(Self {
            ctx: Rc::new(TextContext::named(String::from("<repl>"))),
            editor: DefaultEditor::new()?,
            interpreter: Interpreter::new(),
            lineno: 1,
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
        let mut src = ReplSource {
            ctx: self.ctx.clone(),
            line: Some(line),
            lineno: self.lineno,
        };
        match self.interpreter.run(&mut src) {
            Ok(eval) => match eval {
                Evaluation::Continuation => self.prep_continuation(),
                Evaluation::Expression(expr) => self.print_expr(expr),
            },
            Err(err) => self.print_err(err),
        }
    }

    fn print_expr(&mut self, expr: Expression) {
        if expr.has_repr() {
            println!("==> {}", expr)
        }
        self.reset();
    }

    fn print_err(&mut self, err: Error) {
        eprint!("{}", err.verbose_display());
        self.reset();
    }

    fn prep_continuation(&mut self) {
        self.prompt = CONT;
        self.lineno += 1;
    }

    fn reset(&mut self) {
        self.prompt = INPUT;
        self.lineno = 1;
    }
}

struct ReplSource {
    ctx: Rc<TextContext>,
    line: Option<String>,
    lineno: LineNumber,
}

// TODO: can this be a macro
impl TextSource for ReplSource {
    fn context(&self) -> Rc<TextContext> {
        self.ctx.clone()
    }
}

impl Iterator for ReplSource {
    type Item = TextLine;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(line) = self.line.take() {
            Some(TextLine {
                ctx: self.context(),
                line,
                lineno: self.lineno,
            })
        } else {
            None
        }
    }
}
