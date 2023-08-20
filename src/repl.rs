use crate::args::Args;
use rustyline::{DefaultEditor, Result};
use std::rc::Rc;
use zara::{
    txt::{LineNumber, TextContext, TextLine, TextSource},
    Error, Evaluation, Expression, Interpreter,
};

const INPUT: &str = "λ:> ";
const CONT: &str = "... ";

pub(crate) struct Repl {
    editor: DefaultEditor,
    interpreter: Interpreter,
    prompt: &'static str,
    running: bool, // TODO: will be needed for repl quit
    src: ReplSource,
}

impl Repl {
    pub(crate) fn new(options: Args) -> Result<Self> {
        Ok(Self {
            editor: DefaultEditor::new()?,
            interpreter: Interpreter::new(options.tokens, false),
            prompt: INPUT,
            running: true,
            src: ReplSource::new(),
        })
    }

    pub(crate) fn run(&mut self) -> Result<()> {
        while self.running {
            self.src.set_line(self.editor.readline(self.prompt)?);
            self.runline();
        }
        Ok(())
    }

    fn runline(&mut self) {
        match self.interpreter.run(&mut self.src) {
            Ok(eval) => match eval {
                Evaluation::Continuation => self.prep_continuation(),
                Evaluation::Expression(expr) => self.print_expr(expr),
            },
            Err(err) => self.print_err(err),
        }
    }

    fn print_expr(&mut self, expr: Expression) {
        if expr.has_repr() {
            println!("==> {expr}")
        }
        self.reset();
    }

    fn print_err(&mut self, err: Error) {
        eprint!("{}", err.verbose_display());
        self.reset();
    }

    fn prep_continuation(&mut self) {
        self.prompt = CONT;
        self.src.advance();
    }

    fn reset(&mut self) {
        self.prompt = INPUT;
        self.src.reset();
    }
}

struct ReplSource {
    ctx: Rc<TextContext>,
    line: Option<String>,
    lineno: LineNumber,
}

impl ReplSource {
    fn new() -> Self {
        Self {
            ctx: TextContext::named("<repl>").into(),
            line: None,
            lineno: 1,
        }
    }

    fn set_line(&mut self, line: String) {
        self.line = Some(line);
    }

    fn advance(&mut self) {
        self.lineno += 1;
    }

    fn reset(&mut self) {
        self.lineno = 1;
    }
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
