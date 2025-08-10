use rustyline::{Config, Editor, Result, error::ReadlineError, history::MemHistory};
use std::process::ExitCode;
use zara::{Error, Evaluation, Exception, Interpreter, RunMode, Signal, Value, src::StringSource};

const INPUT: &str = "λ:> ";
const CONT: &str = "... ";

pub(crate) struct Repl {
    editor: ZaraEditor,
    exit: ExitCode,
    prompt: &'static str,
    running: bool,
    runtime: Interpreter,
    src: StringSource,
}

impl Repl {
    pub(crate) fn new(mode: RunMode, args: impl IntoIterator<Item = String>) -> Result<Self> {
        Ok(Self {
            editor: create_editor()?,
            exit: ExitCode::SUCCESS,
            prompt: INPUT,
            running: true,
            runtime: Interpreter::new(mode, args),
            src: StringSource::empty("<repl>"),
        })
    }

    pub(crate) fn run(&mut self) -> Result<ExitCode> {
        while self.running {
            self.readline()?;
            self.runline();
        }
        Ok(self.exit)
    }

    fn readline(&mut self) -> Result<()> {
        match self.editor.readline(self.prompt) {
            Err(ReadlineError::Eof) => {
                self.running = false;
            }
            Err(err @ ReadlineError::Interrupted) => {
                self.reset();
                eprintln!("{err}");
            }
            Err(err) => Err(err)?,
            Ok(line) => {
                self.editor.add_history_entry(&line)?;
                self.update(line);
            }
        }
        Ok(())
    }

    fn runline(&mut self) {
        match self.runtime.run(&mut self.src) {
            Ok(Evaluation::Continuation) => self.continuation(),
            Ok(Evaluation::Ex(Exception::Exit(code))) => {
                self.exit = code;
                self.running = false;
            }
            Ok(Evaluation::Ex(Exception::Signal(ex))) => self.print_signal(&ex),
            Ok(Evaluation::Val(v)) => self.print_value(&v),
            Err(err) => self.print_err(&err),
        }
    }

    fn print_signal(&mut self, sig: &Signal) {
        println!("unhandled-exception => {sig}");
        self.ready();
    }

    fn print_value(&mut self, v: &Value) {
        if !v.is_unspecified() {
            println!("==> {v}");
        }
        self.ready();
    }

    fn print_err(&mut self, err: &Error) {
        eprint!("{}", err.display_message());
        self.ready();
    }

    fn continuation(&mut self) {
        self.prompt = CONT;
    }

    fn ready(&mut self) {
        self.prompt = INPUT;
    }

    fn reset(&mut self) {
        self.runtime.clear();
        self.ready();
    }

    fn update(&mut self, line: String) {
        if self.prompt == CONT {
            self.src.cont(line);
        } else {
            self.src.set(line);
        }
    }
}

type ZaraEditor = Editor<(), MemHistory>;

fn create_editor() -> Result<ZaraEditor> {
    ZaraEditor::with_history(Config::default(), MemHistory::default())
}
