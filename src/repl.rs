use rustyline::{Config, Editor, Result, history::MemHistory};
use std::rc::Rc;
use zara::{
    Error, Evaluation, Interpreter, RunMode, Value,
    src::StringSource,
    txt::{LineNumber, TextContext, TextResult, TextSource},
};

const INPUT: &str = "λ:> ";
const CONT: &str = "... ";

pub(crate) struct Repl {
    editor: ZaraEditor,
    prompt: &'static str,
    running: bool, // TODO: will be needed for repl quit
    runtime: Interpreter,
    src: ReplSource,
}

impl Repl {
    pub(crate) fn new(mode: RunMode) -> Result<Self> {
        Ok(Self {
            editor: create_editor()?,
            prompt: INPUT,
            running: true,
            runtime: Interpreter::new(mode),
            src: ReplSource::new(),
        })
    }

    pub(crate) fn run(&mut self) -> Result<()> {
        while self.running {
            self.readline()?;
            self.runline();
        }
        Ok(())
    }

    fn readline(&mut self) -> Result<()> {
        let line = self.editor.readline(self.prompt)?;
        self.editor.add_history_entry(&line)?;
        self.src.set_line(line);
        Ok(())
    }

    fn runline(&mut self) {
        match self.runtime.run(&mut self.src) {
            Ok(Evaluation::Continuation) => self.continuation(),
            Ok(Evaluation::Val(v)) => self.print_value(v.as_ref()),
            Err(err) => self.print_err(&err),
        }
    }

    fn print_value(&mut self, v: Option<&Value>) {
        if let Some(v) = v {
            println!("==> {}", v.as_datum());
        }
        self.reset();
    }

    fn print_err(&mut self, err: &Error) {
        eprint!("{}", err.display_message());
        self.reset();
    }

    fn continuation(&mut self) {
        self.prompt = CONT;
    }

    fn reset(&mut self) {
        self.prompt = INPUT;
    }
}

type ZaraEditor = Editor<(), MemHistory>;

struct ReplSource(StringSource);

impl ReplSource {
    fn new() -> Self {
        Self(StringSource::empty("<repl>"))
    }

    // NOTE: line may actually be multiple line breaks if the input was
    // copy+pasted into the terminal.
    fn set_line(&mut self, line: String) {
        self.0.set(line);
    }
}

impl Iterator for ReplSource {
    type Item = TextResult;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
}

// TODO: can this be a macro
impl TextSource for ReplSource {
    fn context(&self) -> Rc<TextContext> {
        self.0.context()
    }

    fn lineno(&self) -> LineNumber {
        self.0.lineno()
    }
}

fn create_editor() -> Result<ZaraEditor> {
    ZaraEditor::with_history(Config::default(), MemHistory::default())
}
