use rustyline::{DefaultEditor, Result};
use std::rc::Rc;
use zara::{
    Error, Evaluation, Interpreter, RunMode, Val,
    txt::{LineNumber, TextContext, TextLine, TextResult, TextSource},
};

const INPUT: &str = "λ:> ";
const CONT: &str = "... ";

pub(crate) struct Repl {
    editor: DefaultEditor,
    prompt: &'static str,
    running: bool, // TODO: will be needed for repl quit
    runtime: Interpreter,
    src: ReplSource,
}

impl Repl {
    pub(crate) fn new(mode: RunMode) -> Result<Self> {
        Ok(Self {
            editor: DefaultEditor::new()?,
            prompt: INPUT,
            running: true,
            runtime: Interpreter::new(mode),
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
        match self.runtime.run(&mut self.src) {
            Ok(Evaluation::Continuation) => self.prep_continuation(),
            Ok(Evaluation::Value(v)) => self.print_val(&v),
            Err(err) => self.print_err(&err),
        }
    }

    fn print_val(&mut self, v: &Option<Val>) {
        if let Some(v) = v {
            println!("==> {}", v.as_datum());
        }
        self.reset();
    }

    fn print_err(&mut self, err: &Error) {
        eprint!("{}", err.display_message());
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

impl Iterator for ReplSource {
    type Item = TextResult;

    fn next(&mut self) -> Option<Self::Item> {
        Some(Ok(TextLine {
            ctx: self.context(),
            line: self.line.take()?,
            lineno: self.lineno(),
        }))
    }
}

// TODO: can this be a macro
impl TextSource for ReplSource {
    fn context(&self) -> Rc<TextContext> {
        Rc::clone(&self.ctx)
    }

    fn lineno(&self) -> LineNumber {
        self.lineno
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn source_create() {
        let target = ReplSource::new();

        assert!(matches!(
            target.ctx.as_ref(),
            TextContext {
                name,
                path: None
            } if name == "<repl>"
        ));
        assert!(target.line.is_none());
        assert_eq!(target.lineno(), 1);
    }

    #[test]
    fn source_set_line() {
        let mut target = ReplSource::new();

        target.set_line("foo".to_owned());

        assert!(target.line.is_some());
        assert_eq!(target.line.unwrap(), "foo");
    }

    #[test]
    fn source_advance() {
        let mut target = ReplSource::new();

        target.advance();

        assert_eq!(target.lineno(), 2);
    }

    #[test]
    fn source_reset() {
        let mut target = ReplSource::new();

        target.advance();
        target.advance();

        assert_eq!(target.lineno(), 3);

        target.reset();

        assert_eq!(target.lineno(), 1);
    }

    #[test]
    fn source_context() {
        let target = ReplSource::new();

        let ctx = target.context();

        assert!(Rc::ptr_eq(&ctx, &target.ctx));
    }

    #[test]
    fn source_iterator() {
        let mut target = ReplSource::new();

        let line = target.next();

        assert!(line.is_none());

        target.set_line("foo".to_owned());

        let line = target.next();

        assert!(line.is_some());
        assert!(matches!(
            line.unwrap(),
            Ok(TextLine {
                ctx,
                line,
                lineno: 1,
            }) if Rc::ptr_eq(&ctx, &target.ctx) && line == "foo"
        ));

        let line = target.next();

        assert!(line.is_none());
    }
}
