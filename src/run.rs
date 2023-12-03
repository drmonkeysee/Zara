use crate::{cli::Result, repl::Repl};
use std::{
    io::{self, IsTerminal, Stdin},
    path::Path,
};
use zara::{
    src::{FileSource, LineInputAdapter, LineInputSource, StringSource},
    txt::TextSource,
    Interpreter, RunMode,
};

pub(crate) fn file(mode: RunMode, prg: impl AsRef<Path>) -> Result {
    run(mode, FileSource::file(prg)?)
}

pub(crate) fn prg(mode: RunMode, prg: String) -> Result {
    run(mode, StringSource::new(prg, "<stdin prg>"))
}

pub(crate) fn repl(mode: RunMode) -> Result {
    let mut r = Repl::new(mode)?;
    Ok(r.run()?)
}

pub(crate) fn stdin(mode: RunMode) -> Result {
    run(mode, stdin_source())
}

fn run(mode: RunMode, mut src: impl TextSource) -> Result {
    let mut runtime = Interpreter::new(mode);
    let result = runtime.run(&mut src);
    print_result(&result);
    result?;
    Ok(())
}

fn print_result(result: &zara::Result) {
    match result {
        Ok(eval) => print!("{}", eval.display_message()),
        Err(err) => eprint!("{}", err.display_message()),
    }
}

struct StdinAdapter(Stdin);

impl LineInputAdapter for StdinAdapter {
    fn is_tty(&self) -> bool {
        self.0.is_terminal()
    }

    fn read_line(&mut self, buf: &mut String) -> io::Result<usize> {
        self.0.read_line(buf)
    }
}

fn stdin_source() -> LineInputSource<StdinAdapter> {
    LineInputSource::new(StdinAdapter(io::stdin()), "<stdin>")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn is_stdin_tty() {
        let target = StdinAdapter(io::stdin());

        assert!(target.is_tty());
    }
}
