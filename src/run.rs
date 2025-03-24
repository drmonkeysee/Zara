use crate::{cli::Result, repl::Repl};
use std::{
    io::{self, IsTerminal, Stdin},
    path::Path,
};
use zara::{
    Evaluation, Interpreter, RunMode,
    src::{FileSource, LineInputAdapter, LineInputSource, StringSource},
    txt::TextSource,
};

pub(crate) fn file(mode: RunMode, prg: impl AsRef<Path>) -> Result {
    run(mode, &mut FileSource::file(prg)?)
}

pub(crate) fn prg(mode: RunMode, prg: impl Into<String>) -> Result {
    run(mode, &mut StringSource::new(prg, "<stdin prg>"))
}

pub(crate) fn repl(mode: RunMode) -> Result {
    let mut r = Repl::new(mode)?;
    Ok(r.run()?)
}

pub(crate) fn stdin(mode: RunMode) -> Result {
    run(mode, &mut stdin_source())
}

fn run(mode: RunMode, src: &mut impl TextSource) -> Result {
    let mut runtime = Interpreter::new(mode);
    let mut result = runtime.run(src);
    if matches!(result, Ok(Evaluation::Continuation)) {
        result = runtime
            .unsupported_continuation()
            .map_or(result, |err| Err(err));
    }
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
