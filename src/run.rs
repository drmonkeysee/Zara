use crate::{cli, repl::Repl};
use std::{
    io::{self, IsTerminal, Stdin},
    path::Path,
    process::ExitCode,
};
use zara::{
    Evaluation, Exception, Interpreter, RunMode,
    src::{FileSource, LineInputAdapter, LineInputSource, StringSource},
    txt::TextSource,
};

pub(crate) fn file(
    mode: RunMode,
    prg: impl AsRef<Path>,
    args: impl IntoIterator<Item = String>,
) -> cli::Result {
    run(mode, &mut FileSource::file(prg)?, args)
}

pub(crate) fn prg(
    mode: RunMode,
    prg: impl Into<String>,
    args: impl IntoIterator<Item = String>,
) -> cli::Result {
    run(mode, &mut StringSource::new(prg, "<stdin prg>"), args)
}

pub(crate) fn repl(mode: RunMode, args: impl IntoIterator<Item = String>) -> cli::Result {
    let mut r = Repl::new(mode, args)?;
    Ok(r.run()?)
}

pub(crate) fn stdin(mode: RunMode, args: impl IntoIterator<Item = String>) -> cli::Result {
    run(mode, &mut stdin_source(), args)
}

fn run(
    mode: RunMode,
    src: &mut impl TextSource,
    args: impl IntoIterator<Item = String>,
) -> cli::Result {
    let mut runtime = Interpreter::new(mode, false, args);
    let mut result = runtime.run(src);
    if let Ok(Evaluation::Ex(Exception::Exit(code))) = result {
        return Ok(code);
    }
    if let Ok(Evaluation::Continuation) = result {
        result = runtime.unsupported_continuation().map_or(result, Err);
    }
    print_result(&result);
    result.map_or_else(|err| Err(err.into()), |_| Ok(ExitCode::SUCCESS))
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
