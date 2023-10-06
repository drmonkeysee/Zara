use super::{args::Args, cli::Result, repl::Repl};
use std::{
    io::{self, IsTerminal, Stdin},
    path::Path,
};
use zara::{
    src::{FileSource, LineInputAdapter, LineInputSource, StringSource},
    txt::TextSource,
    Interpreter,
};

pub(crate) fn file(opts: Opts, prg: impl AsRef<Path>) -> Result {
    run(opts, FileSource::file(prg)?)
}

pub(crate) fn prg(opts: Opts, prg: String) -> Result {
    run(opts, StringSource::new(prg, "<stdin prg>"))
}

pub(crate) fn repl(opts: Opts) -> Result {
    let mut r = Repl::new(opts)?;
    Ok(r.run()?)
}

pub(crate) fn stdin(opts: Opts) -> Result {
    run(opts, StdinSrcFactory::new().product)
}

#[derive(Debug)]
pub(crate) struct Opts {
    pub(crate) ast_output: bool,
    pub(crate) token_output: bool,
}

impl Opts {
    pub(crate) fn with_args(args: &Args) -> Self {
        Self {
            ast_output: args.ast,
            token_output: args.tokens,
        }
    }
}

fn run(opts: Opts, mut src: impl TextSource) -> Result {
    let mut runtime = Interpreter::new(opts.token_output, opts.ast_output);
    let result = runtime.run(&mut src);
    print_result(&result);
    result?;
    Ok(())
}

fn print_result(result: &zara::Result) {
    match result {
        Ok(eval) => print!("{}", eval.extended_display()),
        Err(err) => eprint!("{}", err.extended_display()),
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

type StdinSource = LineInputSource<StdinAdapter>;

struct StdinSrcFactory {
    product: StdinSource,
}

impl StdinSrcFactory {
    fn new() -> Self {
        Self {
            product: StdinSource::new(StdinAdapter(io::stdin()), "<stdin>"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn create_opts_from_args() {
        let args = Args {
            ast: true,
            tokens: true,
            ..Default::default()
        };

        let target = Opts::with_args(&args);

        assert!(matches!(
            target,
            Opts {
                ast_output: true,
                token_output: true,
            }
        ));
    }

    #[test]
    fn is_stdin_tty() {
        let target = StdinAdapter(io::stdin());

        assert!(target.is_tty());
    }
}
