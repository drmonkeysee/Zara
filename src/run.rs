use crate::{args::Args, cli::Result, repl::Repl};
use std::{
    io::{self, IsTerminal, Stdin},
    path::Path,
    rc::Rc,
};
use zara::{
    src::{FileSource, StringSource},
    txt::{LineNumber, TextContext, TextLine, TextSource},
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
    run(opts, StdinSource::new())
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

struct StdinSource {
    ctx: Rc<TextContext>,
    stdin: Stdin,
    lineno: LineNumber,
}

impl StdinSource {
    fn new() -> Self {
        Self {
            ctx: TextContext::named("<stdin>").into(),
            stdin: io::stdin(),
            lineno: 1,
        }
    }
}

impl Iterator for StdinSource {
    type Item = TextLine;

    fn next(&mut self) -> Option<Self::Item> {
        let lineno = self.lineno;
        self.lineno += 1;
        let mut buf = String::new();
        self.stdin.read_line(&mut buf).map_or_else(
            |err| {
                eprintln!(
                    "{}:{lineno}\n\tunexpected stdin read error: {err}",
                    self.ctx.name
                );
                None
            },
            |n| {
                if n == 0 || (n == 1 && self.stdin.is_terminal()) {
                    None
                } else {
                    // NOTE: read_line guarantees a trailing \n, safe to pop
                    buf.pop();
                    Some(TextLine {
                        ctx: self.context(),
                        line: buf,
                        lineno,
                    })
                }
            },
        )
    }
}

impl TextSource for StdinSource {
    fn context(&self) -> Rc<TextContext> {
        self.ctx.clone()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    mod opts {
        use super::*;

        #[test]
        fn create_from_args() {
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
    }

    mod stdin {
        use super::*;

        #[test]
        fn create() {
            let target = StdinSource::new();

            assert!(matches!(
                target.ctx.as_ref(),
                TextContext {
                    name,
                    path: None
                } if name == "<stdin>"
            ));
            assert_eq!(target.lineno, 1);
        }

        #[test]
        fn context() {
            let target = StdinSource::new();

            let ctx = target.context();

            assert!(Rc::ptr_eq(&ctx, &target.ctx));
        }
    }
}
