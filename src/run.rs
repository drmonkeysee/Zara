use crate::{args::Args, cli::Result, repl::Repl};
use std::{
    io::{self, IsTerminal, Stdin},
    path::Path,
    rc::Rc,
};
use zara::{
    src::FileSource,
    txt::{LineNumber, TextContext, TextLine, TextSource},
    Interpreter,
};

pub(crate) fn file(opts: Opts, prg: impl AsRef<Path>) -> Result {
    run(opts, FileSource::file(prg)?)
}

pub(crate) fn prg(opts: Opts, prg: String) -> Result {
    run(opts, PrgSource::new(prg))
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

struct PrgSource {
    ctx: Rc<TextContext>,
    lines: <Vec<String> as IntoIterator>::IntoIter,
    lineno: LineNumber,
}

impl PrgSource {
    fn new(src: String) -> Self {
        Self {
            ctx: TextContext::named("<stdin prg>").into(),
            lines: src
                .lines()
                .map(String::from)
                .collect::<Vec<_>>()
                .into_iter(),
            lineno: 1,
        }
    }
}

impl Iterator for PrgSource {
    type Item = TextLine;

    fn next(&mut self) -> Option<Self::Item> {
        let lineno = self.lineno;
        self.lineno += 1;
        Some(TextLine {
            ctx: self.context(),
            line: self.lines.next()?,
            lineno,
        })
    }
}

impl TextSource for PrgSource {
    fn context(&self) -> Rc<TextContext> {
        self.ctx.clone()
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

    mod prg {
        use super::*;

        #[test]
        fn create_from_str() {
            let src = "line of source code";

            let target = PrgSource::new(src.to_owned());

            assert!(matches!(
                target.ctx.as_ref(),
                TextContext {
                    name,
                    path: None
                } if name == "<stdin prg>"
            ));
            assert_eq!(target.lineno, 1);
        }

        #[test]
        fn context() {
            let target = PrgSource::new("foo".to_owned());

            let ctx = target.context();

            assert!(Rc::ptr_eq(&ctx, &target.ctx));
        }

        #[test]
        fn iterate_one_line() {
            let src = "line of source code";
            let mut target = PrgSource::new(src.to_owned());

            let line = target.next();

            assert!(line.is_some());
            assert!(matches!(
                line.unwrap(),
                TextLine {
                    ctx,
                    line,
                    lineno: 1,
                } if Rc::ptr_eq(&ctx, &target.ctx) && line == "line of source code"
            ));

            let line = target.next();

            assert!(line.is_none());
        }

        #[test]
        fn iterate_one_line_with_whitespace() {
            let src = "line of source code  \t  ";
            let mut target = PrgSource::new(src.to_owned());

            let line = target.next();

            assert!(line.is_some());
            assert!(matches!(
                line.unwrap(),
                TextLine {
                    ctx,
                    line,
                    lineno: 1,
                } if Rc::ptr_eq(&ctx, &target.ctx) && line == "line of source code  \t  "
            ));

            let line = target.next();

            assert!(line.is_none());
        }

        #[test]
        fn iterate_one_line_with_newline() {
            let src = "line of source code\n";
            let mut target = PrgSource::new(src.to_owned());

            let line = target.next();

            assert!(line.is_some());
            assert!(matches!(
                line.unwrap(),
                TextLine {
                    ctx,
                    line,
                    lineno: 1,
                } if Rc::ptr_eq(&ctx, &target.ctx) && line == "line of source code"
            ));

            let line = target.next();

            assert!(line.is_none());
        }

        #[test]
        fn iterate_multi_lines() {
            let src = "line1\nline2\nline3\n";
            let mut target = PrgSource::new(src.to_owned());

            let line = target.next();

            assert!(line.is_some());
            assert!(matches!(
                line.unwrap(),
                TextLine {
                    ctx,
                    line,
                    lineno: 1,
                } if Rc::ptr_eq(&ctx, &target.ctx) && line == "line1"
            ));

            let line = target.next();

            assert!(line.is_some());
            assert!(matches!(
                line.unwrap(),
                TextLine {
                    ctx,
                    line,
                    lineno: 2,
                } if Rc::ptr_eq(&ctx, &target.ctx) && line == "line2"
            ));

            let line = target.next();

            assert!(line.is_some());
            assert!(matches!(
                line.unwrap(),
                TextLine {
                    ctx,
                    line,
                    lineno: 3,
                } if Rc::ptr_eq(&ctx, &target.ctx) && line == "line3"
            ));

            let line = target.next();

            assert!(line.is_none());
        }

        #[test]
        fn iterate_multi_lines_windows_style() {
            let src = "line1\r\nline2\r\nline3\r\n";
            let mut target = PrgSource::new(src.to_owned());

            let line = target.next();

            assert!(line.is_some());
            assert!(matches!(
                line.unwrap(),
                TextLine {
                    ctx,
                    line,
                    lineno: 1,
                } if Rc::ptr_eq(&ctx, &target.ctx) && line == "line1"
            ));

            let line = target.next();

            assert!(line.is_some());
            assert!(matches!(
                line.unwrap(),
                TextLine {
                    ctx,
                    line,
                    lineno: 2,
                } if Rc::ptr_eq(&ctx, &target.ctx) && line == "line2"
            ));

            let line = target.next();

            assert!(line.is_some());
            assert!(matches!(
                line.unwrap(),
                TextLine {
                    ctx,
                    line,
                    lineno: 3,
                } if Rc::ptr_eq(&ctx, &target.ctx) && line == "line3"
            ));

            let line = target.next();

            assert!(line.is_none());
        }

        #[test]
        fn iterate_multi_lines_with_whitespace() {
            let src = "line1  \n  line2\t\n\tline3\n";
            let mut target = PrgSource::new(src.to_owned());

            let line = target.next();

            assert!(line.is_some());
            assert!(matches!(
                line.unwrap(),
                TextLine {
                    ctx,
                    line,
                    lineno: 1,
                } if Rc::ptr_eq(&ctx, &target.ctx) && line == "line1  "
            ));

            let line = target.next();

            assert!(line.is_some());
            assert!(matches!(
                line.unwrap(),
                TextLine {
                    ctx,
                    line,
                    lineno: 2,
                } if Rc::ptr_eq(&ctx, &target.ctx) && line == "  line2\t"
            ));

            let line = target.next();

            assert!(line.is_some());
            assert!(matches!(
                line.unwrap(),
                TextLine {
                    ctx,
                    line,
                    lineno: 3,
                } if Rc::ptr_eq(&ctx, &target.ctx) && line == "\tline3"
            ));

            let line = target.next();

            assert!(line.is_none());
        }

        // NOTE: preserve blank lines to keep lineno accurate
        #[test]
        fn iterate_includes_blank_lines() {
            let src = "line1\n   \nline3\n\nline5\n\t\n";
            let mut target = PrgSource::new(src.to_owned());

            let line = target.next();

            assert!(line.is_some());
            assert!(matches!(
                line.unwrap(),
                TextLine {
                    ctx,
                    line,
                    lineno: 1,
                } if Rc::ptr_eq(&ctx, &target.ctx) && line == "line1"
            ));

            let line = target.next();

            assert!(line.is_some());
            assert!(matches!(
                line.unwrap(),
                TextLine {
                    ctx,
                    line,
                    lineno: 2,
                } if Rc::ptr_eq(&ctx, &target.ctx) && line == "   "
            ));

            let line = target.next();

            assert!(line.is_some());
            assert!(matches!(
                line.unwrap(),
                TextLine {
                    ctx,
                    line,
                    lineno: 3,
                } if Rc::ptr_eq(&ctx, &target.ctx) && line == "line3"
            ));

            let line = target.next();

            assert!(line.is_some());
            assert!(matches!(
                line.unwrap(),
                TextLine {
                    ctx,
                    line,
                    lineno: 4,
                } if Rc::ptr_eq(&ctx, &target.ctx) && line == ""
            ));

            let line = target.next();

            assert!(line.is_some());
            assert!(matches!(
                line.unwrap(),
                TextLine {
                    ctx,
                    line,
                    lineno: 5,
                } if Rc::ptr_eq(&ctx, &target.ctx) && line == "line5"
            ));

            let line = target.next();

            assert!(line.is_some());
            assert!(matches!(
                line.unwrap(),
                TextLine {
                    ctx,
                    line,
                    lineno: 6,
                } if Rc::ptr_eq(&ctx, &target.ctx) && line == "\t"
            ));

            let line = target.next();

            assert!(line.is_none());
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
