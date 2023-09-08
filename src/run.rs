use crate::args::Args;
use std::{path::Path, rc::Rc};
use zara::{
    txt::{LineNumber, TextContext, TextLine, TextSource},
    Interpreter, Result,
};

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

pub(crate) fn file(opts: Opts, prg: impl AsRef<Path>) -> Result {
    todo!();
}

pub(crate) fn prg(opts: Opts, prg: String) -> Result {
    let mut src = PrgSource::new(prg);
    let runtime = Interpreter::new(opts.token_output, opts.ast_output);
    let result = runtime.run(&mut src);
    print_terminal_result(&result);
    result
}

pub(crate) fn stdin(opts: Opts) -> Result {
    todo!();
    /*let mut src = StdinSource::new(src);
    let runtime = Interpreter::new(opts.token_output, opts.ast_output);
    let result = runtime.run(&mut src);
    print_terminal_result(&result);
    result*/
}

struct FileSource;

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
                .map(str::trim)
                .filter_map(|s| (!s.is_empty()).then(|| s.to_owned()))
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

fn print_terminal_result(result: &Result) {
    match result {
        Ok(eval) => print!("{}", eval.extended_display()),
        Err(err) => eprint!("{}", err.extended_display()),
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
        fn iterate_multi_lines_trims_whitespace() {
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
    }
}
