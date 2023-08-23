use crate::args::Args;
use std::{path::Path, rc::Rc, str::Split};
use zara::{
    txt::{TextContext, TextLine, TextSource},
    Interpreter, Result,
};

pub(crate) struct Opts {
    pub(crate) ast_output: bool,
    pub(crate) token_output: bool,
}

impl Opts {
    pub(crate) fn with_args(args: &Args) -> Self {
        Self {
            ast_output: false,
            token_output: args.tokens,
        }
    }
}

pub(crate) fn file(opts: Opts, prg: &Path) -> Result {
    todo!();
}

pub(crate) fn stdin(opts: Opts, src: &str) -> Result {
    let mut src = StdinSource::new(src);
    let runtime = Interpreter::new(opts.token_output, opts.ast_output);
    runtime.run(&mut src)
}

struct FileSource;

struct StdinSource<'a> {
    ctx: Rc<TextContext>,
    lines: Split<'a, char>,
}

impl<'a> StdinSource<'a> {
    pub(crate) fn new(src: &'a str) -> Self {
        Self {
            ctx: TextContext::named("<stdin>").into(),
            lines: src.split('\n'),
        }
    }
}

impl TextSource for StdinSource<'_> {
    fn context(&self) -> Rc<TextContext> {
        todo!()
    }
}

impl Iterator for StdinSource<'_> {
    type Item = TextLine;

    fn next(&mut self) -> Option<Self::Item> {
        todo!()
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
                tokens: true,
                ..Default::default()
            };

            let target = Opts::with_args(&args);

            assert!(matches!(
                target,
                Opts {
                    ast_output: false,
                    token_output: true,
                }
            ));
        }
    }

    mod stdin {
        use super::*;

        #[test]
        fn create_from_str() {
            let src = "line of source code";

            let target = StdinSource::new(src);

            assert!(matches!(
                target.ctx.as_ref(),
                TextContext {
                    name,
                    path: None
                } if name == "<stdin>"
            ));
        }
    }
}
