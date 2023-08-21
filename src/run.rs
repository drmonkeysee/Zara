use crate::args::Args;
use std::rc::Rc;
use zara::{txt::TextContext, Result};

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

pub(crate) struct FileSource;

pub(crate) struct StdinSource {
    ctx: Rc<TextContext>,
    stdin: String,
}

impl StdinSource {
    pub(crate) fn new(stdin: String) -> Self {
        Self {
            ctx: TextContext::named("<stdin>").into(),
            stdin,
        }
    }
}

pub(crate) fn file(opts: Opts, prg: FileSource) -> Result {
    todo!();
}

pub(crate) fn stdin(opts: Opts, src: StdinSource) -> Result {
    todo!();
}
