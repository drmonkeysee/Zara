// TODO: can any of these be &str?
#[derive(Clone, Debug)]
pub(crate) struct TextContext {
    pub(crate) filename: Option<String>,
    pub(crate) library: String,
    pub(crate) line: String,
    pub(crate) lineno: u64,
}

impl TextContext {
    pub(crate) fn for_repl(line: String) -> Self {
        Self {
            filename: None,
            library: String::from("<repl>"),
            line,
            lineno: 1,
        }
    }

    pub(crate) fn nextline(self, line: String) -> Self {
        Self {
            filename: self.filename,
            library: self.library,
            line,
            lineno: self.lineno + 1,
        }
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn todo() {
        todo!();
    }
}
