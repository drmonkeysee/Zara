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
            line,
            lineno: self.lineno + 1,
            ..self
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn create_for_repl() {
        let tc = TextContext::for_repl(String::from("line of code"));

        assert!(tc.filename.is_none());
        assert_eq!(tc.library, "<repl>");
        assert_eq!(tc.line, "line of code");
        assert_eq!(tc.lineno, 1);
    }

    #[test]
    fn nextline() {
        let tc = TextContext {
            filename: Some(String::from("lib/mylib")),
            library: String::from("mylib"),
            line: String::from("line of code"),
            lineno: 1,
        }
        .nextline(String::from("next line of code"));

        assert!(matches!(tc.filename, Some(name) if name == "lib/mylib"));
        assert_eq!(tc.library, "mylib");
        assert_eq!(tc.line, "next line of code");
        assert_eq!(tc.lineno, 2);
    }
}
