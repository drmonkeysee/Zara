use std::{
    error::Error,
    fmt::{self, Display, Formatter},
    path::PathBuf,
    rc::Rc,
};

pub type LineNumber = u64;
pub type TextResult = Result<TextLine, TextError>;

pub trait TextSource: Iterator<Item = TextResult> {
    fn can_continue(&self) -> bool {
        false
    }

    fn context(&self) -> Rc<TextContext>;
    fn lineno(&self) -> LineNumber;
}

#[derive(Debug)]
pub struct TextContext {
    pub name: String,
    pub path: Option<PathBuf>,
}

impl TextContext {
    pub fn named(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            path: None,
        }
    }
}

#[derive(Debug)]
pub struct TextLine {
    pub ctx: Rc<TextContext>,
    pub line: String,
    pub lineno: LineNumber,
}

impl TextLine {
    pub fn display_header(&self) -> TextLineHeader {
        TextLineHeader(self)
    }
}

pub struct TextLineHeader<'a>(&'a TextLine);

impl Display for TextLineHeader<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        format_header(&self.0.ctx, self.0.lineno, f)?;
        writeln!(f, "\t{}", self.0.line)
    }
}

#[derive(Debug)]
pub struct TextError {
    pub ctx: Rc<TextContext>,
    err: Box<dyn Error>,
    pub lineno: LineNumber,
}

impl TextError {
    pub fn new(
        ctx: impl Into<Rc<TextContext>>,
        lineno: LineNumber,
        err: impl Into<Box<dyn Error>>,
    ) -> Self {
        Self {
            ctx: ctx.into(),
            err: err.into(),
            lineno,
        }
    }

    pub fn display_header(&self) -> TextErrorHeader {
        TextErrorHeader(self)
    }
}

impl Display for TextError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.source() {
            Some(err) => write!(f, "text source readline failure: {err}"),
            None => f.write_str("unknown text source readline error"),
        }
    }
}

impl Error for TextError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        Some(self.err.as_ref())
    }
}

pub struct TextErrorHeader<'a>(&'a TextError);

impl Display for TextErrorHeader<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        format_header(&self.0.ctx, self.0.lineno, f)?;
        f.write_str("\tunable to read text line\n")
    }
}

fn format_header(ctx: &TextContext, lineno: LineNumber, f: &mut Formatter<'_>) -> fmt::Result {
    write!(f, "{}:{}", ctx.name, lineno)?;
    if let Some(p) = &ctx.path {
        write!(f, " ({})", p.display())?;
    }
    writeln!(f)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Clone, Copy, Debug)]
    struct MockError(i32);

    impl Display for MockError {
        fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
            write!(f, "Mock error: {}", self.0)
        }
    }

    impl Error for MockError {}

    #[test]
    fn context_with_name() {
        let ctx = TextContext::named("foo");

        assert!(matches!(
            ctx,
            TextContext {
                name,
                path: None
            } if name == "foo"
        ));
    }

    #[test]
    fn inner_error() {
        let inner = MockError(20);
        let err = TextError::new(TextContext::named("foo"), 1, inner);

        let src = err.source();

        assert!(src.is_some());
        assert_eq!(src.unwrap().downcast_ref::<MockError>().unwrap().0, inner.0);
    }

    #[test]
    fn error_display() {
        let inner = MockError(20);

        let err = TextError::new(TextContext::named("foo"), 1, inner);

        assert_eq!(
            err.to_string(),
            "text source readline failure: Mock error: 20"
        );
    }
}
