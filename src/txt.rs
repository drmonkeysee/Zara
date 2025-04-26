use std::{
    error::Error,
    fmt::{self, Display, Formatter},
    path::PathBuf,
    rc::Rc,
};

pub type LineNumber = u64;
pub type TextResult = Result<TextLine, TextError>;

pub trait TextSource: Iterator<Item = TextResult> {
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

#[derive(Clone, Debug)]
pub struct TextLine {
    pub ctx: Rc<TextContext>,
    pub line: String,
    pub lineno: LineNumber,
}

impl TextLine {
    #[must_use]
    pub fn display_header(&self) -> TextLineHeader {
        TextLineHeader(self)
    }
}

pub struct TextLineHeader<'a>(&'a TextLine);

impl Display for TextLineHeader<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        format_header(&self.0.ctx, self.0.lineno, f)?;
        writeln!(f, "\t{}", self.0.line)
    }
}

#[derive(Debug)]
pub struct TextError {
    pub ctx: Rc<TextContext>,
    pub lineno: LineNumber,
    err: Box<dyn Error>,
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

    #[must_use]
    pub fn display_header(&self) -> TextErrorHeader {
        TextErrorHeader(self)
    }
}

impl Display for TextError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self.source() {
            Some(err) => write!(f, "readline failure - {err}"),
            None => f.write_str("unknown readline error"),
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
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        format_header(&self.0.ctx, self.0.lineno, f)?;
        writeln!(f, "\t{}", self.0)
    }
}

fn format_header(ctx: &TextContext, lineno: LineNumber, f: &mut Formatter) -> fmt::Result {
    write!(f, "{}:{}", ctx.name, lineno)?;
    if let Some(p) = &ctx.path {
        write!(f, " ({})", p.display())?;
    }
    writeln!(f)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::testutil::some_or_fail;

    #[derive(Clone, Copy, Debug)]
    struct MockError(i32);

    impl Display for MockError {
        fn fmt(&self, f: &mut Formatter) -> fmt::Result {
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

        let src = some_or_fail!(err.source());

        assert_eq!(src.downcast_ref::<MockError>().unwrap().0, inner.0);
    }

    #[test]
    fn error_display() {
        let inner = MockError(20);

        let err = TextError::new(TextContext::named("foo"), 1, inner);

        assert_eq!(err.to_string(), "readline failure - Mock error: 20");
    }
}
