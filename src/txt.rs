use std::{
    error::Error,
    fmt::{self, Display, Formatter},
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
    pub path: Option<String>,
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
