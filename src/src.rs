use crate::txt::{LineNumber, TextContext, TextError, TextLine, TextResult, TextSource};
use std::{
    fs::File,
    io::{BufRead, BufReader, Error, ErrorKind, IsTerminal, Result},
    path::Path,
    rc::Rc,
};

pub struct StringSource {
    ctx: Rc<TextContext>,
    lineno: LineNumber,
    lines: Option<<Vec<String> as IntoIterator>::IntoIter>,
}

impl StringSource {
    pub fn empty(name: impl Into<String>) -> Self {
        Self {
            ctx: TextContext::named(name).into(),
            lineno: 0,
            lines: None,
        }
    }

    pub fn new(src: impl Into<String>, name: impl Into<String>) -> Self {
        let mut me = Self::empty(name);
        me.set(src);
        me
    }

    pub fn set(&mut self, src: impl Into<String>) {
        self.lineno = 0;
        self.lines = Some(
            src.into()
                .lines()
                .map(String::from)
                .collect::<Vec<_>>()
                .into_iter(),
        );
    }

    pub fn clear(&mut self) {
        self.lines = None;
    }
}

impl Iterator for StringSource {
    type Item = TextResult;

    fn next(&mut self) -> Option<Self::Item> {
        self.lineno += 1;
        Some(Ok(TextLine {
            ctx: self.context(),
            line: self.lines.as_mut()?.next()?,
            lineno: self.lineno(),
        }))
    }
}

impl TextSource for StringSource {
    fn context(&self) -> Rc<TextContext> {
        Rc::clone(&self.ctx)
    }

    fn lineno(&self) -> LineNumber {
        self.lineno
    }
}

// NOTE: adapter type to bridge types like BufReader<T> and Stdin which
// *almost* implement the same interface but not quite.
pub trait LineInputAdapter {
    fn is_tty(&self) -> bool;
    fn read_line(&mut self, buf: &mut String) -> Result<usize>;
}

pub struct LineInputSource<T> {
    adapter: T,
    ctx: Rc<TextContext>,
    lineno: LineNumber,
}

impl<T: LineInputAdapter> LineInputSource<T> {
    pub fn new(adapter: T, name: impl Into<String>) -> Self {
        Self {
            adapter,
            ctx: TextContext::named(name).into(),
            lineno: 0,
        }
    }
}

impl<T: LineInputAdapter> Iterator for LineInputSource<T> {
    type Item = TextResult;

    fn next(&mut self) -> Option<Self::Item> {
        self.lineno += 1;
        let mut buf = String::new();
        self.adapter.read_line(&mut buf).map_or_else(
            |err| Some(Err(TextError::new(self.context(), self.lineno(), err))),
            |n| {
                if n == 0 || (n == 1 && self.adapter.is_tty()) {
                    None
                } else {
                    // NOTE: read_line guarantees a trailing \n, safe to pop
                    buf.pop();
                    Some(Ok(TextLine {
                        ctx: self.context(),
                        line: buf,
                        lineno: self.lineno(),
                    }))
                }
            },
        )
    }
}

impl<T: LineInputAdapter> TextSource for LineInputSource<T> {
    fn context(&self) -> Rc<TextContext> {
        Rc::clone(&self.ctx)
    }

    fn lineno(&self) -> LineNumber {
        self.lineno
    }
}

pub struct FileAdapter(BufReader<File>);

impl LineInputAdapter for FileAdapter {
    fn is_tty(&self) -> bool {
        self.0.get_ref().is_terminal()
    }

    fn read_line(&mut self, buf: &mut String) -> Result<usize> {
        self.0.read_line(buf)
    }
}

pub type FileSource = LineInputSource<FileAdapter>;

impl FileSource {
    pub fn file(path: impl AsRef<Path>) -> Result<Self> {
        let name = path
            .as_ref()
            .file_stem()
            .ok_or(Error::new(
                ErrorKind::InvalidInput,
                "unable to extract file name from path",
            ))?
            .to_str()
            .ok_or(Error::new(
                ErrorKind::InvalidInput,
                "unable to convert file name to string",
            ))?
            .to_owned();
        let p = Some(path.as_ref().to_path_buf());
        let f = File::open(path)?;

        Ok(Self {
            adapter: FileAdapter(BufReader::new(f)),
            ctx: TextContext { name, path: p }.into(),
            lineno: 0,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::testutil::some_or_fail;

    mod stringsrc {
        use super::*;

        #[test]
        fn empty() {
            let target = StringSource::empty("test");

            assert!(matches!(
                target.ctx.as_ref(),
                TextContext {
                    name,
                    path: None
                } if name == "test"
            ));
            assert_eq!(target.lineno(), 0);
        }

        #[test]
        fn create_from_str() {
            let src = "line of source code";

            let target = StringSource::new(src, "test");

            assert!(matches!(
                target.ctx.as_ref(),
                TextContext {
                    name,
                    path: None
                } if name == "test"
            ));
            assert_eq!(target.lineno(), 0);
        }

        #[test]
        fn context() {
            let target = StringSource::new("foo", "test");

            let ctx = target.context();

            assert!(Rc::ptr_eq(&ctx, &target.ctx));
        }

        #[test]
        fn iterate_empty() {
            let mut target = StringSource::empty("test");

            let line = target.next();

            assert!(line.is_none());
        }

        #[test]
        fn iterate_one_line() {
            let src = "line of source code";
            let mut target = StringSource::new(src, "test");

            let line = some_or_fail!(target.next());

            assert!(matches!(
                line,
                Ok(TextLine {
                    ctx,
                    line,
                    lineno: 1,
                }) if Rc::ptr_eq(&ctx, &target.ctx) && line == "line of source code"
            ));

            let line = target.next();

            assert!(line.is_none());
        }

        #[test]
        fn iterate_one_line_with_whitespace() {
            let src = "line of source code  \t  ";
            let mut target = StringSource::new(src, "test");

            let line = some_or_fail!(target.next());

            assert!(matches!(
                line,
                Ok(TextLine {
                    ctx,
                    line,
                    lineno: 1,
                }) if Rc::ptr_eq(&ctx, &target.ctx) && line == "line of source code  \t  "
            ));

            let line = target.next();

            assert!(line.is_none());
        }

        #[test]
        fn iterate_one_line_with_newline() {
            let src = "line of source code\n";
            let mut target = StringSource::new(src, "test");

            let line = some_or_fail!(target.next());

            assert!(matches!(
                line,
                Ok(TextLine {
                    ctx,
                    line,
                    lineno: 1,
                }) if Rc::ptr_eq(&ctx, &target.ctx) && line == "line of source code"
            ));

            let line = target.next();

            assert!(line.is_none());
        }

        #[test]
        fn iterate_multi_lines() {
            let src = "line1\nline2\nline3\n";
            let mut target = StringSource::new(src, "test");

            let line = some_or_fail!(target.next());

            assert!(matches!(
                line,
                Ok(TextLine {
                    ctx,
                    line,
                    lineno: 1,
                }) if Rc::ptr_eq(&ctx, &target.ctx) && line == "line1"
            ));

            let line = some_or_fail!(target.next());

            assert!(matches!(
                line,
                Ok(TextLine {
                    ctx,
                    line,
                    lineno: 2,
                }) if Rc::ptr_eq(&ctx, &target.ctx) && line == "line2"
            ));

            let line = some_or_fail!(target.next());

            assert!(matches!(
                line,
                Ok(TextLine {
                    ctx,
                    line,
                    lineno: 3,
                }) if Rc::ptr_eq(&ctx, &target.ctx) && line == "line3"
            ));

            let line = target.next();

            assert!(line.is_none());
        }

        #[test]
        fn iterate_multi_lines_windows_style() {
            let src = "line1\r\nline2\r\nline3\r\n";
            let mut target = StringSource::new(src, "test");

            let line = some_or_fail!(target.next());

            assert!(matches!(
                line,
                Ok(TextLine {
                    ctx,
                    line,
                    lineno: 1,
                }) if Rc::ptr_eq(&ctx, &target.ctx) && line == "line1"
            ));

            let line = some_or_fail!(target.next());

            assert!(matches!(
                line,
                Ok(TextLine {
                    ctx,
                    line,
                    lineno: 2,
                }) if Rc::ptr_eq(&ctx, &target.ctx) && line == "line2"
            ));

            let line = some_or_fail!(target.next());

            assert!(matches!(
                line,
                Ok(TextLine {
                    ctx,
                    line,
                    lineno: 3,
                }) if Rc::ptr_eq(&ctx, &target.ctx) && line == "line3"
            ));

            let line = target.next();

            assert!(line.is_none());
        }

        #[test]
        fn iterate_multi_lines_with_whitespace() {
            let src = "line1  \n  line2\t\n\tline3\n";
            let mut target = StringSource::new(src, "test");

            let line = some_or_fail!(target.next());

            assert!(matches!(
                line,
                Ok(TextLine {
                    ctx,
                    line,
                    lineno: 1,
                }) if Rc::ptr_eq(&ctx, &target.ctx) && line == "line1  "
            ));

            let line = some_or_fail!(target.next());

            assert!(matches!(
                line,
                Ok(TextLine {
                    ctx,
                    line,
                    lineno: 2,
                }) if Rc::ptr_eq(&ctx, &target.ctx) && line == "  line2\t"
            ));

            let line = some_or_fail!(target.next());

            assert!(matches!(
                line,
                Ok(TextLine {
                    ctx,
                    line,
                    lineno: 3,
                }) if Rc::ptr_eq(&ctx, &target.ctx) && line == "\tline3"
            ));

            let line = target.next();

            assert!(line.is_none());
        }

        // NOTE: preserve blank lines to keep lineno accurate
        #[test]
        fn iterate_includes_blank_lines() {
            let src = "line1\n   \nline3\n\nline5\n\t\n";
            let mut target = StringSource::new(src, "test");

            let line = some_or_fail!(target.next());

            assert!(matches!(
                line,
                Ok(TextLine {
                    ctx,
                    line,
                    lineno: 1,
                }) if Rc::ptr_eq(&ctx, &target.ctx) && line == "line1"
            ));

            let line = some_or_fail!(target.next());

            assert!(matches!(
                line,
                Ok(TextLine {
                    ctx,
                    line,
                    lineno: 2,
                }) if Rc::ptr_eq(&ctx, &target.ctx) && line == "   "
            ));

            let line = some_or_fail!(target.next());

            assert!(matches!(
                line,
                Ok(TextLine {
                    ctx,
                    line,
                    lineno: 3,
                }) if Rc::ptr_eq(&ctx, &target.ctx) && line == "line3"
            ));

            let line = some_or_fail!(target.next());

            assert!(matches!(
                line,
                Ok(TextLine {
                    ctx,
                    line,
                    lineno: 4,
                }) if Rc::ptr_eq(&ctx, &target.ctx) && line == ""
            ));

            let line = some_or_fail!(target.next());

            assert!(matches!(
                line,
                Ok(TextLine {
                    ctx,
                    line,
                    lineno: 5,
                }) if Rc::ptr_eq(&ctx, &target.ctx) && line == "line5"
            ));

            let line = some_or_fail!(target.next());

            assert!(matches!(
                line,
                Ok(TextLine {
                    ctx,
                    line,
                    lineno: 6,
                }) if Rc::ptr_eq(&ctx, &target.ctx) && line == "\t"
            ));

            let line = target.next();

            assert!(line.is_none());
        }

        #[test]
        fn reset_lines() {
            let src = "line1\nline2\nline3\n";
            let mut target = StringSource::new(src, "test");

            let line = some_or_fail!(target.next());

            assert!(matches!(
                line,
                Ok(TextLine {
                    ctx,
                    line,
                    lineno: 1,
                }) if Rc::ptr_eq(&ctx, &target.ctx) && line == "line1"
            ));

            let line = some_or_fail!(target.next());

            assert!(matches!(
                line,
                Ok(TextLine {
                    ctx,
                    line,
                    lineno: 2,
                }) if Rc::ptr_eq(&ctx, &target.ctx) && line == "line2"
            ));

            target.set("a different line");

            let line = some_or_fail!(target.next());

            assert!(matches!(
                line,
                Ok(TextLine {
                    ctx,
                    line,
                    lineno: 1,
                }) if Rc::ptr_eq(&ctx, &target.ctx) && line == "a different line"
            ));

            let line = target.next();

            assert!(line.is_none());
        }

        #[test]
        fn clear_lines() {
            let src = "line1\nline2\nline3\n";
            let mut target = StringSource::new(src, "test");

            let line = some_or_fail!(target.next());

            assert!(matches!(
                line,
                Ok(TextLine {
                    ctx,
                    line,
                    lineno: 1,
                }) if Rc::ptr_eq(&ctx, &target.ctx) && line == "line1"
            ));

            let line = some_or_fail!(target.next());

            assert!(matches!(
                line,
                Ok(TextLine {
                    ctx,
                    line,
                    lineno: 2,
                }) if Rc::ptr_eq(&ctx, &target.ctx) && line == "line2"
            ));

            target.clear();

            let line = target.next();

            assert!(line.is_none());
        }
    }

    mod linesrc {
        use super::*;
        use crate::testutil::{err_or_fail, ok_or_fail};
        use std::{error::Error, io};

        struct MockLineAdapter {
            is_tty: bool,
            lines: Vec<&'static str>,
            return_err: bool,
        }

        impl LineInputAdapter for MockLineAdapter {
            fn is_tty(&self) -> bool {
                self.is_tty
            }

            fn read_line(&mut self, buf: &mut String) -> Result<usize> {
                if self.return_err {
                    Err(io::Error::new(io::ErrorKind::Other, "oh no!"))
                } else {
                    self.lines.pop().map_or(Ok(0), |line| {
                        buf.push_str(line);
                        Ok(line.len())
                    })
                }
            }
        }

        #[test]
        fn create() {
            let target = LineInputSource::new(
                MockLineAdapter {
                    is_tty: false,
                    lines: Vec::new(),
                    return_err: false,
                },
                "test",
            );

            assert!(matches!(
                target.ctx.as_ref(),
                TextContext {
                    name,
                    path: None
                } if name == "test"
            ));
            assert_eq!(target.lineno(), 0);
        }

        #[test]
        fn context() {
            let target = LineInputSource::new(
                MockLineAdapter {
                    is_tty: false,
                    lines: Vec::new(),
                    return_err: false,
                },
                "test",
            );

            let ctx = target.context();

            assert!(Rc::ptr_eq(&ctx, &target.ctx));
        }

        #[test]
        fn lines_remove_newline() {
            let mut target = LineInputSource::new(
                MockLineAdapter {
                    is_tty: false,
                    lines: vec!["foo\n", "bar\n"],
                    return_err: false,
                },
                "test",
            );

            let r = some_or_fail!(target.next());
            let line = ok_or_fail!(r);
            assert_eq!(line.line, "bar");
            assert_eq!(line.lineno, 1);

            let r = some_or_fail!(target.next());
            let line = ok_or_fail!(r);
            assert_eq!(line.line, "foo");
            assert_eq!(line.lineno, 2);

            let line = target.next();
            assert!(line.is_none());
        }

        #[test]
        fn non_tty_returns_empty_line() {
            let mut target = LineInputSource::new(
                MockLineAdapter {
                    is_tty: false,
                    lines: vec!["\n", "bar\n"],
                    return_err: false,
                },
                "test",
            );

            let r = some_or_fail!(target.next());
            let line = ok_or_fail!(r);
            assert_eq!(line.line, "bar");
            assert_eq!(line.lineno, 1);

            let r = some_or_fail!(target.next());
            let line = ok_or_fail!(r);
            assert_eq!(line.line, "");
            assert_eq!(line.lineno, 2);

            let line = target.next();
            assert!(line.is_none());
        }

        #[test]
        fn tty_ends_on_empty_line() {
            let mut target = LineInputSource::new(
                MockLineAdapter {
                    is_tty: true,
                    lines: vec!["\n", "bar\n"],
                    return_err: false,
                },
                "test",
            );

            let r = some_or_fail!(target.next());
            let line = ok_or_fail!(r);
            assert_eq!(line.line, "bar");
            assert_eq!(line.lineno, 1);

            let line = target.next();
            assert!(line.is_none());
        }

        #[test]
        fn failed_line() {
            let mut target = LineInputSource::new(
                MockLineAdapter {
                    is_tty: false,
                    lines: Vec::new(),
                    return_err: true,
                },
                "test",
            );

            let r = some_or_fail!(target.next());
            let line = err_or_fail!(r);
            assert_eq!(line.source().unwrap().to_string(), "oh no!");
            assert_eq!(line.line_number(), 1);
        }
    }
}
