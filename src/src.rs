use crate::txt::{LineNumber, TextContext, TextLine, TextSource};
use std::{
    fs::File,
    io::{BufRead, BufReader, IsTerminal, Result},
    path::Path,
    rc::Rc,
};

pub struct StringSource {
    ctx: Rc<TextContext>,
    lines: <Vec<String> as IntoIterator>::IntoIter,
    lineno: LineNumber,
}

impl StringSource {
    pub fn new(src: String, name: impl Into<String>) -> Self {
        Self {
            ctx: TextContext::named(name).into(),
            lines: src
                .lines()
                .map(String::from)
                .collect::<Vec<_>>()
                .into_iter(),
            lineno: 1,
        }
    }
}

impl Iterator for StringSource {
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

impl TextSource for StringSource {
    fn context(&self) -> Rc<TextContext> {
        self.ctx.clone()
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
            lineno: 1,
        }
    }
}

impl<T: LineInputAdapter> Iterator for LineInputSource<T> {
    type Item = TextLine;

    fn next(&mut self) -> Option<Self::Item> {
        let lineno = self.lineno;
        self.lineno += 1;
        let mut buf = String::new();
        self.adapter.read_line(&mut buf).map_or_else(
            |err| {
                // TODO: should this yield an Option<Result<TextLine>> instead
                // of printing to stderr?
                eprintln!(
                    "{}:{lineno}\n\tunexpected line read error: {err}",
                    self.ctx.name
                );
                None
            },
            |n| {
                if n == 0 || (n == 1 && self.adapter.is_tty()) {
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

impl<T: LineInputAdapter> TextSource for LineInputSource<T> {
    fn context(&self) -> Rc<TextContext> {
        self.ctx.clone()
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
            .to_str()
            .unwrap_or("#<invalid-file-path>")
            .to_owned();
        let f = File::open(path)?;
        Self::init(f, name)
    }

    fn init(f: File, n: String) -> Result<Self> {
        Ok(Self::new(FileAdapter(BufReader::new(f)), n))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    mod stringsrc {
        use super::*;

        #[test]
        fn create_from_str() {
            let src = "line of source code";

            let target = StringSource::new(src.to_owned(), "test");

            assert!(matches!(
                target.ctx.as_ref(),
                TextContext {
                    name,
                    path: None
                } if name == "test"
            ));
            assert_eq!(target.lineno, 1);
        }

        #[test]
        fn context() {
            let target = StringSource::new("foo".to_owned(), "test");

            let ctx = target.context();

            assert!(Rc::ptr_eq(&ctx, &target.ctx));
        }

        #[test]
        fn iterate_one_line() {
            let src = "line of source code";
            let mut target = StringSource::new(src.to_owned(), "test");

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
            let mut target = StringSource::new(src.to_owned(), "test");

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
            let mut target = StringSource::new(src.to_owned(), "test");

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
            let mut target = StringSource::new(src.to_owned(), "test");

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
            let mut target = StringSource::new(src.to_owned(), "test");

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
            let mut target = StringSource::new(src.to_owned(), "test");

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
            let mut target = StringSource::new(src.to_owned(), "test");

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

    mod linesrc {
        use super::*;

        struct MockLineAdapter {
            is_tty: bool,
            lines: Vec<&'static str>,
        }

        impl LineInputAdapter for MockLineAdapter {
            fn is_tty(&self) -> bool {
                self.is_tty
            }

            fn read_line(&mut self, buf: &mut String) -> Result<usize> {
                self.lines.pop().map_or(Ok(0), |line| {
                    buf.push_str(line);
                    Ok(line.len())
                })
            }
        }

        #[test]
        fn create() {
            let target = LineInputSource::new(
                MockLineAdapter {
                    is_tty: false,
                    lines: Vec::new(),
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
            assert_eq!(target.lineno, 1);
        }

        #[test]
        fn context() {
            let target = LineInputSource::new(
                MockLineAdapter {
                    is_tty: false,
                    lines: Vec::new(),
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
                },
                "test",
            );

            let line = target.next();
            assert!(line.is_some());
            let line = line.unwrap();
            assert_eq!(line.line, "bar");
            assert_eq!(line.lineno, 1);

            let line = target.next();
            assert!(line.is_some());
            let line = line.unwrap();
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
                },
                "test",
            );

            let line = target.next();
            assert!(line.is_some());
            let line = line.unwrap();
            assert_eq!(line.line, "bar");
            assert_eq!(line.lineno, 1);

            let line = target.next();
            assert!(line.is_some());
            let line = line.unwrap();
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
                },
                "test",
            );

            let line = target.next();
            assert!(line.is_some());
            let line = line.unwrap();
            assert_eq!(line.line, "bar");
            assert_eq!(line.lineno, 1);

            let line = target.next();
            assert!(line.is_none());
        }
    }
}
