#[cfg(test)]
mod tests;

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
