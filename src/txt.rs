use std::{
    fs::File,
    io::{BufRead, BufReader, Result},
    path::Path,
    rc::Rc,
};

pub type LineNumber = u64;

pub trait TextSource: Iterator<Item = TextLine> {
    fn context(&self) -> Rc<TextContext>;
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

pub struct FileSource {
    ctx: Rc<TextContext>,
    lineno: LineNumber,
    reader: BufReader<File>,
}

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

    pub fn library(name: impl Into<String>) -> Result<Self> {
        todo!();
    }

    fn init(f: File, n: String) -> Result<Self> {
        Ok(Self {
            ctx: TextContext::named(n).into(),
            lineno: 1,
            reader: BufReader::new(f),
        })
    }
}

impl Iterator for FileSource {
    type Item = TextLine;

    fn next(&mut self) -> Option<Self::Item> {
        let lineno = self.lineno;
        self.lineno += 1;
        let mut buf = String::new();
        self.reader.read_line(&mut buf).map_or_else(
            |err| {
                eprintln!(
                    "{}:{lineno}\n\tunexpected file read error: {err}",
                    self.ctx.name
                );
                None
            },
            |n| {
                if n == 0 {
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

impl TextSource for FileSource {
    fn context(&self) -> Rc<TextContext> {
        self.ctx.clone()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
}
