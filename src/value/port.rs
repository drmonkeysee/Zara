use super::Value;
use std::{
    fmt::{self, Debug, Display, Formatter},
    fs::File,
    io::{self, BufReader, BufWriter, Read, Write},
    path::PathBuf,
};

pub(crate) type ReadPort = Port<ReadStream>;
pub(crate) type WritePort = Port<WriteStream>;

#[derive(Debug)]
pub(crate) struct Port<T> {
    mode: PortMode,
    stream: T,
}

impl<T: PortStream> Port<T> {
    pub(crate) fn is_textual(&self) -> bool {
        matches!(self.mode, PortMode::Any | PortMode::Textual)
    }

    pub(crate) fn is_binary(&self) -> bool {
        matches!(self.mode, PortMode::Any | PortMode::Binary)
    }

    pub(crate) fn is_open(&self) -> bool {
        self.stream.is_open()
    }

    pub(crate) fn close(&mut self) {
        self.stream.close();
    }
}

impl<T: Display> Display for Port<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.stream.fmt(f)
    }
}

impl ReadPort {
    pub(super) fn file(path: impl Into<PathBuf>) -> Self {
        Self::any_new(ReadSource::File(path.into()))
    }

    pub(super) fn stdin() -> Self {
        Self::txt_new(ReadSource::Stdin)
    }

    fn any_new(source: ReadSource) -> Self {
        Self::new(source, PortMode::Any)
    }

    fn txt_new(source: ReadSource) -> Self {
        Self::new(source, PortMode::Textual)
    }

    fn new(source: ReadSource, mode: PortMode) -> Self {
        Self {
            mode,
            stream: ReadStream {
                buf: Some(source.create_stream()),
                source,
            },
        }
    }

    fn spec(&self) -> PortSpec {
        match self.mode {
            PortMode::Any => PortSpec::Input,
            PortMode::Binary => PortSpec::BinaryInput,
            PortMode::Textual => PortSpec::TextualInput,
        }
    }
}

impl WritePort {
    pub(super) fn file(path: impl Into<PathBuf>) -> Self {
        Self::any_new(WriteSource::File(path.into()))
    }

    pub(super) fn stdout() -> Self {
        Self::txt_new(WriteSource::Stdout)
    }

    pub(super) fn stderr() -> Self {
        Self::txt_new(WriteSource::Stderr)
    }

    fn any_new(source: WriteSource) -> Self {
        Self::new(source, PortMode::Any)
    }

    fn txt_new(source: WriteSource) -> Self {
        Self::new(source, PortMode::Textual)
    }

    fn new(source: WriteSource, mode: PortMode) -> Self {
        Self {
            mode,
            stream: WriteStream {
                buf: Some(source.create_stream()),
                source,
            },
        }
    }

    pub(crate) fn put_char(&mut self, ch: char) -> Result<(), ()> {
        // TODO: return error if mode = binary?
        self.stream.put_char(ch)
    }

    pub(crate) fn put_string(&mut self, s: &str) -> Result<(), ()> {
        // TODO: return error if mode = binary?
        self.stream.put_string(s)
    }

    fn spec(&self) -> PortSpec {
        match self.mode {
            PortMode::Any => PortSpec::Output,
            PortMode::Binary => PortSpec::BinaryOutput,
            PortMode::Textual => PortSpec::TextualOutput,
        }
    }
}

pub(crate) trait PortStream {
    fn is_open(&self) -> bool;
    fn close(&mut self);
}

pub(crate) struct ReadStream {
    buf: Option<Box<dyn Read>>,
    source: ReadSource,
}

impl PortStream for ReadStream {
    fn is_open(&self) -> bool {
        return self.buf.is_some();
    }

    fn close(&mut self) {
        self.buf.take();
    }
}

impl Debug for ReadStream {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("ReadStream")
            .field("buf", &"...")
            .field("source", &self.source)
            .finish()
    }
}

impl Display for ReadStream {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write_port_datum(&self.source, self.is_open(), f)
    }
}

pub(crate) struct WriteStream {
    buf: Option<Box<dyn Write>>,
    source: WriteSource,
}

impl WriteStream {
    // TODO: figure out return value
    fn put_char(&mut self, ch: char) -> Result<(), ()> {
        match &mut self.buf {
            None => todo!("closed port error"),
            Some(w) => {
                // TODO: figure out result and handle error
                write!(w, "{ch}");
                Ok(())
            }
        }
    }

    // TODO: figure out return value
    fn put_string(&mut self, s: &str) -> Result<(), ()> {
        match &mut self.buf {
            None => todo!("closed port error"),
            Some(w) => {
                // TODO: figure out result and handle error
                write!(w, "{s}");
                Ok(())
            }
        }
    }
}

impl PortStream for WriteStream {
    fn is_open(&self) -> bool {
        return self.buf.is_some();
    }

    fn close(&mut self) {
        self.buf.take();
    }
}

impl Debug for WriteStream {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("WriteStream")
            .field("buf", &"...")
            .field("source", &self.source)
            .finish()
    }
}

impl Display for WriteStream {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write_port_datum(&self.source, self.is_open(), f)
    }
}

pub(crate) enum PortSpec {
    BinaryInput,
    BinaryOutput,
    Input,
    Output,
    TextualInput,
    TextualOutput,
}

impl PortSpec {
    pub(crate) fn check(&self, val: &Value) -> Result<(), Option<Self>> {
        match val {
            Value::PortInput(p) => {
                if match self {
                    Self::BinaryInput => p.borrow().is_binary(),
                    Self::Input => true,
                    Self::TextualInput => p.borrow().is_textual(),
                    _ => false,
                } {
                    Ok(())
                } else {
                    Err(Some(p.borrow().spec()))
                }
            }
            Value::PortOutput(p) => {
                if match self {
                    Self::BinaryOutput => p.borrow().is_binary(),
                    Self::Output => true,
                    Self::TextualOutput => p.borrow().is_textual(),
                    _ => false,
                } {
                    Ok(())
                } else {
                    Err(Some(p.borrow().spec()))
                }
            }
            _ => Err(None),
        }
    }
}

impl Display for PortSpec {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::BinaryInput => f.write_str("binary input"),
            Self::BinaryOutput => f.write_str("binary output"),
            Self::Input => f.write_str("input"),
            Self::Output => f.write_str("output"),
            Self::TextualInput => f.write_str("textual input"),
            Self::TextualOutput => f.write_str("textual output"),
        }?;
        f.write_str(" port")
    }
}

#[derive(Debug)]
enum PortMode {
    Any,
    Binary,
    Textual,
}

#[derive(Debug)]
enum ReadSource {
    File(PathBuf),
    Stdin,
}

impl ReadSource {
    fn create_stream(&self) -> Box<dyn Read> {
        match self {
            Self::File(p) => Box::new(BufReader::new(File::open(p).unwrap())),
            Self::Stdin => Box::new(io::stdin()),
        }
    }
}

impl Display for ReadSource {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            // TODO: file errors
            Self::File(p) => write_file_source(p.display(), 'r', f),
            Self::Stdin => f.write_str("stdin"),
        }
    }
}

#[derive(Debug)]
enum WriteSource {
    File(PathBuf),
    Stderr,
    Stdout,
}

impl WriteSource {
    fn create_stream(&self) -> Box<dyn Write> {
        match self {
            // TODO: file errors
            Self::File(p) => Box::new(BufWriter::new(File::create(p).unwrap())),
            Self::Stderr => Box::new(io::stderr()),
            Self::Stdout => Box::new(io::stdout()),
        }
    }
}

impl Display for WriteSource {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::File(p) => write_file_source(p.display(), 'w', f),
            Self::Stderr => f.write_str("stderr"),
            Self::Stdout => f.write_str("stdout"),
        }
    }
}

fn write_port_datum(src: impl Display, open: bool, f: &mut Formatter<'_>) -> fmt::Result {
    write!(f, "{src}")?;
    if !open {
        f.write_str(" (closed)")?;
    }
    Ok(())
}

fn write_file_source(path: impl Display, mode: char, f: &mut Formatter<'_>) -> fmt::Result {
    write!(f, "file:{mode}:{path}")
}
