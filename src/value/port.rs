use super::Value;
use crate::string::SymbolTable;
use std::{
    fmt::{self, Debug, Display, Formatter},
    fs::File,
    io::{self, BufReader, BufWriter, Error, ErrorKind, Read, Write},
    path::PathBuf,
};

pub(crate) type ReadPort = Port<ReadStream>;
pub(crate) type WritePort = Port<WriteStream>;
pub(crate) type PortResult<T = ()> = Result<T, PortError>;

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
    pub(super) fn file(path: impl Into<PathBuf>) -> PortResult<Self> {
        Self::any_new(ReadSource::File(path.into()))
    }

    pub(super) fn stdin() -> Self {
        Self::txt_new(ReadSource::Stdin).expect("unexpected stdin init error")
    }

    fn any_new(source: ReadSource) -> PortResult<Self> {
        Self::new(source, PortMode::Any)
    }

    fn txt_new(source: ReadSource) -> PortResult<Self> {
        Self::new(source, PortMode::Textual)
    }

    fn new(source: ReadSource, mode: PortMode) -> PortResult<Self> {
        Ok(Self {
            mode,
            stream: ReadStream {
                buf: Some(source.create_stream()?),
                source,
            },
        })
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
    pub(super) fn file(path: impl Into<PathBuf>) -> PortResult<Self> {
        Self::any_new(WriteSource::File(path.into()))
    }

    pub(super) fn stdout() -> Self {
        Self::txt_new(WriteSource::Stdout).expect("unexpected stdout init failure")
    }

    pub(super) fn stderr() -> Self {
        Self::txt_new(WriteSource::Stderr).expect("unexpected stderr init failure")
    }

    fn any_new(source: WriteSource) -> PortResult<Self> {
        Self::new(source, PortMode::Any)
    }

    fn txt_new(source: WriteSource) -> PortResult<Self> {
        Self::new(source, PortMode::Textual)
    }

    fn new(source: WriteSource, mode: PortMode) -> PortResult<Self> {
        Ok(Self {
            mode,
            stream: WriteStream {
                buf: Some(source.create_stream()?),
                source,
            },
        })
    }

    pub(crate) fn put_bytes(&mut self, bytes: &[u8]) -> PortResult {
        if self.is_binary() {
            self.stream.put_bytes(bytes)
        } else {
            Err(PortError::ExpectedMode(PortMode::Binary))
        }
    }

    pub(crate) fn put_char(&mut self, ch: char) -> PortResult {
        if self.is_textual() {
            self.stream.put_char(ch)
        } else {
            Err(PortError::ExpectedMode(PortMode::Textual))
        }
    }

    pub(crate) fn put_string(&mut self, s: &str) -> PortResult {
        if self.is_textual() {
            self.stream.put_string(s)
        } else {
            Err(PortError::ExpectedMode(PortMode::Textual))
        }
    }

    pub(crate) fn flush(&mut self) -> PortResult {
        self.stream.flush()
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
        self.buf.is_some()
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
    fn put_bytes(&mut self, bytes: &[u8]) -> PortResult {
        self.io_op(|w| {
            w.write_all(bytes)?;
            Ok(())
        })
    }

    fn put_char(&mut self, ch: char) -> PortResult {
        self.io_op(|w| write!(w, "{ch}"))
    }

    fn put_string(&mut self, s: &str) -> PortResult {
        self.io_op(|w| write!(w, "{s}"))
    }

    fn flush(&mut self) -> PortResult {
        self.io_op(Write::flush)
    }

    fn io_op(&mut self, op: impl FnOnce(&mut Box<dyn Write>) -> io::Result<()>) -> PortResult {
        match &mut self.buf {
            None => Err(PortError::Closed),
            Some(w) => Ok(op(w)?),
        }
    }
}

impl PortStream for WriteStream {
    fn is_open(&self) -> bool {
        self.buf.is_some()
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
pub(crate) enum PortError {
    Closed,
    ExpectedMode(PortMode),
    Io(ErrorKind),
}

impl PortError {
    pub(super) fn to_symbol(&self, sym: &SymbolTable) -> Value {
        Value::Symbol(match self {
            Self::Closed => sym.get("closed-port"),
            Self::ExpectedMode(_) => sym.get("mismatched-port-mode"),
            Self::Io(k) => match k {
                ErrorKind::AlreadyExists => sym.get("already-exists"),
                ErrorKind::BrokenPipe => sym.get("broken-pipe"),
                ErrorKind::ConnectionAborted => sym.get("connection-aborted"),
                ErrorKind::ConnectionRefused => sym.get("connection-refused"),
                ErrorKind::ConnectionReset => sym.get("connection-reset"),
                ErrorKind::DirectoryNotEmpty => sym.get("directory-not-empty"),
                ErrorKind::FileTooLarge => sym.get("file-too-large"),
                ErrorKind::Interrupted => sym.get("operation-interrupted"),
                ErrorKind::InvalidData => sym.get("invalid-data"),
                ErrorKind::InvalidFilename => sym.get("invalid-filename"),
                ErrorKind::IsADirectory => sym.get("is-directory"),
                ErrorKind::NotADirectory => sym.get("not-directory"),
                ErrorKind::NotFound => sym.get("not-found"),
                ErrorKind::PermissionDenied => sym.get("permission-denied"),
                ErrorKind::ReadOnlyFilesystem => sym.get("read-only-filesystem"),
                ErrorKind::StorageFull => sym.get("storage-full"),
                ErrorKind::TimedOut => sym.get("timeout"),
                ErrorKind::UnexpectedEof => sym.get("unexpected-eof"),
                _ => sym.get("nonspecific-error"),
            },
        })
    }
}

impl Display for PortError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Closed => f.write_str("attempted operation on closed port"),
            Self::ExpectedMode(m) => write!(f, "attemped {} operation on {m} port", m.inverse()),
            Self::Io(k) => write!(f, "{k}"),
        }
    }
}

impl From<Error> for PortError {
    fn from(value: Error) -> Self {
        Self::Io(value.kind())
    }
}

#[derive(Debug)]
pub(crate) enum PortMode {
    Any,
    Binary,
    Textual,
}

impl PortMode {
    fn inverse(&self) -> Self {
        match self {
            Self::Any => Self::Any,
            Self::Binary => Self::Textual,
            Self::Textual => Self::Binary,
        }
    }
}

impl Display for PortMode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Any => f.write_str("any"),
            Self::Binary => f.write_str("binary"),
            Self::Textual => f.write_str("textual"),
        }
    }
}

#[derive(Debug)]
enum ReadSource {
    File(PathBuf),
    Stdin,
}

impl ReadSource {
    fn create_stream(&self) -> PortResult<Box<dyn Read>> {
        Ok(match self {
            Self::File(p) => Box::new(BufReader::new(File::open(p)?)),
            Self::Stdin => Box::new(io::stdin()),
        })
    }
}

impl Display for ReadSource {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
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
    fn create_stream(&self) -> PortResult<Box<dyn Write>> {
        Ok(match self {
            Self::File(p) => Box::new(BufWriter::new(File::create(p)?)),
            Self::Stderr => Box::new(io::stderr()),
            Self::Stdout => Box::new(io::stdout()),
        })
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
