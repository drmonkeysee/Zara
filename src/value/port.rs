use super::{TypeName, Value};
use crate::string::SymbolTable;
use std::{
    fmt::{self, Debug, Display, Formatter},
    fs::File,
    io::{self, BufReader, BufWriter, Error, ErrorKind, Read, Stderr, Stdin, Stdout, Write},
    path::PathBuf,
};

pub(crate) type ReadPort = Port<ReadSource, dyn Read>;
pub(crate) type WritePort = Port<WriteSource, dyn Write>;
pub(crate) type PortResult<T = ()> = Result<T, PortError>;

pub(crate) struct Port<T, U: ?Sized> {
    mode: PortMode,
    source: T,
    stream: Option<Box<U>>,
}

impl<T, U: ?Sized> Port<T, U> {
    pub(crate) fn is_textual(&self) -> bool {
        matches!(self.mode, PortMode::Any | PortMode::Textual)
    }

    pub(crate) fn is_binary(&self) -> bool {
        matches!(self.mode, PortMode::Any | PortMode::Binary)
    }

    pub(crate) fn is_open(&self) -> bool {
        self.stream.is_some()
    }

    pub(crate) fn close(&mut self) {
        self.stream.take();
    }
}

#[allow(private_bounds)]
impl<T, U: ?Sized> Port<T, U>
where
    T: StreamSource<U>,
{
    fn new(source: T, mode: PortMode) -> PortResult<Self> {
        let stream = source.create_stream()?;
        Ok(Self {
            mode,
            source,
            stream: Some(stream),
        })
    }
}

impl<T: Debug, U: ?Sized> Debug for Port<T, U> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("Port")
            .field("mode", &self.mode)
            .field("source", &self.source)
            .field("stream", &"...")
            .finish()
    }
}

impl<T: Display, U: ?Sized> Display for Port<T, U> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write_port_datum(&self.source, self.is_open(), f)
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

    fn spec(&self) -> PortSpec {
        match self.mode {
            PortMode::Any => PortSpec::Input,
            PortMode::Binary => PortSpec::BinaryInput,
            PortMode::Textual => PortSpec::TextualInput,
        }
    }
}

impl WritePort {
    pub(super) fn bytevector() -> Self {
        Self::bin_new(WriteSource::ByteVector).expect("unexpected bytevector init failure")
    }

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

    fn bin_new(source: WriteSource) -> PortResult<Self> {
        Self::new(source, PortMode::Binary)
    }

    fn txt_new(source: WriteSource) -> PortResult<Self> {
        Self::new(source, PortMode::Textual)
    }

    pub(crate) fn get_bytevector(&self) -> PortResult<Value> {
        if let WriteSource::ByteVector = self.source {
            match &self.stream {
                None => Err(PortError::Closed),
                Some(_) => {
                    todo!(
                        "it does not seem to be possible to downcast from dyn Write to Vec<u8> through dyn Any"
                    )
                }
            }
        } else {
            Err(PortError::InvalidSource)
        }
    }

    pub(crate) fn put_bytes(&mut self, bytes: &[u8]) -> PortResult {
        if self.is_binary() {
            self.io_op(|w| {
                w.write_all(bytes)?;
                Ok(())
            })
        } else {
            Err(PortError::ExpectedMode(PortMode::Binary))
        }
    }

    pub(crate) fn put_char(&mut self, ch: char) -> PortResult {
        if self.is_textual() {
            self.io_op(|w| write!(w, "{ch}"))
        } else {
            Err(PortError::ExpectedMode(PortMode::Textual))
        }
    }

    pub(crate) fn put_string(&mut self, s: &str) -> PortResult {
        if self.is_textual() {
            self.io_op(|w| write!(w, "{s}"))
        } else {
            Err(PortError::ExpectedMode(PortMode::Textual))
        }
    }

    pub(crate) fn flush(&mut self) -> PortResult {
        self.io_op(Write::flush)
    }

    fn spec(&self) -> PortSpec {
        match self.mode {
            PortMode::Any => PortSpec::Output,
            PortMode::Binary => PortSpec::BinaryOutput,
            PortMode::Textual => PortSpec::TextualOutput,
        }
    }

    fn io_op(&mut self, op: impl FnOnce(&mut Box<dyn Write>) -> io::Result<()>) -> PortResult {
        match &mut self.stream {
            None => Err(PortError::Closed),
            Some(w) => Ok(op(w)?),
        }
    }
}

#[derive(Debug)]
pub(crate) enum ReadSource {
    File(PathBuf),
    Stdin,
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
pub(crate) enum WriteSource {
    ByteVector,
    File(PathBuf),
    Stderr,
    Stdout,
}

impl Display for WriteSource {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::ByteVector => f.write_str(TypeName::BYTEVECTOR),
            Self::File(p) => write_file_source(p.display(), 'w', f),
            Self::Stderr => f.write_str("stderr"),
            Self::Stdout => f.write_str("stdout"),
        }
    }
}

enum ReadPort2 {
    File(PathBuf, Option<BufReader<File>>),
    In(Option<Stdin>),
}

impl ReadPort2 {
    fn is_binary(&self) -> bool {
        match self {
            Self::File(..) => true,
            Self::In(_) => false,
        }
    }

    fn is_textual(&self) -> bool {
        match self {
            Self::File(..) | Self::In(_) => true,
        }
    }

    fn is_open(&self) -> bool {
        match self {
            Self::File(_, o) => o.is_some(),
            Self::In(o) => o.is_some(),
        }
    }

    fn spec(&self) -> PortSpec {
        match self {
            Self::File(..) => PortSpec::Input,
            Self::In(_) => PortSpec::TextualInput,
        }
    }

    fn io_op(&mut self, op: impl FnOnce(&mut dyn Read) -> io::Result<()>) -> PortResult {
        Ok(op(self.get_reader()?)?)
    }

    fn get_reader(&mut self) -> PortResult<&mut dyn Read> {
        match self {
            Self::File(_, o) => o.as_mut().map(as_dynr),
            Self::In(o) => o.as_mut().map(as_dynr),
        }
        .ok_or(PortError::Closed)
    }

    fn close(&mut self) {
        match self {
            Self::File(_, o) => {
                o.take();
            }
            Self::In(o) => {
                o.take();
            }
        }
    }
}

enum WritePort2 {
    ByteVector(Option<Vec<u8>>),
    Err(Option<Stderr>),
    File(PathBuf, Option<BufWriter<File>>),
    Out(Option<Stdout>),
}

impl WritePort2 {
    fn is_binary(&self) -> bool {
        match self {
            Self::ByteVector(_) | Self::File(..) => true,
            Self::Err(_) | Self::Out(_) => false,
        }
    }

    fn is_textual(&self) -> bool {
        match self {
            Self::ByteVector(_) => false,
            Self::Err(_) | Self::File(..) | Self::Out(_) => true,
        }
    }

    fn is_open(&self) -> bool {
        match self {
            Self::ByteVector(o) => o.is_some(),
            Self::Err(o) => o.is_some(),
            Self::File(_, o) => o.is_some(),
            Self::Out(o) => o.is_some(),
        }
    }

    fn spec(&self) -> PortSpec {
        match self {
            Self::ByteVector(_) => PortSpec::BinaryOutput,
            Self::File(..) => PortSpec::Output,
            Self::Err(_) | Self::Out(_) => PortSpec::TextualOutput,
        }
    }

    fn get_bytevector(&self) -> PortResult<Value> {
        if let Self::ByteVector(w) = self {
            match w {
                None => Err(PortError::Closed),
                Some(v) => Ok(Value::bytevector_mut(v.iter().copied())),
            }
        } else {
            Err(PortError::InvalidSource)
        }
    }

    fn put_bytes(&mut self, bytes: &[u8]) -> PortResult {
        if self.is_binary() {
            self.io_op(|w| {
                w.write_all(bytes)?;
                Ok(())
            })
        } else {
            Err(PortError::ExpectedMode(PortMode::Binary))
        }
    }

    fn put_char(&mut self, ch: char) -> PortResult {
        if self.is_textual() {
            self.io_op(|w| write!(w, "{ch}"))
        } else {
            Err(PortError::ExpectedMode(PortMode::Textual))
        }
    }

    fn put_string(&mut self, s: &str) -> PortResult {
        if self.is_textual() {
            self.io_op(|w| write!(w, "{s}"))
        } else {
            Err(PortError::ExpectedMode(PortMode::Textual))
        }
    }

    fn flush(&mut self) -> PortResult {
        self.io_op(|w| w.flush())
    }

    fn io_op(&mut self, op: impl FnOnce(&mut dyn Write) -> io::Result<()>) -> PortResult {
        Ok(op(self.get_writer()?)?)
    }

    fn get_writer(&mut self) -> PortResult<&mut dyn Write> {
        match self {
            Self::ByteVector(o) => o.as_mut().map(as_dynw),
            Self::Err(o) => o.as_mut().map(as_dynw),
            Self::File(_, o) => o.as_mut().map(as_dynw),
            Self::Out(o) => o.as_mut().map(as_dynw),
        }
        .ok_or(PortError::Closed)
    }

    fn close(&mut self) {
        match self {
            Self::ByteVector(o) => {
                o.take();
            }
            Self::Err(o) => {
                o.take();
            }
            Self::File(_, o) => {
                o.take();
            }
            Self::Out(o) => {
                o.take();
            }
        }
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
    InvalidSource,
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
            Self::InvalidSource => sym.get("invalid-port-source"),
        })
    }
}

impl Display for PortError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Closed => f.write_str("attempted operation on closed port"),
            Self::ExpectedMode(m) => write!(f, "attemped {} operation on {m} port", m.inverse()),
            Self::Io(k) => write!(f, "{k}"),
            Self::InvalidSource => f.write_str("invalid port for requested data type"),
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

trait StreamSource<T: ?Sized> {
    fn create_stream(&self) -> PortResult<Box<T>>;
}

impl StreamSource<dyn Read> for ReadSource {
    fn create_stream(&self) -> PortResult<Box<dyn Read>> {
        Ok(match self {
            Self::File(p) => Box::new(BufReader::new(File::open(p)?)),
            Self::Stdin => Box::new(io::stdin()),
        })
    }
}

impl StreamSource<dyn Write> for WriteSource {
    fn create_stream(&self) -> PortResult<Box<dyn Write>> {
        Ok(match self {
            Self::ByteVector => Box::new(Vec::<u8>::new()),
            Self::File(p) => Box::new(BufWriter::new(File::create(p)?)),
            Self::Stderr => Box::new(io::stderr()),
            Self::Stdout => Box::new(io::stdout()),
        })
    }
}

fn as_dynr(r: &mut impl Read) -> &mut dyn Read {
    r as &mut dyn Read
}

fn as_dynw(w: &mut impl Write) -> &mut dyn Write {
    w as &mut dyn Write
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
