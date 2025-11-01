use super::{TypeName, Value};
use crate::string::SymbolTable;
use std::{
    fmt::{self, Debug, Display, Formatter},
    fs::File,
    io::{self, BufReader, BufWriter, Error, ErrorKind, Read, Stderr, Stdin, Stdout, Write},
    path::PathBuf,
};

pub(crate) type PortResult<T = ()> = Result<T, PortError>;

#[derive(Debug)]
pub(crate) enum ReadPort {
    File(Option<BufReader<File>>, PathBuf),
    In(Option<Stdin>),
}

impl ReadPort {
    pub(super) fn file(path: impl Into<PathBuf>) -> PortResult<Self> {
        let p = path.into();
        let f = File::open(&p)?;
        Ok(Self::File(Some(BufReader::new(f)), p))
    }

    pub(super) fn stdin() -> Self {
        Self::In(Some(io::stdin()))
    }

    pub(crate) fn is_binary(&self) -> bool {
        match self {
            Self::File(..) => true,
            Self::In(_) => false,
        }
    }

    pub(crate) fn is_textual(&self) -> bool {
        match self {
            Self::File(..) | Self::In(_) => true,
        }
    }

    pub(crate) fn is_open(&self) -> bool {
        match self {
            Self::File(o, _) => o.is_some(),
            Self::In(o) => o.is_some(),
        }
    }

    pub(crate) fn close(&mut self) {
        match self {
            Self::File(o, _) => {
                o.take();
            }
            Self::In(o) => {
                o.take();
            }
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
            Self::File(o, _) => o.as_mut().map(as_dynr),
            Self::In(o) => o.as_mut().map(as_dynr),
        }
        .ok_or(PortError::Closed)
    }
}

impl Display for ReadPort {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::File(_, p) => write_file_source(p.display(), 'r', f),
            Self::In(_) => f.write_str("stdin"),
        }?;
        write_port_status(self.is_open(), f)
    }
}

#[derive(Debug)]
pub(crate) enum WritePort {
    ByteVector(Option<Vec<u8>>),
    Err(Option<Stderr>, bool),
    File(Option<BufWriter<File>>, PathBuf),
    Out(Option<Stdout>, bool),
}

impl WritePort {
    pub(super) fn bytevector() -> Self {
        Self::ByteVector(Some(Vec::new()))
    }

    pub(super) fn file(path: impl Into<PathBuf>) -> PortResult<Self> {
        let p = path.into();
        let f = File::create(&p)?;
        Ok(Self::File(Some(BufWriter::new(f)), p))
    }

    pub(super) fn stdout(interactive: bool) -> Self {
        Self::Out(Some(io::stdout()), interactive)
    }

    pub(super) fn stderr(interactive: bool) -> Self {
        Self::Err(Some(io::stderr()), interactive)
    }

    pub(crate) fn is_binary(&self) -> bool {
        match self {
            Self::ByteVector(_) | Self::File(..) => true,
            Self::Err(..) | Self::Out(..) => false,
        }
    }

    pub(crate) fn is_textual(&self) -> bool {
        match self {
            Self::ByteVector(_) => false,
            Self::Err(..) | Self::File(..) | Self::Out(..) => true,
        }
    }

    pub(crate) fn is_open(&self) -> bool {
        match self {
            Self::ByteVector(o) => o.is_some(),
            Self::Err(o, _) => o.is_some(),
            Self::File(o, _) => o.is_some(),
            Self::Out(o, _) => o.is_some(),
        }
    }

    pub(crate) fn get_bytevector(&self) -> PortResult<Value> {
        if let Self::ByteVector(w) = self {
            match w {
                None => Err(PortError::Closed),
                Some(v) => Ok(Value::bytevector_mut(v.iter().copied())),
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
            self.io_op(|w| write!(w, "{ch}"))?;
            self.repl_newline(ch != '\n')
        } else {
            Err(PortError::ExpectedMode(PortMode::Textual))
        }
    }

    pub(crate) fn put_string(&mut self, s: &str) -> PortResult {
        if self.is_textual() {
            self.io_op(|w| write!(w, "{s}"))?;
            self.repl_newline(!s.ends_with('\n'))
        } else {
            Err(PortError::ExpectedMode(PortMode::Textual))
        }
    }

    pub(crate) fn flush(&mut self) -> PortResult {
        self.io_op(|w| w.flush())
    }

    pub(crate) fn close(&mut self) {
        match self {
            Self::ByteVector(o) => {
                o.take();
            }
            Self::Err(o, _) => {
                o.take();
            }
            Self::File(o, _) => {
                o.take();
            }
            Self::Out(o, _) => {
                o.take();
            }
        }
    }

    fn spec(&self) -> PortSpec {
        match self {
            Self::ByteVector(_) => PortSpec::BinaryOutput,
            Self::File(..) => PortSpec::Output,
            Self::Err(..) | Self::Out(..) => PortSpec::TextualOutput,
        }
    }

    fn io_op(&mut self, op: impl FnOnce(&mut dyn Write) -> io::Result<()>) -> PortResult {
        Ok(op(self.get_writer()?)?)
    }

    fn get_writer(&mut self) -> PortResult<&mut dyn Write> {
        match self {
            Self::ByteVector(o) => o.as_mut().map(as_dynw),
            Self::Err(o, _) => o.as_mut().map(as_dynw),
            Self::File(o, _) => o.as_mut().map(as_dynw),
            Self::Out(o, _) => o.as_mut().map(as_dynw),
        }
        .ok_or(PortError::Closed)
    }

    // NOTE: rustyline always resets cursor position so without adding a synthetic
    // newline the prompt will overwrite any one-line text output in the REPL.
    fn repl_newline(&mut self, missing_newline: bool) -> PortResult {
        if let Self::Err(_, true) | Self::Out(_, true) = self
            && missing_newline
        {
            self.put_char('\n')
        } else {
            Ok(())
        }
    }
}

impl Display for WritePort {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::ByteVector(_) => f.write_str(TypeName::BYTEVECTOR),
            Self::Err(..) => f.write_str("stderr"),
            Self::File(_, p) => write_file_source(p.display(), 'w', f),
            Self::Out(..) => f.write_str("stdout"),
        }?;
        write_port_status(self.is_open(), f)
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

fn as_dynr(r: &mut impl Read) -> &mut dyn Read {
    r as &mut dyn Read
}

fn as_dynw(w: &mut impl Write) -> &mut dyn Write {
    w as &mut dyn Write
}

fn write_file_source(path: impl Display, mode: char, f: &mut Formatter<'_>) -> fmt::Result {
    write!(f, "file:{mode}:{path}")
}

fn write_port_status(open: bool, f: &mut Formatter<'_>) -> fmt::Result {
    if open {
        Ok(())
    } else {
        f.write_str(" (closed)")
    }
}
