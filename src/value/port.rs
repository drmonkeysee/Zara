use super::{TypeName, Value};
use crate::string::SymbolTable;
use std::{
    fmt::{self, Debug, Display, Formatter},
    fs::File,
    io::{self, BufReader, BufWriter, ErrorKind, Read, Stderr, Stdin, Stdout},
    path::PathBuf,
};

pub(crate) type PortResult<T = ()> = Result<T, PortError>;

#[derive(Debug)]
pub(crate) enum ReadPort {
    ByteVector(BvReader),
    File(Option<BufReader<File>>, PathBuf),
    In(Option<Stdin>),
}

impl ReadPort {
    pub(super) fn bytevector(bytes: impl IntoIterator<Item = u8>) -> Self {
        Self::ByteVector(BvReader::new(bytes))
    }

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
            Self::ByteVector(_) | Self::File(..) => true,
            Self::In(_) => false,
        }
    }

    pub(crate) fn is_textual(&self) -> bool {
        match self {
            Self::ByteVector(_) => false,
            Self::File(..) | Self::In(_) => true,
        }
    }

    pub(crate) fn is_open(&self) -> bool {
        match self {
            Self::ByteVector(r) => r.is_open(),
            Self::File(o, _) => o.is_some(),
            Self::In(o) => o.is_some(),
        }
    }

    pub(crate) fn close(&mut self) {
        match self {
            Self::ByteVector(r) => r.close(),
            Self::File(o, _) => {
                o.take();
            }
            Self::In(o) => {
                o.take();
            }
        }
    }

    pub(crate) fn get_byte(&mut self) -> PortResult<Option<u8>> {
        if self.is_binary() {
            if let Self::ByteVector(r) = self {
                r.get_byte()
            } else {
                todo!();
            }
        } else {
            Err(PortError::ExpectedMode(PortMode::Binary))
        }
    }

    fn spec(&self) -> PortSpec {
        match self {
            Self::ByteVector(_) => PortSpec::BinaryInput,
            Self::File(..) => PortSpec::Input,
            Self::In(_) => PortSpec::TextualInput,
        }
    }

    /*
    fn get_reader(&mut self) -> PortResult<&mut dyn Read> {
        match self {
            Self::ByteVector(o, _) => o.as_mut().map(as_dynr),
            Self::File(o, _) => o.as_mut().map(as_dynr),
            Self::In(o) => o.as_mut().map(as_dynr),
        }
        .ok_or(PortError::Closed)
    }

    fn io_op(&mut self, op: impl FnOnce(&mut dyn Read) -> io::Result<()>) -> PortResult {
        Ok(op(self.get_reader()?)?)
    }
    */
}

impl Display for ReadPort {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::ByteVector(_) => f.write_str(TypeName::BYTEVECTOR),
            Self::File(_, p) => write_file_source(p.display(), 'r', f),
            Self::In(_) => f.write_str("stdin"),
        }?;
        write_port_status(self.is_open(), f)
    }
}

#[derive(Debug)]
pub(crate) struct BvReader {
    buf: Option<Box<[u8]>>,
    cur: usize,
}

impl BvReader {
    fn new(bytes: impl IntoIterator<Item = u8>) -> Self {
        Self {
            buf: Some(bytes.into_iter().collect()),
            cur: usize::MIN,
        }
    }

    fn is_open(&self) -> bool {
        self.buf.is_some()
    }

    fn get_byte(&mut self) -> PortResult<Option<u8>> {
        match &mut self.buf {
            None => Err(PortError::Closed),
            Some(buf) => {
                let b = buf.get(self.cur);
                if b.is_some() {
                    self.cur += 1;
                }
                Ok(b.copied())
            }
        }
    }

    fn close(&mut self) {
        self.buf.take();
    }
}

#[derive(Debug)]
pub(crate) enum WritePort {
    ByteVector(Option<Vec<u8>>),
    Err(Option<Stderr>, bool),
    File(Option<BufWriter<File>>, PathBuf),
    Out(Option<Stdout>, bool),
    String(Option<String>),
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

    pub(super) fn string() -> Self {
        Self::String(Some(String::new()))
    }

    pub(crate) fn is_binary(&self) -> bool {
        match self {
            Self::ByteVector(_) | Self::File(..) => true,
            Self::Err(..) | Self::Out(..) | Self::String(_) => false,
        }
    }

    pub(crate) fn is_textual(&self) -> bool {
        match self {
            Self::ByteVector(_) => false,
            Self::Err(..) | Self::File(..) | Self::Out(..) | Self::String(_) => true,
        }
    }

    pub(crate) fn is_open(&self) -> bool {
        match self {
            Self::ByteVector(o) => o.is_some(),
            Self::Err(o, _) => o.is_some(),
            Self::File(o, _) => o.is_some(),
            Self::Out(o, _) => o.is_some(),
            Self::String(o) => o.is_some(),
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

    pub(crate) fn get_string(&self) -> PortResult<Value> {
        if let Self::String(w) = self {
            match w {
                None => Err(PortError::Closed),
                Some(s) => Ok(Value::string_mut(s)),
            }
        } else {
            Err(PortError::InvalidSource)
        }
    }

    pub(crate) fn put_bytes(&mut self, bytes: &[u8]) -> PortResult {
        if self.is_binary() {
            self.get_writer()?.put_bytes(bytes)
        } else {
            Err(PortError::ExpectedMode(PortMode::Binary))
        }
    }

    pub(crate) fn put_char(&mut self, ch: char) -> PortResult {
        if self.is_textual() {
            self.get_writer()?.put_char(ch)?;
            self.repl_newline(ch != '\n')
        } else {
            Err(PortError::ExpectedMode(PortMode::Textual))
        }
    }

    pub(crate) fn put_string(&mut self, s: &str) -> PortResult {
        if self.is_textual() {
            self.get_writer()?.put_string(s)?;
            self.repl_newline(!s.ends_with('\n'))
        } else {
            Err(PortError::ExpectedMode(PortMode::Textual))
        }
    }

    pub(crate) fn flush(&mut self) -> PortResult {
        self.get_writer()?.flush()
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
            Self::String(o) => {
                o.take();
            }
        }
    }

    fn spec(&self) -> PortSpec {
        match self {
            Self::ByteVector(_) => PortSpec::BinaryOutput,
            Self::File(..) => PortSpec::Output,
            Self::Err(..) | Self::Out(..) | Self::String(_) => PortSpec::TextualOutput,
        }
    }

    fn get_writer(&mut self) -> PortResult<WriteRef<'_>> {
        match self {
            Self::ByteVector(o) => o.as_mut().map(WriteRef::io),
            Self::Err(o, _) => o.as_mut().map(WriteRef::io),
            Self::File(o, _) => o.as_mut().map(WriteRef::io),
            Self::Out(o, _) => o.as_mut().map(WriteRef::io),
            Self::String(o) => o.as_mut().map(WriteRef::fmt),
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
            Self::String(_) => f.write_str(TypeName::STRING),
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
    Fmt,
    InvalidSource,
    Io(ErrorKind),
}

impl PortError {
    pub(super) fn to_symbol(&self, sym: &SymbolTable) -> Value {
        Value::Symbol(match self {
            Self::Closed => sym.get("closed-port"),
            Self::ExpectedMode(_) => sym.get("mismatched-port-mode"),
            Self::Fmt => sym.get("string-error"),
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
            Self::Fmt => f.write_str("unexpected format error"),
            Self::Io(k) => write!(f, "{k}"),
            Self::InvalidSource => f.write_str("invalid port for requested data type"),
        }
    }
}

impl From<fmt::Error> for PortError {
    fn from(_value: fmt::Error) -> Self {
        Self::Fmt
    }
}

impl From<io::Error> for PortError {
    fn from(value: io::Error) -> Self {
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

enum WriteRef<'a> {
    Fmt(&'a mut dyn fmt::Write),
    Io(&'a mut dyn io::Write),
}

impl<'a> WriteRef<'a> {
    fn fmt(w: &'a mut impl fmt::Write) -> Self {
        Self::Fmt(w as &mut dyn fmt::Write)
    }

    fn io(w: &'a mut impl io::Write) -> Self {
        Self::Io(w as &mut dyn io::Write)
    }

    fn put_bytes(&mut self, bytes: &[u8]) -> PortResult {
        self.op(|w| w.write_all(bytes), |_| Err(fmt::Error))
    }

    fn put_char(&mut self, ch: char) -> PortResult {
        self.op(|w| write!(w, "{ch}"), |w| write!(w, "{ch}"))
    }

    fn put_string(&mut self, s: &str) -> PortResult {
        self.op(|w| write!(w, "{s}"), |w| write!(w, "{s}"))
    }

    fn flush(&mut self) -> PortResult {
        self.op(|w| w.flush(), |_| Ok(()))
    }

    fn op(
        &mut self,
        io_op: impl FnOnce(&mut dyn io::Write) -> io::Result<()>,
        fmt_op: impl FnOnce(&mut dyn fmt::Write) -> Result<(), fmt::Error>,
    ) -> PortResult {
        match self {
            Self::Fmt(w) => fmt_op(w)?,
            Self::Io(w) => io_op(w)?,
        }
        Ok(())
    }
}

fn as_dynr(r: &mut impl Read) -> &mut dyn Read {
    r as &mut dyn Read
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::testutil::{err_or_fail, ok_or_fail, some_or_fail};

    #[test]
    fn bv_empty_get_byte() {
        let bytes = Vec::<u8>::new();
        let mut p = ReadPort::bytevector(bytes.iter().copied());

        let r = p.get_byte();

        let b = ok_or_fail!(r);
        assert!(b.is_none());
    }

    #[test]
    fn bv_get_byte() {
        let bytes: Vec<u8> = vec![1, 2, 3];
        let mut p = ReadPort::bytevector(bytes.iter().copied());

        let r = p.get_byte();

        let b = some_or_fail!(ok_or_fail!(r));
        assert_eq!(b, 1);

        let r = p.get_byte();

        let b = some_or_fail!(ok_or_fail!(r));
        assert_eq!(b, 2);

        let r = p.get_byte();

        let b = some_or_fail!(ok_or_fail!(r));
        assert_eq!(b, 3);

        let r = p.get_byte();

        let b = ok_or_fail!(r);
        assert!(b.is_none());
    }

    #[test]
    fn bv_get_byte_when_closed() {
        let bytes: Vec<u8> = vec![1, 2, 3];
        let mut p = ReadPort::bytevector(bytes.iter().copied());

        let r = p.get_byte();

        let b = some_or_fail!(ok_or_fail!(r));
        assert_eq!(b, 1);

        p.close();
        let r = p.get_byte();

        let e = err_or_fail!(r);
        assert!(matches!(e, PortError::Closed));
    }
}
