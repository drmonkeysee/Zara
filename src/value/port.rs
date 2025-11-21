use super::{TypeName, Value};
use crate::string::{
    SymbolTable,
    unicode::{self, UnicodeError},
};
use std::{
    cmp,
    fmt::{self, Debug, Display, Formatter},
    fs::{File, OpenOptions},
    io::{self, BufRead, BufReader, BufWriter, ErrorKind, Stderr, Stdin, Stdout},
    iter,
    path::{Path, PathBuf},
};

pub(crate) type PortResult<T = ()> = Result<T, PortError>;
pub(crate) type PortBool = PortResult<bool>;
pub(crate) type PortValue = PortResult<Value>;
pub(crate) type PortChar = PortResult<Option<char>>;
pub(crate) type PortString = PortResult<Option<String>>;
pub(crate) type PortByte = PortResult<Option<u8>>;
pub(crate) type PortBytes = PortResult<Option<Vec<u8>>>;

#[derive(Debug)]
pub(crate) enum ReadPort {
    ByteVector(BvReader),
    File(FileReader),
    In(Option<Stdin>),
    String(StringReader),
}

impl ReadPort {
    pub(super) fn bytevector(bytes: impl IntoIterator<Item = u8>) -> Self {
        Self::ByteVector(BvReader::new(bytes))
    }

    pub(super) fn file(path: impl Into<PathBuf>) -> PortResult<Self> {
        Ok(Self::File(FileReader::new(path)?))
    }

    pub(super) fn stdin() -> Self {
        Self::In(Some(io::stdin()))
    }

    pub(super) fn string(s: impl Into<String>) -> Self {
        Self::String(StringReader::new(s))
    }

    pub(crate) fn is_binary(&self) -> bool {
        match self {
            Self::ByteVector(_) | Self::File(_) => true,
            Self::In(_) | Self::String(_) => false,
        }
    }

    pub(crate) fn is_textual(&self) -> bool {
        match self {
            Self::ByteVector(_) => false,
            Self::File(_) | Self::In(_) | Self::String(_) => true,
        }
    }

    pub(crate) fn is_open(&self) -> bool {
        self.get_reader().is_open()
    }

    pub(crate) fn close(&mut self) {
        self.get_reader_mut().close();
    }

    pub(crate) fn read_char(&mut self) -> PortChar {
        match self {
            Self::ByteVector(_) => Err(PortError::ExpectedMode(PortMode::Textual)),
            Self::File(r) => r.read_char(),
            Self::In(_) => todo!(),
            Self::String(r) => r.read(),
        }
    }

    pub(crate) fn peek_char(&mut self) -> PortChar {
        match self {
            Self::ByteVector(_) => Err(PortError::ExpectedMode(PortMode::Textual)),
            Self::File(r) => r.peek_char(),
            Self::In(_) => todo!(),
            Self::String(r) => r.peek(),
        }
    }

    pub(crate) fn read_line(&mut self) -> PortString {
        match self {
            Self::ByteVector(_) => Err(PortError::ExpectedMode(PortMode::Textual)),
            Self::File(r) => r.read_line(),
            Self::In(_) => todo!(),
            Self::String(r) => r.read_line(),
        }
    }

    pub(crate) fn char_ready(&self) -> PortBool {
        match self {
            Self::ByteVector(_) => Err(PortError::ExpectedMode(PortMode::Textual)),
            Self::File(r) => r.read_ready(),
            Self::In(_) => todo!(),
            Self::String(r) => r.ready(),
        }
    }

    pub(crate) fn read_chars(&mut self, k: usize) -> PortString {
        match self {
            Self::ByteVector(_) => Err(PortError::ExpectedMode(PortMode::Textual)),
            Self::File(r) => r.read_chars(k),
            Self::In(_) => todo!(),
            Self::String(r) => r.read_count(k),
        }
    }

    pub(crate) fn read_byte(&mut self) -> PortByte {
        match self {
            Self::ByteVector(r) => r.read(),
            Self::File(r) => r.read_byte(),
            Self::In(_) | Self::String(_) => Err(PortError::ExpectedMode(PortMode::Binary)),
        }
    }

    pub(crate) fn peek_byte(&mut self) -> PortByte {
        match self {
            Self::ByteVector(r) => r.peek(),
            Self::File(r) => r.peek_byte(),
            Self::In(_) | Self::String(_) => Err(PortError::ExpectedMode(PortMode::Binary)),
        }
    }

    pub(crate) fn byte_ready(&self) -> PortBool {
        match self {
            Self::ByteVector(r) => r.ready(),
            Self::File(r) => r.read_ready(),
            Self::In(_) | Self::String(_) => Err(PortError::ExpectedMode(PortMode::Binary)),
        }
    }

    pub(crate) fn read_bytes(&mut self, k: usize) -> PortBytes {
        match self {
            Self::ByteVector(r) => r.read_count(k),
            Self::File(r) => r.read_bytes(k),
            Self::In(_) | Self::String(_) => Err(PortError::ExpectedMode(PortMode::Binary)),
        }
    }

    fn spec(&self) -> PortSpec {
        match self {
            Self::ByteVector(_) => PortSpec::BinaryInput,
            Self::File(..) => PortSpec::Input,
            Self::In(_) | Self::String(_) => PortSpec::TextualInput,
        }
    }

    fn get_reader(&self) -> &dyn Reader {
        match self {
            Self::ByteVector(r) => r as &dyn Reader,
            Self::File(r) => r as &dyn Reader,
            Self::In(_) => todo!(),
            Self::String(r) => r as &dyn Reader,
        }
    }

    fn get_reader_mut(&mut self) -> &mut dyn Reader {
        match self {
            Self::ByteVector(r) => r as &mut dyn Reader,
            Self::File(r) => r as &mut dyn Reader,
            Self::In(_) => todo!(),
            Self::String(r) => r as &mut dyn Reader,
        }
    }
}

impl Display for ReadPort {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::ByteVector(_) => f.write_str(TypeName::BYTEVECTOR),
            Self::File(r) => write_file_source(r.path.display(), 'r', f),
            Self::In(_) => f.write_str("stdin"),
            Self::String(_) => f.write_str(TypeName::STRING),
        }?;
        write_port_status(self.is_open(), f)
    }
}

/*
 * Unlike io::Write, the io::Read (and buffered) traits don't map neatly to
 * Scheme's input functions, so we roll our own implementations for each port type.
 */

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

    fn read(&mut self) -> PortByte {
        self.get_byte(true)
    }

    fn read_count(&mut self, k: usize) -> PortBytes {
        self.get_bytes(k, true)
    }

    fn peek(&mut self) -> PortByte {
        self.get_byte(false)
    }

    fn ready(&self) -> PortBool {
        // TODO: experimental ok_or https://doc.rust-lang.org/std/primitive.bool.html#method.ok_or
        if self.is_open() {
            Ok(true)
        } else {
            Err(PortError::Closed)
        }
    }

    fn get_byte(&mut self, advance: bool) -> PortByte {
        let bytes = self.get_bytes(1, advance)?;
        Ok(bytes.and_then(|b| b.first().copied()))
    }

    fn get_bytes(&mut self, k: usize, advance: bool) -> PortBytes {
        match &mut self.buf {
            None => Err(PortError::Closed),
            Some(buf) => Ok(if k == 0 {
                Some(Vec::new())
            } else if let Some(max) = buf.len().checked_sub(self.cur)
                && max > 0
            {
                match buf.get(self.cur..(self.cur + cmp::min(k, max))) {
                    None => None,
                    Some(slice) => {
                        if advance {
                            self.cur += k;
                        }
                        Some(slice.to_vec())
                    }
                }
            } else {
                None
            }),
        }
    }
}

#[derive(Debug)]
pub(crate) struct FileReader {
    eof: bool,
    file: Option<BufReader<File>>,
    path: PathBuf,
}

impl FileReader {
    fn new(path: impl Into<PathBuf>) -> PortResult<Self> {
        let path = path.into();
        let f = File::open(&path)?;
        Ok(Self {
            eof: false,
            file: Some(BufReader::new(f)),
            path,
        })
    }

    fn read_byte(&mut self) -> PortByte {
        let bytes = self.get_bytes(1, true)?;
        Ok(bytes.and_then(|b| b.first().copied()))
    }

    fn peek_byte(&mut self) -> PortByte {
        let bytes = self.get_bytes(1, false)?;
        Ok(bytes.and_then(|b| b.first().copied()))
    }

    fn read_ready(&self) -> PortBool {
        match &self.file {
            None => Err(PortError::Closed),
            Some(r) => Ok(self.eof || !r.buffer().is_empty()),
        }
    }

    fn read_bytes(&mut self, k: usize) -> PortBytes {
        self.get_bytes(k, true)
    }

    fn read_char(&mut self) -> PortChar {
        self.get_char(true)
    }

    fn peek_char(&mut self) -> PortChar {
        self.get_char(false)
    }

    fn read_chars(&mut self, k: usize) -> PortString {
        if k == 0 {
            Ok(Some(String::new()))
        } else if self.eof {
            Ok(None)
        } else {
            let buf = iter::from_fn(|| self.read_char().transpose())
                .take(k)
                .collect::<PortResult<Vec<_>>>()?
                .into_iter()
                .collect::<String>();
            Ok(if buf.is_empty() { None } else { Some(buf) })
        }
    }

    fn read_line(&mut self) -> PortString {
        match &mut self.file {
            None => Err(PortError::Closed),
            Some(r) => {
                let mut buf = String::new();
                let c = r.read_line(&mut buf)?;
                Ok(if c == 0 {
                    None
                } else {
                    if buf.ends_with('\n') {
                        buf.pop();
                    }
                    Some(buf)
                })
            }
        }
    }

    fn get_bytes(&mut self, k: usize, advance: bool) -> PortBytes {
        match &self.file {
            None => Err(PortError::Closed),
            Some(_) => {
                if k == 0 {
                    Ok(Some(Vec::new()))
                } else if self.eof {
                    Ok(None)
                } else {
                    self.read_buffer(k, advance)
                }
            }
        }
    }

    fn get_char(&mut self, advance: bool) -> PortChar {
        Ok(match self.peek_byte()? {
            None => None,
            Some(prefix) => match self.get_bytes(unicode::utf8_char_len(prefix)?, advance)? {
                None => None,
                Some(seq) => Some(unicode::char_from_utf8(&seq)?),
            },
        })
    }

    fn read_buffer(&mut self, mut k: usize, advance: bool) -> PortBytes {
        let mut bytes = Vec::new();
        let r = self.file.as_mut().expect("expected active file handle");
        while k > 0 {
            if r.buffer().is_empty() {
                let fill = r.fill_buf()?;
                if fill.is_empty() {
                    self.eof = true;
                    break;
                }
            }
            let buf = r.buffer();
            let max = cmp::min(k, buf.len());
            if max > 0 {
                if let Some(slice) = buf.get(..max) {
                    bytes.extend_from_slice(slice);
                }
                if advance {
                    r.consume(max);
                }
                if max <= k {
                    k -= max;
                }
            }
        }
        Ok(if bytes.is_empty() { None } else { Some(bytes) })
    }
}

// TODO: https://doc.rust-lang.org/std/string/struct.String.html#method.into_chars
#[derive(Debug)]
pub(crate) struct StringReader(BvReader);

impl StringReader {
    fn new(s: impl Into<String>) -> Self {
        Self(BvReader::new(s.into().into_bytes()))
    }

    fn read(&mut self) -> PortChar {
        self.get_char(true)
    }

    fn read_count(&mut self, k: usize) -> PortString {
        if k == 0 {
            Ok(Some(String::new()))
        } else {
            let buf = iter::from_fn(|| self.read().transpose())
                .take(k)
                .collect::<PortResult<Vec<_>>>()?
                .into_iter()
                .collect::<String>();
            Ok(if buf.is_empty() { None } else { Some(buf) })
        }
    }

    fn peek(&mut self) -> PortChar {
        self.get_char(false)
    }

    fn ready(&self) -> PortBool {
        self.0.ready()
    }

    fn read_line(&mut self) -> PortString {
        let buf = iter::from_fn(|| self.read().transpose())
            .take_while(|item| item.as_ref().map_or(true, |ch| *ch != '\n'))
            .collect::<PortResult<Vec<_>>>()?
            .into_iter()
            .collect::<String>();
        Ok(if buf.is_empty() { None } else { Some(buf) })
    }

    fn get_char(&mut self, advance: bool) -> PortChar {
        Ok(match self.0.peek()? {
            None => None,
            Some(prefix) => match self.0.get_bytes(unicode::utf8_char_len(prefix)?, advance)? {
                None => None,
                Some(seq) => Some(unicode::char_from_utf8(&seq)?),
            },
        })
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

    pub(super) fn file(path: impl Into<PathBuf>, mode: FileMode) -> PortResult<Self> {
        let p = path.into();
        Ok(Self::File(Some(BufWriter::new(mode.open(&p)?)), p))
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

    pub(crate) fn get_bytevector(&self) -> PortValue {
        if let Self::ByteVector(w) = self {
            match w {
                None => Err(PortError::Closed),
                Some(v) => Ok(Value::bytevector_mut(v.iter().copied())),
            }
        } else {
            Err(PortError::InvalidSource)
        }
    }

    pub(crate) fn get_string(&self) -> PortValue {
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
        if missing_newline && let Self::Err(_, true) | Self::Out(_, true) = self {
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
    InvalidPath,
    InvalidSource,
    Io(ErrorKind),
    Unicode(UnicodeError),
}

impl PortError {
    pub(super) fn to_symbol(&self, sym: &SymbolTable) -> Value {
        Value::Symbol(match self {
            Self::Closed => sym.get("closed-port"),
            Self::ExpectedMode(_) => sym.get("mismatched-port-mode"),
            Self::Fmt => sym.get("string-error"),
            Self::InvalidPath => sym.get("invalid-path-representation"),
            Self::InvalidSource => sym.get("invalid-port-source"),
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
            Self::Unicode(_) => sym.get("unicode-error"),
        })
    }
}

impl Display for PortError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Closed => f.write_str("attempted operation on closed port"),
            Self::ExpectedMode(m) => write!(f, "attemped {} operation on {m} port", m.inverse()),
            Self::Fmt => f.write_str("unexpected format error"),
            Self::InvalidPath => f.write_str("file path has no valid unicode representation"),
            Self::InvalidSource => f.write_str("invalid port for requested data type"),
            Self::Io(k) => write!(f, "{k}"),
            Self::Unicode(u) => write!(f, "{u}"),
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

impl From<UnicodeError> for PortError {
    fn from(value: UnicodeError) -> Self {
        Self::Unicode(value)
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

pub(crate) enum FileMode {
    Append,
    Truncate,
}

impl FileMode {
    fn open(&self, p: impl AsRef<Path>) -> io::Result<File> {
        match self {
            Self::Append => OpenOptions::new().append(true).open(p),
            Self::Truncate => File::create(p),
        }
    }
}

trait Reader {
    fn is_open(&self) -> bool;
    fn close(&mut self);
}

impl Reader for BvReader {
    fn is_open(&self) -> bool {
        self.buf.is_some()
    }

    fn close(&mut self) {
        self.buf.take();
    }
}

impl Reader for FileReader {
    fn is_open(&self) -> bool {
        self.file.is_some()
    }

    fn close(&mut self) {
        self.file.take();
    }
}

impl Reader for StringReader {
    fn is_open(&self) -> bool {
        self.0.is_open()
    }

    fn close(&mut self) {
        self.0.close();
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
    fn bv_empty_read_byte() {
        let mut p = ReadPort::bytevector(Vec::new());

        let r = p.read_byte();

        let b = ok_or_fail!(r);
        assert!(b.is_none());
    }

    #[test]
    fn bv_read_byte() {
        let mut p = ReadPort::bytevector([1, 2, 3]);

        let r = p.read_byte();

        let b = some_or_fail!(ok_or_fail!(r));
        assert_eq!(b, 1);

        let r = p.read_byte();

        let b = some_or_fail!(ok_or_fail!(r));
        assert_eq!(b, 2);

        let r = p.read_byte();

        let b = some_or_fail!(ok_or_fail!(r));
        assert_eq!(b, 3);

        let r = p.read_byte();

        let b = ok_or_fail!(r);
        assert!(b.is_none());
    }

    #[test]
    fn bv_read_byte_when_closed() {
        let mut p = ReadPort::bytevector([1, 2, 3]);

        let r = p.read_byte();

        let b = some_or_fail!(ok_or_fail!(r));
        assert_eq!(b, 1);

        p.close();
        let r = p.read_byte();

        let e = err_or_fail!(r);
        assert!(matches!(e, PortError::Closed));
    }

    #[test]
    fn bv_byte_ready() {
        let mut p = ReadPort::bytevector([1]);

        assert!(ok_or_fail!(p.byte_ready()));

        let r = p.read_byte();
        let b = some_or_fail!(ok_or_fail!(r));

        assert_eq!(b, 1);
        assert!(ok_or_fail!(p.byte_ready()));

        let r = p.read_byte();
        let b = ok_or_fail!(r);

        assert!(b.is_none());

        p.close();
        let r = p.byte_ready();

        let e = err_or_fail!(r);
        assert!(matches!(e, PortError::Closed));
    }

    #[test]
    fn bv_empty_read_bytes() {
        let mut p = ReadPort::bytevector(Vec::new());

        let r = p.read_bytes(3);

        let b = ok_or_fail!(r);
        assert!(b.is_none());
    }

    #[test]
    fn bv_read_all_bytes() {
        let mut p = ReadPort::bytevector([1, 2, 3]);

        let r = p.read_bytes(3);

        let b = some_or_fail!(ok_or_fail!(r));
        assert_eq!(*b, [1, 2, 3]);
    }

    #[test]
    fn bv_read_no_bytes() {
        let mut p = ReadPort::bytevector([1, 2, 3]);

        let r = p.read_bytes(0);

        let b = some_or_fail!(ok_or_fail!(r));
        assert_eq!(*b, []);
    }

    #[test]
    fn bv_read_some_bytes_from_start() {
        let mut p = ReadPort::bytevector([1, 2, 3, 4, 5]);

        let r = p.read_bytes(3);

        let b = some_or_fail!(ok_or_fail!(r));
        assert_eq!(*b, [1, 2, 3]);
    }

    #[test]
    fn bv_read_bytes_subset() {
        let mut p = ReadPort::bytevector([1, 2, 3, 4, 5]);

        let _ = p.read_byte();

        let r = p.read_bytes(3);

        let b = some_or_fail!(ok_or_fail!(r));
        assert_eq!(*b, [2, 3, 4]);
    }

    #[test]
    fn bv_read_too_many_bytes() {
        let mut p = ReadPort::bytevector([1, 2, 3]);

        let r = p.read_bytes(5);

        let b = some_or_fail!(ok_or_fail!(r));
        assert_eq!(*b, [1, 2, 3]);
    }

    #[test]
    fn bv_read_too_many_bytes_subset() {
        let mut p = ReadPort::bytevector([1, 2, 3, 4, 5]);

        let _ = p.read_byte();
        let _ = p.read_byte();

        let r = p.read_bytes(5);

        let b = some_or_fail!(ok_or_fail!(r));
        assert_eq!(*b, [3, 4, 5]);
    }

    #[test]
    fn bv_read_bytes_out_of_bytes() {
        let mut p = ReadPort::bytevector([1, 2, 3]);

        let r = p.read_bytes(5);

        let b = some_or_fail!(ok_or_fail!(r));
        assert_eq!(*b, [1, 2, 3]);

        let r = p.read_bytes(5);

        let b = ok_or_fail!(r);
        assert!(b.is_none());
    }

    #[test]
    fn bv_read_no_bytes_after_eof() {
        let mut p = ReadPort::bytevector([1, 2, 3]);

        let r = p.read_bytes(5);

        let b = some_or_fail!(ok_or_fail!(r));
        assert_eq!(*b, [1, 2, 3]);

        let r = p.read_bytes(0);

        let b = some_or_fail!(ok_or_fail!(r));
        assert_eq!(*b, []);
    }
}
