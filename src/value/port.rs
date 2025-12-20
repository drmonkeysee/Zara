mod datum;

use super::{TypeName, Value};
use crate::{
    ReadError,
    eval::Frame,
    string::{
        SymbolTable,
        unicode::{self, UnicodeError},
    },
};
use std::{
    cmp,
    fmt::{self, Display, Formatter, Write},
    fs::{File, OpenOptions},
    io::{self, BufRead, BufReader, BufWriter, ErrorKind, Seek, SeekFrom, Stderr, Stdin, Stdout},
    iter,
    path::{Path, PathBuf},
};

pub(crate) type PortResult<T = ()> = Result<T, PortError>;
pub(crate) type PortBool = PortResult<bool>;
pub(crate) type PortPosition = PortResult<usize>;
pub(crate) type PortValue = PortResult<Value>;
pub(crate) type PortChar = PortResult<Option<char>>;
pub(crate) type PortString = PortResult<Option<String>>;
pub(crate) type PortDatum = PortResult<Option<Value>>;
pub(crate) type PortByte = PortResult<Option<u8>>;
pub(crate) type PortBytes = PortResult<Option<Vec<u8>>>;

#[derive(Debug)]
pub(crate) enum ReadPort {
    ByteVector(BvReader),
    File(FileReader),
    In(StdinReader),
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
        Self::In(StdinReader::new())
    }

    pub(super) fn string(s: impl Into<String>) -> Self {
        Self::String(StringReader::new(s))
    }

    pub(crate) fn is_binary(&self) -> bool {
        matches!(self, Self::ByteVector(_) | Self::File(_))
    }

    pub(crate) fn is_textual(&self) -> bool {
        matches!(self, Self::File(_) | Self::In(_) | Self::String(_))
    }

    pub(crate) fn is_open(&self) -> bool {
        self.get_reader().is_open()
    }

    pub(crate) fn is_seekable(&self) -> bool {
        self.is_binary()
    }

    pub(crate) fn char_ready(&self) -> PortBool {
        match self {
            Self::ByteVector(_) => Err(PortError::ExpectedMode(PortMode::Textual)),
            Self::File(r) => r.read_ready(),
            Self::In(r) => r.ready(),
            Self::String(r) => r.ready(),
        }
    }

    pub(crate) fn byte_ready(&self) -> PortBool {
        match self {
            Self::ByteVector(r) => r.ready(),
            Self::File(r) => r.read_ready(),
            Self::In(_) | Self::String(_) => Err(PortError::ExpectedMode(PortMode::Binary)),
        }
    }

    pub(crate) fn tell(&mut self) -> PortPosition {
        match self {
            Self::ByteVector(r) => r.tell(),
            Self::File(r) => r.tell(),
            Self::In(_) | Self::String(_) => Err(PortError::Io(ErrorKind::NotSeekable)),
        }
    }

    pub(crate) fn seek(&mut self, pos: PortSeek) -> PortPosition {
        match self {
            Self::ByteVector(r) => r.seek(pos),
            Self::File(r) => r.seek(pos),
            Self::In(_) | Self::String(_) => Err(PortError::Io(ErrorKind::NotSeekable)),
        }
    }

    pub(crate) fn close(&mut self) {
        self.get_reader_mut().close();
    }

    pub(crate) fn read_char(&mut self) -> PortChar {
        self.get_char_reader_mut()?.read_char()
    }

    pub(crate) fn peek_char(&mut self) -> PortChar {
        self.get_char_reader_mut()?.peek_char()
    }

    pub(crate) fn read_line(&mut self) -> PortString {
        match self {
            Self::ByteVector(_) => Err(PortError::ExpectedMode(PortMode::Textual)),
            Self::File(r) => r.read_line(),
            Self::In(r) => r.read_line(),
            Self::String(r) => r.read_line(),
        }
    }

    pub(crate) fn read_chars(&mut self, k: usize) -> PortString {
        match self {
            Self::ByteVector(_) => Err(PortError::ExpectedMode(PortMode::Textual)),
            Self::File(r) => r.read_chars(k),
            Self::In(r) => r.read_count(k),
            Self::String(r) => r.read_count(k),
        }
    }

    pub(crate) fn read_datum(&mut self, env: &Frame) -> PortDatum {
        if self.is_open() {
            let label = self.to_string();
            datum::parse(self.get_char_reader_mut()?, env, label)
        } else {
            Err(PortError::Closed)
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
            Self::In(r) => r as &dyn Reader,
            Self::String(r) => r as &dyn Reader,
        }
    }

    fn get_reader_mut(&mut self) -> &mut dyn Reader {
        match self {
            Self::ByteVector(r) => r as &mut dyn Reader,
            Self::File(r) => r as &mut dyn Reader,
            Self::In(r) => r as &mut dyn Reader,
            Self::String(r) => r as &mut dyn Reader,
        }
    }

    fn get_char_reader_mut(&mut self) -> PortResult<&mut dyn CharReader> {
        match self {
            Self::ByteVector(_) => Err(PortError::ExpectedMode(PortMode::Textual)),
            Self::File(r) => Ok(r as &mut dyn CharReader),
            Self::In(r) => Ok(r as &mut dyn CharReader),
            Self::String(r) => Ok(r as &mut dyn CharReader),
        }
    }
}

impl Display for ReadPort {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write_port_start(f)?;
        match self {
            Self::ByteVector(_) => f.write_str(TypeName::BYTEVECTOR),
            Self::File(r) => write_file_source(r.path.display(), 'r', f),
            Self::In(_) => f.write_str("stdin"),
            Self::String(_) => f.write_str(TypeName::STRING),
        }?;
        write_port_status(self.is_open(), f)?;
        write_port_end(f)
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

    fn ready(&self) -> PortBool {
        // TODO: experimental ok_or https://doc.rust-lang.org/std/primitive.bool.html#method.ok_or
        if self.is_open() {
            Ok(true)
        } else {
            Err(PortError::Closed)
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

    fn tell(&mut self) -> PortPosition {
        if self.is_open() {
            Ok(self.cur)
        } else {
            Err(PortError::Closed)
        }
    }

    fn seek(&mut self, pos: PortSeek) -> PortPosition {
        match &self.buf {
            None => Err(PortError::Closed),
            Some(b) => {
                self.cur = match pos {
                    PortSeek::Start(p) => usize::MIN.checked_add_signed(p),
                    PortSeek::Current(p) => self.cur.checked_add_signed(p),
                    PortSeek::End(p) => b.len().checked_add_signed(p),
                }
                .ok_or(ErrorKind::InvalidInput)?;
                self.tell()
            }
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

    fn read_ready(&self) -> PortBool {
        match &self.file {
            None => Err(PortError::Closed),
            Some(f) => Ok(self.eof || !f.buffer().is_empty()),
        }
    }

    fn read_byte(&mut self) -> PortByte {
        let bytes = self.get_bytes(1, true)?;
        Ok(bytes.and_then(|b| b.first().copied()))
    }

    fn peek_byte(&mut self) -> PortByte {
        let bytes = self.get_bytes(1, false)?;
        Ok(bytes.and_then(|b| b.first().copied()))
    }

    fn tell(&mut self) -> PortPosition {
        match &mut self.file {
            None => Err(PortError::Closed),
            Some(f) => seekable_position(f),
        }
    }

    fn seek(&mut self, pos: PortSeek) -> PortPosition {
        match &mut self.file {
            None => Err(PortError::Closed),
            Some(f) => set_seekable_position(f, pos),
        }
    }

    fn read_bytes(&mut self, k: usize) -> PortBytes {
        self.get_bytes(k, true)
    }

    fn read_chars(&mut self, k: usize) -> PortString {
        if k == 0 {
            Ok(Some(String::new()))
        } else if self.eof {
            Ok(None)
        } else {
            extract_string(iter::from_fn(|| self.read_char().transpose()), |it| {
                it.take(k)
            })
        }
    }

    fn read_line(&mut self) -> PortString {
        match &mut self.file {
            None => Err(PortError::Closed),
            Some(f) => {
                let mut buf = String::new();
                Ok(if f.read_line(&mut buf)? > 0 {
                    if buf.ends_with('\n') {
                        buf.pop();
                    }
                    Some(buf)
                } else {
                    None
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
        extract_char(self, Self::peek_byte, Self::get_bytes, advance)
    }

    fn read_buffer(&mut self, mut k: usize, advance: bool) -> PortBytes {
        let mut bytes = Vec::new();
        let f = self
            .file
            .as_mut()
            .expect("open port should have active file handle");
        while k > 0 {
            if f.buffer().is_empty() {
                let fill = f.fill_buf()?;
                if fill.is_empty() {
                    self.eof = true;
                    break;
                }
            }
            let buf = f.buffer();
            let max = cmp::min(k, buf.len());
            if max > 0 {
                if let Some(slice) = buf.get(..max) {
                    bytes.extend_from_slice(slice);
                }
                if advance {
                    f.consume(max);
                }
                if max <= k {
                    k -= max;
                }
            }
        }
        Ok(if bytes.is_empty() { None } else { Some(bytes) })
    }
}

#[derive(Debug)]
pub(crate) struct StdinReader {
    rbuf: String,
    stream: Option<Stdin>,
}

impl StdinReader {
    const BUFFER_CAPACITY: usize = 1 << 13; // 8KB

    fn new() -> Self {
        Self {
            rbuf: String::new(),
            stream: Some(io::stdin()),
        }
    }

    fn ready(&self) -> PortBool {
        match &self.stream {
            None => Err(PortError::Closed),
            Some(_) => Ok(!self.rbuf.is_empty()),
        }
    }

    fn read_count(&mut self, k: usize) -> PortString {
        if k == 0 {
            Ok(Some(String::new()))
        } else {
            extract_string(iter::from_fn(|| self.read_char().transpose()), |it| {
                it.take(k)
            })
        }
    }

    fn read_line(&mut self) -> PortString {
        match &mut self.stream {
            None => Err(PortError::Closed),
            Some(r) => {
                // NOTE: utf-8 encoding should ensure that any 0xa is a valid '\n'
                let r = Ok(match self.rbuf.bytes().position(|b| b == 0xa) {
                    None => {
                        let mut buf = self.rbuf.split_off(0).chars().rev().collect::<String>();
                        r.read_line(&mut buf)?;
                        if buf.is_empty() {
                            None
                        } else {
                            if buf.ends_with('\n') {
                                buf.pop();
                            }
                            Some(buf)
                        }
                    }
                    Some(at) => {
                        let bufnl = self.rbuf.split_off(at);
                        Some(bufnl[1..].chars().rev().collect::<String>())
                    }
                });
                self.trim_buffer();
                r
            }
        }
    }

    fn trim_buffer(&mut self) {
        if self.rbuf.capacity() > Self::BUFFER_CAPACITY && self.rbuf.len() < self.rbuf.capacity() {
            self.rbuf.shrink_to_fit();
        }
    }
}

// TODO: https://doc.rust-lang.org/std/string/struct.String.html#method.into_chars
#[derive(Debug)]
pub(crate) struct StringReader(BvReader);

impl StringReader {
    fn new(s: impl Into<String>) -> Self {
        Self(BvReader::new(s.into().into_bytes()))
    }

    fn ready(&self) -> PortBool {
        self.0.ready()
    }

    fn read_count(&mut self, k: usize) -> PortString {
        if k == 0 {
            Ok(Some(String::new()))
        } else {
            extract_string(self.char_stream(), |it| it.take(k))
        }
    }

    fn read_line(&mut self) -> PortString {
        extract_string(self.char_stream(), |it| {
            it.take_while(|item| item.as_ref().map_or(true, |ch| *ch != '\n'))
        })
    }

    fn get_char(&mut self, advance: bool) -> PortChar {
        extract_char(&mut self.0, BvReader::peek, BvReader::get_bytes, advance)
    }

    fn char_stream(&mut self) -> impl Iterator<Item = PortExtractChar> {
        iter::from_fn(|| self.read_char().transpose())
    }
}

#[derive(Debug)]
pub(crate) enum WritePort {
    ByteVector(Option<BvWriter>),
    Err(Option<Stderr>, bool),
    File(Option<BufWriter<File>>, PathBuf),
    Out(Option<Stdout>, bool),
    String(Option<String>),
}

impl WritePort {
    pub(super) fn bytevector() -> Self {
        Self::ByteVector(Some(BvWriter::new()))
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
        matches!(self, Self::ByteVector(_) | Self::File(..))
    }

    pub(crate) fn is_textual(&self) -> bool {
        matches!(
            self,
            Self::Err(..) | Self::File(..) | Self::Out(..) | Self::String(_)
        )
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

    pub(crate) fn is_seekable(&self) -> bool {
        self.is_binary()
    }

    pub(crate) fn get_bytevector(&self) -> PortValue {
        if let Self::ByteVector(w) = self {
            match w {
                None => Err(PortError::Closed),
                Some(v) => Ok(v.to_bv()),
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

    pub(crate) fn tell(&mut self) -> PortPosition {
        seekable_position(self.get_seekable()?)
    }

    pub(crate) fn seek(&mut self, pos: PortSeek) -> PortPosition {
        set_seekable_position(self.get_seekable()?, pos)
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

    fn get_seekable(&mut self) -> PortResult<&mut dyn Seek> {
        match self {
            Self::ByteVector(o) => o.as_mut().map(|f| Ok(f as &mut dyn Seek)),
            Self::File(o, _) => o.as_mut().map(|f| Ok(f as &mut dyn Seek)),
            _ => Some(Err(PortError::Io(ErrorKind::NotSeekable))),
        }
        .unwrap_or(Err(PortError::Closed))
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
        write_port_start(f)?;
        match self {
            Self::ByteVector(_) => f.write_str(TypeName::BYTEVECTOR),
            Self::Err(..) => f.write_str("stderr"),
            Self::File(_, p) => write_file_source(p.display(), 'w', f),
            Self::Out(..) => f.write_str("stdout"),
            Self::String(_) => f.write_str(TypeName::STRING),
        }?;
        write_port_status(self.is_open(), f)?;
        write_port_end(f)
    }
}

#[derive(Debug)]
pub(crate) struct BvWriter {
    bytes: Vec<u8>,
    cur: usize,
}

impl BvWriter {
    fn new() -> Self {
        Self {
            bytes: Vec::new(),
            cur: usize::MIN,
        }
    }

    fn to_bv(&self) -> Value {
        Value::bytevector_mut(self.bytes.iter().copied())
    }
}

impl io::Write for BvWriter {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        let r = if self.cur < self.bytes.len() {
            let mut slice = &mut self.bytes[self.cur..];
            slice.write(buf)
        } else {
            if self.cur > self.bytes.len() {
                let mut gap =
                    iter::repeat_n(0x0u8, self.cur - self.bytes.len()).collect::<Vec<_>>();
                self.bytes.append(&mut gap);
            }
            self.bytes.write(buf)
        };
        if let Ok(n) = r {
            self.cur += n;
        }
        r
    }

    fn flush(&mut self) -> io::Result<()> {
        self.bytes.flush()
    }
}

impl Seek for BvWriter {
    fn seek(&mut self, pos: SeekFrom) -> io::Result<u64> {
        let curr = u64::try_from(self.cur).map_err(|_| ErrorKind::InvalidData)?;
        let end = u64::try_from(self.bytes.len()).map_err(|_| ErrorKind::InvalidData)?;
        let new = match pos {
            SeekFrom::Start(p) => Some(p),
            SeekFrom::Current(p) => curr.checked_add_signed(p),
            SeekFrom::End(p) => end.checked_add_signed(p),
        }
        .ok_or(ErrorKind::InvalidInput)?;
        self.cur = new.try_into().map_err(|_| ErrorKind::InvalidInput)?;
        Ok(new)
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
    Read(ReadError),
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
                ErrorKind::InvalidInput => sym.get("invalid-input"),
                ErrorKind::IsADirectory => sym.get("is-directory"),
                ErrorKind::NotADirectory => sym.get("not-directory"),
                ErrorKind::NotFound => sym.get("not-found"),
                ErrorKind::NotSeekable => sym.get("not-seekable"),
                ErrorKind::PermissionDenied => sym.get("permission-denied"),
                ErrorKind::ReadOnlyFilesystem => sym.get("read-only-filesystem"),
                ErrorKind::StorageFull => sym.get("storage-full"),
                ErrorKind::TimedOut => sym.get("timeout"),
                ErrorKind::UnexpectedEof => sym.get("unexpected-eof"),
                _ => sym.get("nonspecific-error"),
            },
            Self::Read(_) => sym.get("read-error"),
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
            Self::Io(k) => k.fmt(f),
            Self::Read(err) => err.fmt(f),
            Self::Unicode(u) => u.fmt(f),
        }
    }
}

impl From<fmt::Error> for PortError {
    fn from(_value: fmt::Error) -> Self {
        Self::Fmt
    }
}

impl From<ErrorKind> for PortError {
    fn from(value: ErrorKind) -> Self {
        Self::Io(value)
    }
}

impl From<io::Error> for PortError {
    fn from(value: io::Error) -> Self {
        value.kind().into()
    }
}

impl From<ReadError> for PortError {
    fn from(value: ReadError) -> Self {
        Self::Read(value)
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

#[derive(Clone, Copy)]
pub(crate) enum PortSeek {
    Start(isize),
    Current(isize),
    End(isize),
}

impl TryFrom<PortSeek> for SeekFrom {
    type Error = PortError;

    fn try_from(value: PortSeek) -> Result<Self, Self::Error> {
        Ok(match value {
            PortSeek::Start(p) => Self::Start(p.try_into().map_err(|_| ErrorKind::InvalidInput)?),
            PortSeek::Current(p) => {
                Self::Current(p.try_into().map_err(|_| ErrorKind::InvalidInput)?)
            }
            PortSeek::End(p) => Self::End(p.try_into().map_err(|_| ErrorKind::InvalidInput)?),
        })
    }
}

#[derive(Clone, Copy)]
pub(crate) enum FileMode {
    Append,
    Truncate,
}

impl FileMode {
    fn open(self, p: impl AsRef<Path>) -> io::Result<File> {
        match self {
            Self::Append => OpenOptions::new().append(true).open(p),
            Self::Truncate => File::create(p),
        }
    }
}

type PortExtractChar = PortResult<char>;

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

impl Reader for StdinReader {
    fn is_open(&self) -> bool {
        self.stream.is_some()
    }

    fn close(&mut self) {
        self.stream.take();
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

trait CharReader {
    fn read_char(&mut self) -> PortChar;
    fn peek_char(&mut self) -> PortChar;
}

impl CharReader for FileReader {
    fn read_char(&mut self) -> PortChar {
        self.get_char(true)
    }

    fn peek_char(&mut self) -> PortChar {
        self.get_char(false)
    }
}

impl CharReader for StdinReader {
    fn read_char(&mut self) -> PortChar {
        match &mut self.stream {
            None => Err(PortError::Closed),
            Some(r) => {
                if self.rbuf.is_empty() {
                    let mut b = String::new();
                    r.read_line(&mut b)?;
                    self.rbuf.push_str(&(b.chars().rev().collect::<String>()));
                    self.trim_buffer();
                }
                Ok(self.rbuf.pop())
            }
        }
    }

    fn peek_char(&mut self) -> PortChar {
        let r = self.read_char();
        if let Ok(Some(c)) = r {
            self.rbuf.push(c);
        }
        r
    }
}

impl CharReader for StringReader {
    fn read_char(&mut self) -> PortChar {
        self.get_char(true)
    }

    fn peek_char(&mut self) -> PortChar {
        self.get_char(false)
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

fn extract_char<T>(
    reader: &mut T,
    peek: impl FnOnce(&mut T) -> PortByte,
    bytes: impl FnOnce(&mut T, usize, bool) -> PortBytes,
    advance: bool,
) -> PortChar {
    Ok(match peek(reader)? {
        None => None,
        Some(prefix) => match bytes(reader, unicode::utf8_char_len(prefix)?, advance)? {
            None => None,
            Some(seq) => Some(unicode::char_from_utf8(&seq)?),
        },
    })
}

fn extract_string<S: Iterator<Item = PortExtractChar>, R: Iterator<Item = PortExtractChar>>(
    stream: S,
    extract: impl FnOnce(S) -> R,
) -> PortString {
    let buf = extract(stream)
        .collect::<PortResult<Vec<_>>>()?
        .into_iter()
        .collect::<String>();
    Ok(if buf.is_empty() { None } else { Some(buf) })
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

fn write_port_start(f: &mut Formatter<'_>) -> fmt::Result {
    f.write_str("#<port ")
}

fn write_port_end(f: &mut Formatter<'_>) -> fmt::Result {
    f.write_char('>')
}

fn seekable_position(sk: &mut (impl Seek + ?Sized)) -> PortPosition {
    sk.stream_position()?
        .try_into()
        .map_err(|_| PortError::Io(ErrorKind::InvalidData))
}

fn set_seekable_position(sk: &mut (impl Seek + ?Sized), pos: PortSeek) -> PortPosition {
    sk.seek(pos.try_into()?)?
        .try_into()
        .map_err(|_| PortError::Io(ErrorKind::InvalidData))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::testutil::{err_or_fail, ok_or_fail, some_or_fail};

    mod binary {
        use super::*;

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

        #[test]
        fn bv_input_start_position() {
            let mut p = ReadPort::bytevector([1, 2, 3, 4, 5]);

            let r = p.tell();

            let pos = ok_or_fail!(r);
            assert_eq!(pos, 0);
        }

        #[test]
        fn bv_input_position_moves_on_read() {
            let mut p = ReadPort::bytevector([1, 2, 3, 4, 5]);

            let _ = p.read_byte();
            let _ = p.read_byte();

            let r = p.tell();

            let pos = ok_or_fail!(r);
            assert_eq!(pos, 2);
        }

        #[test]
        fn bv_input_seek_from_start() {
            let mut p = ReadPort::bytevector([1, 2, 3, 4, 5]);

            let r = p.seek(PortSeek::Start(2));

            let pos = ok_or_fail!(r);
            assert_eq!(pos, 2);
        }

        #[test]
        fn bv_input_seek_from_current() {
            let mut p = ReadPort::bytevector([1, 2, 3, 4, 5]);

            let _ = p.read_byte();
            let _ = p.read_byte();

            let r = p.seek(PortSeek::Current(1));

            let pos = ok_or_fail!(r);
            assert_eq!(pos, 3);
        }

        #[test]
        fn bv_input_seek_from_end() {
            let mut p = ReadPort::bytevector([1, 2, 3, 4, 5]);

            let _ = p.read_byte();
            let _ = p.read_byte();

            let r = p.seek(PortSeek::End(-1));

            let pos = ok_or_fail!(r);
            assert_eq!(pos, 4);
        }

        #[test]
        fn bv_input_seek_past_end() {
            let mut p = ReadPort::bytevector([1, 2, 3, 4, 5]);

            let _ = p.read_byte();
            let _ = p.read_byte();

            let r = p.seek(PortSeek::End(1));

            let pos = ok_or_fail!(r);
            assert_eq!(pos, 6);
            let b = ok_or_fail!(p.read_byte());
            assert!(b.is_none());
        }

        #[test]
        fn bv_input_seek_negative() {
            let mut p = ReadPort::bytevector([1, 2, 3, 4, 5]);

            let r = p.seek(PortSeek::Start(-1));

            let err = err_or_fail!(r);
            assert!(matches!(err, PortError::Io(ErrorKind::InvalidInput)));
        }

        #[test]
        fn bv_output_write_nothing() {
            let mut p = WritePort::bytevector();

            let r = p.put_bytes(&[]);

            assert!(r.is_ok());
            assert_eq!(ok_or_fail!(p.tell()), 0);
            let bv = ok_or_fail!(p.get_bytevector());
            assert_eq!(bv.as_datum().to_string(), "#u8()");
        }

        #[test]
        fn bv_output_write_bytes() {
            let mut p = WritePort::bytevector();

            let r = p.put_bytes(&[1, 2, 3]);

            assert!(r.is_ok());
            assert_eq!(ok_or_fail!(p.tell()), 3);
            let bv = ok_or_fail!(p.get_bytevector());
            assert_eq!(bv.as_datum().to_string(), "#u8(1 2 3)");
        }

        #[test]
        fn bv_output_write_inner_slice() {
            let mut p = WritePort::bytevector();

            let r = p.put_bytes(&[1, 2, 3, 4, 5]);

            assert!(r.is_ok());

            let r = p.seek(PortSeek::Start(1));

            assert_eq!(ok_or_fail!(r), 1);

            let r = p.put_bytes(&[9, 8, 7]);

            assert!(r.is_ok());
            assert_eq!(ok_or_fail!(p.tell()), 4);
            let bv = ok_or_fail!(p.get_bytevector());
            assert_eq!(bv.as_datum().to_string(), "#u8(1 9 8 7 5)");
        }

        #[test]
        fn bv_output_write_inner_slice_to_end() {
            let mut p = WritePort::bytevector();

            let r = p.put_bytes(&[1, 2, 3, 4, 5]);

            assert!(r.is_ok());

            let r = p.seek(PortSeek::Start(1));

            assert_eq!(ok_or_fail!(r), 1);

            let r = p.put_bytes(&[9, 8, 7, 6]);

            assert!(r.is_ok());
            assert_eq!(ok_or_fail!(p.tell()), 5);
            let bv = ok_or_fail!(p.get_bytevector());
            assert_eq!(bv.as_datum().to_string(), "#u8(1 9 8 7 6)");
        }

        #[test]
        fn bv_output_write_spillover_slice() {
            let mut p = WritePort::bytevector();

            let r = p.put_bytes(&[1, 2, 3, 4, 5]);

            assert!(r.is_ok());

            let r = p.seek(PortSeek::End(-2));

            assert_eq!(ok_or_fail!(r), 3);

            let r = p.put_bytes(&[9, 8, 7, 6]);

            assert!(r.is_ok());
            assert_eq!(ok_or_fail!(p.tell()), 7);
            let bv = ok_or_fail!(p.get_bytevector());
            assert_eq!(bv.as_datum().to_string(), "#u8(1 2 3 9 8 7 6)");
        }

        #[test]
        fn bv_output_write_past_end() {
            let mut p = WritePort::bytevector();

            let r = p.put_bytes(&[1, 2, 3, 4, 5]);

            assert!(r.is_ok());

            let r = p.seek(PortSeek::End(3));

            assert_eq!(ok_or_fail!(r), 8);

            let r = p.put_bytes(&[10]);

            assert!(r.is_ok());
            assert_eq!(ok_or_fail!(p.tell()), 9);
            let bv = ok_or_fail!(p.get_bytevector());
            assert_eq!(bv.as_datum().to_string(), "#u8(1 2 3 4 5 0 0 0 10)");
        }
    }

    mod textual {
        use super::*;

        #[test]
        fn str_empty_read_char() {
            let mut p = ReadPort::string("");

            let r = p.read_char();

            let c = ok_or_fail!(r);
            assert!(c.is_none());
        }

        #[test]
        fn str_read_char() {
            let mut p = ReadPort::string("abc");

            let r = p.read_char();

            let c = some_or_fail!(ok_or_fail!(r));
            assert_eq!(c, 'a');

            let r = p.read_char();

            let c = some_or_fail!(ok_or_fail!(r));
            assert_eq!(c, 'b');

            let r = p.read_char();

            let c = some_or_fail!(ok_or_fail!(r));
            assert_eq!(c, 'c');

            let r = p.read_char();

            let c = ok_or_fail!(r);
            assert!(c.is_none());
        }

        #[test]
        fn str_read_char_when_closed() {
            let mut p = ReadPort::string("abc");

            let r = p.read_char();

            let c = some_or_fail!(ok_or_fail!(r));
            assert_eq!(c, 'a');

            p.close();
            let r = p.read_char();

            let e = err_or_fail!(r);
            assert!(matches!(e, PortError::Closed));
        }

        #[test]
        fn str_char_ready() {
            let mut p = ReadPort::string("a");

            assert!(ok_or_fail!(p.char_ready()));

            let r = p.read_char();
            let c = some_or_fail!(ok_or_fail!(r));

            assert_eq!(c, 'a');
            assert!(ok_or_fail!(p.char_ready()));

            let r = p.read_char();
            let c = ok_or_fail!(r);

            assert!(c.is_none());

            p.close();
            let r = p.char_ready();

            let e = err_or_fail!(r);
            assert!(matches!(e, PortError::Closed));
        }

        #[test]
        fn str_empty_read_chars() {
            let mut p = ReadPort::string("");

            let r = p.read_chars(3);

            let s = ok_or_fail!(r);
            assert!(s.is_none());
        }

        #[test]
        fn str_read_all_chars() {
            let mut p = ReadPort::string("abc");

            let r = p.read_chars(3);

            let s = some_or_fail!(ok_or_fail!(r));
            assert_eq!(s, "abc");
        }

        #[test]
        fn str_read_no_chars() {
            let mut p = ReadPort::string("abc");

            let r = p.read_chars(0);

            let s = some_or_fail!(ok_or_fail!(r));
            assert_eq!(s, "");
        }

        #[test]
        fn str_read_some_chars_from_start() {
            let mut p = ReadPort::string("abcde");

            let r = p.read_chars(3);

            let s = some_or_fail!(ok_or_fail!(r));
            assert_eq!(s, "abc");
        }

        #[test]
        fn str_read_chars_subset() {
            let mut p = ReadPort::string("abcde");

            let _ = p.read_char();

            let r = p.read_chars(3);

            let s = some_or_fail!(ok_or_fail!(r));
            assert_eq!(s, "bcd");
        }

        #[test]
        fn str_read_too_many_chars() {
            let mut p = ReadPort::string("abc");

            let r = p.read_chars(5);

            let s = some_or_fail!(ok_or_fail!(r));
            assert_eq!(s, "abc");
        }

        #[test]
        fn str_read_too_many_chars_subset() {
            let mut p = ReadPort::string("abcde");

            let _ = p.read_char();
            let _ = p.read_char();

            let r = p.read_chars(5);

            let s = some_or_fail!(ok_or_fail!(r));
            assert_eq!(s, "cde");
        }

        #[test]
        fn str_read_chars_out_of_chars() {
            let mut p = ReadPort::string("abc");

            let r = p.read_chars(5);

            let s = some_or_fail!(ok_or_fail!(r));
            assert_eq!(s, "abc");

            let r = p.read_chars(5);

            let s = ok_or_fail!(r);
            assert!(s.is_none());
        }

        #[test]
        fn str_read_no_chars_after_eof() {
            let mut p = ReadPort::string("abc");

            let r = p.read_chars(5);

            let s = some_or_fail!(ok_or_fail!(r));
            assert_eq!(s, "abc");

            let r = p.read_chars(0);

            let s = some_or_fail!(ok_or_fail!(r));
            assert_eq!(s, "");
        }

        #[test]
        fn str_readline_empty() {
            let mut p = ReadPort::string("");

            let r = p.read_line();

            let s = ok_or_fail!(r);
            assert!(s.is_none());
        }

        #[test]
        fn str_readline_single_line() {
            let mut p = ReadPort::string("foobar");

            let r = p.read_line();

            let s = some_or_fail!(ok_or_fail!(r));
            assert_eq!(s, "foobar");
        }

        #[test]
        fn str_readline_multi_lines() {
            let mut p = ReadPort::string("foo\nbar\nbaz");

            let r = p.read_line();

            let s = some_or_fail!(ok_or_fail!(r));
            assert_eq!(s, "foo");

            let r = p.read_line();

            let s = some_or_fail!(ok_or_fail!(r));
            assert_eq!(s, "bar");

            let r = p.read_line();

            let s = some_or_fail!(ok_or_fail!(r));
            assert_eq!(s, "baz");

            let r = p.read_line();

            let s = ok_or_fail!(r);
            assert!(s.is_none());
        }

        #[test]
        fn str_readline_multi_lines_ends_with_newline() {
            let mut p = ReadPort::string("foo\nbar\nbaz\n");

            let r = p.read_line();

            let s = some_or_fail!(ok_or_fail!(r));
            assert_eq!(s, "foo");

            let r = p.read_line();

            let s = some_or_fail!(ok_or_fail!(r));
            assert_eq!(s, "bar");

            let r = p.read_line();

            let s = some_or_fail!(ok_or_fail!(r));
            assert_eq!(s, "baz");

            let r = p.read_line();

            let s = ok_or_fail!(r);
            assert!(s.is_none());
        }
    }
}
