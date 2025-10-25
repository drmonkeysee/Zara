use super::Value;
use std::{
    fmt::{self, Debug, Display, Formatter},
    io::{self, BufReader, BufWriter, Read, Write},
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
        matches!(self.mode, PortMode::Textual)
    }

    pub(crate) fn is_binary(&self) -> bool {
        matches!(self.mode, PortMode::Binary)
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
            Value::PortInput(p) if p.borrow().is_binary() => match self {
                Self::BinaryInput | Self::Input => Ok(()),
                _ => Err(Some(p.borrow().spec())),
            },
            Value::PortInput(p) if p.borrow().is_textual() => match self {
                Self::Input | Self::TextualInput => Ok(()),
                _ => Err(Some(p.borrow().spec())),
            },
            Value::PortOutput(p) if p.borrow().is_binary() => match self {
                Self::BinaryOutput | Self::Output => Ok(()),
                _ => Err(Some(p.borrow().spec())),
            },
            Value::PortOutput(p) if p.borrow().is_textual() => match self {
                Self::Output | Self::TextualOutput => Ok(()),
                _ => Err(Some(p.borrow().spec())),
            },
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

impl ReadPort {
    pub(super) fn stdin() -> Self {
        Self::text_new(ReadSource::Stdin)
    }

    fn text_new(source: ReadSource) -> Self {
        Self {
            mode: PortMode::Textual,
            stream: ReadStream {
                buf: Some(source.create_stream()),
                source,
            },
        }
    }

    fn spec(&self) -> PortSpec {
        match self.mode {
            PortMode::Binary => PortSpec::BinaryInput,
            PortMode::Textual => PortSpec::TextualInput,
        }
    }
}

impl WritePort {
    pub(super) fn stdout() -> Self {
        Self::text_new(WriteSource::Stdout)
    }

    pub(super) fn stderr() -> Self {
        Self::text_new(WriteSource::Stderr)
    }

    fn text_new(source: WriteSource) -> Self {
        Self {
            mode: PortMode::Textual,
            stream: WriteStream {
                buf: Some(source.create_stream()),
                source,
            },
        }
    }

    fn spec(&self) -> PortSpec {
        match self.mode {
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
    buf: Option<Box<BufReader<dyn Read>>>,
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
    buf: Option<Box<BufWriter<dyn Write>>>,
    source: WriteSource,
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

#[derive(Debug)]
enum PortMode {
    Binary,
    Textual,
}

#[derive(Debug)]
enum ReadSource {
    Stdin,
}

impl ReadSource {
    fn create_stream(&self) -> Box<BufReader<dyn Read>> {
        match self {
            Self::Stdin => read_buffer_with(io::stdin()),
        }
    }
}

impl Display for ReadSource {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Stdin => f.write_str("stdin"),
        }
    }
}

#[derive(Debug)]
enum WriteSource {
    Stderr,
    Stdout,
}

impl WriteSource {
    fn create_stream(&self) -> Box<BufWriter<dyn Write>> {
        match self {
            Self::Stderr => write_buffer_with(io::stderr()),
            Self::Stdout => write_buffer_with(io::stdout()),
        }
    }
}

impl Display for WriteSource {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Stderr => f.write_str("stderr"),
            Self::Stdout => f.write_str("stdout"),
        }
    }
}

fn read_buffer_with(r: impl Read + 'static) -> Box<BufReader<dyn Read>> {
    Box::new(BufReader::new(r))
}

fn write_buffer_with(w: impl Write + 'static) -> Box<BufWriter<dyn Write>> {
    Box::new(BufWriter::new(w))
}

fn write_port_datum(src: impl Display, open: bool, f: &mut Formatter<'_>) -> fmt::Result {
    write!(f, "{src}")?;
    if !open {
        f.write_str(" (closed)")?;
    }
    Ok(())
}
