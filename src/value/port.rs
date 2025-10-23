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

    fn is_open(&self) -> bool {
        self.stream.is_open()
    }
}

impl<T: Display> Display for Port<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.stream.fmt(f)
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
}

pub(crate) trait PortStream {
    fn is_open(&self) -> bool;
}

pub(crate) struct ReadStream {
    buf: Option<Box<BufReader<dyn Read>>>,
    source: ReadSource,
}

impl PortStream for ReadStream {
    fn is_open(&self) -> bool {
        return self.buf.is_some();
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
        Display::fmt(&self.source, f)
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
        Display::fmt(&self.source, f)
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn stderr_display() {
        let p = WriteSource::Stderr;

        assert_eq!(p.to_string(), "stderr");
    }

    #[test]
    fn stdin_display() {
        let p = ReadSource::Stdin;

        assert_eq!(p.to_string(), "stdin");
    }

    #[test]
    fn stdout_display() {
        let p = WriteSource::Stdout;

        assert_eq!(p.to_string(), "stdout");
    }
}
