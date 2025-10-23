use std::{
    fmt::{self, Debug, Formatter},
    io::{self, BufReader, BufWriter, Read, Write},
};

pub(crate) type ReadPort = Port<ReadStream>;
pub(crate) type WritePort = Port<WriteStream>;

#[derive(Debug)]
pub(crate) struct Port<T> {
    mode: PortMode,
    stream: T,
}

impl ReadPort {
    fn stdin() -> Self {
        Self::text_new(InputSource::Stdin)
    }

    fn text_new(source: InputSource) -> Self {
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
    fn stderr() -> Self {
        Self::text_new(OutputSource::Stderr)
    }

    fn stdout() -> Self {
        Self::text_new(OutputSource::Stdout)
    }

    fn text_new(source: OutputSource) -> Self {
        Self {
            mode: PortMode::Textual,
            stream: WriteStream {
                buf: Some(source.create_stream()),
                source,
            },
        }
    }
}

pub(crate) struct ReadStream {
    buf: Option<Box<BufReader<dyn Read>>>,
    source: InputSource,
}

impl Debug for ReadStream {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("ReadStream")
            .field("buf", &"...")
            .field("source", &self.source)
            .finish()
    }
}

pub(crate) struct WriteStream {
    buf: Option<Box<BufWriter<dyn Write>>>,
    source: OutputSource,
}

impl Debug for WriteStream {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("WriteStream")
            .field("buf", &"...")
            .field("source", &self.source)
            .finish()
    }
}

#[derive(Debug)]
enum PortMode {
    Binary,
    Textual,
}

#[derive(Debug)]
enum InputSource {
    Stdin,
}

impl InputSource {
    fn create_stream(&self) -> Box<BufReader<dyn Read>> {
        match self {
            Self::Stdin => read_buffer_with(io::stdin()),
        }
    }
}

#[derive(Debug)]
enum OutputSource {
    Stderr,
    Stdout,
}

impl OutputSource {
    fn create_stream(&self) -> Box<BufWriter<dyn Write>> {
        match self {
            Self::Stderr => write_buffer_with(io::stderr()),
            Self::Stdout => write_buffer_with(io::stdout()),
        }
    }
}

fn read_buffer_with(r: impl Read + 'static) -> Box<BufReader<dyn Read>> {
    Box::new(BufReader::new(r))
}

fn write_buffer_with(w: impl Write + 'static) -> Box<BufWriter<dyn Write>> {
    Box::new(BufWriter::new(w))
}
