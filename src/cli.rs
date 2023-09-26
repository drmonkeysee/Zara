use rustyline::error::ReadlineError;
use std::{
    error,
    fmt::{self, Display, Formatter},
    io, result,
};

pub(crate) type Result = result::Result<(), Error>;

#[derive(Debug)]
pub(crate) enum Error {
    Io(io::Error),
    Repl(ReadlineError),
    Run(zara::Error),
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Io(err) => err.fmt(f),
            Self::Repl(err) => err.fmt(f),
            Self::Run(err) => err.fmt(f),
        }
    }
}

impl error::Error for Error {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        Some(match self {
            Self::Io(err) => err,
            Self::Repl(err) => err,
            Self::Run(err) => err,
        })
    }
}

impl From<io::Error> for Error {
    fn from(value: io::Error) -> Self {
        Self::Io(value)
    }
}

impl From<ReadlineError> for Error {
    fn from(value: ReadlineError) -> Self {
        Self::Repl(value)
    }
}

impl From<zara::Error> for Error {
    fn from(value: zara::Error) -> Self {
        Self::Run(value)
    }
}
