use std::{
    fmt::{Display, Error, Formatter},
    write,
};

#[derive(Debug)]
pub enum Literal {
    Boolean(bool),
    Character(char),
}

impl Display for Literal {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match self {
            Self::Boolean(b) => write!(f, "#{}", if *b { 't' } else { 'f' }),
            Self::Character(c) => write!(f, "#\\{c}"),
        }
    }
}
