use std::rc::Rc;

pub type LineNumber = u64;

pub trait TextSource: Iterator<Item = TextLine> {
    fn context(&self) -> Rc<TextContext>;
}

#[derive(Debug)]
pub struct TextContext {
    pub name: String,
    pub path: Option<String>,
}

impl TextContext {
    pub fn named(name: String) -> Self {
        Self { name, path: None }
    }
}

#[derive(Debug)]
pub struct TextLine {
    pub ctx: Rc<TextContext>,
    pub line: String,
    pub lineno: LineNumber,
}

#[cfg(test)]
mod tests {
    #[test]
    fn test() {
        todo!();
    }
}
