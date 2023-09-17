use crate::lex::{tokenize::Scanner, tokens::TokenKind};

pub(super) struct BlockComment<'me, 'str> {
    cont: bool,
    depth: usize,
    scan: &'me mut Scanner<'str>,
}

impl<'me, 'str> BlockComment<'me, 'str> {
    pub(super) fn new(scan: &'me mut Scanner<'str>) -> Self {
        Self {
            cont: false,
            depth: 0,
            scan,
        }
    }

    pub(super) fn cont(depth: usize, scan: &'me mut Scanner<'str>) -> Self {
        Self {
            cont: true,
            depth,
            scan,
        }
    }

    pub(super) fn consume(&mut self) -> TokenKind {
        while !self.scan.consumed() {
            if let Some((_, ch)) = self.scan.find_any_char(&['|', '#']) {
                if self.end_block(ch) {
                    if self.depth == 0 {
                        return self.end_kind();
                    } else {
                        self.depth -= 1;
                    }
                } else if self.new_block(ch) {
                    self.depth += 1;
                }
            }
        }
        self.continuation_kind()
    }

    fn end_block(&mut self, ch: char) -> bool {
        ch == '|' && self.scan.char_if_eq('#').is_some()
    }

    fn new_block(&mut self, ch: char) -> bool {
        ch == '#' && self.scan.char_if_eq('|').is_some()
    }

    fn continuation_kind(&self) -> TokenKind {
        if self.cont {
            TokenKind::CommentBlockFragment(self.depth)
        } else {
            TokenKind::CommentBlockBegin(self.depth)
        }
    }

    fn end_kind(&self) -> TokenKind {
        if self.cont {
            TokenKind::CommentBlockEnd
        } else {
            TokenKind::CommentBlock
        }
    }
}

// NOTE: functionality is covered by Tokenizer and Continuation tests
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn blockcomment_new() {
        let mut s = Scanner::new("end comment #| nested |# |#");

        let target = BlockComment::new(&mut s);

        assert_eq!(target.cont, false);
        assert_eq!(target.depth, 0);
    }

    #[test]
    fn blockcomment_cont() {
        let mut s = Scanner::new("end comment #| nested |# |#");

        let target = BlockComment::cont(3, &mut s);

        assert_eq!(target.cont, true);
        assert_eq!(target.depth, 3);
    }
}
