use std::{iter::Peekable, ops::Range, str::CharIndices};

pub fn tokenize(text: &str) -> Vec<Token> {
    let mut tokens: Vec<Token> = Vec::new();
    let mut stream = CharStream::on(text);
    while let Some((idx, ch)) = stream.eat() {
        if ch.is_ascii_whitespace() {
            continue;
        }
        let mut tokenizer = Tokenizer::start(idx);
        match ch {
            '#' => tokenizer.for_hash(&mut stream),
            '(' => tokenizer.kind(TokenKind::ParenLeft),
            ')' => tokenizer.kind(TokenKind::ParenRight),
            _ => tokenizer.kind(TokenKind::Unimplemented(ch)),
        };
        tokens.push(tokenizer.extract());
    }
    tokens
}

type CharIter<'a> = Peekable<CharIndices<'a>>;

struct CharStream<'a> {
    text: &'a str,
    iterator: CharIter<'a>,
}

impl<'a> CharStream<'a> {
    fn on(text: &'a str) -> Self {
        CharStream {
            text: text,
            iterator: text.char_indices().peekable(),
        }
    }

    fn peek(&mut self) -> Option<&<CharIter as Iterator>::Item> {
        self.iterator.peek()
    }

    fn eat(&mut self) -> Option<<CharIter as Iterator>::Item> {
        self.iterator.next()
    }

    fn pos(&mut self) -> usize {
        if let Some(&(idx, _)) = self.peek() {
            idx
        } else {
            self.text.len()
        }
    }

    fn scan_to_delimiter(&mut self) -> usize {
        while let Some(&(idx, ch)) = self.peek() {
            if is_delimiter(ch) {
                return idx;
            }
            self.eat();
        }
        self.text.len()
    }

    fn lexeme(&self, range: Range<usize>) -> &str {
        self.text.get(range).unwrap_or_default()
    }
}

#[derive(Debug)]
pub struct Token {
    kind: TokenKind,
    lexeme: Range<usize>,
}

#[derive(Debug)]
enum TokenKind {
    ExpectedBoolean(bool),
    HashInvalid,
    HashUnterminated,
    Literal(LiteralKind),
    ParenLeft,
    ParenRight,
    Undefined,
    Unimplemented(char),
    VectorOpen,
}

#[derive(Debug)]
enum LiteralKind {
    Boolean(bool),
}

#[derive(Default)]
struct Tokenizer {
    start: usize,
    end: Option<usize>,
    kind: Option<TokenKind>,
}

impl Tokenizer {
    fn start(start: usize) -> Self {
        Tokenizer {
            start: start,
            ..Default::default()
        }
    }

    fn end(&mut self, end: usize) -> &mut Self {
        self.end = Some(end);
        self
    }

    fn kind(&mut self, kind: TokenKind) -> &mut Self {
        self.kind = Some(kind);
        self
    }

    fn for_hash(&mut self, stream: &mut CharStream) -> &mut Self {
        if let Some((idx, ch)) = stream.eat() {
            match ch {
                'f' => self.for_bool("alse", false, stream),
                't' => self.for_bool("rue", true, stream),
                '\\' => {
                    // TODO: handle character literal
                    todo!()
                }
                '(' => self.end(idx + 1).kind(TokenKind::VectorOpen),
                _ => self
                    .end(stream.scan_to_delimiter())
                    .kind(TokenKind::HashInvalid),
            }
        } else {
            self.kind(TokenKind::HashUnterminated)
        }
    }

    fn for_bool(&mut self, pattern: &str, val: bool, stream: &mut CharStream) -> &mut Self {
        let start = stream.pos();
        let end = stream.scan_to_delimiter();
        let lexeme = stream.lexeme(start..end);
        self.end(end)
            .kind(if lexeme.is_empty() || lexeme == pattern {
                TokenKind::Literal(LiteralKind::Boolean(val))
            } else {
                TokenKind::ExpectedBoolean(val)
            })
    }

    fn extract(self) -> Token {
        Token {
            kind: self.kind.unwrap_or(TokenKind::Undefined),
            lexeme: self.start..self.end.unwrap_or(self.start + 1),
        }
    }
}

fn is_delimiter(ch: char) -> bool {
    match ch {
        '"' | '(' | ')' | ';' | '|' => true,
        _ if ch.is_ascii_whitespace() => true,
        _ => false,
    }
}
