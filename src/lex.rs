use std::{iter::Peekable, ops::Range, str::CharIndices};

pub fn tokenize(text: &str) -> Vec<Token> {
    let mut tokens: Vec<Token> = Vec::new();
    let mut stream = CharStream::on(text);
    while let Some(&(idx, ch)) = stream.peek() {
        if ch.is_ascii_whitespace() {
            stream.eat();
        } else {
            tokens.push(match ch {
                '#' => tokenize_hash(&mut stream),
                '(' => {
                    stream.eat();
                    Token::char(idx, TokenKind::ParenLeft)
                }
                ')' => {
                    stream.eat();
                    Token::char(idx, TokenKind::ParenRight)
                }
                _ => {
                    stream.eat();
                    Token::char(idx, TokenKind::Unimplemented(ch))
                }
            });
        }
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

    fn eat_word(&mut self, first: usize) -> (usize, &str) {
        let mut last = first;
        self.eat();
        while let Some(&(idx, ch)) = self.peek() {
            match ch {
                '"' | '(' | ')' | ';' | '|' => break,
                _ if ch.is_ascii_whitespace() => break,
                _ => {
                    last = idx;
                    self.eat();
                }
            }
        }
        (last, &self.text[first..=last])
    }
}

#[derive(Debug)]
pub struct Token {
    kind: TokenKind,
    lexeme: Range<usize>,
}

impl Token {
    fn char(pos: usize, kind: TokenKind) -> Self {
        Token::text(pos, pos, kind)
    }

    fn text(first: usize, last: usize, kind: TokenKind) -> Self {
        Token {
            kind: kind,
            lexeme: (first..last + 1),
        }
    }
}

#[derive(Debug)]
enum TokenKind {
    ExpectedBoolean,
    HashInvalid,
    HashUnterminated,
    Literal(LiteralKind),
    ParenLeft,
    ParenRight,
    Unimplemented(char),
}

#[derive(Debug)]
enum LiteralKind {
    Boolean(bool),
}

fn tokenize_hash(stream: &mut CharStream) -> Token {
    let (first, _) = stream.eat().unwrap();
    if let Some(&(idx, ch)) = stream.peek() {
        match ch {
            't' | 'f' => {
                let (last, word) = stream.eat_word(idx);
                match word {
                    "t" | "true" => {
                        Token::text(first, last, TokenKind::Literal(LiteralKind::Boolean(true)))
                    }
                    "f" | "false" => {
                        Token::text(first, last, TokenKind::Literal(LiteralKind::Boolean(false)))
                    }
                    _ => Token::text(first, last, TokenKind::ExpectedBoolean),
                }
            }
            _ => {
                let (last, _) = stream.eat_word(idx);
                Token::text(first, last, TokenKind::HashInvalid)
            }
        }
    } else {
        stream.eat();
        Token::char(first, TokenKind::HashUnterminated)
    }
}
