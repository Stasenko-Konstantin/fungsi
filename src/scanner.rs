use std::cell::RefCell;
use std::ops::Add;
use crate::error::error;
use crate::token::{Token, TokenType, make_keywords};

struct Span {
    x: i32,
    y: i32,
}

impl Span {
    pub(crate) fn from(span: (i32, i32)) -> Span {
        Span { x: span.0, y: span.1 }
    }
}

pub struct Lexer<'a> {
    input: RefCell<String>,
    span: Span,
    tokens: RefCell<&'a mut Vec<Token>>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: String, span: (i32, i32), tokens: &'a mut Vec<Token>) -> Self {
        Lexer { input: RefCell::new(input), span: Span::from(span), tokens: RefCell::new(tokens) }
    }

    fn pop(&mut self) -> Option<char> {
        self.input.get_mut().pop()
    }

    fn add(&mut self, c: &str) {
        self.input = RefCell::new(self.input.get_mut().clone().add(c));
    }

    fn push(&mut self, token: TokenType, content: &str) {
        self.tokens.get_mut().push(
            Token { token, content: content.to_string(), span: (self.span.y, self.span.x) });
    }

    fn take(&mut self, c: char, p: Box<dyn Fn(char) -> bool>, ) -> Option<String> {
        let mut result: String = String::from(c);
        loop {
            let c = self.pop()?;
            if p(c) {
                result = result.add(c.to_string().as_str());
            } else {
                self.add(c.to_string().as_str());
                break;
            }
        }
        Some(result)
    }

    pub fn scan(&mut self, repl: bool) {
        let symbols = "\\|/?><!#@'`^~%&*-_+=";
        let keywords = make_keywords();
        'outer: loop {
            let c = match self.pop() {
                Some(c) => c,
                None => break
            };
            match c {
                '@' => self.push(TokenType::LAMBDA, "@"),
                '^' => self.push(TokenType::RETURN, "^"),
                '|' => self.push(TokenType::DELIMITER, "|"),
                ',' => self.push(TokenType::COMMA, ","),
                '.' => self.push(TokenType::DOT, "."),
                ':' => {
                    let c = match self.pop() {
                        Some(c) => c,
                        None => break
                    };
                    if c == '=' {
                        self.push(TokenType::BIND, ":=");
                        self.span.x += 1;
                    } else {
                        self.add(c.to_string().as_str());
                        self.push(TokenType::ATOM, ":")
                    }
                }
                '(' | '[' | '{' => self.push(TokenType::LPAREN, "("),
                ')' | ']' | '}' => self.push(TokenType::RPAREN, ")"),
                '"' => {
                    let c = match self.pop() { // this and below
                        Some(c) => c,
                        None => break
                    };
                    let string = match self.take(c, Box::new(|c: char| c != '"')) {
                        Some(r) => r,
                        None => break 'outer,
                    };
                    match self.pop() { // this and above
                        None => break,
                        _ => {}
                    };
                    let len = string.len() as i32;
                    self.push(TokenType::STRING, string.as_str());
                    self.span.x += len + 2;
                    continue;
                }
                '\r' | '\t' | ' ' => {}
                '\n' => {
                    self.span.x = 0;
                    self.span.y += 1;
                    self.push(TokenType::COMMA, ",");
                }
                _ if c.is_digit(10) => {
                    let num = match self.take(c, Box::new(|c: char| c.is_digit(10) || c == '.')) {
                        Some(r) => r,
                        None => break 'outer,
                    };
                    let len = num.len() as i32;
                    self.push(TokenType::NUM, num.as_str());
                    self.span.x += len;
                    continue;
                }
                _ if c.is_alphabetic() || symbols.contains(c) => {
                    let name = match self.take(c, Box::new(|c: char| c.is_alphabetic() || symbols.contains(c))) {
                        Some(r) => r,
                        None => break 'outer,
                    };
                    let len = name.len() as i32;
                    if keywords.contains_key(name.as_str()) {
                        let token = *keywords.get(name.as_str()).unwrap();
                        self.push(token, name.as_str());
                    } else if name == ";" { // comment
                        loop {
                            let c = match self.pop() {
                                Some(c) => c,
                                None => break 'outer,
                            };
                            self.span.x += 1;
                            if c == '\n' {
                                self.span.x = 0;
                                self.span.y += 1;
                                self.push(TokenType::COMMA, ",");
                                break;
                            }
                        }
                        continue;
                    } else {
                        self.push(TokenType::NAME, name.as_str());
                    }
                    self.span.x += len;
                    continue;
                }
                _ => {
                    error(format!("lexer error {{ UnknownSym }} : y = {}, x = {}, c = {}", self.span.y, self.span.x, c), repl)
                }
            }
            self.span.x += 1;
        }
        self.push(TokenType::EOF, "");
    }
}

pub fn scan(input: &str, tokens: &mut Vec<Token>, repl: bool) {
    let input = input.chars().rev().collect::<String>();
    let mut lexer = Lexer::new(input, (0, 0), tokens);
    lexer.scan(repl);
}

#[cfg(test)]
mod test_lexer {
    use crate::token::{Token, TokenType};

    #[test]
    fn scan_test() {
        let mut tokens: Vec<Token> = Vec::new();
        crate::scanner::scan("::= hello    \"a a\" 123.123 ", &mut tokens, false);
        assert_eq!(vec![Token { token: TokenType::ATOM, content: ":".to_string(), span: (0, 0) },
                        Token { token: TokenType::BIND, content: ":=".to_string(), span: (0, 1) },
                        Token { token: TokenType::NAME, content: "hello".to_string(), span: (0, 4) },
                        Token { token: TokenType::STRING, content: "a a".to_string(), span: (0, 13) },
                        Token { token: TokenType::NUM, content: "123.123".to_string(), span: (0, 19) },
                        Token { token: TokenType::EOF, content: "".to_string(), span: (0, 28) }],
                   tokens);
    }
}