use std::cell::RefCell;
use std::ops::Add;
use crate::error::error;
use crate::token::{Token, TokenType, make_keywords};

pub fn scan(input: String, tokens: &mut RefCell<Vec<Token>>, repl: bool) {
    let symbols = "\\|/?><!#@`^~%&*-_+=";
    let keywords = make_keywords();
    let mut input: RefCell<String> = RefCell::new(input.chars().rev().collect());
    let mut x = 0;
    let mut y = 0;
    'outer: loop {
        let c = match input.get_mut().pop() {
            Some(c) => c,
            None => break
        };
        match c {
            '@' => tokens.get_mut().push(
                Token { token: TokenType::LAMBDA, content: "@".to_string(), span: (y, x) }),
            '^' => tokens.get_mut().push(
                Token { token: TokenType::RETURN, content: "^".to_string(), span: (y, x) }),
            '|' => tokens.get_mut().push(
                Token { token: TokenType::DELIMITER, content: "|".to_string(), span: (y, x) }),
            ',' => tokens.get_mut().push(
                Token { token: TokenType::COMMA, content: ",".to_string(), span: (y, x) }),
            '.' => tokens.get_mut().push(
                Token { token: TokenType::DOT, content: ".".to_string(), span: (y, x) }),
            ':' => {
                let c = match input.get_mut().pop() {
                    Some(c) => c,
                    None => break
                };
                if c == '=' {
                    tokens.get_mut().push(
                        Token { token: TokenType::BIND, content: ":=".to_string(), span: (y, x) });
                    x += 1;
                } else {
                    input = RefCell::new(input.take().add(c.to_string().as_str()));
                    tokens.get_mut().push(
                        Token { token: TokenType::ATOM, content: ":".to_string(), span: (y, x) })
                }
            }
            '=' => tokens.get_mut().push(
                Token { token: TokenType::EQ, content: "=".to_string(), span: (y, x) }),
            '(' | '[' => tokens.get_mut().push(
                Token { token: TokenType::LPAREN, content: "(".to_string(), span: (y, x) }),
            ')' | ']' => tokens.get_mut().push(
                Token { token: TokenType::RPAREN, content: ")".to_string(), span: (y, x) }),
            '"' => {
                let mut string: String = String::from(c);
                loop {
                    let c = match input.get_mut().pop() {
                        Some(c) => c,
                        None => break 'outer,
                    };
                    if c != '"' {
                        string = string.add(c.to_string().as_str());
                    } else {
                        input = RefCell::new(input.take().add(c.to_string().as_str()));
                        break;
                    }
                }
                let len = string.len() as i64;
                tokens.get_mut().push(
                    Token { token: TokenType::STRING, content: string, span: (y, x) });
                x += len;
                continue;
            }
            '\r' | '\t' | ' ' => {}
            '\n' => {
                x = 0;
                y += 1;
                tokens.get_mut().push(
                    Token { token: TokenType::COMMA, content: ",".to_string(), span: (y, x) });
            }
            _ if c.is_digit(10) => {
                let mut num: String = String::from(c);
                loop {
                    let c = match input.get_mut().pop() {
                        Some(c) => c,
                        None => break 'outer,
                    };
                    if c.is_digit(10) || c == '.' {
                        num = num.add(c.to_string().as_str());
                    } else {
                        input = RefCell::new(input.take().add(c.to_string().as_str()));
                        break;
                    }
                }
                let len = num.len() as i64;
                tokens.get_mut().push(
                    Token { token: TokenType::NUM, content: num, span: (y, x) });
                x += len;
                continue;
            }
            _ if c.is_alphabetic() || symbols.contains(c) => {
                let mut name: String = String::from(c);
                loop {
                    let c = match input.get_mut().pop() {
                        Some(c) => c,
                        None => break 'outer,
                    };
                    if c.is_alphabetic() || symbols.contains(c) {
                        name = name.add(c.to_string().as_str());
                    } else {
                        input = RefCell::new(input.take().add(c.to_string().as_str()));
                        break;
                    }
                }
                let len = name.len() as i64;
                if keywords.contains_key(&*name) {
                    let token = *keywords.get(&*name).unwrap();
                    tokens.get_mut().push(
                        Token { token, content: name, span: (y, x) });
                } else if name == "NB" { // comment
                    loop {
                        let c = match input.get_mut().pop() {
                            Some(c) => c,
                            None => break 'outer,
                        };
                        x += 1;
                        if c == '\n' {
                            x = 0;
                            y += 1;
                            tokens.get_mut().push(
                                Token { token: TokenType::COMMA, content: ",".to_string(), span: (y, x) });
                            break;
                        }
                    }
                    continue;
                } else {
                    tokens.get_mut().push(
                        Token { token: TokenType::NAME, content: name, span: (y, x) });
                }
                x += len;
                continue;
            }
            _ => {
                error(format!("lexer error {{ UnknownSym }} : y = {}, x = {}, c = {}", y, x, c), repl)
            }
        }
        x += 1;
    }
    tokens.get_mut().push(
        Token { token: TokenType::EOF, content: "".to_string(), span: (y, x) });
}

#[cfg(test)]
mod test_lexer {
    use std::cell::{Ref, RefCell};
    use crate::token::{Token, TokenType};

    #[test]
    fn scan_test() {
        let mut tokens: RefCell<Vec<Token>> = RefCell::new(Vec::new());
        crate::lexer::scan("::= hello    123.123 ".to_string(), &mut tokens, false);
        assert_eq!(RefCell::new(vec![Token { token: TokenType::ATOM, content: ":".to_string(), span: (0, 0) },
                                     Token { token: TokenType::BIND, content: ":=".to_string(), span: (0, 1) },
                                     Token { token: TokenType::NAME, content: "hello".to_string(), span: (0, 4) },
                                     Token { token: TokenType::NUM, content: "123.123".to_string(), span: (0, 14) },
                                     Token { token: TokenType::EOF, content: "".to_string(), span: (0, 15) }]),
                   tokens);
    }
}