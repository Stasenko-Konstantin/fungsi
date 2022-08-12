use std::ops::Add;
use std::process::exit;
use crate::error::error;
use crate::token::{Token, TokenType, make_keywords};

pub fn scan(input: String, repl: bool) -> Vec<Token> {
    let mut input: String = input.chars().rev().collect();
    let symbols = "\\|/?><!#@`^~%&*-_+=";
    let keywords = make_keywords();
    let mut tokens = Vec::new();
    let mut x = 0;
    let mut y = 0;
    'outer: loop {
        let c = match input.pop() {
            Some(c) => c,
            None => break
        };
        match c {
            '@' => tokens.push(
                Token { token: TokenType::LAMBDA, content: "@".to_string(), span: (y, x) }),
            '^' => tokens.push(
                Token { token: TokenType::RETURN, content: "^".to_string(), span: (y, x) }),
            '|' => tokens.push(
                Token { token: TokenType::DELIMITER, content: "|".to_string(), span: (y, x) }),
            ',' => tokens.push(
                Token { token: TokenType::COMMA, content: ",".to_string(), span: (y, x) }),
            '.' => tokens.push(
                Token { token: TokenType::DOT, content: ".".to_string(), span: (y, x) }),
            ':' => {
                let c = match input.pop() {
                    Some(c) => c,
                    None => break
                };
                if c == '=' {
                    tokens.push(
                        Token { token: TokenType::BIND, content: ":=".to_string(), span: (y, x) });
                    x += 1;
                } else {
                    input = input.add(c.to_string().as_str());
                    tokens.push(
                        Token { token: TokenType::ATOM, content: ":".to_string(), span: (y, x) })
                }
            }
            '=' => tokens.push(
                Token { token: TokenType::EQ, content: "=".to_string(), span: (y, x) }),
            '(' | '[' => tokens.push(
                Token { token: TokenType::LPAREN, content: "(".to_string(), span: (y, x) }),
            ')' | ']' => tokens.push(
                Token { token: TokenType::RPAREN, content: ")".to_string(), span: (y, x) }),
            '\r' | '\t' | ' ' => {}
            '\n' => {
                x = 0;
                y += 1;
                tokens.push(
                    Token { token: TokenType::COMMA, content: ",".to_string(), span: (y, x) });
            }
            _ if c.is_digit(10) => {
                let mut num: String = String::from(c);
                loop {
                    let c = match input.pop() {
                        Some(c) => c,
                        None => break 'outer,
                    };
                    if c.is_digit(10) || c == '.' {
                        num = num.add(c.to_string().as_str());
                    } else {
                        input = input.add(c.to_string().as_str());
                        break;
                    }
                }
                let len = num.len() as i64;
                tokens.push(
                    Token { token: TokenType::NUM, content: num, span: (y, x) });
                x += len;
                continue
            }
            _ if c.is_alphabetic() || symbols.contains(c) => {
                let mut name: String = String::from(c);
                loop {
                    let c = match input.pop() {
                        Some(c) => c,
                        None => break 'outer,
                    };
                    if c.is_alphabetic() || symbols.contains(c) {
                        name = name.add(c.to_string().as_str());
                    } else {
                        input = input.add(c.to_string().as_str());
                        break;
                    }
                }
                let len = name.len() as i64;
                if keywords.contains_key(&*name) {
                    let token = *keywords.get(&*name).unwrap();
                    tokens.push(
                        Token { token, content: name, span: (y, x) });
                } else if name == "NB" { // comment
                    loop {
                        let c = match input.pop() {
                            Some(c) => c,
                            None => break 'outer,
                        };
                        x += 1;
                        if c == '\n' {
                            x = 0;
                            y += 1;
                            tokens.push(
                                Token { token: TokenType::COMMA, content: ",".to_string(), span: (y, x) });
                            break
                        }
                    }
                    continue
                } else {
                    tokens.push(
                        Token { token: TokenType::NAME, content: name, span: (y, x) });
                }
                x += len;
                continue
            }
            _ => {
                error(format!("lexer error: y = {}, x = {}, c = {}", y, x, c), repl)
            }
        }
        x += 1;
    }
    tokens.push(Token { token: TokenType::EOF, content: "".to_string(), span: (y, 0) });
    tokens
}

#[cfg(test)]
mod test_lexer {
    use crate::token::{Token, TokenType};

    #[test]
    fn scan_test() {
        assert_eq!(vec![Token { token: TokenType::ATOM, content: ":".to_string(), span: (0, 0) },
                        Token { token: TokenType::BIND, content: ":=".to_string(), span: (0, 1) },
                        Token { token: TokenType::NAME, content: "hello".to_string(), span: (0, 4) },
                        Token { token: TokenType::NUM, content: "123.123".to_string(), span: (0, 14) },
                        Token { token: TokenType::EOF, content: "".to_string(), span: (0, 15) }],
                   crate::lexer::scan("::= hello    123.123 ".to_string(), false));
    }
}