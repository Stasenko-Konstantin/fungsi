use std::collections::HashMap;
use std::ops::Add;
use std::process::exit;
use crate::token::{Token, TokenType, make_keywords};

pub fn scan(input: String, repl: bool) -> Vec<Token> {
    let mut input: String = input.chars().rev().collect();
    let symbols = "\\|/?.><!#@`^~%&*-_+=";
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
                        Token{token: TokenType::ATOM, content: ":".to_string(), span: (y, x)})
                }
            },
            '(' | '[' => tokens.push(
                Token { token: TokenType::LPAREN, content: "(".to_string(), span: (y, x) }),
            ')' | ']' => tokens.push(
                Token { token: TokenType::COMMA, content: ")".to_string(), span: (y, x) }),
            '\r' | '\t' | ' ' => {},
            '\n' => {
                x = 0;
                y += 1;
            },
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
                        break
                    }
                }
                let len = num.len() as i64;
                tokens.push(
                    Token { token: TokenType::NUM, content: num, span: (y, x) });
                x += len;
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
                        break
                    }
                }
                let len = name.len() as i64;
                tokens.push(
                    Token { token: TokenType::NAME, content: name, span: (y, x) });
                x += len;
            }
            _ => {
                println!("lexer error: y = {}, x = {}, c = {}", y, x, c);
                if !repl {
                    exit(1);
                }
            },
        }
        x += 1;
    }
    tokens
}