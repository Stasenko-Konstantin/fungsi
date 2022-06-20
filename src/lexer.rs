use std::collections::HashMap;
use std::process::exit;
use crate::token::{Token, TokenType, make_keywords};

pub fn scan(input: String) -> Vec<Token> {
    let mut input = input;
    let symbols = "\\|/?.><!#@`^~%&*-_+=";
    let keywords = make_keywords();
    let mut tokens = Vec::new();
    let mut x = 0;
    let mut y = 0;
    loop {
        let c = match input.chars().nth(0) {
            Some(c) => c,
            None => break
        };
        println!("{}", c);
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
                if input.chars().next().unwrap() == '=' {
                    tokens.push(
                        Token { token: TokenType::BIND, content: ":+".to_string(), span: (y, x) })
                } else {
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
            _ => {
                println!("lexer error: y = {}, x = {}, c = {}", y, x, c);
                exit(1);
            },
        }
        input = input[1..input.len()-1].to_string();
        x += 1;
    }
    tokens
}