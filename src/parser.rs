use std::cell::RefCell;
use crate::error::error;
use crate::object::*;
use crate::token::*;

type Program<T> = Vec<Object<T>>;

pub fn parse<T>(tokens: Vec<Token>, repl: bool) -> (Option<RefCell<Object<T>>>, i32) {
    if tokens.len() == 0 {
        return (None, 0)
    }
    let start = &tokens[0];
    if !start.token.is_gen_sym() {
        error(format!("parser error: y = {}, x = {}, c = {}", start.span.0, start.span.1, start.content), repl);
    }
    let program: Program<T>;
    let skip = -1;
    return (None, 0)
}