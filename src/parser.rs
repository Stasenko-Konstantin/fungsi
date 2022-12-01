use crate::error::error;
use crate::object::*;
use crate::token::*;
use std::cell::RefCell;

type Program<T> = Vec<Object<T>>;

pub fn parse<T>(tokens: Vec<Token>, repl: bool) -> (Option<RefCell<Object<T>>>, i32) {
    if tokens.is_empty() {
        return (None, 0);
    }
    let start = &tokens[0];
    if !start.token.is_gen_sym() {
        error(
            format!(
                "parser error {{ NonGenSym }} : y = {}, x = {}, c = {}",
                start.span.0, start.span.1, start.content
            ),
            repl,
        );
    }
    let program: Program<T>;
    let skip = -1;
    (None, 0)
}
