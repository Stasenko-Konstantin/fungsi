use std::cell::RefCell;
use crate::object::*;
use crate::token::*;

type Program<T> = Vec<Object<T>>;

pub fn parse<T>(tokens: Vec<Token>) -> (Option<RefCell<Object<T>>>, i32) {
    if tokens.len() == 0 {
        return (None, 0)
    }
    let program: Program<T>;
    let skip = -1;
    return (None, 0)
}