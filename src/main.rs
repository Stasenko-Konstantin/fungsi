#![feature(fn_traits)]
#![feature(unboxed_closures)]
#![feature(generic_associated_types)]

mod lexer;
mod token;
mod object;
mod evaluator;

use std::env::args;
use std::fs;
use std::io::Write;
use crate::object::{Env, make_builtins};

const HELP: &str = "help";

fn main() {
    let args: Vec<String> = args().collect();
    let mut env = Env{parent: None, defs: make_builtins()};
    if args.len() == 1 {
        loop {
            print!("< ");
            let _ = std::io::stdout().flush();
            let mut input = String::new();
            std::io::stdin().read_line(&mut input).unwrap();
            eval(input, &mut env, true)
        }
    } else if args.len() == 2 {
        match args[1].clone().as_str() {
            "help" | "--help" => println!("{}", HELP),
            file => load(file, &mut env),
        }
    } else {
        println!("{}", HELP);
    }
}

fn load(file: &str, env: &mut Env) {
    let input = fs::read_to_string(file).expect(HELP);
    eval(input, env, false)
}

fn eval(input: String, env: &mut Env, repl: bool) {
    let tokens = lexer::scan(input, repl);
    println!("> {:?}", tokens);
}
