#![feature(fn_traits)]
#![feature(unboxed_closures)]
#![feature(result_option_inspect)]

mod error;
mod evaluator;
mod object;
mod parser;
mod scanner;
mod token;

use crate::object::{Env, Object};
use crate::token::Token;
use std::cell::RefCell;
use std::env::args;
use std::fs;
use std::io::Write;

const HELP: &str = "help";

fn main() {
    let args: Vec<String> = args().collect();
    let mut env: Env<()> = Env {
        parent: None,
        defs: object::make_builtins(),
    };
    if args.len() == 1 {
        loop {
            print!("< ");
            let _ = std::io::stdout().flush();
            let mut input = String::new();
            std::io::stdin().read_line(&mut input).unwrap();
            eval(input.as_str(), &mut env, true)
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

fn load<T>(file: &str, env: &mut Env<T>) {
    let input = fs::read_to_string(file)
        .inspect_err(|e| eprintln!("{}\n{}", HELP, e))
        .unwrap();
    eval(input.as_str(), env, false)
}

fn eval<T>(input: &str, env: &mut Env<T>, repl: bool) {
    let mut tokens: Vec<Token> = Vec::new();
    scanner::scan(format!("{} ", input).as_str(), &mut tokens, repl);
    println!("{:?}", tokens);
    let objects: (Option<RefCell<Object<()>>>, i32) = parser::parse(tokens.clone(), repl);
    // println!("> {}", objects.0.unwrap().get_mut().get_content());
}
