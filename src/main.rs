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
use crate::object::Env;

const HELP: &str = "help";

fn main() {
    let args: Vec<String> = args().collect();
    if args.len() == 1 {
        loop {
            print!("< ");
            let _ = std::io::stdout().flush();
            let mut input = String::new();
            std::io::stdin().read_line(&mut input).unwrap();
            eval(input, true)
        }
    } else if args.len() == 2 {
        match args[1].clone().as_str() {
            "help" | "--help" => println!("{}", HELP),
            file => load(file),
        }
    } else {
        println!("{}", HELP);
    }
}

fn load(file: &str) {
    let input = fs::read_to_string(file).expect(HELP);
    eval(input, false)
}

fn eval(input: String, repl: bool) {
    let tokens = lexer::scan(input, repl);
    println!("> {:?}", tokens);
}
