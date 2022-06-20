mod lexer;
mod token;

use std::io::Write;

fn main() {
    loop {
        print!("< ");
        let _ = std::io::stdout().flush();
        let mut input = String::new();
        std::io::stdin().read_line(&mut input).unwrap();
        let tokens = lexer::scan(input);
        print!("> {:?}", tokens);
    }
}
