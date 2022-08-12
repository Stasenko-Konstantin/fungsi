use std::process::exit;

pub fn error(err: String, repl: bool) {
    eprintln!("{}", err);
    if !repl {
        exit(1);
    }
}