use std::{path::PathBuf, str::FromStr};

use shittyc::*;

fn main() {
    use std::io::{Read, Write};
    let mut buf = Vec::new();
    if let Err(e) = std::io::stdin().lock().read_to_end(&mut buf) {
        eprintln!("I/O error: {}", e);
        std::process::exit(1);
    }

    let mut l = lex::Lexer::new(std::path::Path::new("<stdin>"), buf);
    l.include_paths.push(PathBuf::from_str("/usr/include").unwrap());

    let mut cu = ast::Unit::new();
    if let Err(e) = cu.parse(&mut l) {
        eprintln!("error: {:?}", e);
        std::process::exit(1);
    }

    let mut s = String::new();
    cu.write(&mut s).unwrap();

    std::io::stdout().write_all(s.as_bytes()).unwrap();
}
