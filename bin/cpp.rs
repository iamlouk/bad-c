use shittyc::lex;

fn main() {
    use std::io::{Read, Write};
    let mut buf = Vec::new();
    if let Err(e) = std::io::stdin().lock().read_to_end(&mut buf) {
        eprintln!("I/O error: {}", e);
        std::process::exit(1);
    }

    let l = lex::Lexer::new(std::path::Path::new("<stdin>"), buf);
    let s = lex::Tok::dump(l.into_iter().map(|res| {
        if let Err(e) = res {
            eprintln!("{:?}", e);
            std::process::exit(1);
        }

        res.unwrap().1
    }))
    .unwrap();

    std::io::stdout().write_all(s.as_bytes()).unwrap();
}
