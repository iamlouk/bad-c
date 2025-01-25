use std::io::Write;
use std::path::PathBuf;
use std::str::FromStr;

use shittyc::*;

use clap::Parser;

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Args {
    #[arg(short, long, value_name = "source-file")]
    input_file: PathBuf,

    #[arg(short, long, value_name = "output-file")]
    output_file: PathBuf,

    #[arg(short = 'I', long)]
    include_paths: Vec<PathBuf>,

    #[arg(short = 'E', action)]
    preprocess: bool,

    #[arg(short = 'O', action)]
    optimize: bool,

    #[arg(short, long, action)]
    unparse: bool,

    #[arg(short, long, action)]
    emitir: bool,

    #[arg(short, long)]
    passes: Vec<String>,
}

fn main() {
    let args = Args::parse_from(std::env::args_os());

    let buf = match std::fs::read(&args.input_file) {
        Ok(buf) => buf,
        Err(e) => {
            eprintln!("I/O error: {}", e);
            std::process::exit(1);
        }
    };

    let mut l = lex::Lexer::new(std::path::Path::new("<stdin>"), buf);
    l.include_paths.push(PathBuf::from_str("/usr/include").unwrap());
    for include_path in args.include_paths {
        l.include_paths.push(include_path);
    }

    let output_file = std::fs::File::create(&args.output_file);
    let mut output_file = std::io::BufWriter::new(match output_file {
        Ok(f) => f,
        Err(e) => {
            eprintln!("I/O error: {}", e);
            std::process::exit(1);
        }
    });

    if args.preprocess {
        let s = lex::Tok::dump(l.map(|res| match res {
            Ok((_, tok)) => tok,
            Err(e) => {
                eprintln!("preprocessing error: {:?}", e);
                std::process::exit(1);
            }
        }))
        .expect("Tok::dump error");
        use std::io::Write;
        output_file.write_all(s.as_bytes()).expect("I/O error");
        output_file.flush().expect("I/O error");
        return
    }

    let mut cu = ast::Unit::new();
    if let Err(e) = cu.parse(&mut l) {
        eprintln!("parsing error: {:?}", e);
        std::process::exit(1);
    }

    let mut s = String::new();
    if args.unparse {
        cu.write(&mut s).expect("internal dump error");
        output_file.write_all(s.as_bytes()).expect("I/O error");
        output_file.flush().expect("I/O error");
        return
    }

    let target = rv64::RV64::new();
    for f in cu.functions_iter() {
        f.gen_ir();
        if args.optimize {
            if let Err(e) = f.opt(crate::opts::DEFAULT_OPTS, &target) {
                eprintln!("opt. error: {:?}", e);
                std::process::exit(1);
            }
        } else {
            let passes: Vec<&str> = args.passes.iter().map(|s| s.as_str()).collect();
            if let Err(e) = f.opt(&passes, &target) {
                eprintln!("opt. error: {:?}", e);
                std::process::exit(1);
            }
        }

        if args.emitir {
            s.clear();
            f.write_ir(&mut s).expect("internal dump error");
            output_file.write_all(s.as_bytes()).expect("I/O error");
            output_file.flush().expect("I/O error");
        }
    }
}
