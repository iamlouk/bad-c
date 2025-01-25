use rv64::RV64;
use shittyc::*;

fn ir_test(target: &dyn Target, input_file: &str, expected_file: &str, passes: &[&str]) {
    let root = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let input_file = root.join(input_file);
    let expected_file = root.join(expected_file);
    let buf = std::fs::read(&input_file).expect("I/O error");

    let mut l = lex::Lexer::new(std::path::Path::new("<stdin>"), buf);
    let mut cu = ast::Unit::new();
    cu.parse(&mut l).expect("parsing error");

    for f in cu.functions_iter() {
        f.gen_ir();
        f.opt(passes, target).expect("opt. error");
    }

    let mut irdump = String::new();
    for f in cu.functions_iter() {
        f.write_ir(&mut irdump).expect("internal dump error");
    }

    let expected = std::fs::read_to_string(&expected_file).expect("I/O error");
    eprintln!("; result:\n{}", &irdump);
    eprintln!("; expected:\n{}", &expected);
    assert_eq!(expected, irdump);
}

#[test]
fn fourtytwo() {
    let target = RV64::new();
    ir_test(&target, "tests/fourtytwo.c", "tests/fourtytwo.ir", &["dce"]);
}

#[test]
fn add() {
    let target = RV64::new();
    ir_test(&target, "tests/add.c", "tests/add.ir", &[]);
}

#[test]
fn mem2reg() {
    let target = RV64::new();
    ir_test(&target, "tests/mem2reg.c", "tests/mem2reg.ir", &["mem2reg", "dce"]);
}
