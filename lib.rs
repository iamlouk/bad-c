#![feature(assert_matches)]
#![feature(if_let_guard)]
#![allow(dead_code)]

use std::path::Path;
use std::rc::Rc;

pub mod ast;
pub mod ir;
pub mod lex;
pub mod parse;

#[derive(Clone, PartialEq, PartialOrd, Debug)]
pub struct SLoc {
    pub file: Rc<Path>,
    pub line: u32,
    pub col: u32,
}

impl SLoc {
    pub fn unknown() -> Self {
        Self { file: Rc::from(Path::new("<internal>")), line: 0, col: 0 }
    }

    pub fn new(file: &Path, line: usize, col: usize) -> Self {
        Self {
            file: Rc::from(file),
            line: line.min(u32::MAX as usize) as u32,
            col: col.min(u32::MAX as usize) as u32,
        }
    }
}

#[allow(dead_code)]
#[derive(Debug)]
pub enum Error {
    IO(SLoc, std::io::Error),
    EndOfFile(SLoc),
    InvalidInt(SLoc, std::num::ParseIntError),
    Lex(SLoc, String),
    Unexpected(SLoc, lex::Tok, &'static str),
    Type(SLoc, ast::Type, &'static str),
    NotAType,
    UnknownSymbol(SLoc, Rc<str>),
}
