#![feature(assert_matches)]
#![feature(if_let_guard)]
#![feature(iter_collect_into)]
#![allow(dead_code)]
// ir::Inst and ir::Block hash by address, but contain (Ref)Cells.
#![allow(clippy::mutable_key_type)]

use std::collections::{BTreeSet, HashMap};
use std::path::Path;
use std::rc::Rc;

pub mod ast;
pub mod ir;
pub mod irgen;
pub mod lex;
pub mod opts;
pub mod parse;
pub mod rv64;

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

pub struct RegAllocRes {
    used_callee_save_regs: BTreeSet<ir::PReg>,
    used_caller_save_regs: BTreeSet<ir::PReg>,
    used_stack_size_without_allocs: usize,
    caller_saved_regs_alive_over_call: HashMap<Rc<ir::Inst>, Vec<ir::PReg>>
}

pub trait Target {
    fn name(&self) -> &'static str;
    fn argument_regs(&self) -> &[ir::PReg];
    fn return_reg(&self) -> ir::PReg;
    fn temporary_regs(&self) -> &[ir::PReg];
    fn callee_saved_regs(&self) -> &[ir::PReg];

    fn write_function_prologue(&mut self, f: &ast::Function, regalloc: &mut RegAllocRes, w: &mut dyn std::fmt::Write) -> std::fmt::Result;
    fn write_function_epilogue(&mut self, f: &ast::Function, regalloc: &RegAllocRes, w: &mut dyn std::fmt::Write) -> std::fmt::Result;
    fn write_label(&mut self, f: &ast::Function, bb: &Rc<ir::Block>, w: &mut dyn std::fmt::Write) -> std::fmt::Result;
    fn write_instr(&mut self, f: &ast::Function, i: &ir::Inst, w: &mut dyn std::fmt::Write) -> std::fmt::Result;
}
