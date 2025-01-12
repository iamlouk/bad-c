use crate::{
    ast::Function,
    ir::{Block, Loop},
    Target,
};

mod cse;
mod dce;
mod mem2reg;
mod regalloc;

impl Function {
    pub fn opt(&self, passes: &[String], target: &dyn Target) -> Result<bool, String> {
        eprintln!("=== {} ({}) ===", self.name.as_ref(), target.name());
        let mut bbs = self.ir.borrow_mut();
        Block::reorder_into_rpo(&mut bbs);
        Block::recalc_doms_and_verify(&bbs);
        drop(bbs);
        let mut changed = false;
        for pass in passes {
            match pass.as_str() {
                "dce" => {
                    eprintln!("--- DCE ---");
                    let bbs = self.ir.borrow();
                    changed |= dce::run(&bbs) > 0
                }
                "cse" => {
                    eprintln!("--- CSE ---");
                    let mut bbs = self.ir.borrow_mut();
                    Block::reorder_into_rpo(&mut bbs);
                    Block::recalc_doms_and_verify(&bbs);
                    changed |= cse::run(&bbs) > 0;
                }
                "mem2reg" => {
                    eprintln!("--- mem2reg ---");
                    // mem2reg depends on the DT.
                    let mut bbs = self.ir.borrow_mut();
                    Block::reorder_into_rpo(&mut bbs);
                    Block::recalc_doms_and_verify(&bbs);
                    changed |= mem2reg::run(&bbs)
                }
                "regalloc" => {
                    eprintln!("--- regalloc ---");
                    // regalloc depends on RPO BB order.
                    let mut bbs = self.ir.borrow_mut();
                    Block::reorder_into_rpo(&mut bbs);
                    Block::recalc_doms_and_verify(&bbs);
                    regalloc::run(self, &bbs, target);
                }
                "dump" => {
                    eprintln!("--- IR dump ---");
                    let mut s = String::new();
                    self.write_ir(&mut s).unwrap();
                    use std::io::Write;
                    std::io::stderr().write_all(s.as_bytes()).unwrap();
                }
                "dumploops" => {
                    eprintln!("--- Loops ---");
                    let l = Loop::find(&self.ir.borrow());
                    let mut s = String::new();
                    l.write(&mut s, &mut String::new()).unwrap();
                    use std::io::Write;
                    std::io::stderr().write_all(s.as_bytes()).unwrap();
                }
                _ => return Err(format!("unknown pass: {:?}", *pass)),
            }
        }
        let mut bbs = self.ir.borrow_mut();
        Block::reorder_into_rpo(&mut bbs);
        Block::recalc_doms_and_verify(&bbs);
        Ok(changed)
    }
}
