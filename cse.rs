use std::rc::Rc;

use crate::ir::*;

impl Inst {
    pub fn is_equal_to(self: &Rc<Inst>, other: &Rc<Inst>) -> bool {
        if self.ty != other.ty || self.num_ops() != other.num_ops() {
            return false
        }
        assert!(!self.has_sideeffect() && !self.is_load(), "todo: add more cases");
        if !(match (&self.opc, &other.opc) {
            (OpC::BinOp { op: op0 }, OpC::BinOp { op: op1 }) => op0 == op1,
            (OpC::Cmp { op: op0 }, OpC::Cmp { op: op1 }) => op0 == op1,
            (OpC::PtrAdd { scaled_by: s0 }, OpC::PtrAdd { scaled_by: s1 }) => s0 == s1,
            (OpC::Const { val: val0 }, OpC::Const { val: val1 }) => val0 == val1,
            (_, _) => false
        }) {
            return false
        }

        self.ops.borrow().iter().zip(other.ops.borrow().iter()).all(|(a, b)| a == b)
    }
}

// Simple common subexpression elimination.
pub fn run(bbs: &[Rc<Block>]) -> usize {
    let mut to_remove: Vec<Rc<Inst>> = Vec::new();
    let mut available_exprs: Vec<Vec<Rc<Inst>>> = bbs.iter().map(|_| Vec::new()).collect();

    for bb in bbs.iter() {
        let doms: Vec<_> = bb.doms.borrow().iter().collect();

        'instr_loop: for i in bb.instrs.borrow().iter() {
            // TODO: Load CSE (requires basic alias analysis).
            if i.has_sideeffect() || i.is_load() {
                continue
            }

            for didx in doms.iter() {
                for di in available_exprs[*didx].iter() {
                    if i.is_equal_to(di) {
                        eprintln!("cse: replace {} by {}", i, di);
                        to_remove.push(i.clone());
                        i.replace_all_uses_with(di);
                        continue 'instr_loop;
                    }
                }
            }

            available_exprs[bb.idx.get()].push(i.clone());
        }
    }

    for remove in to_remove.iter() {
        remove.drop_operands_and_unlink()
    }
    to_remove.len()
}
