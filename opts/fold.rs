use std::rc::Rc;

use crate::{ast::BinOp, ir::{Block, Inst, OpC}};

pub fn fold(i: &Inst) -> Option<Rc<Inst>> {
    match i.opc {
        OpC::BinOp { op } => {
            let op0 = i.get_operand(0);
            let op1 = i.get_operand(1);
            match (op, &op0.opc, &op1.opc) {
                // FIXME: Use APInt!
                (BinOp::Add, OpC::Const { val: op0 }, OpC::Const { val: op1 }) => Some(Inst::new(&i.ty, OpC::Const { val: op0 + op1 }, None, &[])),
                (BinOp::Mul, OpC::Const { val: op0 }, OpC::Const { val: op1 }) => Some(Inst::new(&i.ty, OpC::Const { val: op0 * op1 }, None, &[])),
                _ => None
            }
        }
        _ => None
    }
}

pub fn run(bbs: &[Rc<Block>]) -> usize {
    let mut num_folded = 0;
    let mut instrs = Vec::new();
    let mut change = true;
    while change {
        change = false;
        for b in bbs {
            instrs.clone_from(&*b.instrs.borrow());
            for i in instrs.iter() {
                if !i.has_sideeffect() && i.num_uses() == 0 {
                    println!("    dce: {}", i);
                    i.drop_operands_and_unlink();
                    change = true;
                    continue;
                }

                if let Some(folded) = fold(&**i) {
                    println!("folding: {}", i);
                    println!("     to: {}", folded);
                    folded.insert_before(i);
                    i.replace_all_uses_with(&folded);
                    i.drop_operands_and_unlink();
                    num_folded += 1;
                    change = true;
                }
            }
        }
    }
    num_folded
}
