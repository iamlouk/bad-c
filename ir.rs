use crate::{ast::{BinOp, Decl, Expr, Type}, SLoc};
use std::rc::{Rc, Weak};
use std::cell::RefCell;
use bit_set::BitSet;

pub enum OpC {
    Alloca { decl: Rc<Decl> },
    Global { decl: Rc<Decl> },
    Phi,
    BinOp { signed: bool, op: BinOp },
    Cmp { signed: bool, op: BinOp },
    Cast,
    Br,
    Call,
    Ret,
    Load { is_volatile: bool },
    Store { is_volatile: bool },
    Const { signed: bool, bits: u8, val: i64 },
}

pub struct Inst {
    idx: usize,
    block: Weak<RefCell<Block>>,
    users: Vec<Weak<RefCell<Inst>>>,
    ops: Vec<Rc<RefCell<Inst>>>,
    ty: Type,
    reg: u64,
    sloc: SLoc,
    opc: OpC,
}

impl std::fmt::Display for Inst {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.ty != Type::Void {
            write!(f, "%{}:{} = ", self.idx, self.ty)?;
        }
        match &self.opc {
            OpC::Alloca { decl } => write!(f, "alloca {}", decl.ty)?,
            OpC::Global { decl } => write!(f, "global {}", decl.ty)?,
            OpC::Phi => write!(f, "phi")?,
            OpC::BinOp { signed: true, op } => write!(f, "{}s", Expr::binop_to_str(*op))?,
            OpC::BinOp { signed: false, op } => write!(f, "{}u", Expr::binop_to_str(*op))?,
            OpC::Cmp { signed: true, op } => write!(f, "cmps {}", Expr::binop_to_str(*op))?,
            OpC::Cmp { signed: false, op } => write!(f, "cmpu {}", Expr::binop_to_str(*op))?,
            OpC::Cast => write!(f, "cast from {} to {}:", self.ops[0].borrow().ty, self.ty)?,
            OpC::Br => write!(f, "br")?,
            OpC::Call => write!(f, "call")?,
            OpC::Ret => write!(f, "ret")?,
            OpC::Const { signed: false, bits, val } => write!(f, "const {}u{}", val, bits)?,
            OpC::Const { signed: true, bits, val } => write!(f, "const {}s{}", val, bits)?,
            OpC::Load { is_volatile: _ } => write!(f, "load")?,
            OpC::Store { is_volatile: _ } => write!(f, "store")?,
        }
        for op in self.ops.iter() {
            write!(f, " %{}", op.borrow().idx)?;
        }
        Ok(())
    }
}

pub struct Block {
    idx: usize,
    doms: BitSet,
    preds: Vec<Weak<RefCell<Block>>>,
    succs: Vec<Weak<RefCell<Block>>>,
    instrs: Vec<Rc<RefCell<Inst>>>,
}

impl Block {
    // Return true if a dominates b.
    pub fn dominates_blocks(a: &Block, b: &Block) -> bool { b.doms.contains(a.idx) }

    // Return true if a dominates b.
    pub fn dominates(a: &Inst, b: &Inst) -> bool {
        let block_a = a.block.upgrade().unwrap();
        let block_b = b.block.upgrade().unwrap();
        if block_a.borrow().idx != block_b.borrow().idx {
            return Self::dominates_blocks(&block_a.borrow(), &block_b.borrow())
        }

        let block = block_a.borrow();
        for inst in block.instrs.iter() {
            let inst = inst.borrow();
            if std::ptr::eq(a as *const _, &*inst as *const _) {
                return true
            }
            if std::ptr::eq(b as *const _, &*inst as *const _) {
                return false
            }
        }
        panic!("a or b not in their parent?!")
    }

    // Fixpoint alg. for calculating dominance.
    pub fn recalc_idoms(blocks: &[Rc<RefCell<Block>>]) {
        let mut entry = blocks[0].borrow_mut();
        entry.idx = 0;
        entry.doms.clear();
        entry.doms.insert(0);
        drop(entry);
        for (i, b) in blocks.iter().enumerate().skip(1) {
            let mut b = b.borrow_mut();
            b.idx = i;
            b.doms.clear();
            for i in 0..blocks.len() {
                b.doms.insert(i);
            }
        }

        let mut change = true;
        let mut dset = BitSet::new();
        while change {
            change = false;
            for b in blocks.iter().skip(1) {
                dset.clone_from(&b.borrow().doms);
                for pred in &b.borrow().preds {
                    let pred = pred.upgrade().unwrap();
                    let preddoms = &pred.borrow().doms;
                    dset.intersect_with(preddoms);
                }
                let mut b = b.borrow_mut();
                dset.insert(b.idx);
                change |= dset != b.doms;
                b.doms.clone_from(&dset);
            }
        }

        // Some basic checks that the CFG is valid:
        for b in blocks {
            let b = b.borrow();
            let len = b.instrs.len();
            assert!(len > 0);
            for inst in b.instrs[0..(len - 1)].iter() {
                let inst = inst.borrow();
                if matches!(inst.opc, OpC::Phi) {
                    assert!(inst.ops.len() == b.preds.len());
                    for (op, pred) in inst.ops.iter().zip(b.preds.iter()) {
                        let op = op.borrow();
                        let opbb = op.block.upgrade().unwrap();
                        let pred = pred.upgrade().unwrap();
                        assert!(Block::dominates_blocks(&opbb.borrow(), &pred.borrow()));
                    }
                    continue;
                }

                for op in inst.ops.iter() {
                    let op = op.borrow();
                    assert!(op.ty != Type::Void && Block::dominates(&op, &inst));
                }
            }
            let term = b.instrs[len - 1].borrow();
            assert!(term.ty == Type::Void);
            match term.opc {
                OpC::Br if term.ops.len() == 1 && term.ops[0].borrow().ty == Type::Bool => {
                    assert!(b.succs.len() == 2);
                    assert!(Block::dominates(&term.ops[0].borrow(), &term));
                },
                OpC::Br if term.ops.is_empty() => assert!(b.succs.len() == 1),
                OpC::Ret if term.ops.is_empty() => assert!(b.succs.is_empty()),
                OpC::Ret if term.ops.len() == 1 => {
                    let op = term.ops[0].borrow();
                    assert!(b.succs.is_empty() && op.ty == Type::Void);
                    assert!(Block::dominates(&op, &term));
                },
                _ => panic!("not a valid terminator: {}", term)
            }
        }
    }
}
