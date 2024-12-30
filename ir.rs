use crate::{
    ast::{BinOp, Decl, Expr, Function, Type},
    SLoc,
};
use bit_set::BitSet;
use std::fmt::Write;
use std::rc::{Rc, Weak};
use std::{
    cell::{Cell, RefCell},
    collections::VecDeque,
};

static IDGEN: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);

#[derive(Debug)]
pub enum OpC {
    NOp,
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
    Const { val: i64 },
}

#[derive(Debug)]
pub struct Inst {
    idx: Cell<usize>,
    visited: Cell<bool>,
    block: RefCell<Weak<Block>>,
    pub users: RefCell<Vec<(usize, Rc<Inst>)>>,
    pub ops: RefCell<Vec<Rc<Inst>>>,
    ty: Type,
    reg: Cell<u64>,
    sloc: Option<SLoc>,
    pub opc: OpC,
}

impl std::cmp::PartialEq for Inst {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self as *const _, other as *const _)
    }
}

impl std::hash::Hash for Inst {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        state.write_usize(self as *const _ as usize);
    }
}

impl std::fmt::Display for Inst {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.ty != Type::Void {
            write!(f, "%{}:{} = ", self.idx.get(), self.ty)?;
        }
        match &self.opc {
            OpC::NOp => write!(f, "nop")?,
            OpC::Alloca { decl } => {
                write!(f, "alloca {} (name={:?})", decl.ty, decl.name.as_ref())?
            }
            OpC::Global { decl } => write!(f, "global {}", decl.ty)?,
            OpC::Phi => write!(f, "phi")?,
            OpC::BinOp { signed: true, op } => write!(f, "{}s", Expr::binop_to_str(*op))?,
            OpC::BinOp { signed: false, op } => write!(f, "{}u", Expr::binop_to_str(*op))?,
            OpC::Cmp { signed: true, op } => write!(f, "cmps {}", Expr::binop_to_str(*op))?,
            OpC::Cmp { signed: false, op } => write!(f, "cmpu {}", Expr::binop_to_str(*op))?,
            OpC::Cast => write!(f, "cast to {}:", self.ty)?,
            OpC::Br => write!(f, "br")?,
            OpC::Call => write!(f, "call")?,
            OpC::Ret => write!(f, "ret")?,
            OpC::Const { val } => write!(f, "const {}", val)?,
            OpC::Load { is_volatile: _ } => write!(f, "load from")?,
            OpC::Store { is_volatile: _ } => write!(f, "store to")?,
        }
        let ops = self.ops.borrow();
        for (i, op) in ops.iter().enumerate() {
            if i != 0 {
                f.write_char(',')?;
            }
            write!(f, " %{}", op.idx.get())?;
        }
        Ok(())
    }
}

impl Inst {
    pub fn new(ty: Type, opc: OpC, sloc: Option<SLoc>) -> Rc<Self> {
        let idx = IDGEN.fetch_add(1, std::sync::atomic::Ordering::AcqRel);
        Rc::new(Self {
            idx: Cell::new(idx),
            visited: Cell::new(false),
            block: RefCell::new(Weak::new()),
            users: RefCell::new(Vec::new()),
            ops: RefCell::new(Vec::new()),
            ty,
            reg: Cell::new(u64::MAX),
            sloc,
            opc,
        })
    }

    // Return true if a dominates b.
    pub fn dominates(a: &Self, b: &Self) -> bool {
        let block_a = a.block.borrow().upgrade().unwrap();
        let block_b = b.block.borrow().upgrade().unwrap();
        if block_a.idx != block_b.idx {
            return Block::dominates(&block_b, &block_b)
        }

        let instrs = block_a.instrs.borrow();
        for inst in instrs.iter() {
            if std::ptr::eq(a as *const _, &**inst as *const _) {
                return true
            }
            if std::ptr::eq(b as *const _, &**inst as *const _) {
                return false
            }
        }
        panic!("a or b not in their parent?!")
    }

    pub fn add_operand(self: &Rc<Self>, op: Rc<Self>) {
        let mut ops = self.ops.borrow_mut();
        let mut users = op.users.borrow_mut();
        users.push((ops.len(), self.clone()));
        ops.push(op.clone());
    }

    pub fn has_sideeffect(&self) -> bool {
        match self.opc {
            OpC::NOp
            | OpC::Cast
            | OpC::Phi
            | OpC::Cmp { .. }
            | OpC::BinOp { .. }
            | OpC::Alloca { .. }
            | OpC::Load { is_volatile: false }
            | OpC::Global { .. }
            | OpC::Const { .. } => false,
            OpC::Load { is_volatile: true }
            | OpC::Call
            | OpC::Br
            | OpC::Ret
            | OpC::Store { .. } => true,
        }
    }

    pub fn num_uses(&self) -> usize {
        self.users.borrow().len()
    }

    pub fn does_not_escape(&self) -> bool {
        assert!(matches!(self.opc, OpC::Alloca { .. }));
        for (useidx, useop) in self.users.borrow().iter() {
            assert!(&*useop.ops.borrow()[*useidx] == self);
            match (&useop.opc, useidx) {
                (OpC::Load { is_volatile: false }, 0) => continue,
                (OpC::Store { is_volatile: false }, 0) => continue,
                _ => return false,
            }
        }
        true
    }

    pub fn drop_operands_and_unlink(&self) {
        assert!(self.num_uses() == 0);
        let mut ops = self.ops.borrow_mut();
        for (idx, op) in ops.iter().enumerate() {
            let mut users = op.users.borrow_mut();
            let idx = users.iter().position(|(uidx, uop)| *uidx == idx && **uop == *self).unwrap();
            users.remove(idx);
        }
        ops.clear();
        let block = self.block.borrow().upgrade().unwrap();
        let mut instrs = block.instrs.borrow_mut();
        let idx = instrs.iter().position(|op| **op == *self).unwrap();
        instrs.remove(idx);
    }
}

#[derive(Debug)]
pub struct Block {
    idx: Cell<usize>,
    pub visited: Cell<bool>,
    doms: RefCell<BitSet>,
    pub preds: RefCell<Vec<Rc<Block>>>,
    pub succs: RefCell<Vec<Rc<Block>>>,
    pub instrs: RefCell<Vec<Rc<Inst>>>,
}

impl std::fmt::Display for Block {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, ".bb{}:\n\t; preds=[", self.idx.get())?;
        let preds = self.preds.borrow();
        for pred in preds.iter() {
            write!(f, " .bb{}", pred.idx.get())?;
        }
        f.write_str(" ], doms=[")?;
        let doms = self.doms.borrow();
        for idx in doms.iter() {
            write!(f, " .bb{}", idx)?;
        }
        f.write_str(" ]\n")?;
        let instrs = self.instrs.borrow();
        for inst in instrs.iter() {
            f.write_char('\t')?;
            inst.fmt(f)?;
            f.write_char('\n')?;
        }
        f.write_str("\t; succs=[")?;
        let succs = self.succs.borrow();
        for succ in succs.iter() {
            write!(f, " .bb{}", succ.idx.get())?;
        }
        f.write_str(" ]\n")?;
        Ok(())
    }
}

impl std::cmp::PartialEq for Block {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self as *const _, other as *const _)
    }
}

impl std::hash::Hash for Block {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        state.write_usize(self as *const _ as usize);
    }
}

impl Block {
    pub fn create_and_append(bbs: &mut Vec<Rc<Block>>) -> Rc<Block> {
        let idx = IDGEN.fetch_add(1, std::sync::atomic::Ordering::AcqRel);
        let bb = Rc::new(Self {
            idx: Cell::new(idx),
            visited: Cell::new(false),
            doms: RefCell::new(BitSet::new()),
            preds: RefCell::new(Vec::new()),
            succs: RefCell::new(Vec::new()),
            instrs: RefCell::new(Vec::new()),
        });
        bbs.push(bb.clone());
        bb
    }

    pub fn connect(self: &Rc<Block>, succ: &Rc<Block>) {
        let mut succs = self.succs.borrow_mut();
        succs.push(succ.clone());
        let mut preds = succ.preds.borrow_mut();
        preds.push(self.clone());
    }

    pub fn disconnect(self: &Rc<Block>, succ: &Rc<Block>) {
        let mut succs = self.succs.borrow_mut();
        let mut preds = succ.preds.borrow_mut();
        let succidx = succs.iter().position(|s| s == succ).unwrap();
        let predidx = preds.iter().position(|p| p == self).unwrap();
        succs.remove(succidx);
        preds.remove(predidx);
    }

    pub fn append(self: &Rc<Block>, inst: Rc<Inst>) {
        assert!(inst.block.borrow().upgrade().is_none());
        *inst.block.borrow_mut() = Rc::downgrade(self);
        self.instrs.borrow_mut().push(inst.clone());
    }

    // Return true if a dominates b.
    pub fn dominates(a: &Block, b: &Block) -> bool {
        let bdoms = b.doms.borrow();
        bdoms.contains(a.idx.get())
    }

    pub fn reorder_into_rpo(blocks: &mut Vec<Rc<Block>>) {
        let entry = blocks[0].clone();
        assert!(entry.preds.borrow().is_empty());
        blocks.iter().for_each(|b| b.visited.set(false));

        let mut rpo = VecDeque::new();
        let mut worklist = vec![(entry, false)];
        while let Some((b, done)) = worklist.pop() {
            if done {
                rpo.push_front(b);
                continue;
            }
            if b.visited.get() {
                continue;
            }
            b.visited.set(true);
            worklist.push((b.clone(), true));
            for succ in b.succs.borrow().iter() {
                worklist.push((succ.clone(), false));
            }
        }

        #[allow(clippy::never_loop)]
        for b in blocks.iter().filter(|b| !rpo.contains(*b)) {
            let succs = b.succs.borrow().clone();
            for succ in succs {
                b.disconnect(&succ);
            }
            let instrs = b.instrs.borrow().clone();
            for i in instrs.iter().rev() {
                i.drop_operands_and_unlink();
            }
        }

        blocks.clear();
        rpo.into_iter().collect_into(blocks);
    }

    // Fixpoint alg. for calculating dominance.
    pub fn recalc_doms_and_verify(blocks: &[Rc<Block>]) {
        let entry = blocks[0].clone();
        entry.idx.set(0);
        let mut doms = entry.doms.borrow_mut();
        doms.clear();
        doms.insert(0);
        for (i, b) in blocks.iter().enumerate().skip(1) {
            b.idx.set(i);
            let mut doms = b.doms.borrow_mut();
            doms.clear();
            for i in 0..blocks.len() {
                doms.insert(i);
            }
        }
        drop(doms);

        let mut change = true;
        let mut dset = BitSet::new();
        while change {
            change = false;
            for b in blocks.iter().skip(1) {
                dset.clone_from(&b.doms.borrow());
                for pred in b.preds.borrow().iter() {
                    dset.intersect_with(&*pred.doms.borrow());
                }
                dset.insert(b.idx.get());
                change |= dset != *b.doms.borrow();
                b.doms.borrow_mut().clone_from(&dset);
            }
        }

        // Some basic checks that the CFG is valid:
        for b in blocks {
            let instrs = b.instrs.borrow();
            let preds = b.preds.borrow();
            let succs = b.succs.borrow();
            let len = instrs.len();
            for (i, inst) in instrs.iter().enumerate() {
                for (useidx, user) in inst.users.borrow().iter() {
                    let userop = &user.ops.borrow()[*useidx];
                    assert!(std::ptr::eq(&**inst as *const _, &**userop as *const _));
                    assert!(matches!(inst.opc, OpC::Phi) || Inst::dominates(inst, userop));
                }

                let block = inst.block.borrow().upgrade().unwrap();
                assert!(block.idx == b.idx);

                let ops = inst.ops.borrow();
                match inst.opc {
                    OpC::Phi => {
                        assert!(ops.len() == preds.len());
                        for (op, pred) in ops.iter().zip(preds.iter()) {
                            assert!(Block::dominates(&op.block.borrow().upgrade().unwrap(), pred));
                        }
                        continue;
                    }
                    OpC::Br => {
                        assert!(i == len - 1, "missing terminator");
                        assert!(inst.ty == Type::Void);
                        assert!(
                            (ops.len() == 0 && succs.len() == 1)
                                || (ops.len() == 1 && succs.len() == 2)
                        );
                    }
                    OpC::Ret => {
                        assert!(i == len - 1, "missing terminator");
                        assert!(inst.ty == Type::Void && succs.len() == 0 && ops.len() <= 1);
                    }
                    _ => {}
                }

                for op in ops.iter() {
                    assert!(op.ty != Type::Void && Inst::dominates(op, inst));
                }
            }
        }
    }
}

impl Function {
    pub fn write_ir(&self, w: &mut dyn std::fmt::Write) -> std::fmt::Result {
        write!(w, "function {}(", self.name.as_ref())?;
        for (i, (name, ty)) in self.args.iter().enumerate() {
            write!(w, "{}{}: {}", if i == 0 { "" } else { ", " }, name.as_ref(), ty)?;
        }
        write!(w, ") -> {} {{\n", self.retty)?;
        let bbs = self.ir.borrow();
        for bb in bbs.iter() {
            write!(w, "{}\n", bb)?;
        }
        w.write_str("}\n\n")
    }
}
