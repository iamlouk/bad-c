use crate::{
    ast::{BinOp, Decl, Expr, Function, Type},
    SLoc, Target,
};
use bit_set::BitSet;
use std::rc::{Rc, Weak};
use std::{
    cell::{Cell, RefCell},
    collections::VecDeque,
};
use std::{collections::HashMap, fmt::Write};

static IDGEN: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);

pub type PReg = (u32, &'static str);

#[derive(Debug, Clone, Copy)]
pub enum PLoc {
    None,
    Reg(PReg),
    Stack(usize),
}

pub enum OpC {
    NOp,
    Alloca { decl: Rc<Decl> },
    Global { decl: Rc<Decl> },
    Arg { idx: usize },
    Phi { name: Option<Rc<str>> },
    BinOp { signed: bool, op: BinOp },
    Cmp { signed: bool, op: BinOp },
    Cast,
    Br,
    Call,
    Ret,
    Load { is_volatile: bool },
    Store { is_volatile: bool },
    Const { val: i64 },
    Undef,
}

pub struct Inst {
    pub idx: Cell<usize>,
    visited: Cell<bool>,
    block: RefCell<Weak<Block>>,
    pub users: RefCell<Vec<(usize, Rc<Inst>)>>,
    pub ops: RefCell<Vec<Rc<Inst>>>,
    pub ty: Type,
    pub ploc: Cell<PLoc>,
    pub sloc: Option<SLoc>,
    pub opc: OpC,
}

pub struct Block {
    pub idx: Cell<usize>,
    pub visited: Cell<bool>,
    pub idom: Cell<Option<usize>>,
    pub doms: RefCell<BitSet>,
    pub preds: RefCell<Vec<Rc<Block>>>,
    pub succs: RefCell<Vec<Rc<Block>>>,
    pub instrs: RefCell<Vec<Rc<Inst>>>,
}

pub struct Loop {
    pub header: Option<Rc<Block>>,
    pub bbs: BitSet,
    pub latches: BitSet,
    pub subloops: RefCell<Vec<Rc<Loop>>>,
}

impl std::cmp::PartialEq for Inst {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self as *const _, other as *const _)
    }
}

impl std::cmp::Eq for Inst {}

impl std::hash::Hash for Inst {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        state.write_usize(self as *const _ as usize);
    }
}

impl std::fmt::Display for Inst {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.ploc.get() {
            PLoc::Reg((_, name)) => write!(f, "{}:\t", name)?,
            PLoc::Stack(off) => write!(f, "stack[{}]:\t", off)?,
            PLoc::None => (),
        }
        if self.ty != Type::Void {
            write!(f, "%{}:{} = ", self.idx.get(), self.ty)?;
        }
        let ops = self.ops.borrow();
        match &self.opc {
            OpC::NOp => write!(f, "nop")?,
            OpC::Undef => write!(f, "undef")?,
            OpC::Alloca { decl } => {
                write!(f, "alloca {} (name={:?})", decl.ty, decl.name.as_ref(),)?
            }
            OpC::Arg { idx } => write!(f, "farg#{}", idx)?,
            OpC::Global { decl } => write!(f, "global {}", decl.ty)?,
            OpC::Phi { name: None } => write!(f, "phi")?,
            OpC::Phi { name: Some(name) } => write!(f, "phi('{}')", name.as_ref())?,
            OpC::BinOp { signed: _, op } => {
                assert!(ops.len() == 2 && !Expr::is_cmp(*op));
                write!(
                    f,
                    "%{}:{} {} %{}:{}",
                    ops[0].idx.get(),
                    &ops[0].ty,
                    Expr::binop_to_str(*op),
                    ops[1].idx.get(),
                    &ops[0].ty
                )?;
                return Ok(())
            }
            OpC::Cmp { signed: _, op } => {
                assert!(ops.len() == 2 && Expr::is_cmp(*op));
                write!(
                    f,
                    "%{}:{} {} %{}:{}",
                    ops[0].idx.get(),
                    &ops[0].ty,
                    Expr::binop_to_str(*op),
                    ops[1].idx.get(),
                    &ops[0].ty
                )?;
                return Ok(())
            }
            OpC::Cast => write!(f, "cast to {}:", self.ty)?,
            OpC::Br => write!(f, "br")?,
            OpC::Call => write!(f, "call")?,
            OpC::Ret => write!(f, "ret")?,
            OpC::Const { val } => write!(f, "const {}", val)?,
            OpC::Load { is_volatile: _ } => write!(f, "load from")?,
            OpC::Store { is_volatile: _ } => write!(f, "store to")?,
        }
        for (i, op) in ops.iter().enumerate() {
            if i != 0 {
                f.write_char(',')?;
            }
            write!(f, " %{}:{}", op.idx.get(), &op.ty)?;
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
            ploc: Cell::new(PLoc::None),
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
            | OpC::Undef
            | OpC::Cast
            | OpC::Phi { .. }
            | OpC::Arg { .. }
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

    pub fn is_phi(&self) -> bool {
        matches!(self.opc, OpC::Phi { .. })
    }
    pub fn is_arg(&self) -> bool {
        matches!(self.opc, OpC::Arg { .. })
    }
    pub fn is_load(&self) -> bool {
        matches!(self.opc, OpC::Load { .. })
    }
    pub fn is_store(&self) -> bool {
        matches!(self.opc, OpC::Store { .. })
    }
    pub fn is_alloca(&self) -> bool {
        matches!(self.opc, OpC::Alloca { .. })
    }
    pub fn get_decl(&self) -> Rc<Decl> {
        match &self.opc {
            OpC::Alloca { decl } => decl.clone(),
            _ => panic!("not a decl."),
        }
    }

    pub fn get_block(&self) -> Rc<Block> {
        let block = self.block.borrow();
        block.upgrade().unwrap().clone()
    }

    pub fn get_operand(&self, idx: usize) -> Rc<Inst> {
        self.ops.borrow()[idx].clone()
    }

    pub fn replace_all_uses_with(&self, newval: Rc<Inst>) {
        assert_eq!(self.ty, newval.ty);
        let mut uses = self.users.borrow_mut();
        let mut newuses = newval.users.borrow_mut();
        for (useidx, user) in uses.iter() {
            user.ops.borrow_mut()[*useidx] = newval.clone();
            newuses.push((*useidx, user.clone()));
        }
        uses.clear();
    }

    pub fn insert_before(self: &Rc<Inst>, inst: Rc<Inst>) {
        let block = inst.get_block();
        let idx = block.instrs.borrow().iter().position(|i| **i == *inst).unwrap();
        block.insert(idx, self.clone());
    }
}

impl std::fmt::Display for Block {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, ".bb{}:\t; preds=[", self.idx.get())?;
        let preds = self.preds.borrow();
        for pred in preds.iter() {
            write!(f, " .bb{}", pred.idx.get())?;
        }
        if let Some(idom) = self.idom.get() {
            writeln!(f, " ], idom=.bb{}", idom)?;
        } else {
            writeln!(f, " ]")?;
        }
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

impl std::cmp::Eq for Block {}

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
            idom: Cell::new(None),
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

    pub fn insert(self: &Rc<Block>, idx: usize, inst: Rc<Inst>) {
        assert!(inst.block.borrow().upgrade().is_none());
        *inst.block.borrow_mut() = Rc::downgrade(self);
        self.instrs.borrow_mut().insert(idx, inst.clone());
    }

    pub fn insert_before_terminator(self: &Rc<Block>, inst: Rc<Inst>) {
        assert!(inst.block.borrow().upgrade().is_none());
        *inst.block.borrow_mut() = Rc::downgrade(self);
        let mut instrs = self.instrs.borrow_mut();
        let pos = instrs.len() - 2;
        instrs.insert(pos, inst);
    }

    // Return true if a dominates b.
    pub fn dominates(a: &Block, b: &Block) -> bool {
        let bdoms = b.doms.borrow();
        bdoms.contains(a.idx.get())
    }

    pub fn get_terminator(&self) -> Rc<Inst> {
        let instrs = self.instrs.borrow();
        let term = instrs.last().unwrap();
        assert!(matches!(term.opc, OpC::Br | OpC::Ret));
        term.clone()
    }

    pub fn num_preds(&self) -> usize {
        self.preds.borrow().len()
    }
    pub fn num_succs(&self) -> usize {
        self.succs.borrow().len()
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

        let mut idx = blocks.len();
        for b in blocks.iter() {
            let instrs = b.instrs.borrow();
            for i in instrs.iter() {
                i.idx.set(idx);
                idx += 1;
            }
        }
    }

    // Fixpoint alg. for calculating dominance.
    pub fn recalc_doms_and_verify(blocks: &[Rc<Block>]) {
        let entry = blocks[0].clone();
        entry.idx.set(0);
        entry.idom.set(None);
        let mut doms = entry.doms.borrow_mut();
        doms.clear();
        doms.insert(0);
        for (i, b) in blocks.iter().enumerate().skip(1) {
            b.idx.set(i);
            b.idom.set(None);
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
                let preds = b.preds.borrow();
                for pred in preds.iter() {
                    dset.intersect_with(&*pred.doms.borrow());
                }
                dset.insert(b.idx.get());
                change |= dset != *b.doms.borrow();
                b.doms.borrow_mut().clone_from(&dset);
            }
        }

        // Some basic checks that the CFG is valid:
        for b in blocks {
            let preds = b.preds.borrow();
            if b.idx.get() != 0 && preds.len() > 0 {
                // BBs should always be in RPO order here, so simply selecting the BB common in all
                // predecessor dom. sets with the highest idx should work?
                let mut bs = preds[0].doms.borrow().clone();
                for pred in preds.iter().skip(1) {
                    bs.intersect_with(&pred.doms.borrow());
                }
                b.idom.set(bs.iter().last());
            }

            let instrs = b.instrs.borrow();
            let succs = b.succs.borrow();
            let len = instrs.len();
            for (i, inst) in instrs.iter().enumerate() {
                for (useidx, user) in inst.users.borrow().iter() {
                    let userop = &user.ops.borrow()[*useidx];
                    assert!(std::ptr::eq(&**inst as *const _, &**userop as *const _));
                    assert!(inst.is_phi() || Inst::dominates(inst, userop));
                }

                let block = inst.block.borrow().upgrade().unwrap();
                assert!(block.idx == b.idx);

                let ops = inst.ops.borrow();
                match inst.opc {
                    OpC::Arg { .. } => assert!(b.idx.get() == 0),
                    OpC::Phi { .. } => {
                        assert!(ops.len() == preds.len());
                        for (op, pred) in ops.iter().zip(preds.iter()) {
                            assert!(Block::dominates(&op.block.borrow().upgrade().unwrap(), pred));
                        }
                        continue;
                    }
                    OpC::Store { .. } => {
                        assert!(inst.ty == Type::Void && ops.len() == 2);
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

impl Loop {
    pub fn write(&self, w: &mut dyn std::fmt::Write, ident: &mut String) -> std::fmt::Result {
        let subloops = self.subloops.borrow();
        if let Some(header) = &self.header {
            writeln!(w, "{}- header: .bb{}", ident, header.idx.get())?;
            writeln!(w, "{}  latches: {:?}", ident, self.latches)?;
            writeln!(w, "{}  bbs: {:?}", ident, self.bbs)?;
            writeln!(w, "{}  subloops: #{}", ident, subloops.len())?;
        } else {
            writeln!(w, "{}- outer-most loops: #{}", ident, subloops.len())?;
        }
        ident.push('\t');
        for l in subloops.iter() {
            l.write(w, ident)?;
        }
        ident.pop();
        Ok(())
    }

    fn add_bbs(&mut self, latch: &Rc<Block>) {
        let header = self.header.as_ref().unwrap();
        eprintln!("loops: new backage: .bb{} -> .bb{}", latch.idx.get(), header.idx.get());
        let mut worklist: Vec<Rc<Block>> = Vec::new();
        worklist.push(latch.clone());
        while let Some(b) = worklist.pop() {
            let new = self.bbs.insert(b.idx.get());
            if !new || b == *header {
                continue;
            }

            for pred in b.preds.borrow().iter() {
                if Block::dominates(header, pred) {
                    worklist.push(pred.clone());
                }
            }
        }
    }

    pub fn get_loop_for(self: &Rc<Loop>, idx: usize) -> Rc<Loop> {
        assert!(self.bbs.contains(idx));
        for sl in self.subloops.borrow().iter() {
            if sl.bbs.contains(idx) {
                return sl.get_loop_for(idx)
            }
        }
        self.clone()
    }

    pub fn find(bbs: &[Rc<Block>]) -> Rc<Loop> {
        let mut root = Rc::new(Loop {
            header: None,
            bbs: BitSet::new(),
            latches: BitSet::new(),
            subloops: RefCell::new(Vec::new()),
        });

        let rootref = Rc::get_mut(&mut root).unwrap();
        let mut headers: HashMap<Rc<Block>, Rc<Loop>> = HashMap::new();
        for b in bbs.iter() {
            rootref.bbs.insert(b.idx.get());
            for succ in b.succs.borrow().iter() {
                if Block::dominates(succ, b) {
                    if let Some(l) = headers.get_mut(succ) {
                        let l = Rc::get_mut(l).unwrap();
                        l.latches.insert(b.idx.get());
                        l.add_bbs(b);
                    } else {
                        let mut l = Loop {
                            header: Some(succ.clone()),
                            bbs: BitSet::new(),
                            latches: BitSet::new(),
                            subloops: RefCell::new(Vec::new()),
                        };
                        l.latches.insert(b.idx.get());
                        l.add_bbs(b);
                        headers.insert(succ.clone(), Rc::new(l));
                    }
                }
            }
        }

        for b in bbs.iter() {
            if let Some(l) = headers.get(b) {
                let parent = root.get_loop_for(b.idx.get());
                parent.subloops.borrow_mut().push(l.clone());
            }
        }

        root
    }
}

impl Function {
    pub fn write_ir(&self, w: &mut dyn std::fmt::Write) -> std::fmt::Result {
        write!(w, "function {}(", self.name.as_ref())?;
        for (i, (name, ty)) in self.args.iter().enumerate() {
            write!(w, "{}{}: {}", if i == 0 { "" } else { ", " }, name.as_ref(), ty)?;
        }
        writeln!(w, ") -> {} {{", self.retty)?;
        let bbs = self.ir.borrow();
        for bb in bbs.iter() {
            writeln!(w, "{}", bb)?;
        }
        w.write_str("}\n\n")
    }

    pub fn opt(&self, passes: &[String], target: &dyn Target) -> Result<bool, String> {
        eprintln!("=== {} ({}) ===", self.name.as_ref(), target.name());
        let mut bbs = self.ir.borrow_mut();
        Block::reorder_into_rpo(&mut bbs);
        Block::recalc_doms_and_verify(&bbs);
        drop(bbs);
        let mut changed = false;
        for pass in passes {
            use crate::dce;
            use crate::mem2reg;
            use crate::regalloc;
            match pass.as_str() {
                "dce" => {
                    eprintln!("--- DCE ---");
                    let bbs = self.ir.borrow();
                    changed |= dce::run(&bbs) > 0
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
