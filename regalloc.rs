use std::rc::Rc;

use bit_set::BitSet;

use crate::ast::Function;
use crate::ast::Type;
use crate::ir::*;
use crate::Target;

pub fn run(_f: &Function, bbs: &[Rc<Block>], target: &dyn Target) {
    let argregs = target.argument_regs();

    let mut uses: Vec<BitSet> = bbs.iter().map(|_| BitSet::new()).collect();
    let mut defs = uses.clone();
    let mut liveins = uses.clone();
    let mut liveouts = uses.clone();
    let mut alivevals = Vec::new();
    for arg in bbs[0].instrs.borrow().iter() {
        if let OpC::Arg { idx } = &arg.opc {
            liveins[0].insert(arg.idx.get());
            uses[0].insert(arg.idx.get());
            arg.ploc.set(match argregs.get(*idx) {
                Some(preg) => PLoc::Reg(*preg),
                None => unimplemented!("arguments passed on the stack")
            });
            alivevals.push(arg.clone());
        }
    }

    for (bidx, bb) in bbs.iter().enumerate() {
        assert!(bidx == bb.idx.get());
        let preds = bb.preds.borrow();
        for i in bb.instrs.borrow().iter() {
            if i.is_phi() {
                // PHIs, at least in terms of his live-value analysis, are modeled like
                // a move into the PHI in each of the preds, not as a instruction in
                // the BB itself.
                uses[bidx].insert(i.idx.get());
                for (opidx, op) in i.ops.borrow().iter().enumerate() {
                    let predbb = preds[opidx].idx.get();
                    if op.get_block().idx.get() != predbb {
                        uses[predbb].insert(op.idx.get());
                    }
                    defs[predbb].insert(i.idx.get());
                }
            } else {
                if i.ty != Type::Void {
                    defs[bidx].insert(i.idx.get());
                }
                for op in i.ops.borrow().iter() {
                    if op.is_phi() || op.get_block() != *bb {
                        uses[bidx].insert(op.idx.get());
                    }
                }
            }
        }
    }
    for (bidx, _) in bbs.iter().enumerate() {
        eprintln!("defs for .bb{}: {:?}", bidx, &defs[bidx]);
        eprintln!("uses for .bb{}: {:?}", bidx, &uses[bidx]);
    }

    let mut changed = true;
    let mut iters = 0;
    while changed {
        iters += 1;
        changed = false;
        for (bidx, bb) in bbs.iter().enumerate().rev() {
            let mut li = liveouts[bidx].clone();
            li.difference_with(&defs[bidx]);
            li.union_with(&uses[bidx]);
            changed |= li != liveins[bidx];
            liveins[bidx] = li;

            let mut lo = BitSet::new();
            for s in bb.succs.borrow().iter() {
                lo.union_with(&liveins[s.idx.get()]);
            }
            changed |= lo != liveouts[bidx];
            liveouts[bidx] = lo;
        }
    }
    eprintln!("live-value analysis took {} iters:", iters);
    for (bidx, _) in bbs.iter().enumerate() {
        eprintln!("live-ins  for .bb{}: {:?}", bidx, &liveins[bidx]);
        eprintln!("live-outs for .bb{}: {:?}", bidx, &liveouts[bidx]);
    }

    // Super basic linear scan:
    let mut stack_offset: usize = 0;
    let mut free_regs: Vec<_> = target.callee_saved_regs().iter().rev().cloned().collect();
    for (bidx, bb) in bbs.iter().enumerate() {
        let liveout = &liveouts[bidx];
        for i in bb.instrs.borrow().iter() {
            if i.ty != Type::Void && !i.is_arg() {
                i.ploc.set(match free_regs.pop() {
                    Some(r) => PLoc::Reg(r),
                    None => {
                        let off = stack_offset;
                        stack_offset += i.ty.sizeof();
                        PLoc::Stack(off)
                    }
                });
                alivevals.push(i.clone());
            }

            alivevals.retain(|val| {
                if !val.is_arg() && !liveout.contains(val.idx.get()) {
                    for (_, u) in val.users.borrow().iter() {
                        if u.is_phi() || u.idx.get() >= i.idx.get() {
                            return true;
                        }
                    }
                    if let PLoc::Reg(r) = val.ploc.get() {
                        free_regs.push(r);
                    }
                    return false;
                }
                true
            })
        }
    }
}
