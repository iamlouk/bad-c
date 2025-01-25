use std::rc::Rc;

use crate::ir::{Block, Loop};

impl Loop {
    pub fn get_preheader(self: &Loop) -> Option<Rc<Block>> {
        let h = self.header.clone()?;
        let preds = h.preds.borrow();
        let mut entries = preds.iter().filter(|pred| !self.bbs.contains(pred.idx.get()));
        let preheader = entries.next();
        if preheader.is_none_or(|p| p.num_succs() != 1) || entries.next().is_some() {
            return None
        }

        preheader.cloned()
    }
}

fn licm(bbs: &[Rc<Block>], l: Rc<Loop>) -> usize {
    let mut hoisted = 0;
    for sl in l.subloops.borrow().iter() {
        hoisted += licm(bbs, sl.clone());
    }

    let Some(preheader) = l.get_preheader() else {
        return hoisted;
    };

    eprintln!(
        "loop with header .bb{} has preheader .bb{}",
        l.header.clone().unwrap().idx.get(),
        preheader.idx.get()
    );
    let mut to_hoist = Vec::new();
    for bidx in l.bbs.iter() {
        let bb = bbs[bidx].clone();
        'instr_loop: for i in bb.instrs.borrow().iter().filter(|i| !i.has_sideeffect()) {
            for op in i.ops.borrow().iter() {
                let opbb = op.get_block();
                if l.bbs.contains(opbb.idx.get()) && !to_hoist.contains(op) {
                    continue 'instr_loop;
                }
            }
            eprintln!("hoisting into .bb{}: {}", preheader.idx.get(), i);
            to_hoist.push(i.clone());
            hoisted += 1;
        }
    }

    for i in to_hoist {
        i.unlink();
        i.insert_before(&preheader.get_terminator());
    }

    hoisted
}

// Simple loop invariant code motion.
pub fn run(bbs: &[Rc<Block>]) -> usize {
    let loops = Loop::find(bbs);
    licm(bbs, loops)
}
