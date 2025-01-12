use std::{collections::HashSet, rc::Rc};

use crate::ir::*;

// Idea for a future mem2reg pass, simplifying DCE:
// - Calculate dominance fronteer set per block!
// - Check that there are no escaping uses.

impl Inst {
    // DT must be valid!
    pub fn is_dead_store(&self) -> bool {
        if !matches!(self.opc, OpC::Store { is_volatile: false }) {
            return false;
        }

        // For now, only check if stores to allocas are dead. Those are simpler to eliminate
        // because the store is never obserable from the outside.
        let ptr = self.ops.borrow()[0].clone();
        if !matches!(ptr.opc, OpC::Alloca { .. }) || !ptr.does_not_escape() {
            return false;
        }

        // A store is dead if:
        // - The alloca is only used as address by loads and stores (does not escape).
        // - There are no loads anymore in any block reachable from that of the store.
        // - Another store dominated by this store also dominates all using loads.
        // TODO: Do this complicated stuff...
        // Meanwhile: Just check if there is no load at all.
        for (_, user) in ptr.users.borrow().iter() {
            if matches!(user.opc, OpC::Store { is_volatile: false }) {
                continue;
            }
            return false;
        }
        true
    }
}

pub fn run(bbs: &[Rc<Block>]) -> usize {
    let mut deleted = 0;

    let mut worklistset = HashSet::new();
    for bb in bbs {
        for inst in bb.instrs.borrow().iter() {
            if (!inst.has_sideeffect() && inst.num_uses() == 0) || inst.is_dead_store() {
                worklistset.insert(inst.clone());
            }
        }
    }

    let mut worklist = Vec::new();
    for iter in 0.. {
        eprintln!("dce: iter={}, worklist-size={}", iter, worklistset.len());
        worklist.clear();
        worklistset.iter().map(Rc::clone).collect_into(&mut worklist);
        if worklist.is_empty() {
            break;
        }

        worklistset.clear();
        for i in worklist.iter() {
            assert!(i.num_uses() == 0 && (!i.has_sideeffect() || i.is_dead_store()));
            let ops = i.ops.borrow().clone();
            deleted += 1;
            eprintln!("dce: {}", i);
            worklistset.remove(i);
            i.drop_operands_and_unlink();
            for op in ops {
                if (!op.has_sideeffect() && op.num_uses() == 0) || op.is_dead_store() {
                    worklistset.insert(op.clone());
                } else if op.is_alloca() {
                    for (_, user) in op.users.borrow().iter() {
                        if (!user.has_sideeffect() && user.num_uses() == 0) || user.is_dead_store()
                        {
                            worklistset.insert(user.clone());
                        }
                    }
                }
            }
        }
    }

    deleted
}
