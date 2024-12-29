use std::rc::Rc;

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
            if **user == *self {
                continue;
            }
            if matches!(user.opc, OpC::Store { is_volatile: false }) {
                continue;
            }
            return false;
        }
        true
    }
}

fn run(bbs: &[Rc<Block>]) -> usize {
    let mut worklist = Vec::new();
    for bb in bbs {
        for inst in bb.instrs.borrow().iter() {
            if (!inst.has_sideeffect() && inst.num_uses() == 0) || inst.is_dead_store() {
                worklist.push(inst.clone());
            }
        }
    }

    let mut deleted = 0;
    while let Some(inst) = worklist.pop() {
        assert!(inst.num_uses() == 0 && !(inst.has_sideeffect() || inst.is_dead_store()));
        for op in inst.ops.borrow().iter() {
            assert!(op.num_uses() >= 1);
            if !op.has_sideeffect() && op.num_uses() == 1 {
                worklist.push(op.clone());
            }
        }
        deleted += 1;
        inst.drop_operands_and_unlink();
    }

    deleted
}
