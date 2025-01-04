use bit_set::BitSet;
use std::{collections::HashMap, rc::Rc};

use crate::ir::*;

// Replace phi nodes where all inc. values are the same or phi nodes that include
// themselves.
pub fn simplify_phis(bbs: &[Rc<Block>]) -> bool {
    let mut instrs: Vec<Rc<Inst>> = Vec::new();
    let mut changes = false;
    for bb in bbs.iter() {
        bb.instrs.borrow().clone_into(&mut instrs);
        for i in instrs.iter() {
            if !i.is_phi() {
                break;
            }

            let mut ok = true;
            let mut newval: Option<Rc<Inst>> = None;
            for op in i.ops.borrow().iter().filter(|op| **op != *i) {
                if newval.is_none() {
                    newval = Some(op.clone());
                    continue;
                } else if newval.clone().unwrap() == *op {
                    continue;
                }
                ok = false;
                break;
            }

            if ok {
                if let Some(newval) = newval {
                    i.replace_all_uses_with(newval);
                    i.drop_operands_and_unlink();
                    changes = true;
                }
            }
        }
    }
    changes
}

pub fn run(bbs: &[Rc<Block>]) -> bool {
    // Find allocas that can be promoted.
    let mut allocas = Vec::new();
    for i in bbs[0].instrs.borrow().iter() {
        if let OpC::Alloca { .. } = &i.opc {
            if !i.does_not_escape() || i.num_uses() == 0 {
                continue;
            }
            eprintln!("promotable to SSA value: {}", i);
            allocas.push(i.clone());
        }
    }
    if allocas.is_empty() {
        return false
    }

    // Calculate dominance frontiers.
    let mut domfrontiers: Vec<BitSet> = Vec::with_capacity(bbs.len());
    for bb in bbs {
        domfrontiers.push(BitSet::new());
        for dom in bb.doms.borrow().iter() {
            for succ in bb.succs.borrow().iter() {
                if !succ.doms.borrow().contains(dom) {
                    domfrontiers[dom].insert(succ.idx.get());
                }
            }
        }
    }
    eprintln!("dominance frontiers: {:?}", domfrontiers);

    // Map of (block-idx, alloca) to phi. Created phis have no predecessors.
    let mut phis: HashMap<(usize, Rc<Inst>), Rc<Inst>> = HashMap::new();
    for a in allocas.iter() {
        // For every alloca, create a PHI in the dominance frontier of any other block.
        let name = Some(a.get_decl().name.clone());
        for bb in bbs {
            for frontieridx in domfrontiers[bb.idx.get()].iter() {
                if phis.contains_key(&(frontieridx, a.clone())) {
                    continue;
                }

                let bb_needing_phi = bbs[frontieridx].clone();
                let phi = Inst::new(
                    (*a.ty.ety()).clone(),
                    OpC::Phi { name: name.clone() },
                    a.sloc.clone(),
                );
                bb_needing_phi.insert(0, phi.clone());
                phis.insert((frontieridx, a.clone()), phi);
            }
        }
    }

    // Map of (block-idx, alloca) to value leaving this BB: another phi or the last stored value.
    let mut blockoutvals: HashMap<(usize, Rc<Inst>), Rc<Inst>> = HashMap::new();
    for a in allocas.iter() {
        for bb in bbs {
            let mut outval = phis.get(&(bb.idx.get(), a.clone())).cloned();
            if outval.is_none() && bb.num_preds() == 1 {
                outval = blockoutvals.get(&(bb.preds.borrow()[0].idx.get(), a.clone())).cloned();
            }
            for i in bb.instrs.borrow().iter().rev() {
                if i.is_store() && i.get_operand(0) == *a {
                    outval = Some(i.get_operand(1));
                }
            }
            let outval = match outval {
                Some(val) => val,
                None => {
                    let undef = Inst::new((*a.ty.ety()).clone(), OpC::Undef, a.sloc.clone());
                    bb.insert_before_terminator(undef.clone());
                    undef
                }
            };
            // eprintln!(".bb{}: %{}:{} -> value leaving: {}",
            //           bb.idx.get(), a.idx.get(), a.ty, &outval);
            blockoutvals.insert((bb.idx.get(), a.clone()), outval);
        }
    }

    // Add operands to PHIs:
    for ((bbidx, a), phi) in phis.iter() {
        let preds = bbs[*bbidx].preds.borrow();
        for pred in preds.iter() {
            let val = blockoutvals.get(&(pred.idx.get(), a.clone())).unwrap();
            phi.add_operand(val.clone());
        }
    }

    // Finally, replace all loads/stores with uses of these PHIs:
    let mut instrs: Vec<Rc<Inst>> = Vec::new();
    for a in allocas.iter() {
        for bb in bbs.iter() {
            let preds = bb.preds.borrow();
            let mut val = match phis.get(&(bb.idx.get(), a.clone())) {
                Some(val) => Some(val.clone()),
                None => {
                    if preds.len() == 1 {
                        blockoutvals.get(&(preds[0].idx.get(), a.clone())).cloned()
                    } else {
                        assert!(preds.len() == 0);
                        None
                    }
                }
            };

            bb.instrs.borrow().clone_into(&mut instrs);
            for i in instrs.iter() {
                if i.is_store() && i.get_operand(0) == *a {
                    val = Some(i.get_operand(1));
                    i.drop_operands_and_unlink();
                }
                if i.is_load() && i.get_operand(0) == *a {
                    i.replace_all_uses_with(if let Some(val) = &val {
                        val.clone()
                    } else {
                        let undef = Inst::new((*a.ty.ety()).clone(), OpC::Undef, a.sloc.clone());
                        undef.insert_before(i.clone());
                        undef
                    });
                    i.drop_operands_and_unlink();
                }
            }
        }
    }

    // Remove some redundant PHIs.
    while simplify_phis(bbs) {}

    true
}
