use bit_set::BitSet;
use std::{collections::HashMap, rc::Rc};

use crate::ir::*;

pub fn run(bbs: &[Rc<Block>]) -> bool {
    // Find allocas that can be promoted.
    let mut allocas = Vec::new();
    for i in bbs[0].instrs.borrow().iter() {
        if let OpC::Alloca { decl } = &i.opc {
            if decl.is_argument || !i.does_not_escape() || i.num_uses() == 0 {
                continue;
            }
            eprintln!("promotable: {}", i);
            allocas.push(i.clone());
        }
    }

    // Calculate dominator frontiers.
    // block out values: map of (block-idx, alloca) to last stored value.
    let mut blockoutvals: HashMap<(usize, Rc<Inst>), Rc<Inst>> = HashMap::new();
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

        for inst in bb.instrs.borrow().iter().rev() {
            if inst.is_store() && allocas.contains(&inst.get_operand(0)) {
                let alloca = inst.get_operand(0);
                let val = inst.get_operand(1);
                blockoutvals.insert((bb.idx.get(), alloca), val);
            }
        }
    }
    for (idx, bb) in bbs.iter().enumerate() {
        assert_eq!(idx, bb.idx.get());
        eprintln!("dominance frontier of .bb{}: {:?}", idx, domfrontiers[idx]);
    }

    // Map of (block-idx, alloca) to phi. Created phis have no predecessors.
    // Also track the last store in every bb and add remember that value.
    let mut phis: HashMap<(usize, Rc<Inst>), Rc<Inst>> = HashMap::new();
    for a in allocas.iter() {
        for (_, user) in a.users.borrow().iter() {
            assert!(user.is_load() || user.is_store());
            let bb = user.get_block();
            if user.is_store() {
                let stored_val = user.get_operand(1);
                for df in domfrontiers[bb.idx.get()].iter() {
                    if phis.contains_key(&(df, a.clone())) {
                        continue;
                    }
                    let phi = Inst::new(stored_val.ty.clone(), OpC::Phi, None);
                    bbs[df].insert(0, phi.clone());
                    phis.insert((df, a.clone()), phi.clone());

                    blockoutvals.entry((df, a.clone())).or_insert(phi);
                }
            }
        }
    }

    for ((bbidx, alloca), val) in blockoutvals.iter() {
        eprintln!(".bb{}: out-val for %{}:{} -> {}", bbidx, alloca.idx.get(), alloca.ty, val);
    }

    // Add values to the phis.
    for ((bbidx, alloca), phi) in phis.iter() {
        let mut vals: HashMap<usize, Rc<Inst>> = HashMap::new();
        let preds = bbs[*bbidx].preds.borrow();
        for pred in preds.iter() {
            let predidx = pred.idx.get();
            let mut pred = pred.clone();
            loop {
                if let Some(val) = blockoutvals.get(&(pred.idx.get(), alloca.clone())) {
                    vals.insert(predidx, val.clone());
                    break;
                }
                if let Some(idom) = pred.idom.get() {
                    pred = bbs[idom].clone();
                } else {
                    let undef = Inst::new(phi.ty.clone(), OpC::Undef, None);
                    bbs[predidx].insert_before_terminator(undef.clone());
                    vals.insert(predidx, undef);
                    break;
                }
            }
        }
        for pred in preds.iter() {
            phi.add_operand(vals.get(&pred.idx.get()).unwrap().clone());
        }
    }

    // Finally, replace loads with phis/vals and delete stores.
    let mut instrs: Vec<Rc<Inst>> = Vec::new();
    let mut invals = phis;
    for bb in bbs {
        for alloca in allocas.iter() {
            let mut val = invals.get(&(bb.idx.get(), alloca.clone())).cloned();
            bb.instrs.borrow().clone_into(&mut instrs);
            for i in instrs.iter() {
                if i.is_store() && *i.get_operand(0) == **alloca {
                    val = Some(i.get_operand(1));
                }
                if i.is_load() && *i.get_operand(0) == **alloca {
                    let val = match &val {
                        Some(val) => val.clone(),
                        None => {
                            let undef = Inst::new(i.ty.clone(), OpC::Undef, None);
                            undef.insert_before(i.clone());
                            undef
                        }
                    };
                    i.replace_all_uses_with(val);
                }
            }

            if let Some(val) = val {
                for succ in bb.succs.borrow().iter() {
                    invals.entry((succ.idx.get(), alloca.clone())).or_insert(val.clone());
                }
            }
        }
    }

    !allocas.is_empty()
}
