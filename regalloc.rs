use std::rc::Rc;

use crate::ast::Function;
use crate::ir::*;
use crate::Target;

struct LiveInterval {
    preg: PReg,
    start: Rc<Inst>,
    lastuse: Option<Rc<Inst>>,
}

pub fn run(f: &Function, bbs: &[Rc<Block>], target: &dyn Target) {
    _ = f;
    _ = bbs;
    _ = target;
    unimplemented!()
}

