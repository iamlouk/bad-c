use std::rc::Rc;

use crate::ast::*;
use crate::ir::*;

fn gen_ir(f: &Function) {
    let mut bbs = f.ir.borrow_mut();
    assert!(bbs.is_empty());
    assert!(f.body.is_some(), "todo");
    let bb = Block::create_and_append(&mut bbs);
    for decl in f.decls.iter() {
        let i =
            Inst::new(decl.ty.clone(), OpC::Alloca { decl: decl.clone() }, Some(decl.sloc.clone()));
        *decl.stack_slot.borrow_mut() = Some(i.clone());
        bb.append(i);
    }

    // TODO: Insert a return in case of void functions?
    gen_ir_stmt(bb, f.body.as_ref().unwrap(), &mut bbs);
}

fn gen_ir_stmt(cbb: Rc<Block>, stmt: &Stmt, bbs: &mut Vec<Rc<Block>>) -> Rc<Block> {
    match stmt {
        Stmt::Compound { stmts, .. } => {
            let mut bb = cbb;
            for stmt in stmts {
                bb = gen_ir_stmt(bb, stmt, bbs);
            }
            bb
        }
        Stmt::If { cond, then, otherwise: None, sloc } => {
            let cond = gen_ir_expr(cbb.clone(), cond.as_ref());
            let cbr = Inst::new(Type::Void, OpC::Br, Some(sloc.clone()));
            cbr.add_operand(cond);
            cbb.append(cbr);

            let thenentrybb = Block::create_and_append(bbs);
            let thenexitbb = gen_ir_stmt(thenentrybb.clone(), then.as_ref(), bbs);
            let br = Inst::new(Type::Void, OpC::Br, Some(sloc.clone()));
            thenexitbb.append(br);

            let joinbb = Block::create_and_append(bbs);
            Block::connect(&cbb, &thenentrybb);
            Block::connect(&cbb, &joinbb);
            Block::connect(&thenexitbb, &joinbb);
            joinbb
        }
        _ => unimplemented!(),
    }
}

fn gen_ir_expr(cbb: Rc<Block>, expr: &Expr) -> Rc<Inst> {
    match expr {
        Expr::IntLit { sloc, typ, num } => {
            let i = Inst::new(typ.clone(), OpC::Const { val: *num }, Some(sloc.clone()));
            cbb.append(i.clone());
            i
        }
        _ => unimplemented!(),
    }
}
