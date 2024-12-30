use std::rc::Rc;

use crate::ast::*;
use crate::ir::*;

fn gen_ir_stmt(cbb: Rc<Block>, stmt: &Stmt, bbs: &mut Vec<Rc<Block>>) -> Rc<Block> {
    match stmt {
        Stmt::NoOp { .. } => cbb,
        Stmt::Decls { sloc: _, decls } => {
            for decl in decls {
                let alloca = decl.stack_slot.borrow();
                if let Some(init) = &decl.init {
                    let expr = gen_ir_expr(cbb.clone(), init.as_ref());
                    let store = Inst::new(
                        Type::Void,
                        OpC::Store { is_volatile: false },
                        Some(decl.sloc.clone()),
                    );
                    store.add_operand(alloca.as_ref().unwrap().clone());
                    store.add_operand(expr);
                    cbb.append(store);
                }
            }
            cbb
        }
        Stmt::Expr { sloc: _, expr } => {
            gen_ir_expr(cbb.clone(), expr.as_ref());
            cbb
        }
        Stmt::Compound { stmts, .. } => {
            let mut bb = cbb;
            for stmt in stmts {
                bb = gen_ir_stmt(bb, stmt, bbs);
            }
            bb
        }
        Stmt::Ret { sloc, val } => {
            let ret = Inst::new(Type::Void, OpC::Ret, Some(sloc.clone()));
            if let Some(val) = val {
                ret.add_operand(gen_ir_expr(cbb.clone(), val));
            }
            cbb.append(ret);

            // Create a new empty BB with no predecessor:
            Block::create_and_append(bbs)
        }
        Stmt::If { cond, then, otherwise, sloc } => {
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
            if let Some(otherwise) = otherwise {
                let elsebb = Block::create_and_append(bbs);
                Block::connect(&cbb, &elsebb);
                let elsebb = gen_ir_stmt(elsebb, otherwise.as_ref(), bbs);
                Block::connect(&elsebb, &joinbb);
            } else {
                Block::connect(&cbb, &joinbb);
                Block::connect(&thenexitbb, &joinbb);
            }
            joinbb
        }
        Stmt::While { .. } => unimplemented!(),
        Stmt::For { sloc, init, cond, incr, body } => {
            let cbb = gen_ir_stmt(cbb, init.as_ref(), bbs);
            let br = Inst::new(Type::Void, OpC::Br, Some(sloc.clone()));
            cbb.append(br);

            let condbb = Block::create_and_append(bbs);
            let cond = gen_ir_expr(condbb.clone(), cond.as_ref());
            let cbr = Inst::new(Type::Void, OpC::Br, Some(sloc.clone()));
            cbr.add_operand(cond);
            condbb.append(cbr);

            let loopentrybb = Block::create_and_append(bbs);
            let loopexitbb = gen_ir_stmt(loopentrybb.clone(), body.as_ref(), bbs);
            gen_ir_expr(loopexitbb.clone(), incr.as_ref());
            let br = Inst::new(Type::Void, OpC::Br, Some(sloc.clone()));
            loopexitbb.append(br);

            let joinbb = Block::create_and_append(bbs);
            Block::connect(&cbb, &condbb);
            Block::connect(&condbb, &loopentrybb);
            Block::connect(&condbb, &joinbb);
            Block::connect(&loopexitbb, &condbb);
            joinbb
        }
    }
}

fn gen_ir_expr(cbb: Rc<Block>, expr: &Expr) -> Rc<Inst> {
    match expr {
        Expr::IntLit { sloc, typ, num } => {
            let i = Inst::new(typ.clone(), OpC::Const { val: *num }, Some(sloc.clone()));
            cbb.append(i.clone());
            i
        }
        Expr::Id { sloc, typ, decl, .. } => {
            let alloca = decl.stack_slot.borrow().as_ref().unwrap().clone();
            let i = Inst::new(typ.clone(), OpC::Load { is_volatile: false }, Some(sloc.clone()));
            i.add_operand(alloca);
            cbb.append(i.clone());
            i
        }
        Expr::BinOp { sloc, typ, op, lhs, rhs } => {
            let lhs = gen_ir_expr(cbb.clone(), lhs.as_ref());
            let rhs = gen_ir_expr(cbb.clone(), rhs.as_ref());
            let i = Inst::new(
                typ.clone(),
                OpC::BinOp { signed: typ.is_signed(), op: *op },
                Some(sloc.clone()),
            );
            i.add_operand(lhs);
            i.add_operand(rhs);
            cbb.append(i.clone());
            i
        }
        Expr::Assign { sloc, op: None, lhs, rhs, .. } => match lhs.as_ref() {
            Expr::Id { decl, .. } => {
                let alloca = decl.stack_slot.borrow().as_ref().unwrap().clone();
                let rhs = gen_ir_expr(cbb.clone(), rhs.as_ref());
                let i =
                    Inst::new(Type::Void, OpC::Store { is_volatile: false }, Some(sloc.clone()));
                i.add_operand(alloca);
                i.add_operand(rhs);
                cbb.append(i.clone());
                i
            }
            _ => unimplemented!(),
        },
        _ => unimplemented!(),
    }
}

impl Function {
    pub fn gen_ir(&self) {
        let mut bbs = self.ir.borrow_mut();
        assert!(bbs.is_empty());
        assert!(self.body.is_some(), "todo");
        let bb = Block::create_and_append(&mut bbs);
        for decl in self.decls.iter() {
            let i = Inst::new(
                decl.ty.clone(),
                OpC::Alloca { decl: decl.clone() },
                Some(decl.sloc.clone()),
            );
            *decl.stack_slot.borrow_mut() = Some(i.clone());
            bb.append(i);
        }

        // TODO: Insert a return in case of void functions?
        gen_ir_stmt(bb, self.body.as_ref().unwrap(), &mut bbs);
    }
}
