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
                        &Type::Void,
                        OpC::Store { is_volatile: false },
                        Some(&decl.sloc),
                        &[alloca.as_ref().unwrap(), &expr],
                    );
                    cbb.append(&store);
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
            let ret = Inst::new(&Type::Void, OpC::Ret, Some(sloc), &[]);
            if let Some(val) = val {
                ret.add_operand(&gen_ir_expr(cbb.clone(), val));
            }
            cbb.append(&ret);

            // Create a new empty BB with no predecessor:
            Block::create_and_append(bbs)
        }
        Stmt::If { cond, then, otherwise, sloc } => {
            let cond = gen_ir_expr(cbb.clone(), cond.as_ref());
            let cbr = Inst::new(&Type::Void, OpC::Br, Some(sloc), &[&cond]);
            cbb.append(&cbr);

            let thenentrybb = Block::create_and_append(bbs);
            let thenexitbb = gen_ir_stmt(thenentrybb.clone(), then.as_ref(), bbs);
            let br = Inst::new(&Type::Void, OpC::Br, Some(sloc), &[]);
            thenexitbb.append(&br);

            let joinbb = Block::create_and_append(bbs);
            Block::connect(&cbb, &thenentrybb);
            Block::connect(&thenexitbb, &joinbb);
            if let Some(otherwise) = otherwise {
                let elsebb = Block::create_and_append(bbs);
                Block::connect(&cbb, &elsebb);
                let elsebb = gen_ir_stmt(elsebb, otherwise.as_ref(), bbs);
                Block::connect(&elsebb, &joinbb);
            } else {
                Block::connect(&cbb, &joinbb);
            }
            joinbb
        }
        Stmt::While { sloc, cond, body } => {
            let br = Inst::new(&Type::Void, OpC::Br, Some(sloc), &[]);
            cbb.append(&br);

            let condbb = Block::create_and_append(bbs);
            let cond = gen_ir_expr(condbb.clone(), cond.as_ref());
            let cbr = Inst::new(&Type::Void, OpC::Br, Some(sloc), &[&cond]);
            condbb.append(&cbr);

            let loopentrybb = Block::create_and_append(bbs);
            let loopexitbb = gen_ir_stmt(loopentrybb.clone(), body.as_ref(), bbs);
            let br = Inst::new(&Type::Void, OpC::Br, Some(sloc), &[]);
            loopexitbb.append(&br);

            let joinbb = Block::create_and_append(bbs);
            Block::connect(&cbb, &condbb);
            Block::connect(&condbb, &loopentrybb);
            Block::connect(&condbb, &joinbb);
            Block::connect(&loopexitbb, &condbb);
            joinbb
        }
        Stmt::For { sloc, init, cond, incr, body } => {
            let cbb = gen_ir_stmt(cbb, init.as_ref(), bbs);
            let br = Inst::new(&Type::Void, OpC::Br, Some(sloc), &[]);
            cbb.append(&br);

            let condbb = Block::create_and_append(bbs);
            let cond = gen_ir_expr(condbb.clone(), cond.as_ref());
            let cbr = Inst::new(&Type::Void, OpC::Br, Some(sloc), &[&cond]);
            condbb.append(&cbr);

            let loopentrybb = Block::create_and_append(bbs);
            let loopexitbb = gen_ir_stmt(loopentrybb.clone(), body.as_ref(), bbs);
            gen_ir_expr(loopexitbb.clone(), incr.as_ref());
            let br = Inst::new(&Type::Void, OpC::Br, Some(sloc), &[]);
            loopexitbb.append(&br);

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
            let i = Inst::new(typ, OpC::Const { val: (*num).into() }, Some(sloc), &[]);
            cbb.append(&i);
            i
        }
        Expr::Id { sloc, typ, decl, .. } => {
            let alloca = decl.stack_slot.borrow().as_ref().unwrap().clone();
            let i = Inst::new(typ, OpC::Load { is_volatile: false }, Some(sloc), &[&alloca]);
            cbb.append(&i);
            i
        }
        Expr::BinOp { sloc, typ, op, lhs, rhs } => {
            let lhs = gen_ir_expr(cbb.clone(), lhs.as_ref());
            let rhs = gen_ir_expr(cbb.clone(), rhs.as_ref());
            assert_eq!(lhs.ty, rhs.ty);
            let i = Inst::new(
                typ,
                if Expr::is_cmp(*op) { OpC::Cmp { op: *op } } else { OpC::BinOp { op: *op } },
                Some(sloc),
                &[&lhs, &rhs],
            );
            cbb.append(&i);
            i
        }
        Expr::UnaryOp { sloc, typ, op: UnaryOp::Neg, val } => {
            assert!(typ.is_numerical());
            let val = gen_ir_expr(cbb.clone(), val.as_ref());
            let zero = Inst::new(typ, OpC::Const { val: 0.into() }, Some(sloc), &[]);
            cbb.append(&zero);
            let sub = Inst::new(typ, OpC::BinOp { op: BinOp::Sub }, Some(sloc), &[&zero, &val]);
            cbb.append(&sub);
            sub
        }
        Expr::Assign { sloc, op, lhs, rhs, .. } => {
            let dst = match lhs.as_ref() {
                Expr::Id { decl, .. } => decl.stack_slot.borrow().as_ref().unwrap().clone(),
                Expr::Deref { ptr, .. } => gen_ir_expr(cbb.clone(), ptr),
                _ => unimplemented!(),
            };

            let mut val = gen_ir_expr(cbb.clone(), rhs.as_ref());
            if let Some(binop) = op {
                let load =
                    Inst::new(&val.ty, OpC::Load { is_volatile: false }, Some(sloc), &[&dst]);
                cbb.append(&load);
                let binop =
                    Inst::new(&val.ty, OpC::BinOp { op: *binop }, Some(sloc), &[&load, &val]);
                cbb.append(&binop);
                val = binop;
            }

            let store = Inst::new(
                &Type::Void,
                OpC::Store { is_volatile: false },
                Some(sloc),
                &[&dst, &val],
            );
            cbb.append(&store);
            val
        }
        Expr::AddressOf { sloc, typ, op } => match op.as_ref() {
            Expr::Deref { ptr, .. } => gen_ir_expr(cbb, ptr),
            Expr::Id { decl, .. } => {
                let alloca = decl.stack_slot.borrow().as_ref().unwrap().clone();
                alloca
            }
            _ => {
                _ = sloc;
                _ = typ;
                unimplemented!()
            }
        },
        Expr::Deref { sloc, typ, ptr } => match ptr.as_ref() {
            Expr::AddressOf { op, .. } => gen_ir_expr(cbb, op),
            _ => {
                let ptr = gen_ir_expr(cbb.clone(), ptr);
                let load = Inst::new(typ, OpC::Load { is_volatile: false }, Some(sloc), &[&ptr]);
                cbb.append(&load);
                load
            }
        },
        Expr::PtrAdd { sloc, pty, ptr, offset } => {
            let ptr = gen_ir_expr(cbb.clone(), ptr);
            let offset = gen_ir_expr(cbb.clone(), offset);
            let scale = Inst::new(
                &offset.ty,
                OpC::Const { val: pty.ety().sizeof().into() },
                Some(sloc),
                &[],
            );
            cbb.append(&scale);
            let scaled = Inst::new(
                &offset.ty,
                OpC::BinOp { op: BinOp::Mul },
                Some(sloc),
                &[&offset, &scale],
            );
            cbb.append(&scaled);
            let cast = Inst::new(pty, OpC::Cast, Some(sloc), &[&scaled]);
            cbb.append(&cast);
            let add = Inst::new(pty, OpC::BinOp { op: BinOp::Add }, Some(sloc), &[&ptr, &cast]);
            cbb.append(&add);
            add
        }
        _ => unimplemented!(),
    }
}

impl Function {
    pub fn gen_ir(&self) {
        let mut bbs = self.ir.borrow_mut();
        assert!(bbs.is_empty());
        assert!(self.body.is_some(), "todo");
        let bb = Block::create_and_append(&mut bbs);
        for (idx, decl) in self.decls.iter().filter(|d| d.is_argument).enumerate() {
            let arg = Inst::new(&decl.ty, OpC::Arg { idx }, Some(&decl.sloc), &[]);
            *decl.arg_ir_inst.borrow_mut() = Some(arg.clone());
            bb.append(&arg);
        }
        for decl in self.decls.iter() {
            let i = Inst::new(
                &Type::new_ptr(&decl.ty),
                OpC::Alloca { decl: decl.clone() },
                Some(&decl.sloc),
                &[],
            );
            *decl.stack_slot.borrow_mut() = Some(i.clone());
            bb.append(&i);
            if decl.is_argument {
                let arg = decl.arg_ir_inst.borrow().clone().unwrap();
                let s = Inst::new(
                    &Type::Void,
                    OpC::Store { is_volatile: false },
                    Some(&decl.sloc),
                    &[&i, &arg],
                );
                bb.append(&s);
            }
        }

        // TODO: Insert a return in case of void functions?
        gen_ir_stmt(bb, self.body.as_ref().unwrap(), &mut bbs);
    }
}
