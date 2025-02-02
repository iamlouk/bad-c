use crate::ast::BinOp;
use crate::ast::Type;
use crate::ir::*;
use crate::Target;

#[derive(Default)]
pub struct RV64 {}

impl RV64 {
    pub fn new() -> Self {
        Self {}
    }
}

impl Target for RV64 {
    fn name(&self) -> &'static str {
        "rv64"
    }

    fn return_reg(&self) -> PReg {
        (10, "a0")
    }

    fn argument_regs(&self) -> &[crate::ir::PReg] {
        &[
            (10, "a0"),
            (11, "a1"),
            (12, "a2"),
            (13, "a3"),
            (14, "a4"),
            (15, "a5"),
            (16, "a6"),
            (17, "a7"),
        ]
    }

    fn temporary_regs(&self) -> &[crate::ir::PReg] {
        &[(5, "t0"), (6, "t1"), (7, "t2"), (28, "t3"), (29, "t4"), (30, "t5"), (31, "t6")]
    }

    fn callee_saved_regs(&self) -> &[crate::ir::PReg] {
        &[
            (9, "s1"),
            (18, "s2"),
            (19, "s3"),
            (20, "s4"),
            (21, "s5"),
            (22, "s6"),
            (23, "s7"),
            (24, "s8"),
            (25, "s9"),
            (26, "s10"),
            (27, "s11"),
        ]
    }

    fn write_function_prologue(&mut self, f: &crate::ast::Function, regalloc: &mut crate::RegAllocRes, w: &mut dyn std::fmt::Write) -> std::fmt::Result {
        let name = f.name.as_ref();
        writeln!(w, "\t.globl\t{}", name)?;
        writeln!(w, "\t.type\t{}, @function", name)?;
        regalloc.used_callee_save_regs.insert((8, "fp"));
        writeln!(w, "{}:\n\taddi\tsp, sp, -{}", name, regalloc.used_callee_save_regs.len() * 8)?;
        for (i, reg) in regalloc.used_callee_save_regs.iter().enumerate() {
            writeln!(w, "\tsd\t{}, {}(sp)", reg.1, (i + 1) * 8)?;
        }
        writeln!(w, "\tmv\tfp, sp")?;
        if regalloc.used_stack_size_without_allocs != 0 {
            writeln!(w, "\taddi\tsp, sp, -{}", regalloc.used_stack_size_without_allocs)?;
        }
        assert!(regalloc.used_stack_size_without_allocs == 0);
        Ok(())
    }

    fn write_label(&mut self, _f: &crate::ast::Function, bb: &std::rc::Rc<crate::ir::Block>, w: &mut dyn std::fmt::Write) -> std::fmt::Result {
        writeln!(w, ".bb{}:", bb.idx.get())
    }

    fn write_instr(&mut self, _f: &crate::ast::Function, i: &crate::ir::Inst, w: &mut dyn std::fmt::Write) -> std::fmt::Result {
        let mut opregs = vec![];
        for (idx, op) in i.ops.borrow().iter().enumerate() {
            opregs.push(match op.ploc.get() {
                PLoc::None => panic!("run regalloc first!"),
                PLoc::Reg(r) => r,
                PLoc::Stack(off, 4) => {
                    let treg = self.temporary_regs()[idx];
                    writeln!(w, "\tlw\t{}, -{}(fp)", treg.1, off)?;
                    treg
                }
                PLoc::Stack(off, 8) => {
                    let treg = self.temporary_regs()[idx];
                    writeln!(w, "\tld\t{}, -{}(fp)", treg.1, off)?;
                    treg
                }
                PLoc::Stack(_off, size) => unimplemented!("load from stack of size {}", size)
            })
        }

        let resreg = match i.ploc.get() {
            PLoc::None => (0xff, "<?>"),
            PLoc::Reg(r) => r,
            PLoc::Stack(_, _) => self.temporary_regs()[0]
        };

        match &i.opc {
            OpC::NOp => {},
            OpC::Arg { idx } => {
                assert_eq!(i.ploc.get(), PLoc::Reg(self.argument_regs()[*idx]));
            },
            OpC::Const { val } => writeln!(w, "\tli\t{}, {}", resreg.1, val)?,
            OpC::BinOp { op } => match (op, &i.ty) {
                (BinOp::Add, Type::Int { bits: 32, signed: _ }) => writeln!(w, "\taddw\t{}, {}, {}", resreg.1, opregs[0].1, opregs[1].1)?,
                (BinOp::Add, Type::Int { bits: 64, signed: _ }) => writeln!(w, "\tadd\t{}, {}, {}", resreg.1, opregs[0].1, opregs[1].1)?,
                (BinOp::Sub, Type::Int { bits: 32, signed: _ }) => writeln!(w, "\tsubw\t{}, {}, {}", resreg.1, opregs[0].1, opregs[1].1)?,
                (BinOp::Sub, Type::Int { bits: 64, signed: _ }) => writeln!(w, "\tsub\t{}, {}, {}", resreg.1, opregs[0].1, opregs[1].1)?,
                _ => unimplemented!("inst.: {}", i)
            }
            OpC::Ret => if i.num_ops() == 1 {
                writeln!(w, "\tmv\t{}, {}", self.return_reg().1, opregs[0].1)?;
            }
            _ => unimplemented!("inst.: {}", i)
        };

        if let PLoc::Stack(off, size) = i.ploc.get() {
            match size {
                4 => writeln!(w, "\tsw\t{}, -{}(fp)", resreg.1, off)?,
                8 => writeln!(w, "\tsd\t{}, -{}(fp)", resreg.1, off)?,
                _ => unimplemented!("store to stack of size {}", size)
            }
        }

        Ok(())
    }

    fn write_function_epilogue(&mut self, f: &crate::ast::Function, regalloc: &crate::RegAllocRes, w: &mut dyn std::fmt::Write) -> std::fmt::Result {
        let name = f.name.as_ref();
        writeln!(w, ".exit:")?;
        if regalloc.used_stack_size_without_allocs != 0 {
            writeln!(w, "\taddi\tsp, sp, {}", regalloc.used_stack_size_without_allocs)?;
        }
        for (i, reg) in regalloc.used_callee_save_regs.iter().enumerate().rev() {
            writeln!(w, "\tld\t{}, {}(sp)", reg.1, (i + 1) * 8)?;
        }
        writeln!(w, "\taddi\tsp, sp, {}\n\tret\n\t.size\t{}, .-{}", regalloc.used_callee_save_regs.len() * 8, name, name)?;
        Ok(())
    }
}
