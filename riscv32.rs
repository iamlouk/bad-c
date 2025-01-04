use crate::ir::*;
use crate::Target;

#[derive(Default)]
pub struct RISCV32 {}

impl RISCV32 {
    pub fn new() -> Self {
        Self {}
    }
}

impl Target for RISCV32 {
    fn name(&self) -> &'static str {
        "riscv32m"
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
}
