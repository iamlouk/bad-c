use crate::{ir, Error, SLoc};
use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;

#[allow(dead_code)]
#[derive(Clone, PartialEq, Eq, PartialOrd, Debug)]
pub enum Type {
    Unknown,
    Void,
    Bool,
    Int { bits: u8, signed: bool },
    Ptr { ety: Rc<Type>, volatile: bool, constant: bool, restrict: bool },
    Array { ety: Rc<Type>, size: Option<usize> },
    Struct { name: Option<Rc<str>>, fields: Rc<Vec<(Rc<str>, Type)>> },
    Union { name: Option<Rc<str>>, fields: Rc<Vec<(Rc<str>, Type)>> },
    Enum { name: Option<Rc<str>>, vals: Rc<Vec<(Rc<str>, i32)>> },
    Fn { retty: Rc<Type>, argtys: Rc<Vec<(Rc<str>, Type)>> },
}

impl Type {
    pub fn is_bool(&self) -> bool {
        *self == Type::Bool
    }
    pub fn is_numerical(&self) -> bool {
        matches!(self, Self::Int { .. })
    }
    pub fn is_pointer(&self) -> bool {
        matches!(self, Self::Ptr { .. })
    }

    pub fn lookup_field(&self, sloc: &SLoc, name: Rc<str>) -> Result<(Type, usize), Error> {
        let fields = match self {
            Type::Struct { name: _, fields } => fields,
            Type::Union { name: _, fields } => fields,
            other => {
                return Err(Error::Type(sloc.clone(), other.clone(), "struct or union expected"))
            }
        };

        for (idx, (fname, ftyp)) in fields.iter().enumerate() {
            if **fname == *name {
                return Ok((ftyp.clone(), idx));
            }
        }

        Err(Error::Type(sloc.clone(), self.clone(), "struct or union does not have such a field"))
    }

    fn write(&self, decl: &str, f: &mut dyn std::fmt::Write) -> std::fmt::Result {
        match self {
            Type::Unknown => write!(f, "??? {}", decl),
            Type::Void => write!(f, "void {}", decl),
            Type::Bool => write!(f, "bool {}", decl),
            Type::Int { bits: 32, signed: false } => write!(f, "unsigned {}", decl),
            Type::Int { bits: 32, signed: true } => write!(f, "int {}", decl),
            Type::Int { bits: 64, signed: false } => write!(f, "unsigned long {}", decl),
            Type::Int { bits: 64, signed: true } => write!(f, "long int {}", decl),
            Type::Int { bits, signed } => {
                write!(f, "{}int{}_t {}", if *signed { "" } else { "u" }, bits, decl)
            }
            Type::Ptr { ety, volatile, constant, restrict } => {
                ety.write("", f)?;
                if *volatile {
                    f.write_str("volatile ")?;
                }
                if *constant {
                    f.write_str("constant ")?;
                }
                f.write_char('*')?;
                if *restrict {
                    f.write_str("restrict ")?;
                }
                f.write_str(decl)
            }
            Type::Array { ety, size: Some(size) } => {
                ety.write(decl, f)?;
                write!(f, "[{}]", size)
            }
            Type::Array { ety, size: None } => {
                ety.write(decl, f)?;
                f.write_str("[]")
            }
            Type::Struct { name: Some(name), fields: _ } => {
                write!(f, "struct {} {}", name, decl)
            }
            Type::Struct { name: None, fields } => {
                write!(f, "struct {{ ")?;
                for (field_name, typ) in fields.iter() {
                    typ.write(field_name, f)?;
                    f.write_str("; ")?;
                }
                write!(f, "}} {}", decl)
            }
            Type::Union { name: Some(name), fields: _ } => {
                write!(f, "union {} {}", name, decl)
            }
            Type::Union { name: None, fields } => {
                write!(f, "union {{ ")?;
                for (field_name, typ) in fields.iter() {
                    typ.write(field_name, f)?;
                    write!(f, ";")?;
                }
                write!(f, " }} {}", decl)
            }
            Type::Enum { name: Some(name), vals: _ } => {
                write!(f, "enum {} {}", name, decl)
            }
            Type::Enum { name: None, vals } => {
                write!(f, "enum {{")?;
                for (i, (name, val)) in vals.iter().enumerate() {
                    write!(f, "{}{} = {:x}", if i == 0 { " " } else { ", " }, name, val)?;
                }
                write!(f, " }} {}", decl)
            }
            Type::Fn { retty, argtys } => {
                retty.write("", f)?;
                write!(f, "(*{})(", decl)?;
                for (i, (arg_name, typ)) in argtys.iter().enumerate() {
                    typ.write(arg_name, f)?;
                    f.write_str(if i == 0 { "" } else { ", " })?;
                }
                f.write_char(')')
            }
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Bool => write!(f, "bool"),
            Type::Int { bits, signed: false } => write!(f, "u{}", bits),
            Type::Int { bits, signed: true } => write!(f, "s{}", bits),
            _ => self.write("", f)
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct Function {
    pub name: Rc<str>,
    pub sloc: SLoc,
    pub retty: Type,
    pub args: Vec<(Rc<str>, Type)>,
    pub body: Option<Box<Stmt>>,
    pub is_static: bool,
    pub decls: Vec<Rc<Decl>>,
    pub ir: RefCell<Vec<Rc<ir::Block>>>
}

impl Function {
    fn write(&self, f: &mut dyn std::fmt::Write) -> std::fmt::Result {
        f.write_str(if self.is_static { "static " } else { "extern " })?;
        self.retty.write("", f)?;
        f.write_str(&self.name)?;
        f.write_char('(')?;
        for (i, (argname, argty)) in self.args.iter().enumerate() {
            f.write_str(if i == 0 { "" } else { ", " })?;
            argty.write(argname, f)?;
        }
        if let Some(body) = &self.body {
            f.write_str(")\n")?;
            let mut ident = "   ".to_owned();
            body.write(&mut ident, f)?;
            f.write_str("\n")?;
        } else {
            f.write_str(";\n\n")?;
        }
        Ok(())
    }
}

#[allow(dead_code)]
#[derive(Debug)]
pub struct Decl {
    pub sloc: SLoc,
    pub is_argument: bool,
    pub is_local: bool,
    pub name: Rc<str>,
    pub ty: Type,
    pub init: Option<Box<Expr>>,
    pub idx: usize,
    pub stack_slot: std::cell::RefCell<Option<Rc<ir::Inst>>>
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub enum Stmt {
    NoOp { sloc: SLoc },
    Expr { sloc: SLoc, expr: Box<Expr> },
    Decls { sloc: SLoc, decls: Vec<Rc<Decl>> },
    Compound { sloc: SLoc, stmts: Vec<Stmt> },
    While { sloc: SLoc, cond: Box<Expr>, body: Box<Stmt> },
    For { sloc: SLoc, init: Box<Stmt>, cond: Box<Expr>, incr: Box<Expr>, body: Box<Stmt> },
    If { sloc: SLoc, cond: Box<Expr>, then: Box<Stmt>, otherwise: Option<Box<Stmt>> },
    Ret { sloc: SLoc, val: Option<Box<Expr>> },
}

impl Stmt {
    fn write(&self, ident: &mut String, f: &mut dyn std::fmt::Write) -> std::fmt::Result {
        f.write_str(ident)?;
        match self {
            Stmt::NoOp { .. } => f.write_str(";\n"),
            Stmt::Expr { expr, .. } => {
                expr.write(f)?;
                f.write_str(";\n")
            }
            Stmt::Decls { decls, .. } => {
                for (i, decl) in decls.iter().enumerate() {
                    if i != 0 {
                        f.write_str(ident)?;
                    }
                    decl.ty.write(&decl.name, f)?;
                    if let Some(init) = decl.init.as_ref() {
                        f.write_str(" = ")?;
                        init.write(f)?;
                    }
                    f.write_str(";\n")?;
                }
                Ok(())
            }
            Stmt::Compound { stmts, .. } => {
                f.write_str("{\n")?;
                ident.push(' ');
                for stmt in stmts {
                    stmt.write(ident, f)?;
                }
                ident.pop();
                f.write_str(ident)?;
                f.write_str("}\n")?;
                Ok(())
            }
            Stmt::While { cond, body, .. } => {
                f.write_str("while (")?;
                cond.write(f)?;
                f.write_str(")\n")?;
                ident.push(' ');
                body.write(ident, f)?;
                ident.pop();
                Ok(())
            }
            Stmt::For { init, cond, incr, body, .. } => {
                f.write_str("for (")?;
                init.write(&mut String::new(), f)?;
                f.write_str(ident)?;
                f.write_str("     ")?;
                cond.write(f)?;
                f.write_str("; ")?;
                incr.write(f)?;
                f.write_str(")\n")?;
                ident.push(' ');
                body.write(ident, f)?;
                ident.pop();
                Ok(())
            }
            Stmt::If { cond, then, otherwise, .. } => {
                f.write_str("if (")?;
                cond.write(f)?;
                f.write_str(")\n")?;
                ident.push(' ');
                then.write(ident, f)?;
                ident.pop();
                if let Some(otherwise) = otherwise {
                    f.write_str(ident)?;
                    f.write_str("else\n")?;
                    ident.push(' ');
                    otherwise.write(ident, f)?;
                    ident.pop();
                }
                Ok(())
            }
            Stmt::Ret { val: None, .. } => f.write_str("return;\n"),
            Stmt::Ret { val: Some(val), .. } => {
                f.write_str("return ")?;
                val.write(f)?;
                f.write_str(";\n")
            }
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinOp {
    EQ,
    NE,
    LT,
    LE,
    GT,
    GE,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXOr,
    LogicalAnd,
    LogicalOr,
}

#[allow(dead_code)]
#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
    Neg,
    LogicalNot,
    BitwiseNot,
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub enum Expr {
    Id { sloc: SLoc, typ: Type, name: Rc<str>, decl: Rc<Decl> },
    IntLit { sloc: SLoc, typ: Type, num: i64 },
    Assign { sloc: SLoc, typ: Type, op: Option<BinOp>, lhs: Box<Expr>, rhs: Box<Expr> },
    Cast { sloc: SLoc, typ: Type, val: Box<Expr> },
    UnaryOp { sloc: SLoc, typ: Type, op: UnaryOp, val: Box<Expr> },
    BinOp { sloc: SLoc, typ: Type, op: BinOp, lhs: Box<Expr>, rhs: Box<Expr> },
    Call { sloc: SLoc, typ: Type, func: Box<Expr>, args: Vec<Expr> },
    Deref { sloc: SLoc, typ: Type, ptr: Box<Expr> },
    FieldAccess { sloc: SLoc, typ: Type, obj: Box<Expr>, field: Rc<str>, idx: usize },
}

impl Expr {
    pub fn get_typ(&self) -> Type {
        (match self {
            Expr::Id { typ, .. } => typ,
            Expr::IntLit { typ, .. } => typ,
            Expr::Assign { typ, .. } => typ,
            Expr::Cast { typ, .. } => typ,
            Expr::UnaryOp { typ, .. } => typ,
            Expr::BinOp { typ, .. } => typ,
            Expr::Call { typ, .. } => typ,
            Expr::Deref { typ, .. } => typ,
            Expr::FieldAccess { typ, .. } => typ,
        })
        .clone()
    }

    fn is_id(&self) -> bool {
        matches!(self, Expr::Id { .. })
    }

    pub fn binop_to_str(op: BinOp) -> &'static str {
        match op {
            BinOp::Add => "+",
            BinOp::Sub => "-",
            BinOp::Mul => "*",
            BinOp::Div => "/",
            BinOp::Mod => "%",
            BinOp::BitwiseAnd => "&",
            BinOp::BitwiseOr => "|",
            BinOp::BitwiseXOr => "^",
            BinOp::EQ => "==",
            BinOp::NE => "!=",
            BinOp::GE => ">=",
            BinOp::GT => ">",
            BinOp::LE => "<=",
            BinOp::LT => "<",
            BinOp::LogicalOr => "||",
            BinOp::LogicalAnd => "&&",
        }
    }

    pub fn is_cmp(op: BinOp) -> bool {
        matches!(op, BinOp::EQ | BinOp::NE | BinOp::GE | BinOp::GT | BinOp::LE | BinOp::LT)
    }

    pub fn is_assignable(&self) -> bool {
        matches!(self, Expr::Id { .. } | Expr::Deref { .. } | Expr::FieldAccess { .. })
    }

    pub fn is_constant(&self, max: i64) -> Option<i64> {
        match self {
            Expr::IntLit { num, .. } if *num >= 0 && *num < max => Some(*num),
            _ => None,
        }
    }

    fn write(&self, f: &mut dyn std::fmt::Write) -> std::fmt::Result {
        match self {
            Expr::Id { name, .. } => write!(f, "{}", name),
            Expr::IntLit { num, .. } => write!(f, "{:#x}", num),
            Expr::Assign { op: Some(op), lhs, rhs, .. } => {
                f.write_char('(')?;
                lhs.write(f)?;
                write!(f, ") {}= (", Expr::binop_to_str(*op))?;
                rhs.write(f)?;
                f.write_char(')')
            }
            Expr::Assign { op: None, lhs, rhs, .. } => {
                f.write_char('(')?;
                lhs.write(f)?;
                f.write_str(") = (")?;
                rhs.write(f)?;
                f.write_char(')')
            }
            Expr::Cast { typ, val, .. } => {
                write!(f, "(")?;
                typ.write("", f)?;
                write!(f, ")")?;
                val.write(f)
            }
            Expr::UnaryOp { op, val, .. } => {
                f.write_char(match op {
                    UnaryOp::Neg => '-',
                    UnaryOp::BitwiseNot => '~',
                    UnaryOp::LogicalNot => '!',
                })?;
                f.write_char('(')?;
                val.write(f)?;
                f.write_char(')')
            }
            Expr::BinOp { op, lhs, rhs, .. } => {
                f.write_char('(')?;
                lhs.write(f)?;
                write!(f, ") {} (", Expr::binop_to_str(*op))?;
                rhs.write(f)?;
                f.write_char(')')
            }
            Expr::Call { func, args, .. } => {
                if !func.is_id() {
                    f.write_char('(')?;
                }
                func.write(f)?;
                if !func.is_id() {
                    f.write_char(')')?;
                }
                for (i, arg) in args.iter().enumerate() {
                    f.write_str(if i == 0 { "" } else { ", " })?;
                    arg.write(f)?;
                }
                write!(f, ")")
            }
            Expr::Deref { ptr, .. } => {
                f.write_str("*(")?;
                ptr.write(f)?;
                f.write_char(')')
            }
            Expr::FieldAccess { obj, field, .. } => {
                f.write_char('(')?;
                obj.write(f)?;
                f.write_str(").")?;
                f.write_str(field)
            }
        }
    }
}

#[derive(Debug)]
pub enum Entry {
    Structdef { sloc: SLoc, name: Rc<str>, ty: Type },
    Typedef { sloc: SLoc, name: Rc<str>, ty: Type },
    Variable { sloc: SLoc, name: Rc<str>, ty: Type, init: Option<Expr> },
    Function { f: Rc<Function> },
}

#[derive(Debug, Default)]
pub struct Unit {
    pub types: HashMap<Rc<str>, Type>,
    pub typedefs: HashMap<Rc<str>, Type>,
    pub entries: Vec<Entry>,
    pub local_decls: Vec<Rc<Decl>>,
    pub scopes: Vec<HashMap<Rc<str>, Rc<Decl>>>,
}

impl Unit {
    pub fn write(&self, w: &mut dyn std::fmt::Write) -> std::fmt::Result {
        for e in self.entries.iter() {
            match e {
                Entry::Typedef { name, ty, .. } => {
                    w.write_str("typedef ")?;
                    ty.write("", w)?;
                    w.write_char(' ')?;
                    w.write_str(name)?;
                    w.write_str("\n\n")?;
                }
                Entry::Function { f } => {
                    f.write(w)?;
                }
                _ => unimplemented!(),
            }
        }
        Ok(())
    }
}
