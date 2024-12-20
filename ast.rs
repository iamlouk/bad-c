use crate::{Error, SLoc};
use std::rc::Rc;

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
        match self {
            Self::Int { .. } => true,
            _ => false,
        }
    }
    pub fn is_pointer(&self) -> bool {
        match self {
            Self::Ptr { .. } => true,
            _ => false,
        }
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
            if &**fname == &*name {
                return Ok((ftyp.clone(), idx));
            }
        }

        return Err(Error::Type(
            sloc.clone(),
            self.clone(),
            "struct or union does not have such a field",
        ));
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
                // TODO: Check if ety is a function or array, and change repr. in that case.
                if *volatile {
                    write!(f, "volatile ")?;
                }
                if *constant {
                    write!(f, "constant ")?;
                }
                if *restrict {
                    write!(f, "restrict ")?;
                }
                write!(f, "*{}", decl)
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

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct Function {
    pub name: Rc<str>,
    pub sloc: SLoc,
    pub retty: Type,
    pub args: Vec<(Rc<str>, Type)>,
    pub body: Option<Box<Stmt>>,
    pub is_static: bool,
    pub locals: Vec<Rc<Decl>>,
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
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub enum Stmt {
    NoOp {
        sloc: SLoc,
        ident: u8,
    },
    Expr {
        sloc: SLoc,
        ident: u8,
        expr: Box<Expr>,
    },
    Decls {
        sloc: SLoc,
        ident: u8,
        decls: Vec<Rc<Decl>>,
    },
    Compound {
        sloc: SLoc,
        ident: u8,
        stmts: Vec<Stmt>,
    },
    While {
        sloc: SLoc,
        ident: u8,
        cond: Box<Expr>,
        body: Box<Stmt>,
    },
    For {
        sloc: SLoc,
        ident: u8,
        init: Box<Stmt>,
        cond: Box<Expr>,
        incr: Box<Expr>,
        body: Box<Stmt>,
    },
    If {
        sloc: SLoc,
        ident: u8,
        cond: Box<Expr>,
        then: Box<Stmt>,
        otherwise: Option<Box<Stmt>>,
    },
    Ret {
        sloc: SLoc,
        ident: u8,
        val: Option<Box<Expr>>,
    },
}

#[allow(dead_code)]
#[derive(Debug, Clone, Copy)]
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
    fn get_typ(&self) -> Type {
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

    fn binop_to_str(op: BinOp) -> &'static str {
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

    fn is_cmp(op: BinOp) -> bool {
        match op {
            BinOp::EQ | BinOp::NE | BinOp::GE | BinOp::GT | BinOp::LE | BinOp::LT => true,
            _ => false,
        }
    }

    fn is_assignable(&self) -> bool {
        match self {
            Expr::Id { .. } => true,
            Expr::Deref { .. } => true,
            Expr::FieldAccess { .. } => true,
            _ => false,
        }
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
                f.write_char(')')?;
                lhs.write(f)?;
                write!(f, ") {}= (", Expr::binop_to_str(*op))?;
                rhs.write(f)?;
                f.write_char(')')
            }
            Expr::Assign { op: None, lhs, rhs, .. } => {
                f.write_char(')')?;
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
