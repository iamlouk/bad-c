use std::cell::RefCell;
use std::collections::HashMap;

use crate::ast::*;
use crate::lex::*;
use crate::*;

impl Unit {
    pub fn new() -> Self {
        let mut s = Self::default();
        s.scopes.push(HashMap::new());
        s
    }

    pub fn lookup(&self, sloc: &SLoc, id: Rc<str>) -> Result<Rc<Decl>, Error> {
        for scope in self.scopes.iter().rev() {
            if let Some(d) = scope.get(&id) {
                return Ok(d.clone())
            }
        }
        Err(Error::UnknownSymbol(sloc.clone(), id.clone()))
    }

    pub fn parse(&mut self, lex: &mut Lexer) -> Result<(), Error> {
        loop {
            assert!(self.scopes.len() == 1);
            let is_static = lex.consume_if_next(Tok::Static)?;
            let is_extern = lex.consume_if_next(Tok::Extern)?;
            assert!(!(is_static && is_extern));
            let (sloc, tok) = lex.peek()?;
            if tok == Tok::EndOfFile {
                break;
            }

            if tok == Tok::SemiColon {
                lex.peeked.take();
                continue;
            }

            if tok == Tok::Typedef {
                let (name, ty) = parse_typedef(self, lex)?;
                self.entries.push(Entry::Typedef { sloc, name, ty });
                continue;
            }

            if let Ok(ty) = try_parse_type(self, lex) {
                let (_, name) = lex.expect_id("expected identifier after type")?;
                if lex.peek()?.1 == Tok::LParen {
                    let f = parse_function(self, lex, is_static, ty, name)?;
                    self.entries.push(Entry::Function { f });
                    continue;
                }

                unimplemented!()
            }

            unimplemented!("top-level construct: {}", tok)
        }

        Ok(())
    }
}

// Parses the `float*` part of `float *A[N]`.
fn try_parse_type(state: &mut Unit, lex: &mut Lexer) -> Result<Type, Error> {
    let mut ty = match lex.next()? {
        (_, Tok::Void) => Type::Void,
        (_, Tok::Bool) => Type::Bool,
        (_, Tok::Int) => Type::Int { bits: 32, signed: true },
        (_, Tok::Signed) => match lex.peek()?.1 {
            Tok::Char => {
                lex.peeked.take();
                Type::Int { bits: 8, signed: true }
            }
            Tok::Short => {
                lex.peeked.take();
                Type::Int { bits: 16, signed: true }
            }
            Tok::Int => {
                lex.peeked.take();
                Type::Int { bits: 32, signed: true }
            }
            Tok::Long => {
                lex.peeked.take();
                lex.consume_if_next(Tok::Int)?;
                Type::Int { bits: 64, signed: true }
            }
            _ => Type::Int { bits: 32, signed: false },
        },
        (_, Tok::Unsigned) => match lex.peek()?.1 {
            Tok::Char => {
                lex.peeked.take();
                Type::Int { bits: 8, signed: false }
            }
            Tok::Short => {
                lex.peeked.take();
                Type::Int { bits: 16, signed: false }
            }
            Tok::Int => {
                lex.peeked.take();
                Type::Int { bits: 32, signed: false }
            }
            Tok::Long => {
                lex.peeked.take();
                lex.consume_if_next(Tok::Int)?;
                Type::Int { bits: 64, signed: false }
            }
            _ => Type::Int { bits: 32, signed: false },
        },
        (_, Tok::Long) => {
            lex.consume_if_next(Tok::Long)?;
            lex.consume_if_next(Tok::Int)?;
            Type::Int { bits: 64, signed: true }
        }
        (_, Tok::Char) => Type::Int { bits: 8, signed: false },
        (_, Tok::Id(id)) if let Some(ty) = state.typedefs.get(&id) => ty.clone(),
        (_, Tok::Struct) => unimplemented!(),
        (_, Tok::Union) => unimplemented!(),
        (_, Tok::Enum) => unimplemented!(),
        (sloc, tok) => {
            lex.unread((sloc, tok));
            return Err(Error::NotAType)
        }
    };
    let (mut constant, mut volatile) = (false, false);
    loop {
        match lex.next()? {
            (_, Tok::Const) => {
                constant = true;
                continue;
            }
            (_, Tok::Volatile) => {
                volatile = true;
                continue;
            }
            (_, Tok::Restrict) => match &mut ty {
                Type::Ptr { restrict, .. } => {
                    *restrict = true;
                    continue;
                }
                _ => continue,
            },
            (_, Tok::Star) => {
                ty = Type::Ptr { ety: Rc::new(ty), volatile, constant, restrict: false };
            }
            (sloc, tok) => {
                lex.unread((sloc, tok));
                break;
            }
        }
        (constant, volatile) = (false, false);
    }
    Ok(ty)
}

// Parses the `[N]` part of `float *A[N]`.
// FIXME: `float *a, b;` will result in `b` being parsed as `float` instead.
fn parse_type_suffix(state: &mut Unit, lex: &mut Lexer, mut ty: Type) -> Result<Type, Error> {
    _ = state;
    loop {
        ty = match lex.peek()? {
            (_, Tok::LBracket) => {
                lex.peeked.take();
                let mut size: Option<usize> = None;
                if let Tok::IntLit { val, .. } = lex.peek()?.1 {
                    size = Some(val as usize);
                }
                lex.expect_tok(Tok::RBracket, "expected closing ']'")?;
                Type::Array { ety: Rc::new(ty), size }
            }
            _ => break,
        };
    }
    Ok(ty)
}

fn parse_typedef(state: &mut Unit, lex: &mut Lexer) -> Result<(Rc<str>, Type), Error> {
    assert!(lex.peeked.is_some() && lex.peeked.as_ref().unwrap().1 == Tok::Typedef);
    lex.peeked.take();
    let ty = try_parse_type(state, lex)?;
    // TODO: This won't work on function pointers I think...
    let ty = parse_type_suffix(state, lex, ty)?;
    let (_, name) = lex.expect_id("expected identifier after type in typedef")?;
    state.typedefs.insert(name.clone(), ty.clone());
    Ok((name, ty))
}

fn parse_stmt(state: &mut Unit, lex: &mut Lexer) -> Result<Stmt, Error> {
    let (sloc, tok) = lex.peek()?;
    if tok == Tok::LBraces {
        lex.peeked.take();
        state.scopes.push(HashMap::new());
        let mut stmts = Vec::new();
        loop {
            if lex.consume_if_next(Tok::RBraces)? {
                break;
            }
            stmts.push(parse_stmt(state, lex)?);
            lex.consume_if_next(Tok::SemiColon)?;
        }
        state.scopes.pop();
        return Ok(Stmt::Compound { sloc, stmts })
    }

    if let Ok(basety) = try_parse_type(state, lex) {
        let mut decls = Vec::new();
        loop {
            let (sloc, name) = lex.expect_id("expected name of decl")?;
            let ty = parse_type_suffix(state, lex, basety.clone())?;
            let mut init = None;
            if lex.consume_if_next(Tok::Assign)? {
                init = Some(parse_expr(state, lex)?);
            }
            let d = Rc::new(Decl {
                sloc,
                is_argument: false,
                is_local: true,
                name: name.clone(),
                ty,
                init,
                idx: 0,
                stack_slot: RefCell::new(None),
            });
            decls.push(d.clone());
            state.scopes.last_mut().unwrap().insert(name, d.clone());
            state.local_decls.push(d);
            if lex.consume_if_next(Tok::Comma)? {
                continue;
            }
            lex.expect_tok(Tok::SemiColon, "expected semicolon to end local decls")?;
            break;
        }
        return Ok(Stmt::Decls { sloc, decls })
    }

    assert!(tok != Tok::If, "unimplemented: stmt starting with: {}", tok);
    assert!(tok != Tok::For, "unimplemented: stmt starting with: {}", tok);
    assert!(tok != Tok::While, "unimplemented: stmt starting with: {}", tok);

    Ok(Stmt::Expr { sloc, expr: parse_expr(state, lex)? })
}

fn parse_expr(state: &mut Unit, lex: &mut Lexer) -> Result<Box<Expr>, Error> {
    let lhs = parse_binary_expr(state, lex, 0)?;
    let (sloc, tok) = lex.peek()?;
    if let Some(op) = match tok {
        Tok::Assign => Some(None),
        Tok::AssignAdd => Some(Some(BinOp::Add)),
        Tok::AssignSub => Some(Some(BinOp::Sub)),
        _ => None,
    } {
        lex.peeked.take();
        let rhs = parse_expr(state, lex)?;
        if !lhs.is_assignable() || lhs.get_typ() != rhs.get_typ() {
            return Err(Error::Type(
                sloc,
                rhs.get_typ(),
                "LHS expression is not assignable or RHS is of different type",
            ))
        }
        return Ok(Box::new(Expr::Assign { sloc, typ: lhs.get_typ(), op, lhs, rhs }))
    }

    Ok(lhs)
}

fn parse_binary_expr(state: &mut Unit, lex: &mut Lexer, min_prec: u64) -> Result<Box<Expr>, Error> {
    fn precedence(tok: Tok) -> Option<(BinOp, u64)> {
        match tok {
            Tok::LogicalOr => Some((BinOp::LogicalOr, 100)),
            Tok::LogicalAnd => Some((BinOp::LogicalAnd, 100)),
            Tok::BitwiseOr => Some((BinOp::BitwiseOr, 200)),
            Tok::Ampersand => Some((BinOp::BitwiseAnd, 200)),
            Tok::BitwiseXOr => Some((BinOp::BitwiseXOr, 200)),
            Tok::Equal => Some((BinOp::EQ, 300)),
            Tok::NotEqual => Some((BinOp::NE, 300)),
            Tok::Smaller => Some((BinOp::LT, 400)),
            Tok::Bigger => Some((BinOp::GT, 400)),
            Tok::SmallerOrEqual => Some((BinOp::LE, 400)),
            Tok::BiggerOrEqual => Some((BinOp::GT, 400)),
            Tok::Plus => Some((BinOp::Add, 600)),
            Tok::Minus => Some((BinOp::Sub, 600)),
            Tok::Star => Some((BinOp::Mul, 700)),
            Tok::Divide => Some((BinOp::Div, 700)),
            Tok::Modulo => Some((BinOp::Mod, 700)),
            _ => None,
        }
    }

    let mut lhs = parse_final_expr(state, lex)?;
    while let Some((op, prec)) = precedence(lex.peek()?.1) {
        if prec < min_prec {
            break
        }
        let (sloc, _) = lex.next()?;
        let rhs = parse_binary_expr(state, lex, prec + 1)?;
        let t = lhs.get_typ();
        if t != rhs.get_typ() {
            eprintln!("lhs: {:?}", &lhs);
            eprintln!("rhs: {:?}", &rhs);
            return Err(Error::Type(sloc, t, "different types on sides of boolean expr."))
        }
        if (op == BinOp::LogicalOr || op == BinOp::LogicalAnd) && !t.is_bool() {
            return Err(Error::Type(sloc, t, "'&&' and '||' operands need to be boolean"))
        }
        if !(op == BinOp::LogicalOr || op == BinOp::LogicalAnd || t.is_numerical()) {
            return Err(Error::Type(sloc, t, "expected operands of numerical type"))
        }
        lhs = Box::new(Expr::BinOp {
            sloc,
            typ: if Expr::is_cmp(op) { Type::Bool } else { lhs.get_typ() },
            op,
            lhs,
            rhs,
        });
    }
    Ok(lhs)
}

fn parse_final_expr(state: &mut Unit, lex: &mut Lexer) -> Result<Box<Expr>, Error> {
    let (sloc, tok) = lex.next()?;
    let mut expr = match tok {
        Tok::True => Box::new(Expr::IntLit { sloc, typ: Type::Bool, num: 1 }),
        Tok::False => Box::new(Expr::IntLit { sloc, typ: Type::Bool, num: 0 }),
        Tok::BitwiseNot => {
            let val = parse_final_expr(state, lex)?;
            let typ = val.get_typ();
            if !typ.is_numerical() {
                return Err(Error::Type(sloc, typ, "expected a numerical type"))
            }
            Box::new(Expr::UnaryOp { sloc, typ, op: UnaryOp::BitwiseNot, val })
        }
        Tok::Minus => {
            let val = parse_final_expr(state, lex)?;
            let typ = val.get_typ();
            if !typ.is_numerical() {
                return Err(Error::Type(sloc, typ, "expected a numerical type"))
            }
            Box::new(Expr::UnaryOp { sloc, typ, op: UnaryOp::Neg, val })
        }
        Tok::Star => {
            let ptr = parse_final_expr(state, lex)?;
            let typ = match ptr.get_typ() {
                Type::Ptr { ety, .. } => (*ety).clone(),
                typ => return Err(Error::Type(sloc, typ, "expected a pointer")),
            };
            Box::new(Expr::Deref { sloc, typ, ptr })
        }
        Tok::LParen => match try_parse_type(state, lex) {
            Ok(typ) => {
                lex.expect_tok(Tok::RParen, "cast expression")?;
                let val = parse_final_expr(state, lex)?;
                Box::new(Expr::Cast { sloc, typ, val })
            }
            Err(Error::NotAType) => {
                let val = parse_expr(state, lex)?;
                lex.expect_tok(Tok::RParen, "closing parenthesis")?;
                val
            }
            Err(e) => return Err(e),
        },
        Tok::IntLit { signed, bits, val } => {
            Box::new(Expr::IntLit { sloc, num: val, typ: Type::Int { bits, signed } })
        }
        Tok::Id(id) => {
            let decl = state.lookup(&sloc, id.clone())?;
            Box::new(Expr::Id { sloc, typ: decl.ty.clone(), name: id, decl })
        }
        _ => unimplemented!("final expr starting with: {:?}", tok),
    };

    loop {
        expr = match lex.peek()?.1 {
            Tok::Dot => {
                let (sloc, _) = lex.next()?;
                let field = lex.expect_id("field name")?.1;
                let (typ, _, offset) = expr.get_typ().lookup_field(&sloc, field.clone())?;
                Box::new(Expr::FieldAccess { sloc, typ, obj: expr, field, offset })
            }
            Tok::Arrow => {
                let (sloc, _) = lex.next()?;
                let field = lex.expect_id("field name")?.1;
                let (styp, (typ, _, offset)) = match expr.get_typ() {
                    Type::Ptr { ety, .. } => (ety.clone(), ety.lookup_field(&sloc, field.clone())?),
                    t => return Err(Error::Type(sloc, t, "expected a pointer to a struct")),
                };
                Box::new(Expr::FieldAccess {
                    sloc: sloc.clone(),
                    typ,
                    obj: Box::new(Expr::Deref { sloc, typ: (*styp).clone(), ptr: expr }),
                    field,
                    offset,
                })
            }
            Tok::LBracket => {
                let (sloc, _) = lex.next()?;
                let offset = parse_expr(state, lex)?;
                lex.expect_tok(Tok::RBracket, "closing subscript bracket")?;
                if !offset.get_typ().is_numerical() {
                    return Err(Error::Type(sloc, expr.get_typ(), "expected a numerical offset"))
                }
                let typ = match expr.get_typ() {
                    Type::Ptr { ety, .. } => (*ety).clone(),
                    t => return Err(Error::Type(sloc, t, "expected a pointer")),
                };
                Box::new(Expr::Deref {
                    sloc: sloc.clone(),
                    typ,
                    ptr: Box::new(Expr::BinOp {
                        sloc,
                        typ: expr.get_typ(),
                        op: BinOp::Add,
                        lhs: expr,
                        rhs: offset,
                    }),
                })
            }
            Tok::LParen => {
                let (sloc, _) = lex.next()?;
                let mut args: Vec<Expr> = vec![];
                loop {
                    let arg = parse_expr(state, lex)?;
                    args.push(*arg);
                    if lex.peek()?.1 == Tok::Comma {
                        lex.next()?;
                        continue
                    }
                    lex.expect_tok(Tok::RParen, "end of call argument list")?;
                    break
                }

                let typ = match expr.get_typ() {
                    Type::Fn { retty, argtys } => {
                        if args.len() != argtys.len() {
                            return Err(Error::Type(
                                sloc,
                                expr.get_typ(),
                                "wrong number of arguments",
                            ))
                        }
                        for (a, (_, b)) in args.iter().zip(argtys.iter()) {
                            if a.get_typ() != *b {
                                return Err(Error::Type(sloc, b.clone(), "wrong argument type"))
                            }
                        }
                        (*retty).clone()
                    }
                    other => return Err(Error::Type(sloc, other, "expected a function")),
                };

                Box::new(Expr::Call { sloc, typ, func: expr, args })
            }
            _ => break,
        }
    }

    Ok(expr)
}

// Parses everything after the `(` in `static void foo(int a) { ... }`.
fn parse_function(
    state: &mut Unit,
    lex: &mut Lexer,
    is_static: bool,
    retty: Type,
    name: Rc<str>,
) -> Result<Rc<Function>, Error> {
    let (sloc, t) = lex.peeked.take().unwrap();
    assert!(t == Tok::LParen);
    assert!(state.local_decls.is_empty() && state.scopes.len() == 1);
    let mut f = Function {
        name,
        sloc,
        retty,
        args: vec![],
        body: None,
        is_static,
        decls: vec![],
        ir: RefCell::new(Vec::new()),
    };
    lex.peeked.take();
    state.scopes.push(HashMap::new());
    if !lex.consume_if_next(Tok::RParen)? {
        loop {
            let ty = try_parse_type(state, lex)?;
            let (sloc, name) = lex.expect_id("expected argument name")?;
            let ty = parse_type_suffix(state, lex, ty)?;
            f.args.push((name.clone(), ty.clone()));

            let d = Rc::new(Decl {
                sloc,
                is_argument: true,
                is_local: true,
                name: name.clone(),
                ty,
                init: None,
                idx: f.decls.len(),
                stack_slot: RefCell::new(None),
            });

            state.scopes[1].insert(name, d.clone());
            f.decls.push(d);
            if lex.consume_if_next(Tok::Comma)? {
                continue;
            }
            lex.expect_tok(Tok::RParen, "expected closing ')' after function arguments")?;
            break;
        }
    }

    if lex.consume_if_next(Tok::SemiColon)? {
        state.scopes.pop();
        return Ok(Rc::new(f))
    }

    f.body = Some(Box::new(parse_stmt(state, lex)?));
    state.scopes.pop();
    Ok(Rc::new(f))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn unparse(input: &str) -> String {
        let buf = input.as_bytes().to_vec();
        let mut lex = Lexer::new(std::path::Path::new("text.c"), buf);
        let mut d = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        d.push("tests/include");
        lex.include_paths.push(d);

        let mut cu = Unit::new();

        cu.parse(&mut lex).unwrap();
        let mut s = String::new();
        cu.write(&mut s).unwrap();
        s
    }

    #[test]
    fn top_level_basics() {
        let s = unparse(
            "typedef unsigned long int uint64_t;

            static uint64_t const *foo(int N, long * restrict A);",
        );
        assert_eq!(
            s,
            "typedef unsigned long  uint64_t\n\n\
             static unsigned long constant *foo(int N, long int *restrict A;\n\n"
        );
    }

    #[test]
    fn single_bb() {
        let s = unparse(
            "void foo(int x, int y, int z, int *a, int *b) {
                int tmp1, tmp2;
                tmp1 = x + y;
                tmp2 = tmp1 * (z - tmp1);
                *a = b[tmp2 * z];
                tmp2 = *b * (a[x] - z);
                b[42] = tmp2;
            }",
        );
        assert_eq!(
            s,
            "extern void foo(int x, int y, int z, int *a, int *b)\n   {\n    \
             int tmp1;\n    int tmp2;\n    \
             (tmp1) = ((x) + (y));\n    \
             (tmp2) = ((tmp1) * ((z) - (tmp1)));\n    \
             (*(a)) = (*((b) + ((tmp2) * (z))));\n    \
             (tmp2) = ((*(b)) * ((*((a) + (x))) - (z)));\n    \
             (*((b) + (0x2a))) = (tmp2);\n   }\n\n"
        );
    }
}
