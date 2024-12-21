use crate::ast::*;
use crate::lex::*;
use crate::*;

impl Unit {
    pub fn parse(&mut self, lex: &mut Lexer) -> Result<(), Error> {
        loop {
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
    let mut f = Function { name, sloc, retty, args: vec![], body: None, is_static, decls: vec![] };
    lex.peeked.take();
    if !lex.consume_if_next(Tok::RParen)? {
        loop {
            let ty = try_parse_type(state, lex)?;
            let (sloc, name) = lex.expect_id("expected argument name")?;
            let ty = parse_type_suffix(state, lex, ty)?;
            f.args.push((name.clone(), ty.clone()));
            f.decls.push(Rc::new(Decl {
                sloc,
                is_argument: true,
                is_local: true,
                name,
                ty,
                init: None,
                idx: f.decls.len(),
            }));

            if lex.consume_if_next(Tok::Comma)? {
                continue;
            }
            lex.expect_tok(Tok::RParen, "expected closing ')' after function arguments")?;
            break;
        }
    }

    if lex.consume_if_next(Tok::SemiColon)? {
        return Ok(Rc::new(f))
    }
    unimplemented!()
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;

    fn unparse(input: &str) -> String {
        let buf = input.as_bytes().to_vec();
        let mut lex = Lexer::new(std::path::Path::new("text.c"), buf);
        let mut d = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        d.push("tests/include");
        lex.include_paths.push(d);

        let mut cu = Unit {
            types: HashMap::new(),
            typedefs: HashMap::new(),
            globals: HashMap::new(),
            entries: vec![],
        };

        cu.parse(&mut lex).unwrap();
        let mut s = String::new();
        cu.write(&mut s).unwrap();
        s
    }

    #[test]
    fn top_level_basics() {
        let s = unparse(
            "
            typedef unsigned long int uint64_t;

            static uint64_t const *foo(int N, long * restrict A);
        ",
        );
        assert_eq!(
            s,
            "typedef unsigned long  uint64_t\n\n\
             static unsigned long constant *foo(int N, long int *restrict A;\n\n"
        );
    }
}
