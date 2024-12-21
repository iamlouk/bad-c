use crate::ast::*;
use crate::lex::*;
use crate::*;

use std::collections::HashMap;

pub struct Parser {
    types: HashMap<Rc<str>, Type>,
    typedefs: HashMap<Rc<str>, Type>,
    globals: HashMap<Rc<str>, Rc<Decl>>,
}

fn try_parse_type(state: &mut Parser, lex: &mut Lexer) -> Result<Type, Error> {
    let mut ty = match lex.next()? {
        (_, Tok::Void) => Type::Void,
        (_, Tok::Bool) => Type::Bool,
        (_, Tok::Int) => Type::Int { bits: 32, signed: true },
        (_, Tok::Signed) => match lex.peek()?.1 {
            Tok::Char => {
                lex.next()?;
                Type::Int { bits: 8, signed: true }
            }
            Tok::Short => {
                lex.next()?;
                Type::Int { bits: 16, signed: true }
            }
            Tok::Int => {
                lex.next()?;
                Type::Int { bits: 32, signed: true }
            }
            Tok::Long => {
                lex.next()?;
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
        match lex.peek()? {
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
            _ => break,
        }
        (constant, volatile) = (false, false);
    }
    Ok(ty)
}

fn try_parse_type_suffix(state: &mut Parser, lex: &mut Lexer, mut ty: Type) -> Result<Type, Error> {
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
