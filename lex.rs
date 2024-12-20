use std::collections::{HashMap, HashSet, VecDeque};
use std::fmt::{Display, Write};
use std::path::Path;
use std::rc::Rc;

use crate::{Error, SLoc};

#[allow(dead_code)]
#[derive(Clone, PartialEq, PartialOrd, Debug)]
pub enum Tok {
    /* Special: */
    EndOfFile,
    EndOfMacro,
    Backslash,
    StartOfDirective,
    SkipMe,

    /* Literals: */
    Id(Rc<str>),
    String(Rc<str>),
    IntLit { signed: bool, bits: u8, val: i64 },
    CharLit(char),

    /* Keywords: */
    Alignas,
    Alignof,
    Auto,
    Bool,
    Break,
    Case,
    Char,
    Const,
    Constexpr,
    Continue,
    Default,
    Do,
    Double,
    Else,
    Enum,
    Extern,
    False,
    Float,
    For,
    Goto,
    If,
    Inline,
    Int,
    Long,
    Nullptr,
    Register,
    Restrict,
    Return,
    Short,
    Signed,
    Sizeof,
    Static,
    StaticAssert,
    Struct,
    Switch,
    ThreadLocal,
    True,
    Typedef,
    Typeof,
    Union,
    Unsigned,
    Void,
    Volatile,
    While,
    Attribute,

    /* Operators */
    Assign,
    AssignAdd,
    AssignSub,
    AssignMul,
    AssignDiv,
    AssignMod,
    AssignBitAnd,
    AssignBitOr,
    AssignBitXOr,
    AssignLeftShift,
    AssignRightShift,
    PlusPlus,
    MinusMinus,
    Plus,
    Minus,
    Star,
    Divide,
    Modulo,
    BitwiseNot,
    Ampersand,
    BitwiseOr,
    BitwiseXOr,
    ShiftLeft,
    ShiftRight,
    LogicalNot,
    LogicalAnd,
    LogicalOr,
    Equal,
    NotEqual,
    Smaller,
    Bigger,
    SmallerOrEqual,
    BiggerOrEqual,

    /* Others */
    LParen,
    RParen,
    LBracket,
    RBracket,
    LBraces,
    RBraces,
    Arrow,
    Dot,
    Comma,
    QuestionMark,
    Colon,
    SemiColon,
}

impl Display for Tok {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Tok::*;
        f.write_str(match self {
            EndOfFile => return Ok(()),
            EndOfMacro => return write!(f, "<END-OF-MACRO>"),
            Backslash => return write!(f, "<BACKSLASH>"),
            StartOfDirective => return write!(f, "<START-OF-DIRECTIVE>"),
            SkipMe => return write!(f, "<SKIP-ME>"),
            Id(id) => return write!(f, "{}", &**id),
            String(str) => return write!(f, "{:?}", &**str),
            IntLit { signed: true, bits: 32, val } => return write!(f, "{}", val),
            IntLit { signed: true, bits: 64, val } => return write!(f, "{}l", val),
            IntLit { signed: false, bits: 32, val } => return write!(f, "{}u", val),
            IntLit { signed: false, bits: 64, val } => return write!(f, "{}ul", val),
            IntLit { signed, bits, val } => {
                return write!(f, "{:#X}{}{}", val, if *signed { "i" } else { "u" }, bits)
            }
            CharLit(x) => match *x {
                '\n' => "'\\n'",
                '\r' => "'\\r'",
                '\t' => "'\\t'",
                '\0' => "'\\0'",
                x => return write!(f, "{:?}", x),
            },
            Alignas => "_Alignas",
            Alignof => "_Alignof",
            Auto => "auto",
            Bool => "bool",
            Break => "break",
            Case => "case",
            Char => "char",
            Const => "const",
            Constexpr => "constexpr",
            Continue => "continue",
            Default => "default",
            Do => "do",
            Double => "double",
            Else => "else",
            Enum => "enum",
            Extern => "extern",
            False => "false",
            Float => "float",
            For => "for",
            Goto => "goto",
            If => "if",
            Inline => "inline",
            Int => "int",
            Long => "long",
            Nullptr => "nullptr",
            Register => "register",
            Restrict => "restrict",
            Return => "return",
            Short => "short",
            Signed => "signed",
            Sizeof => "sizeof",
            Static => "static",
            StaticAssert => "_Static_assert",
            Struct => "struct",
            Switch => "switch",
            ThreadLocal => "_Thread_local",
            True => "true",
            Typedef => "typedef",
            Typeof => "typeof",
            Union => "union",
            Unsigned => "unsigned",
            Void => "void",
            Volatile => "volatile",
            While => "while",
            Attribute => "__attribute__",
            Assign => "=",
            AssignAdd => "+=",
            AssignSub => "-=",
            AssignMul => "*=",
            AssignDiv => "/=",
            AssignMod => "%=",
            AssignBitAnd => "&=",
            AssignBitOr => "|=",
            AssignBitXOr => "^=",
            AssignLeftShift => "<<=",
            AssignRightShift => ">>=",
            PlusPlus => "++",
            MinusMinus => "--",
            Plus => "+",
            Minus => "-",
            Star => "*",
            Divide => "/",
            Modulo => "%",
            BitwiseNot => "~",
            Ampersand => "&",
            BitwiseOr => "|",
            BitwiseXOr => "^",
            ShiftLeft => "<<",
            ShiftRight => ">>",
            LogicalNot => "!",
            LogicalAnd => "&&",
            LogicalOr => "||",
            Equal => "==",
            NotEqual => "!=",
            Smaller => "<",
            Bigger => ">",
            SmallerOrEqual => "<=",
            BiggerOrEqual => ">=",
            LParen => "(",
            RParen => ")",
            LBracket => "[",
            RBracket => "]",
            LBraces => "{",
            RBraces => "}",
            Arrow => "->",
            Dot => ".",
            Comma => ",",
            QuestionMark => "?",
            Colon => ":",
            SemiColon => ";",
        })
    }
}

impl Tok {
    fn equal_to_str(&self, m: &str) -> bool {
        match self {
            Tok::Id(id) => id.as_ref() == m,
            _ => {
                // TODO: Handle keywords etc.!
                assert!(format!("{}", self) != m);
                false
            }
        }
    }

    pub fn dump(toks: impl Iterator<Item = Tok>) -> Result<String, std::fmt::Error> {
        let mut s = String::new();
        let mut ident = String::new();
        let mut newline = false;
        let mut parens = 0;
        for t in toks {
            if t == Tok::RBraces {
                ident.pop();
            }
            if newline {
                s.write_str(&ident)?;
                newline = false;
            }
            write!(&mut s, "{} ", t)?;
            match t {
                Tok::LParen => parens += 1,
                Tok::RParen => parens -= 1,
                Tok::SemiColon | Tok::RBraces if parens == 0 => {
                    newline = true;
                    s.write_char('\n')?;
                }
                Tok::LBraces if parens == 0 => {
                    newline = true;
                    s.write_char('\n')?;
                    ident.push('\t');
                }
                _ => {}
            }
        }

        Ok(s)
    }
}

struct File {
    sloc: SLoc,
    data: Vec<u8>,
    pos: usize,
}

#[derive(Clone, Debug)]
struct Macro {
    sloc: SLoc,
    parameters: Vec<Rc<str>>,
    value: Vec<(SLoc, Tok)>,
    disabled: bool,
}

impl Macro {
    fn disabled() -> Self {
        Self { sloc: SLoc::unknown(), parameters: vec![], value: vec![], disabled: true }
    }
}

struct State {
    string_pool: HashSet<Rc<str>>,
    macros: Vec<HashMap<Rc<str>, Macro>>,
    buf: String,
}

impl State {
    fn get_buf(&mut self) -> Rc<str> {
        if let Some(rc) = self.string_pool.get(self.buf.as_str()) {
            rc.clone()
        } else {
            let rc: Rc<str> = Rc::from(self.buf.as_str());
            self.string_pool.insert(rc.clone());
            rc
        }
    }

    fn pool(&mut self, s: &str) -> Rc<str> {
        if let Some(rc) = self.string_pool.get(s) {
            rc.clone()
        } else {
            let rc: Rc<str> = Rc::from(s);
            self.string_pool.insert(rc.clone());
            rc
        }
    }

    fn get_macro(&mut self, id: &str) -> Option<&Macro> {
        for map in self.macros.iter().rev() {
            if let Some(m) = map.get(id) {
                if m.disabled {
                    return None;
                }
                return Some(m);
            }
        }
        None
    }
}

pub struct Lexer {
    files: Vec<File>,
    state: State,
    peeked: Option<(SLoc, Tok)>,
    tokqueue: VecDeque<(SLoc, Tok)>,
    expand: bool,
}

impl File {
    fn next_u8(&mut self) -> Option<u8> {
        self.sloc.col += 1;
        self.pos += 1;
        self.data.get(self.pos - 1).cloned()
    }

    fn skip_whitespace(&mut self) -> bool {
        while self.pos < self.data.len() {
            match self.data[self.pos] {
                b' ' | b'\t' | b'\r' => {
                    self.pos += 1;
                    self.sloc.col += 1;
                    continue;
                }
                b'\n' => {
                    self.pos += 1;
                    self.sloc.line += 1;
                    self.sloc.col = 1;
                    continue;
                }
                b'/' if self.data.get(self.pos + 1).cloned() == Some(b'/') => {
                    self.pos += 2;
                    while self.pos < self.data.len() && self.data[self.pos] != b'\n' {
                        self.pos += 1;
                    }
                    continue;
                }
                b'/' if self.data.get(self.pos + 1).cloned() == Some(b'*') => {
                    self.pos += 2;
                    self.sloc.col += 2;
                    while self.pos < self.data.len() {
                        let c = self.data[self.pos];
                        self.pos += 1;
                        self.sloc.col += 1;
                        if c == b'\n' {
                            self.sloc.line += 1;
                            self.sloc.col = 1;
                        }
                        if c == b'*' && self.data.get(self.pos).cloned() == Some(b'/') {
                            self.pos += 1;
                            self.sloc.col += 1;
                            break;
                        }
                    }
                    continue;
                }
                _ => return false,
            }
        }
        true
    }

    fn next(&mut self, state: &mut State) -> Result<(SLoc, Tok), Error> {
        self.skip_whitespace();
        let sloc = self.sloc.clone();
        if self.pos >= self.data.len() {
            return Ok((sloc, Tok::EndOfFile));
        }

        let c = self.data[self.pos] as char;
        self.pos += 1;
        match c {
            '{' => Ok((sloc, Tok::LBraces)),
            '}' => Ok((sloc, Tok::RBraces)),
            '(' => Ok((sloc, Tok::LParen)),
            ')' => Ok((sloc, Tok::RParen)),
            '[' => Ok((sloc, Tok::LBracket)),
            ']' => Ok((sloc, Tok::RBracket)),
            ':' => Ok((sloc, Tok::Colon)),
            ',' => Ok((sloc, Tok::Comma)),
            ';' => Ok((sloc, Tok::SemiColon)),
            '?' => Ok((sloc, Tok::QuestionMark)),
            '.' => Ok((sloc, Tok::Dot)),
            '*' => Ok((sloc, Tok::Star)),
            '/' => Ok((sloc, Tok::Divide)),
            '~' => Ok((sloc, Tok::BitwiseNot)),
            '^' => Ok((sloc, Tok::BitwiseXOr)),
            '%' => Ok((sloc, Tok::Modulo)),
            '|' => match self.data.get(self.pos).cloned() {
                Some(b'|') => {
                    self.next_u8();
                    Ok((sloc, Tok::LogicalOr))
                }
                Some(b'=') => {
                    self.next_u8();
                    Ok((sloc, Tok::AssignBitOr))
                }
                _ => Ok((sloc, Tok::BitwiseOr)),
            },
            '&' => match self.data.get(self.pos).cloned() {
                Some(b'&') => {
                    self.next_u8();
                    Ok((sloc, Tok::LogicalAnd))
                }
                Some(b'=') => {
                    self.next_u8();
                    Ok((sloc, Tok::AssignBitAnd))
                }
                _ => Ok((sloc, Tok::Ampersand)),
            },
            '+' => match self.data.get(self.pos).cloned() {
                Some(b'+') => {
                    self.next_u8();
                    Ok((sloc, Tok::PlusPlus))
                }
                Some(b'=') => {
                    self.next_u8();
                    Ok((sloc, Tok::AssignAdd))
                }
                _ => Ok((sloc, Tok::Plus)),
            },
            '-' => match self.data.get(self.pos).cloned() {
                Some(b'-') => {
                    self.next_u8();
                    Ok((sloc, Tok::MinusMinus))
                }
                Some(b'>') => {
                    self.next_u8();
                    Ok((sloc, Tok::Arrow))
                }
                Some(b'=') => {
                    self.next_u8();
                    Ok((sloc, Tok::AssignSub))
                }
                _ => Ok((sloc, Tok::Minus)),
            },
            '!' => match self.data.get(self.pos).cloned() {
                Some(b'=') => {
                    self.next_u8();
                    Ok((sloc, Tok::NotEqual))
                }
                _ => Ok((sloc, Tok::LogicalNot)),
            },
            '=' => match self.data.get(self.pos).cloned() {
                Some(b'=') => {
                    self.next_u8();
                    Ok((sloc, Tok::Equal))
                }
                _ => Ok((sloc, Tok::Assign)),
            },
            '<' => match self.data.get(self.pos).cloned() {
                Some(b'=') => {
                    self.next_u8();
                    Ok((sloc, Tok::SmallerOrEqual))
                }
                Some(b'<') => {
                    self.next_u8();
                    Ok((sloc, Tok::ShiftLeft))
                }
                _ => Ok((sloc, Tok::Smaller)),
            },
            '>' => match self.data.get(self.pos).cloned() {
                Some(b'=') => {
                    self.next_u8();
                    Ok((sloc, Tok::BiggerOrEqual))
                }
                Some(b'>') => {
                    self.next_u8();
                    Ok((sloc, Tok::ShiftRight))
                }
                _ => Ok((sloc, Tok::Bigger)),
            },

            '\'' => match (self.next_u8(), self.next_u8()) {
                (Some(c), Some(b'\'')) if c != b'\\' => Ok((sloc, Tok::CharLit(c as char))),
                (Some(b'\\'), Some(c)) => {
                    let c = match c {
                        b'\\' => '\\',
                        b'n' => '\n',
                        b't' => '\t',
                        b'0' => '\0',
                        _ => return Err(Error::Lex(sloc, "invalid character litteral".to_owned())),
                    };
                    if self.next_u8() != Some(b'\'') {
                        Err(Error::Lex(sloc, "invalid character litteral".to_owned()))
                    } else {
                        Ok((sloc, Tok::CharLit(c)))
                    }
                }
                _ => Err(Error::Lex(sloc, "invalid character litteral".to_owned())),
            },

            '"' => {
                state.buf.clear();
                while let Some(mut c) = self.next_u8() {
                    if c == b'"' {
                        let res = state.get_buf();
                        return Ok((sloc, Tok::String(res)));
                    }

                    if c == b'\\' {
                        c = match self.next_u8() {
                            Some(b'\\') => b'\\',
                            Some(b'\n') => b'\n',
                            Some(b'\t') => b'\t',
                            Some(c) => {
                                return Err(Error::Lex(
                                    sloc,
                                    format!("invalid string escape character: {:?}", c),
                                ))
                            }
                            None => return Err(Error::EndOfFile(sloc)),
                        }
                    }

                    state.buf.push(c as char);
                }

                Err(Error::Lex(sloc, "unterminated string litteral".to_owned()))
            }

            '0'..='9' => {
                state.buf.clear();
                let radix = match (c, self.data.get(self.pos).cloned()) {
                    ('0', Some(b'x')) => {
                        self.next_u8();
                        16
                    }
                    ('0', Some(b'o')) => {
                        self.next_u8();
                        8
                    }
                    ('0', Some(b'b')) => {
                        self.next_u8();
                        2
                    }
                    (c, _) => {
                        state.buf.push(c);
                        10
                    }
                };

                while let Some(c) = self.data.get(self.pos).cloned() {
                    if c != b'_' && c != b'.' && !c.is_ascii_hexdigit() {
                        break;
                    }
                    state.buf.push(c as char);
                    self.next_u8();
                }

                let (signed, bits) = match self.data.get(self.pos).cloned() {
                    Some(x) if x == b'u' || x == b'i' => {
                        self.next_u8();
                        let signed = x == b'i';
                        let (a, b) = (
                            self.data.get(self.pos).cloned(),
                            self.data.get(self.pos + 1).cloned(),
                        );
                        match (a, b) {
                            (Some(b'8'), _) => {
                                self.next_u8();
                                (signed, 8)
                            }
                            (Some(b'1'), Some(b'6')) => {
                                self.next_u8();
                                self.next_u8();
                                (signed, 16)
                            }
                            (Some(b'3'), Some(b'2')) => {
                                self.next_u8();
                                self.next_u8();
                                (signed, 32)
                            }
                            (Some(b'6'), Some(b'4')) => {
                                self.next_u8();
                                self.next_u8();
                                (signed, 64)
                            }
                            (Some(b'l'), _) => {
                                self.next_u8();
                                (signed, 64)
                            }
                            (Some(x), _) if !x.is_ascii_alphanumeric() => (signed, 32),
                            (None, _) => (signed, 32),
                            (a, b) => {
                                return Err(Error::Lex(
                                    sloc,
                                    format!(
                                        "unexpected interger literal suffix: {}{}",
                                        a.unwrap_or(b' ') as char,
                                        b.unwrap_or(b' ') as char
                                    ),
                                ))
                            }
                        }
                    }
                    Some(b'l') => {
                        self.next_u8();
                        (true, 64)
                    }
                    _ => (true, 32),
                };

                match i64::from_str_radix(state.buf.as_str(), radix) {
                    Ok(num) => Ok((sloc, Tok::IntLit { signed, bits, val: num })),
                    Err(e) => Err(Error::InvalidInt(sloc, e)),
                }
            }

            '_' | 'a'..='z' | 'A'..='Z' => {
                state.buf.clear();
                state.buf.push(c);
                while let Some(c) = self.data.get(self.pos).cloned() {
                    if !matches!(c, b'_' | b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9') {
                        break;
                    }
                    state.buf.push(c as char);
                    self.next_u8();
                }

                match state.buf.as_str() {
                    "alignas" => return Ok((sloc, Tok::Alignas)),
                    "alignof" => return Ok((sloc, Tok::Alignof)),
                    "auto" => return Ok((sloc, Tok::Auto)),
                    "bool" => return Ok((sloc, Tok::Bool)),
                    "break" => return Ok((sloc, Tok::Break)),
                    "case" => return Ok((sloc, Tok::Case)),
                    "char" => return Ok((sloc, Tok::Char)),
                    "const" => return Ok((sloc, Tok::Const)),
                    "constexpr" => return Ok((sloc, Tok::Constexpr)),
                    "continue" => return Ok((sloc, Tok::Continue)),
                    "default" => return Ok((sloc, Tok::Default)),
                    "do" => return Ok((sloc, Tok::Do)),
                    "double" => return Ok((sloc, Tok::Double)),
                    "else" => return Ok((sloc, Tok::Else)),
                    "enum" => return Ok((sloc, Tok::Enum)),
                    "extern" => return Ok((sloc, Tok::Extern)),
                    "false" => return Ok((sloc, Tok::False)),
                    "float" => return Ok((sloc, Tok::Float)),
                    "for" => return Ok((sloc, Tok::For)),
                    "goto" => return Ok((sloc, Tok::Goto)),
                    "if" => return Ok((sloc, Tok::If)),
                    "inline" => return Ok((sloc, Tok::Inline)),
                    "int" => return Ok((sloc, Tok::Int)),
                    "long" => return Ok((sloc, Tok::Long)),
                    "nullptr" => return Ok((sloc, Tok::Nullptr)),
                    "register" => return Ok((sloc, Tok::Register)),
                    "restrict" => return Ok((sloc, Tok::Restrict)),
                    "return" => return Ok((sloc, Tok::Return)),
                    "short" => return Ok((sloc, Tok::Short)),
                    "signed" => return Ok((sloc, Tok::Signed)),
                    "sizeof" => return Ok((sloc, Tok::Sizeof)),
                    "static" => return Ok((sloc, Tok::Static)),
                    "static_assert" => return Ok((sloc, Tok::StaticAssert)),
                    "struct" => return Ok((sloc, Tok::Struct)),
                    "switch" => return Ok((sloc, Tok::Switch)),
                    "thread_local" => return Ok((sloc, Tok::ThreadLocal)),
                    "true" => return Ok((sloc, Tok::True)),
                    "typedef" => return Ok((sloc, Tok::Typedef)),
                    "typeof" => return Ok((sloc, Tok::Typeof)),
                    "union" => return Ok((sloc, Tok::Union)),
                    "unsigned" => return Ok((sloc, Tok::Unsigned)),
                    "void" => return Ok((sloc, Tok::Void)),
                    "volatile" => return Ok((sloc, Tok::Volatile)),
                    "while" => return Ok((sloc, Tok::While)),
                    "__attribute__" => return Ok((sloc, Tok::Attribute)),
                    _ => {}
                };

                let id = state.get_buf();
                Ok((sloc, Tok::Id(id)))
            }

            '#' => {
                // Check if this is the first non-whitespace character on this line.
                let mut pos = (self.pos as isize) - 2;
                while pos >= 0 && self.data[pos as usize] != b'\n' {
                    if !self.data[pos as usize].is_ascii_whitespace() {
                        return Err(Error::Lex(
                            sloc,
                            format!(
                                "'#' is only allowed at the start of a line, found: {:?}",
                                self.data[pos as usize] as char
                            ),
                        ));
                    }
                    pos -= 1;
                }

                Ok((sloc, Tok::StartOfDirective))
            }

            c => Err(Error::Lex(sloc, format!("unexpected character: {:?}", c))),
        }
    }
}

impl Lexer {
    pub fn new(path: &Path, data: Vec<u8>) -> Self {
        Self {
            state: State {
                string_pool: HashSet::new(),
                macros: vec![HashMap::new()],
                buf: String::with_capacity(128),
            },
            files: vec![File { sloc: SLoc::new(path, 1, 0), data, pos: 0 }],
            peeked: None,
            tokqueue: VecDeque::new(),
            expand: true,
        }
    }

    pub fn def(&mut self, sloc: SLoc, name: &str, value: &[Tok]) {
        let d = Macro {
            sloc: sloc.clone(),
            parameters: vec![],
            value: value.into_iter().map(|t| (sloc.clone(), t.clone())).collect(),
            disabled: false,
        };
        let name = self.state.pool(name);
        self.state.macros.last_mut().unwrap().insert(name, d);
    }

    pub fn peek(&mut self) -> Result<(SLoc, Tok), Error> {
        if let Some(res) = &self.peeked {
            return Ok(res.clone());
        }
        let res = self.next()?;
        self.peeked = Some(res.clone());
        Ok(res)
    }

    pub fn next(&mut self) -> Result<(SLoc, Tok), Error> {
        if let Some((sloc, tok)) = self.peeked.take() {
            return Ok((sloc, tok));
        }

        let (sloc, tok) = if let Some((sloc, tok)) = self.tokqueue.pop_front() {
            (sloc, tok)
        } else {
            self.files.last_mut().unwrap().next(&mut self.state)?
        };

        match tok {
            Tok::Backslash => {
                if !self.expand {
                    Ok((sloc, tok))
                } else {
                    self.next()
                }
            }
            Tok::SkipMe => self.next(),
            Tok::StartOfDirective => {
                self.expand = false;
                self.directive()?;
                self.expand = true;
                self.next()
            }
            Tok::EndOfFile if self.files.len() > 1 => {
                self.files.pop();
                self.next()
            }
            Tok::EndOfMacro => {
                self.state.macros.pop();
                self.next()
            }
            Tok::Id(id) if let Some(m) = self.state.get_macro(id.as_ref()).cloned() => {
                if !self.expand {
                    return Ok((sloc, Tok::Id(id)));
                }

                if m.parameters.len() > 0 && self.expand_function_macro(id.clone(), &m)? {
                    return self.next();
                }

                let mut disabled = HashMap::new();
                disabled.insert(id, Macro::disabled());
                self.state.macros.push(disabled);
                self.tokqueue.push_front((m.sloc, Tok::EndOfMacro));
                for (sloc, tok) in m.value.into_iter().rev() {
                    self.tokqueue.push_front((sloc, tok));
                }
                self.next()
            }
            tok => Ok((sloc, tok)),
        }
    }

    pub fn expect_id(&mut self, errmsg: &str) -> Result<(SLoc, Rc<str>), Error> {
        match self.next()? {
            (sloc, Tok::Id(id)) => Ok((sloc, id)),
            (sloc, tok) => Err(Error::Lex(sloc, format!("{}, found: '{}'", errmsg, tok))),
        }
    }

    fn directive(&mut self) -> Result<(), Error> {
        // A directive should never be expanded while expanding!
        assert!(self.peeked.is_none() && self.tokqueue.is_empty());
        assert!(self.state.macros.len() == 1 && self.expand == false);
        let (_, dir) = self.expect_id("expected directive")?;
        if dir.as_ref() == "define" {
            let (sloc, name) = self.expect_id("expected macro name")?;
            let mut parameters = vec![];
            if self.peek()?.1 == Tok::LParen {
                self.peeked.take();
                loop {
                    let (sloc, name) = self.expect_id("expected macro parameter name")?;
                    parameters.push(name);
                    match self.next()?.1 {
                        Tok::Comma => continue,
                        Tok::RParen => break,
                        tok => {
                            return Err(Error::Lex(
                                sloc,
                                format!("expected ',' or ')', found: {}", tok),
                            ))
                        }
                    }
                }
            }

            let mut value: Vec<(SLoc, Tok)> = vec![];
            let mut cline = sloc.line;
            loop {
                let (nsloc, tok) = self.next()?;
                if tok == Tok::Backslash {
                    cline += 1;
                    continue;
                }
                if nsloc.line != cline || tok == Tok::EndOfFile {
                    // Push into tokqueue for possible expansion.
                    // Remember that expansion is disabled here.
                    self.tokqueue.push_front((nsloc, tok));
                    break;
                }
                value.push((nsloc, tok));
            }

            self.state
                .macros
                .last_mut()
                .unwrap()
                .insert(name, Macro { sloc, parameters, value, disabled: false });
            return Ok(());
        }

        unimplemented!()
    }

    fn expand_function_macro(&mut self, mname: Rc<str>, m: &Macro) -> Result<bool, Error> {
        assert!(self.peeked.is_none() && self.expand);
        self.expand = false;
        let (sloc, tok) = self.next()?;
        self.expand = true;
        if tok != Tok::LParen {
            // This should work even if the consumed token is a macro because it
            // was consumed with expansion disabled and tokqueue tokens will be
            // expanded. This is not true for peeked tokens!
            self.tokqueue.push_front((sloc, tok));
            return Ok(false);
        }

        let mut parens = 0;
        let mut arguments: Vec<Vec<(SLoc, Tok)>> = vec![vec![]];
        loop {
            let (sloc, tok) = self.next()?;
            if parens == 0 && tok == Tok::RParen {
                self.peeked.take();
                break;
            }
            if parens == 0 && tok == Tok::Comma {
                arguments.push(Vec::new());
                continue;
            }
            if tok == Tok::LParen {
                parens += 1;
            }
            if tok == Tok::RParen {
                parens -= 1;
            }
            arguments.last_mut().unwrap().push((sloc, tok));
        }

        if m.parameters.len() != arguments.len() {
            return Err(Error::Lex(sloc, "macro called with bad number of arguments".to_owned()));
        }

        let mut disabled = HashMap::new();
        disabled.insert(mname, Macro::disabled());
        for p in m.parameters.iter() {
            disabled.insert(p.clone(), Macro::disabled());
        }
        self.state.macros.push(disabled);

        self.tokqueue.push_front((m.sloc.clone(), Tok::EndOfMacro));
        for (sloc, tok) in m.value.iter().rev() {
            let mut tok = tok.clone();
            for (name, value) in m.parameters.iter().zip(arguments.iter()) {
                if tok.equal_to_str(name.as_ref()) {
                    for (sloc, tok) in value.iter().rev() {
                        self.tokqueue.push_front((sloc.clone(), tok.clone()));
                    }
                    tok = Tok::SkipMe;
                    break;
                }
            }
            self.tokqueue.push_front((sloc.clone(), tok));
        }

        Ok(true)
    }
}

impl Iterator for Lexer {
    type Item = Result<(SLoc, Tok), Error>;

    fn next(&mut self) -> Option<<Self as Iterator>::Item> {
        match self.next() {
            Err(e) => Some(Err(e)),
            Ok((_, Tok::EndOfFile)) => None,
            Ok((sloc, tok)) => Some(Ok((sloc, tok))),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::assert_matches::assert_matches;

    fn lex(input: &str) -> Vec<Tok> {
        let buf = input.as_bytes().to_vec();
        let mut lex = Lexer::new(std::path::Path::new("text.c"), buf);
        lex.def(SLoc::unknown(), "__SHITTYC", &[Tok::IntLit { signed: true, bits: 32, val: 1 }]);
        let mut toks = Vec::new();
        loop {
            match lex.next().unwrap() {
                (_, Tok::EndOfFile) => break,
                (_, tok) => toks.push(tok),
            }
        }
        toks
    }

    #[test]
    fn basic() {
        let toks = lex("[foo]+/*comment*/(\"bar\")--42%0x1234 __SHITTYC");
        assert_eq!(toks.len(), 12);
        assert_matches!(toks[0], Tok::LBracket);
        assert_matches!(toks[1], Tok::Id(ref id) if id.as_ref() == "foo");
        assert_matches!(toks[2], Tok::RBracket);
        assert_matches!(toks[3], Tok::Plus);
        assert_matches!(toks[4], Tok::LParen);
        assert_matches!(toks[5], Tok::String(ref s) if s.as_ref() == "bar");
        assert_matches!(toks[6], Tok::RParen);
        assert_matches!(toks[7], Tok::MinusMinus);
        assert_matches!(toks[8], Tok::IntLit { signed: true, bits: 32, val: 42 });
        assert_matches!(toks[9], Tok::Modulo);
        assert_matches!(toks[10], Tok::IntLit { signed: true, bits: 32, val: 0x1234 });
        assert_matches!(toks[11], Tok::IntLit { signed: true, bits: 32, val: 1 });
    }

    #[test]
    fn macros() {
        let toks = lex("#define FOO 42\nFOO");
        let s = Tok::dump(toks.into_iter()).unwrap();
        assert_eq!(s, "42 ");

        let toks = lex("#define BAR FOO \n #define BAZ BAR \n #define BAR 123 \n BAZ");
        let s = Tok::dump(toks.into_iter()).unwrap();
        assert_eq!(s, "123 ");

        let toks = lex("#define min(a, b) (a < b ? a : b)\n min(1, 2)");
        let s = Tok::dump(toks.into_iter()).unwrap();
        assert_eq!(s, "( 1 < 2 ? 1 : 2 ) ");

        let toks = lex("#define foo(a, b) (a + b) \n foo(foo(1, 2), (3 * 4))");
        let s = Tok::dump(toks.into_iter()).unwrap();
        assert_eq!(s, "( ( 1 + 2 ) + ( 3 * 4 ) ) ");

        /*
         * FIXME: Currently failing/known bugs:
         * https://gcc.gnu.org/onlinedocs/cppinternals/Macro-Expansion.html
         *
         *   #define foo(x) bar x
         *   foo(foo) (2)
         *
         * should return "bar foo (2)" but instead currently expands to:
         *
         *   bar bar x ( 2 )
         */
    }

    #[test]
    fn ints() {
        let toks = lex("42,42u,42i8,42u16,42i32,42u64,42ul,42l");
        println!("tokens: {:?}", toks);
        assert_eq!(
            toks.as_slice(),
            &[
                Tok::IntLit { signed: true, bits: 32, val: 42 },
                Tok::Comma,
                Tok::IntLit { signed: false, bits: 32, val: 42 },
                Tok::Comma,
                Tok::IntLit { signed: true, bits: 8, val: 42 },
                Tok::Comma,
                Tok::IntLit { signed: false, bits: 16, val: 42 },
                Tok::Comma,
                Tok::IntLit { signed: true, bits: 32, val: 42 },
                Tok::Comma,
                Tok::IntLit { signed: false, bits: 64, val: 42 },
                Tok::Comma,
                Tok::IntLit { signed: false, bits: 64, val: 42 },
                Tok::Comma,
                Tok::IntLit { signed: true, bits: 64, val: 42 },
            ]
        );
    }

    #[test]
    fn tostring() {
        let toks = lex("for (int i = 0; i < N; i++) { if (a[i]) { foo(i); } else { bar(i); } }");
        let s = Tok::dump(toks.into_iter()).unwrap();
        assert_eq!(s, "for ( int i = 0 ; i < N ; i ++ ) { \n\tif ( a [ i ] ) { \n\t\tfoo ( i ) ; \n\t} \n\telse { \n\t\tbar ( i ) ; \n\t} \n} \n");
    }

    static FIBS_EXAMPLE: &str = "
        extern unsigned fibs(n: unsigned): unsigned =
            if n < 2 { n } else { fibs(n - 2) + fibs(n - 1) };";

    #[test]
    fn fibs() {
        let toks = lex(FIBS_EXAMPLE);
        println!("tokens: {:?}", toks);
        assert_eq!(toks.len(), 35);
        assert_matches!(toks[0], Tok::Extern);
        assert_matches!(toks[1], Tok::Unsigned);
        assert_matches!(toks[2], Tok::Id(ref id) if id.as_ref() == "fibs");
        assert_matches!(toks[3], Tok::LParen);
        assert_matches!(toks[4], Tok::Id(ref id) if id.as_ref() == "n");
        assert_matches!(toks[5], Tok::Colon);
        assert_matches!(toks[6], Tok::Unsigned);
        assert_matches!(toks[7], Tok::RParen);
        assert_matches!(toks[8], Tok::Colon);
        assert_matches!(toks[9], Tok::Unsigned);
        assert_matches!(toks[10], Tok::Assign);
        assert_matches!(toks[11], Tok::If);
        assert_matches!(toks[12], Tok::Id(ref id) if id.as_ref() == "n");
        assert_matches!(toks[13], Tok::Smaller);
        assert_matches!(toks[14], Tok::IntLit { signed: true, bits: 32, val: 2 });
        assert_matches!(toks[15], Tok::LBraces);
        assert_matches!(toks[16], Tok::Id(ref id) if id.as_ref() == "n");
        assert_matches!(toks[17], Tok::RBraces);
        assert_matches!(toks[18], Tok::Else);
        assert_matches!(toks[19], Tok::LBraces);
        assert_matches!(toks[20], Tok::Id(ref id) if id.as_ref() == "fibs");
        assert_matches!(toks[21], Tok::LParen);
        assert_matches!(toks[22], Tok::Id(ref id) if id.as_ref() == "n");
        assert_matches!(toks[23], Tok::Minus);
        assert_matches!(toks[24], Tok::IntLit { signed: true, bits: 32, val: 2 });
        assert_matches!(toks[25], Tok::RParen);
        assert_matches!(toks[26], Tok::Plus);
        assert_matches!(toks[27], Tok::Id(ref id) if id.as_ref() == "fibs");
        assert_matches!(toks[28], Tok::LParen);
        assert_matches!(toks[29], Tok::Id(ref id) if id.as_ref() == "n");
        assert_matches!(toks[30], Tok::Minus);
        assert_matches!(toks[31], Tok::IntLit { signed: true, bits: 32, val: 1 });
        assert_matches!(toks[32], Tok::RParen);
        assert_matches!(toks[33], Tok::RBraces);
        assert_matches!(toks[34], Tok::SemiColon);
    }
}
