use crate::error::TypingError;

#[derive(Clone, Debug, Copy, PartialEq, Eq)]
pub enum BinOp {
    Or,
    And,
    BinOr,
    Xor,
    BinAnd,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

impl BinOp {
    pub fn check_typing(&self, e1: &Box<Expr>, e2: &Box<Expr>) -> Result<Type, TypingError> {
        match self {
            Self::Or | Self::And | Self::Xor | Self::BinOr | Self::BinAnd => {
                match (e1.ty()?, e2.ty()?) {
                    (Type::Int, Type::Int) => Ok(Type::Int),
                    _ => Err(TypingError::from(
                        "Logical and Bitwise operations are on integers",
                    )),
                }
            }
            Self::Eq
            | Self::Ne
            | Self::Lt
            | Self::Le
            | Self::Gt
            | Self::Ge
            | Self::Add
            | Self::Sub
            | Self::Mul
            | Self::Div
            | Self::Mod => match (e1.ty()?, e2.ty()?) {
                (Type::Int, Type::Int) => Ok(Type::Int),
                (Type::Float, Type::Float) => Ok(Type::Float),
                _ => Err(TypingError::from(
                    "Binary operations are for the same number type",
                )),
            },
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UnOp {
    Negate,
    Not,
    Sin,
    Cos,
    Sqrt,
}

impl UnOp {
    pub fn check_typing(&self, e: &Box<Expr>) -> Result<Type, TypingError> {
        let t = e.ty()?;
        match self {
            Self::Negate | Self::Not => match t {
                Type::Int | Type::Float => Ok(t),
                _ => Err(TypingError::from("Must use number with Not or Negate")),
            },
            Self::Sin | Self::Cos | Self::Sqrt => match t {
                Type::Float => Ok(t),
                _ => Err(TypingError::from("Must use float with Sin, Cos or Sqrt")),
            },
        }
    }
}

#[derive(Clone, Debug)]
pub enum Expr {
    Binary(BinOp, Box<Expr>, Box<Expr>),
    Unary(UnOp, Box<Expr>),
    Int(i32),
    Float(f32),
    Str(String),
    VarInt(i32),
    VarFloat(f32),
}

#[derive(Clone, Debug, Copy, PartialEq, Eq)]
pub enum Type {
    Int,
    Float,
    Str,
}

impl Expr {
    pub fn ty(&self) -> Result<Type, TypingError> {
        match self {
            Self::Unary(op, e) => op.check_typing(e),
            Self::Binary(op, e1, e2) => op.check_typing(e1, e2),
            Self::Int(_) | Self::VarInt(_) => Ok(Type::Int),
            Self::Float(_) | Self::VarFloat(_) => Ok(Type::Float),
            Self::Str(_) => Ok(Type::Str),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Param {
    Int(String),
    Float(String),
}

#[derive(Clone, Debug)]
pub struct Sub {
    pub name: String,
    pub params: Vec<Param>,
    pub bloc: Vec<Instr>,
    pub max_offset: usize,
}

#[derive(Clone, Debug)]
pub struct Ecl {
    pub ecli: Vec<String>,
    pub anmi: Vec<String>,
    pub subs: Vec<Sub>,
}

#[derive(Clone, Debug)]
pub enum TimeLabel {
    Set(i32),
    Add(i32),
    Sub(i32),
}

pub fn rank_label_spec(s: String) -> u8 {
    let mut v = 192;
    for c in s.chars() {
        match c {
            'e' | 'E' => v |= 0b00000001,
            'n' | 'N' => v |= 0b00000010,
            'h' | 'H' => v |= 0b00000100,
            'l' | 'L' => v |= 0b00001000,
            'o' | 'O' => v |= 0b00010000,
            'x' | 'X' => v |= 0b00100000,
            _ => panic!("Unknown difficulty specifier: '{c}'"),
        }
    }
    v
}

#[derive(Clone, Debug)]
pub enum Instr {
    Call(i32, Vec<Expr>, usize, u8),
    Label(String),
    AffectInt(i32, Expr, usize, u8),
    AffectFloat(f32, Expr, usize, u8),
    Bloc(Vec<Instr>),
    Goto(String, i32, usize, u8),
    If(Expr, Vec<Instr>, Option<Box<Instr>>, usize, u8),
    Loop(Vec<Instr>, usize, u8),
    Break(usize, u8),
    Continue(usize, u8),
    Return(usize, u8),
    Delete(usize, u8),
    While(Expr, Vec<Instr>, usize, u8),
    DoWhile(Expr, Vec<Instr>, usize, u8),
    None,
    SubCall(String, Vec<Expr>, Option<i32>, usize, u8),
}

impl Instr {
    pub fn is_some(&self) -> bool {
        match self {
            Self::None => false,
            _ => true,
        }
    }
}
