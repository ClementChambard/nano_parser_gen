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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UnOp {
    Negate,
    Not,
    Sin,
    Cos,
    Sqrt,
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
