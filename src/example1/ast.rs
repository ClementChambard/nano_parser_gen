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
}

fn flatten_bloc(bloc: &[Instr]) -> (Vec<Instr>, bool) {
    let mut out = Vec::new();
    for i in bloc {
        match &i.ty {
            InstrType::None => {}
            InstrType::Bloc(l) => {
                let (b, r) = flatten_bloc(l);
                out.extend(b);
                if r {
                    return (out, true);
                }
            }
            InstrType::Loop(b) => {
                let (b, r) = flatten_bloc(b);
                if r {
                    out.extend(b);
                } else {
                    out.push(Instr::new(InstrType::Loop(b), i.time, i.diff, i.i));
                    out.push(Instr::new(InstrType::Delete, 0, 255, i.i + 1));
                }
                return (out, true);
            }
            InstrType::While(e, b) => {
                let (b, _) = flatten_bloc(b);
                out.push(Instr::new(
                    InstrType::While(e.clone(), b),
                    i.time,
                    i.diff,
                    i.i,
                ));
            }
            InstrType::DoWhile(e, b) => {
                let (b, r) = flatten_bloc(b);
                if r {
                    out.extend(b);
                    return (out, true);
                }
                out.push(Instr::new(
                    InstrType::DoWhile(e.clone(), b),
                    i.time,
                    i.diff,
                    i.i,
                ));
            }
            InstrType::If(e, b1, i2) => {
                let (b1, _) = flatten_bloc(b1);
                let i2 = if let Some(si2) = i2 {
                    Some(match &si2.ty {
                        InstrType::Bloc(l) => {
                            let (b, _) = flatten_bloc(l);
                            Box::new(Instr::new(InstrType::Bloc(b), si2.time, si2.diff, si2.i))
                        }
                        _ => {
                            let l = vec![*si2.clone()];
                            let (b, _) = flatten_bloc(&l);
                            let li2 = b.into_iter().next().unwrap();
                            Box::new(li2)
                        }
                    })
                } else {
                    None
                };
                out.push(Instr::new(
                    InstrType::If(e.clone(), b1, i2),
                    i.time,
                    i.diff,
                    i.i,
                ));
            }
            //TODO: other kind of bloc

            // remove unreachable code
            InstrType::Return => {
                out.push(i.clone());
                return (out, true);
            }
            InstrType::Delete => {
                out.push(i.clone());
                return (out, true);
            }
            _ => out.push(i.clone()),
        }
    }
    (out, false)
}

impl Sub {
    pub fn new(name: String, params: Vec<Param>, bloc: Vec<Instr>, max_offset: usize) -> Self {
        let (mut bloc, r) = flatten_bloc(&bloc);
        bloc.insert(
            0,
            Instr::new(
                InstrType::Call(40, vec![Expr::Int(max_offset as i32 * 4)]),
                0,
                255,
                0,
            ),
        );
        if !r {
            bloc.push(Instr::new(InstrType::Return, 0, 255, 1000000));
        }
        Self { name, params, bloc }
    }
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
pub struct Instr {
    i: usize,
    time: usize,
    diff: u8,
    ty: InstrType,
}

impl Instr {
    pub fn new(ty: InstrType, time: usize, diff: u8, i: usize) -> Self {
        Self {
            i: i,
            time: time,
            diff: diff,
            ty,
        }
    }

    pub fn none() -> Self {
        Self {
            i: 0,
            time: 0,
            diff: 0,
            ty: InstrType::None,
        }
    }

    pub fn bloc(b: Vec<Instr>) -> Self {
        Self {
            i: 0,
            time: 0,
            diff: 0,
            ty: InstrType::Bloc(b),
        }
    }

    pub fn is_some(&self) -> bool {
        match self.ty {
            InstrType::None => false,
            _ => true,
        }
    }
}

#[derive(Clone, Debug)]
pub enum InstrType {
    Call(i32, Vec<Expr>),
    AffectInt(i32, Expr),
    AffectFloat(f32, Expr),
    Bloc(Vec<Instr>),
    Goto(String, i32),
    If(Expr, Vec<Instr>, Option<Box<Instr>>),
    Loop(Vec<Instr>),
    Break,
    Continue,
    Return,
    Delete,
    While(Expr, Vec<Instr>),
    DoWhile(Expr, Vec<Instr>),
    SubCall(String, Vec<Expr>, Option<i32>),
    None,
}
