use crate::ast::Instr;

use super::*;

#[derive(Clone, Debug)]
pub enum InstrSub {
    Call(Vec<ast::Expr>),
    Affect(ast::Expr),
    Label,
}

#[derive(Clone, Debug)]
pub enum BinExprPart {
    Some(ast::BinOp, ast::Expr, Box<BinExprPart>),
    None,
}

impl BinExprPart {
    pub fn merge(self, left: ast::Expr) -> ast::Expr {
        let mut e = left;
        let mut s = Box::new(self);
        while let Self::Some(op, right, next) = *s {
            s = next;
            e = ast::Expr::Binary(op, Box::new(e), Box::new(right));
        }
        e
    }

    pub fn chain(self, left: ast::Expr, op: ast::BinOp) -> Self {
        Self::Some(op, left, Box::new(self))
    }
}

#[derive(Clone)]
pub struct Binding {
    name: String,
    offset: Option<usize>,
    _used: bool,
    assigned: bool,
}

#[derive(Clone)]
pub enum BindingT {
    Int(Binding),
    Float(Binding),
}

impl BindingT {
    fn use_it(&mut self, i: usize) -> bool {
        match self {
            Self::Int(b) | Self::Float(b) => {
                if !b.assigned {
                    panic!("Variable {} used before assignment", b.name);
                }
                if b.offset.is_none() {
                    b.offset = Some(i);
                    true
                } else {
                    false
                }
            }
        }
    }
}

#[derive(Clone)]
pub struct Scope {
    bindings: Vec<BindingT>,
    cur_offset: usize,
    max_offset: usize,
}

impl Scope {
    fn check_duplicate(&self, name: &str) {
        if self
            .bindings
            .iter()
            .find(|b| match b {
                BindingT::Int(b) | BindingT::Float(b) => b.name == name,
            })
            .is_some()
        {
            panic!("Duplicate variable {name}");
        }
    }
    pub fn add_int(&mut self, name: String) {
        self.check_duplicate(&name);
        self.bindings.push(BindingT::Int(Binding {
            name,
            offset: None,
            _used: false,
            assigned: false,
        }))
    }
    pub fn add_int_alloced(&mut self, name: String) {
        self.check_duplicate(&name);
        self.bindings.push(BindingT::Int(Binding {
            name,
            offset: Some(self.cur_offset),
            _used: false,
            assigned: true,
        }));
        self.cur_offset += 1;
        self.max_offset = self.cur_offset.max(self.max_offset);
    }
    pub fn add_float(&mut self, name: String) {
        self.check_duplicate(&name);
        self.bindings.push(BindingT::Float(Binding {
            name,
            offset: None,
            _used: false,
            assigned: false,
        }))
    }
    pub fn add_float_alloced(&mut self, name: String) {
        self.check_duplicate(&name);
        self.bindings.push(BindingT::Float(Binding {
            name,
            offset: Some(self.cur_offset),
            _used: false,
            assigned: true,
        }));
        self.cur_offset += 1;
        self.max_offset = self.cur_offset.max(self.max_offset);
    }
    pub fn use_binding(&mut self, name: &str) -> Option<ast::Expr> {
        let binding = self.bindings.iter_mut().find(|b| match b {
            BindingT::Int(b) | BindingT::Float(b) => b.name == name,
        })?;
        if binding.use_it(self.cur_offset) {
            self.cur_offset += 1;
            self.max_offset = self.cur_offset.max(self.max_offset);
        }
        Some(match binding {
            BindingT::Float(b) => ast::Expr::VarFloat((b.offset.unwrap() * 4) as f32),
            BindingT::Int(b) => ast::Expr::VarInt((b.offset.unwrap() * 4) as i32),
        })
    }
    pub fn assign_binding(
        &mut self,
        name: &str,
        e: &ast::Expr,
        time: usize,
        rank: u8,
    ) -> Option<ast::Instr> {
        let binding = self.bindings.iter_mut().find(|b| match b {
            BindingT::Int(b) | BindingT::Float(b) => b.name == name,
        })?;
        match binding {
            BindingT::Float(b) => {
                b.assigned = true;
                if b.offset.is_none() {
                    b.offset = Some(self.cur_offset);
                    self.cur_offset += 1;
                    self.max_offset = self.cur_offset.max(self.max_offset);
                }
                Some(ast::Instr::AffectFloat(
                    (b.offset.unwrap() * 4) as f32,
                    e.clone(),
                    time,
                    rank,
                ))
            }
            BindingT::Int(b) => {
                b.assigned = true;
                if b.offset.is_none() {
                    b.offset = Some(self.cur_offset);
                    self.cur_offset += 1;
                    self.max_offset = self.cur_offset.max(self.max_offset);
                }
                Some(ast::Instr::AffectInt(
                    (b.offset.unwrap() * 4) as i32,
                    e.clone(),
                    time,
                    rank,
                ))
            }
        }
    }
}

#[derive(Clone)]
pub struct ParserData {
    scope: Vec<Scope>,
    pub instrs: Vec<Instr>,
    pub rank: u8,
    pub time: usize,
}

impl ParserData {
    pub fn new() -> Self {
        Self {
            scope: vec![],
            rank: 255,
            instrs: Vec::new(),
            time: 0,
        }
    }

    pub fn add_int(&mut self, name: String) {
        self.scope
            .last_mut()
            .expect("No scope present")
            .add_int(name);
    }
    pub fn add_int_alloced(&mut self, name: String) {
        self.scope
            .last_mut()
            .expect("No scope present")
            .add_int_alloced(name);
    }
    pub fn add_float(&mut self, name: String) {
        self.scope
            .last_mut()
            .expect("No scope present")
            .add_float(name);
    }
    pub fn add_float_alloced(&mut self, name: String) {
        self.scope
            .last_mut()
            .expect("No scope present")
            .add_float_alloced(name);
    }
    pub fn use_binding(&mut self, name: String) -> ast::Expr {
        for s in self.scope.iter_mut().rev() {
            if let Some(b) = s.use_binding(&name) {
                return b;
            }
        }
        panic!("Undeclared identifier {name}");
    }

    pub fn push_scope(&mut self) {
        if let Some(last_scope) = self.scope.last() {
            self.scope.push(Scope {
                bindings: Vec::new(),
                cur_offset: last_scope.cur_offset,
                max_offset: last_scope.max_offset,
            });
        } else {
            self.scope.push(Scope {
                bindings: Vec::new(),
                cur_offset: 0,
                max_offset: 0,
            });
        }
    }

    pub fn assign_binding(&mut self, name: &str, e: ast::Expr) -> ast::Instr {
        for s in self.scope.iter_mut().rev() {
            if let Some(a) = s.assign_binding(&name, &e, self.time, self.rank) {
                return a;
            }
        }
        panic!("Undeclared identifier {name}");
    }

    pub fn max_offset(&self) -> usize {
        self.scope.get(0).expect("No scope present").max_offset
    }

    pub fn pop_scope(&mut self) {
        let scope = self.scope.pop().expect("No scope present");
        if let Some(last_scope) = self.scope.last_mut() {
            last_scope.max_offset = last_scope.max_offset.max(scope.max_offset);
        }
    }
}
