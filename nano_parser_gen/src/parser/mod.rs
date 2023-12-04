use crate::lexer::{Token, Tokens};

#[derive(Clone)]
pub struct NoParserData {}
impl NoParserData {
    pub fn new() -> Self {
        Self {}
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum RuleSymbol<T: Clone + Copy + Eq + PartialEq, N: Clone + Copy + Eq + PartialEq> {
    T(T),
    NT(N),
}

#[derive(Clone, Debug)]
pub enum AstParam<T: Clone, N: Clone> {
    Token(T),
    Ast(N),
}

#[derive(Clone, Debug)]
pub enum AstFunc<T: Clone, N: Clone, D: Clone> {
    Fold(fn(&[AstParam<T, N>], &mut D) -> AstParam<T, N>, usize),
    Exec(fn(&[AstParam<T, N>], &mut D), usize),
}

impl<T: Clone, N: Clone, D: Clone> AstFunc<T, N, D> {
    fn n(&self) -> usize {
        match self {
            Self::Fold(_, n) => *n,
            Self::Exec(_, n) => *n,
        }
    }
}

#[derive(Clone, Debug)]
pub struct ParseTableEntry<
    T: NoData<TN> + Clone + 'static,
    TN: Clone + Copy + Eq + PartialEq + 'static,
    N: NoData<NN> + Clone + 'static,
    NN: Clone + Copy + Eq + PartialEq + 'static,
    D: Clone + 'static,
> {
    pub nt: NN,
    pub t: TN,
    pub expand: &'static [RuleSymbol<TN, NN>],
    pub ast_func: &'static [AstFunc<T, N, D>],
}

pub trait NoData<T> {
    fn no_data(&self) -> T;
}

pub trait GetEOF<T> {
    fn get_eof() -> T;
}

fn ast_func_root<T: Clone, N: Clone, D: Clone>(
    args: &[AstParam<T, N>],
    _: &mut D,
) -> AstParam<T, N> {
    args[0].clone()
}

fn get_production_table_entry<
    T: NoData<TN> + Clone + GetEOF<TN>,
    TN: Clone + Copy + Eq + PartialEq,
    N: NoData<NN> + Clone,
    NN: Clone + Copy + Eq + PartialEq,
    D: Clone,
>(
    table: &[ParseTableEntry<Token<T>, TN, N, NN, D>],
    nt: NN,
    t: TN,
) -> Option<&ParseTableEntry<Token<T>, TN, N, NN, D>> {
    for entry in table {
        if entry.t == t && entry.nt == nt {
            return Some(entry);
        }
    }
    None
}

pub fn parse<
    'r,
    T: NoData<TN> + Clone + GetEOF<TN> + std::fmt::Debug,
    TN: Clone + Copy + Eq + PartialEq + std::fmt::Debug,
    N: NoData<NN> + Clone + std::fmt::Debug,
    NN: Clone + Copy + Eq + PartialEq + std::fmt::Debug,
    D: Clone,
>(
    parse_table: &[ParseTableEntry<Token<T>, TN, N, NN, D>],
    first_nt: NN,
    mut tokens: Tokens<'r, T>,
    mut data: D,
) -> Option<AstParam<Token<T>, N>> {
    let mut symbols_to_derive = vec![RuleSymbol::NT(first_nt), RuleSymbol::T(T::get_eof())];
    let mut cur_token_opt = tokens.next();

    let mut node_stack: Vec<AstParam<Token<T>, N>> = vec![];
    let mut node_stack_sizes: Vec<usize> = vec![0];
    let mut fn_stack: Vec<AstFunc<Token<T>, N, D>> = vec![AstFunc::Fold(ast_func_root, 2)];

    while !symbols_to_derive.is_empty() {
        let Some(ref cur_token) = cur_token_opt else { break; };
        let s = &symbols_to_derive[0];
        // check error
        match s {
            RuleSymbol::NT(nt) => {
                // println!("{:?}", nt);
                if let Some(parsing_table_entry) =
                    get_production_table_entry(parse_table, *nt, cur_token.ty.no_data())
                {
                    node_stack_sizes.push(0);
                    let mut rule_symbols = parsing_table_entry.expand.to_vec();
                    fn_stack.extend(parsing_table_entry.ast_func.to_vec());
                    rule_symbols.extend(symbols_to_derive.into_iter().skip(1));
                    symbols_to_derive = rule_symbols;
                    // println!(
                    //     "{:?}\n{:?}\n{:?}\n{:?}\n",
                    //     symbols_to_derive, fn_stack, node_stack, node_stack_sizes
                    // );
                } else {
                    return None;
                }
            }
            RuleSymbol::T(t) => {
                // println!("{:?}", t);
                if *t == cur_token.ty.no_data() {
                    node_stack.push(AstParam::Token(cur_token.clone()));
                    *node_stack_sizes.last_mut().unwrap() += 1;
                    cur_token_opt = tokens.next();
                    symbols_to_derive = symbols_to_derive[1..].to_vec();
                    // println!(
                    //     "{:?}\n{:?}\n{:?}\n{:?}\n",
                    //     symbols_to_derive, fn_stack, node_stack, node_stack_sizes
                    // );
                } else {
                    return None;
                }
            }
        }
        while let Some(n_last) = fn_stack.last().map(|l| l.n()) {
            if *node_stack_sizes.last().unwrap() >= n_last {
                let last = fn_stack.pop().unwrap();
                match last {
                    AstFunc::Fold(f, n) => {
                        node_stack_sizes.pop();
                        let new_node = f(&node_stack[node_stack.len() - n..], &mut data);
                        node_stack = node_stack[..(node_stack.len() - n)].to_vec();
                        node_stack.push(new_node);
                        if let Some(l) = node_stack_sizes.last_mut() {
                            *l += 1;
                        }
                    }
                    AstFunc::Exec(f, n) => {
                        f(&node_stack[node_stack.len() - n..], &mut data);
                    }
                }
            } else {
                break;
            }
        }
    }
    node_stack.into_iter().next()
}
