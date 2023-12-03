use std::collections::{HashMap, HashSet};

use proc_macro2::{Ident, Span, TokenStream};
use quote::quote;
use syn::{Error, Expr, LitStr, Type};

use crate::{
    ast::{DeclType, GrammarContentDecls, RuleEntity, Rules, TokenDecl, TokenDeclRegex},
    preprocess_rust_code::convert_litteral,
};

pub enum TokenD {
    Regex(Ident, LitStr, Option<Type>),
    Exact(Ident, LitStr, Option<Type>),
}

pub struct AstFuncD {
    func: Expr,
    after: usize,
    last: bool,
    name: Ident,
}

#[derive(Clone)]
pub enum RuleSymbolD {
    T(Ident, Option<Type>),
    NT(Ident),
    Epsilon,
}
impl RuleSymbolD {
    pub fn is_epsilon(&self) -> bool {
        match self {
            Self::Epsilon => true,
            _ => false,
        }
    }
}

impl ToString for RuleSymbolD {
    fn to_string(&self) -> String {
        match self {
            Self::T(i, _) => i.to_string(),
            Self::NT(i) => i.to_string(),
            Self::Epsilon => "<none>".to_string(),
        }
    }
}

pub struct RuleD {
    left: Ident,
    symbols: Vec<RuleSymbolD>,
    ast_funcs: Vec<AstFuncD>,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum AstFuncParamMode {
    TokenGetVal,
    TokenEmpty,
    AstNode,
}

impl RuleD {
    pub fn ast_func(&self, parser_type: &syn::Type) -> TokenStream {
        let mut q = quote!();
        for ast in &self.ast_funcs {
            let mut type_names = Vec::new();
            let mut param_type = Vec::new();
            let mut mappings = Vec::new();
            for (i, p) in self.symbols[..ast.after].iter().enumerate() {
                match p {
                    RuleSymbolD::Epsilon => {
                        type_names.push(Ident::new("NONE", Span::call_site()));
                        param_type.push(AstFuncParamMode::TokenEmpty);
                    }
                    RuleSymbolD::T(tt, ty) => {
                        type_names.push(tt.clone());
                        if ty.is_some() {
                            param_type.push(AstFuncParamMode::TokenGetVal);
                        } else {
                            param_type.push(AstFuncParamMode::TokenEmpty);
                        }
                    }
                    RuleSymbolD::NT(tt) => {
                        type_names.push(tt.clone());
                        param_type.push(AstFuncParamMode::AstNode);
                    }
                }
                mappings.push(Ident::new(&format!("__mapping_{i}__"), ast.name.span()));
            }
            let return_id = Ident::new("__return__", ast.name.span());
            let params_id = Ident::new("__params__", ast.name.span());
            let parser_data_id = Ident::new("__parser_data__", ast.name.span());
            let mut ast_q = quote!();
            for (i, ((m, t), c)) in mappings.iter().zip(type_names).zip(param_type).enumerate() {
                match c {
                    AstFuncParamMode::TokenEmpty => {}
                    AstFuncParamMode::TokenGetVal => {
                        ast_q = quote! { #ast_q let AstParam::Token(Token { ty: TokenType:: #t (mut #m), .. }) = #params_id[#i].clone() else { panic!() }; };
                    }
                    AstFuncParamMode::AstNode => {
                        ast_q = quote! { #ast_q let AstParam::Ast(AstNode:: #t (mut #m)) = #params_id[#i].clone() else { panic!() }; };
                    }
                }
            }
            let name = ast.name.clone();
            let output_type = self.left.clone();
            let e = ast.func.clone();
            let ast_q = if ast.last {
                quote! {
                    #[allow(non_snake_case)]
                    fn #name (#[allow(non_snake_case)] #params_id: &[AstParam], #[allow(non_snake_case)] #parser_data_id: &mut #parser_type) -> AstParam {
                        #ast_q
                        let mut #return_id;
                        #e
                        AstParam::Ast(AstNode:: #output_type(#return_id))
                    }
                }
            } else {
                quote! {
                    #[allow(non_snake_case)]
                    fn #name (#[allow(non_snake_case)] #params_id: &[AstParam], #[allow(non_snake_case)] #parser_data_id: &mut #parser_type) {
                        #ast_q
                        #e
                    }
                }
            };
            q = quote!( #q #ast_q );
        }
        q
    }
}

pub struct GrammarData {
    tokens: Vec<TokenD>,
    skip: Vec<LitStr>,
    parser_data: Type,
    types: Vec<(Ident, Type)>,
    start: Ident,
    nterms: Vec<String>,
    rules: Vec<RuleD>,
    first_sets: HashMap<String, HashSet<String>>,
    follow_sets: HashMap<String, HashSet<String>>,
}

impl GrammarData {
    pub fn new(before: GrammarContentDecls, after: Rules) -> syn::Result<Self> {
        let mut tokens = Vec::new();
        let mut skip = Vec::new();
        let mut parser_data = None;
        let mut types = Vec::new();
        let mut start = None;
        let mut rules = Vec::new();
        let mut terms = Vec::new();
        let mut nterms = Vec::new();
        for decl in &before.decls {
            match &decl.decl {
                DeclType::Token(t) => match t {
                    TokenDecl::Exact(ref t, lit) => {
                        let id_txt = convert_litteral(&lit.value());
                        tokens.push(TokenD::Exact(
                            Ident::new(&id_txt, lit.span()),
                            lit.clone(),
                            t.clone().map(|d| d.ty),
                        ));
                        terms.push(id_txt);
                    }
                    TokenDecl::Regex(ref t, TokenDeclRegex { name, regex, .. }) => {
                        tokens.push(TokenD::Regex(
                            name.clone(),
                            regex.clone(),
                            t.clone().map(|d| d.ty),
                        ));
                        terms.push(name.to_string());
                    }
                },
                DeclType::Skip(s) => skip.push(s.clone()),
                DeclType::ParserData(p) => parser_data = Some(p.clone()),
                DeclType::Start(s) => start = Some(s.clone()),
                DeclType::Type(t, i) => {
                    for id in i {
                        nterms.push(id.to_string());
                        types.push((id.clone(), t.ty.clone()));
                    }
                }
            }
        }
        let start = start.expect("Expected start symbol (%start [symbol])");
        let parser_data = parser_data.expect("Expected parserdata type (%parserdata [type])");
        for r in &after.rules {
            for (i, sr) in r.subrules.iter().enumerate() {
                let mut symbols = Vec::new();
                let mut ast_funcs = Vec::new();
                let mut after = 0;
                let mut ast_func_id = 0;
                for e in &sr.entities {
                    match e {
                        RuleEntity::Epsilon(_) => {
                            symbols.push(RuleSymbolD::Epsilon);
                        }
                        RuleEntity::Exact(lit) => {
                            let lit_v = convert_litteral(&lit.value());
                            if !terms.contains(&lit_v) {
                                return Err(Error::new_spanned(lit, "Unknown token"));
                            }
                            let ty = tokens.iter().find_map(|t| match t {
                                TokenD::Exact(a, _, t) => {
                                    if a.to_string() == lit_v {
                                        t.clone()
                                    } else {
                                        None
                                    }
                                }
                                TokenD::Regex(a, _, t) => {
                                    if a.to_string() == lit_v {
                                        t.clone()
                                    } else {
                                        None
                                    }
                                }
                            });
                            let id = Ident::new(&lit_v, lit.span());
                            symbols.push(RuleSymbolD::T(id, ty));
                            after += 1;
                        }
                        RuleEntity::Ident(id) => {
                            let id_v = id.to_string();
                            if terms.contains(&id_v) {
                                let ty = tokens.iter().find_map(|t| match t {
                                    TokenD::Exact(a, _, t) => {
                                        if a.to_string() == id_v {
                                            t.clone()
                                        } else {
                                            None
                                        }
                                    }
                                    TokenD::Regex(a, _, t) => {
                                        if a.to_string() == id_v {
                                            t.clone()
                                        } else {
                                            None
                                        }
                                    }
                                });
                                symbols.push(RuleSymbolD::T(id.clone(), ty));
                            } else if nterms.contains(&id_v) {
                                symbols.push(RuleSymbolD::NT(id.clone()));
                            } else {
                                return Err(Error::new_spanned(id, "Unknown symbol"));
                            }
                            after += 1;
                        }
                        RuleEntity::AstFunc(a) => {
                            ast_funcs.push(AstFuncD {
                                func: a.clone(),
                                after,
                                last: false,
                                name: Ident::new(
                                    &format!("{}_{}_{}", r.left.to_string(), i, ast_func_id),
                                    r.left.span(),
                                ),
                            });
                            ast_func_id += 1;
                        }
                    }
                }
                if let Some(a) = ast_funcs.last_mut() {
                    let l = symbols
                        .iter()
                        .filter(|s| !s.is_epsilon())
                        .collect::<Vec<_>>()
                        .len();
                    if a.after < l && l > 0 {
                        return Err(Error::new_spanned(
                            r.left.clone(),
                            "This rule has no final ast production rule",
                        ));
                    }
                    a.last = true;
                } else {
                    return Err(Error::new_spanned(
                        r.left.clone(),
                        "This rule has no ast production rule",
                    ));
                }
                rules.push(RuleD {
                    left: r.left.clone(),
                    symbols,
                    ast_funcs,
                });
            }
        }
        let mut this = Self {
            tokens,
            skip,
            parser_data,
            types,
            nterms,
            start,
            rules,
            first_sets: HashMap::new(),
            follow_sets: HashMap::new(),
        };
        this.calculate_first_sets();
        this.calculate_follow_sets();
        Ok(this)
    }

    pub fn calculate_first_sets(&mut self) {
        for nt in &self.nterms {
            self.first_sets.insert(nt.clone(), HashSet::new());
        }

        let mut changed = true;
        while changed {
            changed = false;
            for rule in &self.rules {
                let left = rule.left.to_string();
                let right = &rule.symbols;
                let mut add_eps = true;
                for symbol in right {
                    if let RuleSymbolD::T(term, _) = symbol {
                        if !self
                            .first_sets
                            .get(&left)
                            .expect(&format!("didn't find {left} in first_sets"))
                            .contains(&term.to_string())
                        {
                            self.first_sets
                                .get_mut(&left)
                                .unwrap()
                                .insert(term.to_string());
                            changed = true;
                        }
                        add_eps = false;
                        break;
                    } else if let RuleSymbolD::NT(nt) = symbol {
                        let first_nt = self
                            .first_sets
                            .get(&nt.to_string())
                            .expect(&format!("didn't find {nt} in first_sets"))
                            .clone();
                        for s in &first_nt {
                            if !self
                                .first_sets
                                .get(&left)
                                .expect(&format!("didn't find {left} in first_sets"))
                                .contains(s)
                            {
                                self.first_sets.get_mut(&left).unwrap().insert(s.clone());
                                changed = true;
                            }
                        }
                        if !first_nt.contains("<none>") {
                            add_eps = false;
                            break;
                        }
                    }
                }
                if add_eps
                    && !self
                        .first_sets
                        .get(&left)
                        .expect(&format!("didn't find {} in first_sets", left))
                        .contains("<none>")
                {
                    self.first_sets
                        .get_mut(&left)
                        .unwrap()
                        .insert("<none>".to_string());
                    changed = true;
                }
            }
        }
    }

    pub fn calculate_follow_sets(&mut self) {
        // Initialize follow sets with empty sets
        for symbol in &self.nterms {
            self.follow_sets.insert(symbol.clone(), HashSet::new());
        }
        let fst = self.start.to_string();
        self.follow_sets
            .get_mut(&fst)
            .expect("Didn't find fst inside follow_sets")
            .insert("EOF".to_string());

        let mut changed = true;
        // repeat until the sets are stabilized
        while changed {
            changed = false;

            // check for each rule
            for rule in &self.rules {
                // each symbol
                for (i, symbol) in rule.symbols.iter().enumerate() {
                    // if it is a non terminal.
                    if let RuleSymbolD::NT(nt) = symbol {
                        // in which case, this the symbols after it.
                        let mut rest = Vec::new();
                        for j in (i + 1)..rule.symbols.len() {
                            rest.push(rule.symbols[j].clone());
                        }

                        if let Some(first) = self.getfirst(&rest) {
                            let follow_set = self
                                .follow_sets
                                .get_mut(&nt.to_string())
                                .expect(&format!("didn't find {nt} in follow_sets"));
                            let old_len = follow_set.len();
                            follow_set.extend(first);
                            if follow_set.len() > old_len {
                                changed = true;
                            }
                        }

                        if self.can_derive_epsilon(&rest) {
                            let follow_set_a = self
                                .follow_sets
                                .get(&rule.left.to_string())
                                .expect(&format!("didn't find {} in follow_sets", rule.left))
                                .clone();
                            let follow_set_b = self
                                .follow_sets
                                .get_mut(&nt.to_string())
                                .expect(&format!("didn't find {nt} in follow_sets"));
                            let old_len = follow_set_b.len();
                            follow_set_b.extend(follow_set_a.iter().cloned());
                            if follow_set_b.len() > old_len {
                                changed = true;
                            }
                        }
                    }
                }
            }
        }
    }

    pub fn fill_ll1_production_table(&self) -> TokenStream {
        let mut q = quote!();
        // Iterate through each rule in the grammar.
        for rule in &self.rules {
            let non_terminal = &rule.left;
            let non_terminal_id = rule.left.clone();

            // Calculate the FIRST set for the right-hand side of the rule.
            let mut first_set = self.getfirst(&rule.symbols).unwrap_or(HashSet::new());

            // If the rule is nullable, add the FOLLOW set for the non-terminal.
            if self.can_derive_epsilon(&rule.symbols) {
                let follow_set = &self
                    .follow_sets
                    .get(&rule.left.to_string())
                    .expect(&format!("didn't find {non_terminal} in follow_sets"));
                first_set.extend(follow_set.iter().cloned());
            }

            // For each terminal in the FIRST set, create a production table entry.
            for terminal in first_set {
                if terminal != "<none>" {
                    let term_id = if terminal == "EOF" {
                        Ident::new("EOF", Span::call_site())
                    } else {
                        self.tokens
                            .iter()
                            .find_map(|t| match t {
                                TokenD::Regex(i, _, _) => {
                                    if i.to_string() == terminal {
                                        Some(i)
                                    } else {
                                        None
                                    }
                                }
                                TokenD::Exact(i, _, _) => {
                                    if i.to_string() == terminal {
                                        Some(i)
                                    } else {
                                        None
                                    }
                                }
                            })
                            .expect("Expected to find term_ident")
                            .clone()
                    };
                    let mut qqq = quote!( nt: AstNodeNoData:: #non_terminal_id, t: TokenTypeNoData:: #term_id, );
                    let mut qq = quote!();
                    for r in &rule.symbols {
                        match r {
                            RuleSymbolD::Epsilon => {}
                            RuleSymbolD::NT(nt) => {
                                qq = quote!( #qq Symbol::NT(AstNodeNoData:: #nt ), );
                            }
                            RuleSymbolD::T(t, _) => {
                                qq = quote!( #qq Symbol::T(TokenTypeNoData:: #t ), );
                            }
                        }
                    }
                    let mut ast_funcs = quote!();
                    for ast in rule.ast_funcs.iter().rev() {
                        let ast_func_name = ast.name.clone();
                        let n = rule
                            .symbols
                            .iter()
                            .filter(|p| !p.is_epsilon())
                            .collect::<Vec<_>>()
                            .len()
                            .min(ast.after);
                        ast_funcs = if ast.last {
                            quote!( #ast_funcs AstFunc::Fold(#ast_func_name, #n), )
                        } else {
                            quote!( #ast_funcs AstFunc::Exec(#ast_func_name, #n), )
                        };
                    }
                    qqq = quote!( ParseTableEntry { #qqq expand: &[ #qq ], ast_func: &[ #ast_funcs ] }, );
                    q = quote!( #q #qqq );
                }
            }
        }

        quote!( &[ #q ] ).into()
    }

    fn getfirst(&self, symbols: &[RuleSymbolD]) -> Option<HashSet<String>> {
        let mut out = HashSet::new();
        for s in symbols {
            match s {
                RuleSymbolD::T(s, _) => {
                    out.insert(s.to_string());
                    break;
                }
                RuleSymbolD::NT(s) => {
                    let mut fsts = self
                        .first_sets
                        .get(&s.to_string())
                        .expect(&format!("didn't find {s} in first_sets"))
                        .clone();
                    if fsts.remove("<none>") {
                        out.extend(fsts.clone());
                        continue;
                    }
                    out.extend(fsts.clone());
                    break;
                }
                RuleSymbolD::Epsilon => {}
            }
        }
        if out.is_empty() {
            None
        } else {
            Some(out)
        }
    }

    fn can_derive_epsilon(&self, symbols: &[RuleSymbolD]) -> bool {
        for s in symbols {
            match s {
                RuleSymbolD::Epsilon => {}
                RuleSymbolD::T(_, _) => {
                    return false;
                }
                RuleSymbolD::NT(s) => {
                    let fsts = &self
                        .first_sets
                        .get(&s.to_string())
                        .expect(&format!("didn't find {s} in first_sets"));
                    if !fsts.contains("<none>") {
                        return false;
                    }
                }
            }
        }
        true
    }

    pub fn tokens(&self) -> TokenStream {
        let mut quote = quote!();
        let mut quote2 = quote!();
        let mut quote3 = quote!();
        for t in &self.tokens {
            match t {
                TokenD::Exact(a, _, t) | TokenD::Regex(a, _, t) => {
                    if let Some(t) = t {
                        quote = quote! { #quote
                        #[allow(non_camel_case_types)]
                        #a ( #t ), };
                        quote3 = quote!( #quote3 Self:: #a (_) => TokenTypeNoData:: #a,);
                    } else {
                        quote = quote! { #quote
                        #[allow(non_camel_case_types)]
                        #a, };
                        quote3 = quote!( #quote3 Self:: #a => TokenTypeNoData:: #a,);
                    }
                    quote2 = quote!( #quote2 #[allow(non_camel_case_types)] #a,);
                }
            };
        }
        quote = quote!(
            use parser_gen::from_lexed::FromLexed;
            pub type Token = parser_gen::lexer::Token<TokenType>;
            #[derive(Debug, Clone, PartialEq)]
            pub enum TokenType { #quote EOF }

            impl parser_gen::parser::NoData<TokenTypeNoData> for TokenType {
                fn no_data(&self) -> TokenTypeNoData {
                match self {
                    #quote3
                    Self::EOF => TokenTypeNoData::EOF,
                }
            }
            }

            impl parser_gen::parser::GetEOF<TokenTypeNoData> for TokenType {
                fn get_eof() -> TokenTypeNoData { TokenTypeNoData::EOF }
            }

            #[derive(Clone, Debug, Eq, PartialEq, Copy)]
            pub enum TokenTypeNoData {
            #quote2 EOF
        }
        );
        let mut impl_quote = quote!();
        for t in &self.tokens {
            let q2 = match t {
                TokenD::Exact(a, _, t) | TokenD::Regex(a, _, t) => {
                    let fident = syn::Ident::new(&format!("make_{}", a.to_string()), a.span());

                    if let Some(t) = t {
                        quote! {
                            #[allow(non_snake_case)]
                            fn #fident (s: &str) -> Self {
                                Self:: #a ( #t ::from_lexed(s))
                            }
                        }
                    } else {
                        quote! {
                            #[allow(non_snake_case)]
                            fn #fident (_: &str) -> Self {
                                Self:: #a
                            }
                        }
                    }
                }
            };
            impl_quote = quote!( #impl_quote #q2 );
        }
        quote = quote! { #quote impl TokenType { #impl_quote fn make_t_eof(_: &str) -> Self { Self::EOF }} };

        let mut lexer_quote = quote! { Lexer::builder() };
        for t in &self.tokens {
            match t {
                TokenD::Regex(a, r, _) => {
                    let fident = syn::Ident::new(&format!("make_{}", a.to_string()), a.span());
                    lexer_quote =
                        quote! { #lexer_quote .token(#r, Box::new(TokenType:: #fident), false)};
                }
                TokenD::Exact(a, v, _) => {
                    let fident = syn::Ident::new(&format!("make_{}", a.to_string()), a.span());
                    lexer_quote =
                        quote! { #lexer_quote .token(#v, Box::new(TokenType:: #fident), true)};
                }
            }
        }
        for t in &self.skip {
            lexer_quote = quote! { #lexer_quote .skip(#t) };
        }
        // pub type Lexer = parser_gen::lexer::Lexer<fn(&str) -> TokenType, TokenType>;
        quote = quote! { #quote
            pub type Lexer = parser_gen::lexer::Lexer<TokenType>;
            pub fn lexer() -> Lexer {
                #lexer_quote .eof(Box::new(TokenType::make_t_eof))
                    .build().expect("Expected to be able to build lexer")
            }
        };

        let parser_data_type = self.parser_data.clone();
        let mut q = quote!();
        let mut q2 = quote!();
        let mut q3 = quote!();
        for (id, ty) in &self.types {
            q = quote! { #q
            #[allow(non_camel_case_types)]
            #id ( #ty ), };
            q2 = quote! { #q2
            #[allow(non_camel_case_types)]
            #id,
            };
            q3 = quote! { #q3 Self:: #id(_) => AstNodeNoData:: #id, };
        }
        let start_item = self.start.clone();
        let start_type = self
            .types
            .iter()
            .find(|(i, _)| i.to_string() == start_item.to_string())
            .expect("No type for start item")
            .1
            .clone();
        quote = quote! { #quote

            #[derive(Clone, Debug)]
            pub enum AstNode {
            #q
        }
            impl parser_gen::parser::NoData<AstNodeNoData> for AstNode {
                fn no_data(&self) -> AstNodeNoData {
                    match self {
                        #q3
                    }
                }
            }
            #[derive(Clone, Debug, Eq, PartialEq, Copy)]
            pub enum AstNodeNoData {
            #q2
        }
        pub type Symbol = parser_gen::parser::RuleSymbol<TokenTypeNoData, AstNodeNoData>;
        pub type AstParam = parser_gen::parser::AstParam<Token, AstNode>;
        pub type AstFunc = parser_gen::parser::AstFunc<Token, AstNode, #parser_data_type>;
        pub type ParseTableEntry = parser_gen::parser::ParseTableEntry<Token, TokenTypeNoData, AstNode, AstNodeNoData, #parser_data_type>;

        pub fn parse<'r>(tokens: parser_gen::lexer::Tokens<'r, TokenType>) -> #start_type {
                let Some(AstParam::Ast(AstNode:: #start_item (s))) = parser_gen::parser::parse(PARSE_TABLE, AstNodeNoData:: #start_item, tokens, #parser_data_type ::new()) else { panic!("Parser error"); };
                s
            }

        pub fn parse_source(source: parser_gen::lexer::SourceFile) -> #start_type {
            parse(lexer().tokens(&source))
        }

        };

        for r in &self.rules {
            let r = r.ast_func(&self.parser_data);
            quote = quote!( #quote #r );
        }
        let parse_table_entries = self.fill_ll1_production_table();
        quote = quote!( #quote
        const PARSE_TABLE: &[ParseTableEntry] =
                #parse_table_entries;
        );

        quote.into()
    }
}
