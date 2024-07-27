mod decls;
mod rules;
mod template;

pub use rules::{AstFuncD, RuleD, RuleSymbolD};
pub use template::TemplateParamType;

use std::collections::{HashMap, HashSet};

use proc_macro2::{Span, TokenStream};
use quote::quote;
use syn::{Expr, Ident, LitStr, Type};

use crate::{
    grammar::rules::RuleProcessingData,
    parsing::{decls::GrammarContentDecls, rules::Rules},
};

pub enum TokenD {
    Regex(Ident, LitStr, Option<Type>, Option<Expr>),
    Exact(Ident, LitStr, Option<Type>, Option<Expr>),
}

impl TokenD {
    pub fn get_term_name(&self) -> Ident {
        match self {
            Self::Regex(i, _, _, _) => i.clone(),
            Self::Exact(i, _, _, _) => i.clone(),
        }
    }
}

pub struct GrammarData {
    tokens: Vec<TokenD>,
    skip: Vec<LitStr>,
    parser_data: Option<Type>,
    types: Vec<(Ident, Type)>,
    start: Ident,
    nterms: Vec<String>,
    rules: Vec<RuleD>,
    first_sets: HashMap<String, HashSet<String>>,
    follow_sets: HashMap<String, HashSet<String>>,
}

impl GrammarData {
    pub fn new(before: GrammarContentDecls, after: Rules) -> syn::Result<Self> {
        let (tokens, skip, parser_data, types, start, templates) = decls::process(before)?;
        let terms = tokens
            .iter()
            .map(|t| t.get_term_name().to_string())
            .collect::<Vec<_>>();
        let (mut nterms, mut types) = rules::complete_nterms(types, &after);
        let (rules, o_t) = rules::process(
            &after.rules,
            &RuleProcessingData {
                tokens: &tokens,
                terms: &terms,
                types: &types,
                templates,
            },
        )?;
        nterms.extend(o_t.iter().map(|(i, _)| i.to_string()));
        types.extend(o_t);
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
                                TokenD::Regex(i, _, _, _) => {
                                    if i.to_string() == terminal {
                                        Some(i)
                                    } else {
                                        None
                                    }
                                }
                                TokenD::Exact(i, _, _, _) => {
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
                TokenD::Exact(a, _, t, _) | TokenD::Regex(a, _, t, _) => {
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
            use nano_parser_gen::from_lexed::FromLexed;
            pub type Token = nano_parser_gen::lexer::Token<TokenType>;
            #[derive(Debug, Clone, PartialEq)]
            pub enum TokenType { #quote EOF }

            impl nano_parser_gen::parser::NoData<TokenTypeNoData> for TokenType {
                fn no_data(&self) -> TokenTypeNoData {
                match self {
                    #quote3
                    Self::EOF => TokenTypeNoData::EOF,
                }
            }
            }

            impl nano_parser_gen::parser::GetEOF<TokenTypeNoData> for TokenType {
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
                TokenD::Exact(a, _, t, e) | TokenD::Regex(a, _, t, e) => {
                    let fident = syn::Ident::new(&format!("make_{}", a.to_string()), a.span());

                    if let Some(t) = t {
                        if let Some(e) = e {
                            quote! {
                                #[allow(non_snake_case)]
                                fn #fident (__s__: &str) -> Self {
                                    Self:: #a ( #e )
                                }
                            }
                        } else {
                            quote! {
                                #[allow(non_snake_case)]
                                fn #fident (__s__: &str) -> Self {
                                    Self:: #a ( #t ::from_lexed(__s__))
                                }
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
                TokenD::Regex(a, r, _, _) => {
                    let fident = syn::Ident::new(&format!("make_{}", a.to_string()), a.span());
                    lexer_quote =
                        quote! { #lexer_quote .token(#r, Box::new(TokenType:: #fident), false)};
                }
                TokenD::Exact(a, v, _, _) => {
                    let fident = syn::Ident::new(&format!("make_{}", a.to_string()), a.span());
                    lexer_quote =
                        quote! { #lexer_quote .token(#v, Box::new(TokenType:: #fident), true)};
                }
            }
        }
        for t in &self.skip {
            lexer_quote = quote! { #lexer_quote .skip(#t) };
        }

        // if no parserdata: make default one:
        if self.parser_data.is_none() {
            quote = quote! { #quote
                #[derive(Clone)]
                pub struct DefaultParserType {}
                impl DefaultParserType {
                    pub fn new() -> Self { Self {} }
                }
            };
        }

        // pub type Lexer = nano_parser_gen::lexer::Lexer<fn(&str) -> TokenType, TokenType>;
        quote = quote! { #quote
            pub type Lexer = nano_parser_gen::lexer::Lexer<TokenType>;
            pub fn lexer() -> Lexer {
                #lexer_quote .eof(Box::new(TokenType::make_t_eof))
                    .build().expect("Expected to be able to build lexer")
            }
        };

        let default_parser_data_type: proc_macro::TokenStream = quote!(DefaultParserType).into();
        let parser_data_type = self
            .parser_data
            .clone()
            .unwrap_or(syn::parse::<Type>(default_parser_data_type).unwrap());
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

        impl nano_parser_gen::parser::NoData<AstNodeNoData> for AstNode {
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

        pub type Symbol = nano_parser_gen::parser::RuleSymbol<TokenTypeNoData, AstNodeNoData>;
        pub type AstParam = nano_parser_gen::parser::AstParam<Token, AstNode>;
        pub type AstFunc = nano_parser_gen::parser::AstFunc<Token, AstNode, #parser_data_type>;
        pub type ParseTableEntry = nano_parser_gen::parser::ParseTableEntry<Token, TokenTypeNoData, AstNode, AstNodeNoData, #parser_data_type>;

        pub fn parse<'r>(tokens: nano_parser_gen::lexer::Tokens<'r, TokenType>) -> #start_type {
            let Some(AstParam::Ast(AstNode:: #start_item (s))) = nano_parser_gen::parser::parse(PARSE_TABLE, AstNodeNoData:: #start_item, tokens, #parser_data_type ::new()) else { panic!("Parser error"); };
            s
        }

        pub fn parse_source(source: nano_parser_gen::lexer::SourceFile) -> #start_type {
            parse(lexer().tokens(&source))
        }

        };

        for r in &self.rules {
            let r = r.ast_func(&parser_data_type);
            quote = quote!( #quote #r );
        }

        let parse_table_entries = self.fill_ll1_production_table();
        quote = quote!( #quote pub const PARSE_TABLE: &[ParseTableEntry] = #parse_table_entries; );

        quote.into()
    }
}
