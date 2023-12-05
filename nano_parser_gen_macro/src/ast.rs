use quote::ToTokens;
use syn::{parse::Parse, parse2, token, Error, Expr, Ident, LitStr, Token, Type};

use crate::preprocess_rust_code::{combine_spans, preprocess, preprocess_for_token};

#[derive(Clone)]
pub struct TypeDef {
    _lbracket: Token![<],
    pub ty: Type,
    _rbracket: Token![>],
}

impl Parse for TypeDef {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        Ok(Self {
            _lbracket: input.parse()?,
            ty: input.parse()?,
            _rbracket: input.parse()?,
        })
    }
}

pub struct TokenDeclRegex {
    pub name: Ident,
    pub eq_token: Token![=],
    pub regex: LitStr,
    pub read: Option<(Token![=>], Expr)>,
}

impl Parse for TokenDeclRegex {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let name = input.parse()?;
        let eq_token = input.parse()?;
        let regex = input.parse()?;
        let lookahead = input.lookahead1();
        let read = if lookahead.peek(Token![=>]) {
            let arr = input.parse()?;
            let group: proc_macro2::Group = input.parse()?;
            let preprocessed_tokens = preprocess_for_token(
                [proc_macro2::TokenTree::Group(group.into())]
                    .into_iter()
                    .collect::<proc_macro2::TokenStream>()
                    .into(),
            );
            let parsed = parse2(preprocessed_tokens.into())?;
            Some((arr, parsed))
        } else {
            None
        };
        Ok(Self {
            name,
            eq_token,
            regex,
            read,
        })
    }
}

pub enum TokenDecl {
    Exact(Option<TypeDef>, LitStr),
    Regex(Option<TypeDef>, TokenDeclRegex),
}

impl Parse for TokenDecl {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let lookahead = input.lookahead1();
        let ty = if lookahead.peek(Token![<]) {
            Some(input.parse()?)
        } else {
            None
        };

        let lookahead = input.lookahead1();
        Ok(if lookahead.peek(Ident) {
            Self::Regex(ty, input.parse()?)
        } else {
            Self::Exact(ty, input.parse()?)
        })
    }
}

pub struct GrammarContentDecls {
    pub decls: Vec<Decl>,
}

impl Parse for GrammarContentDecls {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut decls = Vec::new();
        while !input.is_empty() {
            decls.push(input.parse()?);
        }
        Ok(Self { decls })
    }
}

pub enum RuleEntity {
    Ident(Ident),
    Exact(LitStr),
    AstFunc(Expr),
    Epsilon(Ident),
}

impl Parse for RuleEntity {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let lookahead = input.lookahead1();
        Ok(if lookahead.peek(Ident) {
            Self::Ident(input.parse()?)
        } else if lookahead.peek(LitStr) {
            Self::Exact(input.parse()?)
        } else if lookahead.peek(token::Brace) {
            let group: proc_macro2::Group = input.parse()?;
            let preprocessed_tokens = preprocess(
                [proc_macro2::TokenTree::Group(group.into())]
                    .into_iter()
                    .collect::<proc_macro2::TokenStream>()
                    .into(),
            );
            let parsed = parse2(preprocessed_tokens.into())?;
            Self::AstFunc(parsed)
        } else if lookahead.peek(Token![<]) {
            let ab1: Token![<] = input.parse()?;
            let none: Ident = input.parse()?;
            assert!(none.to_string() == "none");
            let ab2: Token![>] = input.parse()?;
            let none = Ident::new(
                "none",
                combine_spans(combine_spans(ab1.span, none.span()), ab2.span),
            );
            Self::Epsilon(none)
        } else {
            return Err(lookahead.error());
        })
    }
}

pub enum PipeOrSemi {
    Pipe(Token![|]),
    Semi(Token![;]),
}

pub struct SubRule {
    pub entities: Vec<RuleEntity>,
    pub end: PipeOrSemi,
}

pub struct Rule {
    pub left: Ident,
    pub coloncolon: Token![::],
    pub eq: Token![=],
    pub subrules: Vec<SubRule>,
}

impl Parse for Rule {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let left = input.parse()?;
        let coloncolon = input.parse()?;
        let eq = input.parse()?;
        let mut subrules = Vec::new();
        'main: loop {
            let mut subrule = Vec::new();
            loop {
                let lookahead = input.lookahead1();
                if lookahead.peek(Token![|]) {
                    subrules.push(SubRule {
                        entities: subrule,
                        end: PipeOrSemi::Pipe(input.parse()?),
                    });
                    break;
                } else if lookahead.peek(Token![;]) {
                    subrules.push(SubRule {
                        entities: subrule,
                        end: PipeOrSemi::Semi(input.parse()?),
                    });
                    break 'main;
                } else {
                    subrule.push(input.parse()?);
                }
            }
        }
        Ok(Self {
            left,
            coloncolon,
            eq,
            subrules,
        })
    }
}

pub struct Rules {
    pub rules: Vec<Rule>,
}

impl Parse for Rules {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut rules = Vec::new();
        while !input.is_empty() {
            rules.push(input.parse()?);
        }
        Ok(Self { rules })
    }
}

pub enum IdOrTy {
    Id(Ident),
    Ty(Token![type]),
}

impl Parse for IdOrTy {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let lookahead = input.lookahead1();
        Ok(if lookahead.peek(Token![type]) {
            Self::Ty(input.parse()?)
        } else {
            Self::Id(input.parse()?)
        })
    }
}

impl ToString for IdOrTy {
    fn to_string(&self) -> String {
        match self {
            Self::Id(i) => i.to_string(),
            Self::Ty(_) => "type".to_string(),
        }
    }
}

impl ToTokens for IdOrTy {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self {
            Self::Id(i) => i.to_tokens(tokens),
            Self::Ty(t) => t.to_tokens(tokens),
        }
    }
}

pub struct Decl {
    _pc: Token![%],
    _decl_ty: IdOrTy,
    pub decl: DeclType,
}

impl Parse for Decl {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let pc = input.parse()?;
        let decl_ty: IdOrTy = input.parse()?;
        let ds = decl_ty.to_string();
        let decl = if ds == "token" {
            DeclType::Token(input.parse()?)
        } else if ds == "skip" {
            DeclType::Skip(input.parse()?)
        } else if ds == "parserdata" {
            DeclType::ParserData(input.parse()?)
        } else if ds == "start" {
            DeclType::Start(input.parse()?)
        } else if ds == "type" {
            let ty = input.parse()?;
            let mut ids = Vec::new();
            loop {
                let lookahead = input.lookahead1();
                if lookahead.peek(Token![%]) {
                    break;
                } else {
                    ids.push(input.parse()?);
                }
            }
            DeclType::Type(ty, ids)
        } else {
            return Err(Error::new_spanned(decl_ty, "unknown decl type"));
        };

        Ok(Self {
            _pc: pc,
            _decl_ty: decl_ty,
            decl,
        })
    }
}

pub enum DeclType {
    Token(TokenDecl),
    Skip(LitStr),
    ParserData(Type),
    Type(TypeDef, Vec<Ident>),
    Start(Ident),
}
