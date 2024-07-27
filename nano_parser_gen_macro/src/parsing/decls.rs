use quote::ToTokens;
use syn::{parse::Parse, parse2, Expr, Ident, LitStr, Token, Type};

use crate::preprocess_rust_code::preprocess_for_token;

use super::template::TemplateDecl;

#[derive(Clone)]
pub struct TypeDef {
    _lbracket: Token![<],
    pub ty: Type,
    _rbracket: Token![>],
}

impl ToTokens for TypeDef {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        self._lbracket.to_tokens(tokens);
        self.ty.to_tokens(tokens);
        self._rbracket.to_tokens(tokens);
    }
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

impl ToTokens for TokenDeclRegex {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        self.name.to_tokens(tokens);
        self.eq_token.to_tokens(tokens);
        self.regex.to_tokens(tokens);
        match &self.read {
            Some((t, e)) => {
                t.to_tokens(tokens);
                e.to_tokens(tokens);
            }
            _ => {}
        }
    }
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

impl ToTokens for TokenDecl {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self {
            Self::Exact(td, ls) => {
                if let Some(td) = td {
                    td.to_tokens(tokens);
                }
                ls.to_tokens(tokens);
            }
            Self::Regex(td, tdr) => {
                if let Some(td) = td {
                    td.to_tokens(tokens);
                }
                tdr.to_tokens(tokens);
            }
        }
    }
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

impl ToTokens for GrammarContentDecls {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        for d in &self.decls {
            d.to_tokens(tokens);
        }
    }
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

impl ToTokens for Decl {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        self._pc.to_tokens(tokens);
        self._decl_ty.to_tokens(tokens);
        self.decl.to_tokens(tokens);
    }
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
        } else if ds == "template" {
            DeclType::Template(input.parse()?)
        } else {
            return Err(syn::Error::new_spanned(decl_ty, "unknown decl type"));
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
    Template(TemplateDecl),
}

impl ToTokens for DeclType {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self {
            Self::Token(d) => d.to_tokens(tokens),
            Self::Skip(s) => s.to_tokens(tokens),
            Self::ParserData(t) => t.to_tokens(tokens),
            Self::Type(td, v) => {
                td.to_tokens(tokens);
                for i in v {
                    i.to_tokens(tokens);
                }
            }
            Self::Start(i) => i.to_tokens(tokens),
            Self::Template(t) => t.to_tokens(tokens),
        }
    }
}
