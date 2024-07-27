use quote::ToTokens;
use syn::{parse::Parse, punctuated::Punctuated, Ident, LitStr, Token, Type};

use crate::parsing::rules::PipeOrSemi;

use super::rules::{RuleEntity, SubRule};

#[derive(Clone)]
pub enum TemplateInstArg {
    Exact(LitStr),
    Type(Type),
    Ident(Ident),
}

impl From<TemplateInstArg> for RuleEntity {
    fn from(value: TemplateInstArg) -> Self {
        match value {
            TemplateInstArg::Ident(i) => RuleEntity::Ident(i),
            TemplateInstArg::Exact(e) => RuleEntity::Exact(e),
            TemplateInstArg::Type(_) => {
                panic!("Can't translate type")
            }
        }
    }
}

impl From<&TemplateInstArg> for RuleEntity {
    fn from(value: &TemplateInstArg) -> Self {
        match value {
            TemplateInstArg::Ident(i) => RuleEntity::Ident(i.clone()),
            TemplateInstArg::Exact(e) => RuleEntity::Exact(e.clone()),
            TemplateInstArg::Type(_) => {
                panic!("Can't translate type")
            }
        }
    }
}

impl std::fmt::Display for TemplateInstArg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Exact(e) => write!(f, "{}", e.value()),
            Self::Type(e) => write!(f, "{}", quote::quote!(#e)),
            Self::Ident(e) => write!(f, "{}", e.to_string()),
        }
    }
}

impl std::fmt::Debug for TemplateInstArg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Exact(e) => write!(f, "Exact({:?})", e.value()),
            Self::Type(e) => write!(f, "Type({})", quote::quote!(#e)),
            Self::Ident(e) => write!(f, "Ident({:?})", e.to_string()),
        }
    }
}

impl Parse for TemplateInstArg {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        if input.peek(LitStr) {
            Ok(Self::Exact(input.parse()?))
        } else if input.peek(Ident) && (input.peek2(Token![,]) || input.peek2(Token![>])) {
            Ok(Self::Ident(input.parse()?))
        } else {
            Ok(Self::Type(input.parse()?))
        }
    }
}

#[derive(Clone)]
pub struct TemplateInst {
    pub name: Ident,
    pub _ab1: Token![<],
    pub args: Punctuated<TemplateInstArg, Token![,]>,
    pub _ab2: Token![>],
}

impl ToTokens for TemplateInst {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        self.name.to_tokens(tokens);
        self._ab1.to_tokens(tokens);
        // TODO: Punctuated
        self._ab2.to_tokens(tokens);
    }
}

impl std::fmt::Display for TemplateInst {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let n = &self.name;
        write!(f, "{}", quote::quote!( #n < ))?;
        for (i, a) in self.args.iter().enumerate() {
            if i != 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", a)?;
        }
        write!(f, ">")
    }
}

#[derive(Clone)]
pub struct TemplateParamDecl {
    pub ident: Ident,
    _colon: Token![:],
    pub ty: Ident,
}

impl ToTokens for TemplateParamDecl {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        self.ident.to_tokens(tokens);
        self._colon.to_tokens(tokens);
        self.ty.to_tokens(tokens);
    }
}

impl Parse for TemplateParamDecl {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let ident = input.parse()?;
        let colon = input.parse()?;
        let ty: Ident = input.parse()?;
        let ty_str = ty.to_string();
        if ty_str != "nt" && ty_str != "ty" && ty_str != "tok" && ty_str != "sym" {
            return Err(syn::Error::new_spanned(ty, "unknown template param type"));
        }
        Ok(Self {
            ident,
            _colon: colon,
            ty,
        })
    }
}

#[derive(Clone)]
pub struct TemplateDecl {
    _lt: Token![<],
    pub ty: Type,
    _gt: Token![>],
    pub name: Ident,
    _lt2: Token![<],
    pub params: Punctuated<TemplateParamDecl, Token![,]>,
    _gt2: Token![>],
    _coloncolon: Token![::],
    _eq: Token![=],
    pub subrules: Vec<SubRule>,
}

impl ToTokens for TemplateDecl {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        self._lt.to_tokens(tokens);
        self.ty.to_tokens(tokens);
        self._gt.to_tokens(tokens);
        self.name.to_tokens(tokens);
        self.params.to_tokens(tokens);
        self._gt2.to_tokens(tokens);
        self._coloncolon.to_tokens(tokens);
        self._eq.to_tokens(tokens);
        for s in &self.subrules {
            s.to_tokens(tokens);
        }
    }
}

impl Parse for TemplateDecl {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let lt = input.parse()?;
        let ty = input.parse()?;
        let gt = input.parse()?;
        let name = input.parse()?;
        let lt2 = input.parse()?;
        let params = input.call(Punctuated::parse_separated_nonempty)?;
        let gt2 = input.parse()?;

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
            _lt: lt,
            ty,
            _gt: gt,
            name,
            _lt2: lt2,
            params,
            _gt2: gt2,
            _coloncolon: coloncolon,
            _eq: eq,
            subrules,
        })
    }
}
