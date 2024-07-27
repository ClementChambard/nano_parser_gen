use quote::ToTokens;
use syn::{parse::Parse, parse2, punctuated::Punctuated, token, Expr, Ident, LitStr, Token};

use crate::preprocess_rust_code::{combine_spans, preprocess};

use super::template::TemplateInst;

#[derive(Clone)]
pub enum RuleEntity {
    Ident(Ident),
    Exact(LitStr),
    AstFunc(Expr),
    Epsilon(Ident),
    TemplateInst(TemplateInst),
}

impl ToTokens for RuleEntity {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self {
            Self::Ident(i) => i.to_tokens(tokens),
            Self::Exact(l) => l.to_tokens(tokens),
            Self::AstFunc(e) => e.to_tokens(tokens),
            Self::Epsilon(e) => e.to_tokens(tokens),
            Self::TemplateInst(t) => t.to_tokens(tokens),
        }
    }
}

impl From<&RuleEntity> for String {
    fn from(value: &RuleEntity) -> Self {
        match value {
            RuleEntity::Ident(i) => i.to_string(),
            RuleEntity::Exact(e) => e.value(),
            RuleEntity::AstFunc(_) => panic!(),
            RuleEntity::Epsilon(_) => String::from("<none>"),
            RuleEntity::TemplateInst(_) => panic!(),
        }
    }
}

impl From<RuleEntity> for String {
    fn from(value: RuleEntity) -> Self {
        String::from(&value)
    }
}

impl std::fmt::Debug for RuleEntity {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Ident(i) => write!(f, "Ident({:?})", i),
            Self::Exact(i) => write!(f, "Exact({:?})", i.value()),
            Self::AstFunc(i) => write!(f, "AstFunc({})", quote::quote!(#i)),
            Self::Epsilon(_) => write!(f, "Epsilon"),
            Self::TemplateInst(i) => write!(f, "TemplateInst({})", i),
        }
    }
}

impl Parse for RuleEntity {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(Token![<]) {
            let ab1: Token![<] = input.parse()?;
            let none: Ident = input.parse()?;
            assert!(none.to_string() == "none");
            let ab2: Token![>] = input.parse()?;
            let none = Ident::new(
                "none",
                combine_spans(combine_spans(ab1.span, none.span()), ab2.span),
            );
            Ok(Self::Epsilon(none))
        } else if lookahead.peek(token::Brace) {
            let group: proc_macro2::Group = input.parse()?;
            let preprocessed_tokens = preprocess(
                [proc_macro2::TokenTree::Group(group.into())]
                    .into_iter()
                    .collect::<proc_macro2::TokenStream>()
                    .into(),
            );
            let parsed = parse2(preprocessed_tokens.into())?;
            Ok(Self::AstFunc(parsed))
        } else if lookahead.peek(LitStr) {
            Ok(Self::Exact(input.parse()?))
        } else if lookahead.peek(Ident) {
            let ident: Ident = input.parse()?;
            if !input.peek(Token![<]) {
                return Ok(Self::Ident(ident));
            }
            if {
                let forked = input.fork();
                let _: Token![<] = forked.parse()?;
                let none_ident: syn::Result<Ident> = forked.parse();
                none_ident.is_ok() && none_ident.unwrap().to_string() == "none"
            } {
                return Ok(Self::Ident(ident));
            }
            Ok(Self::TemplateInst(TemplateInst {
                name: ident,
                _ab1: input.parse()?,
                args: input.call(Punctuated::parse_separated_nonempty)?,
                _ab2: input.parse()?,
            }))
        } else {
            Err(lookahead.error())
        }
    }
}

#[derive(Clone)]
pub enum PipeOrSemi {
    Pipe(Token![|]),
    Semi(Token![;]),
}

impl ToTokens for PipeOrSemi {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self {
            Self::Pipe(p) => p.to_tokens(tokens),
            Self::Semi(s) => s.to_tokens(tokens),
        }
    }
}

#[derive(Clone)]
pub struct SubRule {
    pub entities: Vec<RuleEntity>,
    pub end: PipeOrSemi,
}

impl ToTokens for SubRule {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        for e in &self.entities {
            e.to_tokens(tokens);
        }
        self.end.to_tokens(tokens);
    }
}

pub struct Rule {
    pub left: Ident,
    pub coloncolon: Token![::],
    pub eq: Token![=],
    pub subrules: Vec<SubRule>,
}

impl ToTokens for Rule {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        self.left.to_tokens(tokens);
        self.coloncolon.to_tokens(tokens);
        self.eq.to_tokens(tokens);
        for s in &self.subrules {
            s.to_tokens(tokens);
        }
    }
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

impl ToTokens for Rules {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        for r in &self.rules {
            r.to_tokens(tokens);
        }
    }
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
