use syn::{Ident, LitStr, Type};

use crate::{
    parsing::{
        decls::{Decl, DeclType, GrammarContentDecls, TokenDecl, TokenDeclRegex, TypeDef},
        template::TemplateDecl,
    },
    preprocess_rust_code::convert_litteral,
};

use super::template::Template;
use super::TokenD;

fn add_token(decl: &Decl, t: &TokenDecl, tokens: &mut Vec<TokenD>) -> syn::Result<()> {
    match t {
        TokenDecl::Exact(ref t, lit) => {
            let id_txt = convert_litteral(&lit.value());
            if let Some(_) = tokens
                .iter()
                .find(|t| t.get_term_name().to_string() == id_txt)
            {
                return Err(syn::Error::new_spanned(decl, "token was already declared"));
            }
            tokens.push(TokenD::Exact(
                Ident::new(&id_txt, lit.span()),
                lit.clone(),
                t.clone().map(|d| d.ty),
                None,
            ));
        }
        TokenDecl::Regex(
            ref t,
            TokenDeclRegex {
                name, regex, read, ..
            },
        ) => {
            if let Some(_) = tokens
                .iter()
                .find(|t| t.get_term_name().to_string() == name.to_string())
            {
                return Err(syn::Error::new_spanned(decl, "token was already declared"));
            }
            if let Some((_, e)) = read {
                tokens.push(TokenD::Regex(
                    name.clone(),
                    regex.clone(),
                    t.clone().map(|d| d.ty),
                    Some(e.clone()),
                ));
            } else {
                tokens.push(TokenD::Regex(
                    name.clone(),
                    regex.clone(),
                    t.clone().map(|d| d.ty),
                    None,
                ));
            }
        }
    }
    Ok(())
}

fn add_skip(_decl: &Decl, s: &LitStr, skips: &mut Vec<LitStr>) -> syn::Result<()> {
    skips.push(s.clone());
    Ok(())
}

fn add_parser_data(decl: &Decl, p: &Type, parser_data: &mut Option<Type>) -> syn::Result<()> {
    if let Some(_) = parser_data {
        Err(syn::Error::new_spanned(decl, "parser data already defined"))
    } else {
        *parser_data = Some(p.clone());
        Ok(())
    }
}

fn add_start(decl: &Decl, i: &Ident, start: &mut Option<Ident>) -> syn::Result<()> {
    if let Some(_) = start {
        Err(syn::Error::new_spanned(
            decl,
            "start symbol already defined",
        ))
    } else {
        *start = Some(i.clone());
        Ok(())
    }
}

fn add_type(
    _decl: &Decl,
    t: &TypeDef,
    i: &[Ident],
    types: &mut Vec<(Ident, Type)>,
) -> syn::Result<()> {
    for id in i {
        if let Some(_) = types
            .iter()
            .find(|(nt, _)| nt.to_string() == id.to_string())
        {
            return Err(syn::Error::new_spanned(
                id,
                "non terminal type already defined",
            ));
        }
        types.push((id.clone(), t.ty.clone()));
    }
    Ok(())
}

fn add_template(decl: &Decl, t: &TemplateDecl, templates: &mut Vec<Template>) -> syn::Result<()> {
    let template = Template::new(t);
    if let Some(_) = templates
        .iter()
        .find(|tem| tem.name.to_string() == template.name.to_string())
    {
        return Err(syn::Error::new_spanned(decl, "template already defined"));
    }
    templates.push(template);
    Ok(())
}

pub fn process(
    before: GrammarContentDecls,
) -> syn::Result<(
    Vec<TokenD>,
    Vec<LitStr>,
    Option<Type>,
    Vec<(Ident, Type)>,
    Ident,
    Vec<Template>,
)> {
    let mut tokens: Vec<TokenD> = Vec::new();
    let mut skip = Vec::new();
    let mut parser_data = None;
    let mut types = Vec::new();
    let mut start = None;
    let mut templates = super::template::builtin_templates();
    for decl in &before.decls {
        match &decl.decl {
            DeclType::Token(t) => add_token(decl, t, &mut tokens)?,
            DeclType::Skip(s) => add_skip(decl, s, &mut skip)?,
            DeclType::ParserData(p) => add_parser_data(decl, p, &mut parser_data)?,
            DeclType::Start(s) => add_start(decl, s, &mut start)?,
            DeclType::Type(t, i) => add_type(decl, t, i, &mut types)?,
            DeclType::Template(t) => add_template(decl, t, &mut templates)?,
        }
    }
    // additional checks
    let start = match start {
        Some(s) => s,
        None => {
            return Err(syn::Error::new_spanned(
                before,
                "Expected start symbol (%start [symbol])",
            ));
        }
    };
    Ok((tokens, skip, parser_data, types, start, templates))
}
