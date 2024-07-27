use proc_macro2::{Span, TokenStream};
use quote::quote;
use syn::{Expr, Ident, Type};

use crate::{
    parsing::rules::{Rule, RuleEntity, Rules, SubRule},
    preprocess_rust_code::convert_litteral,
};

use super::{
    template::{Template, TemplateInstance},
    TokenD,
};

pub struct AstFuncD {
    pub func: Expr,
    pub after: usize,
    pub last: bool,
    pub name: Ident,
}

impl std::fmt::Debug for AstFuncD {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let e = &self.func;
        let n = &self.name;
        write!(
            f,
            "AstFuncD(func: {}, after: {}, last: {}, name: {})",
            quote!(#e),
            self.after,
            self.last,
            quote!(#n)
        )?;
        Ok(())
    }
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

impl std::fmt::Debug for RuleSymbolD {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::T(i, _) => write!(f, "T({})", quote!(#i)),
            Self::NT(i) => write!(f, "NT({})", quote!(#i)),
            Self::Epsilon => write!(f, "EPSILON"),
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

#[derive(Debug)]
pub struct RuleD {
    pub left: Ident,
    pub symbols: Vec<RuleSymbolD>,
    pub ast_funcs: Vec<AstFuncD>,
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
            q = if ast.last {
                quote! { #q
                #[allow(non_snake_case)]
                fn #name (#[allow(non_snake_case)] #params_id: &[AstParam], #[allow(non_snake_case)] #parser_data_id: &mut #parser_type) -> AstParam {
                    #ast_q
                    let mut #return_id;
                    #e
                    AstParam::Ast(AstNode:: #output_type(#return_id))
                }
                }
            } else {
                quote! { #q
                #[allow(non_snake_case)]
                fn #name (#[allow(non_snake_case)] #params_id: &[AstParam], #[allow(non_snake_case)] #parser_data_id: &mut #parser_type) {
                    #ast_q
                    #e
                }
                }
            };
        }
        q
    }
}

pub struct RuleProcessingData<'a> {
    pub tokens: &'a [TokenD],
    pub terms: &'a [String],
    pub types: &'a [(Ident, Type)],
    pub templates: Vec<Template>,
}

fn check_add_instance<'a>(
    instance: &TemplateInstance<'a>,
    template_instances: &mut Vec<TemplateInstance<'a>>,
    rpd: &RuleProcessingData,
) -> (Vec<RuleD>, Vec<(Ident, Type)>) {
    if let Some(_) = template_instances
        .iter()
        .find(|t| t.name.to_string() == instance.name.to_string())
    {
        return (vec![], vec![]);
    }
    template_instances.push(instance.clone());
    let rules = instance.rules(rpd.terms, rpd.tokens).unwrap();
    let types = vec![(instance.name.clone(), instance.ty.clone())];
    (rules, types)
}

fn process_rule<'a>(
    i: usize,
    r: &Rule,
    sr: &SubRule,
    rpd: &'a RuleProcessingData,
    template_instances: &mut Vec<TemplateInstance<'a>>,
) -> syn::Result<(Vec<RuleD>, Vec<(Ident, Type)>)> {
    let mut symbols = Vec::new();
    let mut ast_funcs = Vec::new();
    let mut after = 0;
    let mut ast_func_id = 0;
    let mut out_rules = vec![];
    let mut out_types = vec![];
    for e in &sr.entities {
        match e {
            RuleEntity::TemplateInst(t) => {
                // find template
                let Some(template) = rpd.templates.iter().find(|temp| temp.name.to_string() == t.name.to_string()) else {
                    return Err(syn::Error::new_spanned(t, "Unknown template"));
                };
                let (instance, other) = template.instanciate(
                    t.args.iter().map(|i| i.clone()).collect(),
                    &rpd.templates,
                    rpd.types,
                    rpd.tokens,
                );
                let (o_r, o_t) = check_add_instance(&instance, template_instances, rpd);
                out_rules.extend(o_r);
                out_types.extend(o_t);
                for o in other {
                    let (o_r, o_t) = check_add_instance(&o, template_instances, rpd);
                    out_rules.extend(o_r);
                    out_types.extend(o_t);
                }
                symbols.push(RuleSymbolD::NT(instance.name.clone()));
                after += 1;
            }
            RuleEntity::Epsilon(_) => {
                symbols.push(RuleSymbolD::Epsilon);
            }
            RuleEntity::Exact(lit) => {
                let lit_v = convert_litteral(&lit.value());
                if !rpd.terms.contains(&lit_v) {
                    return Err(syn::Error::new_spanned(lit, "Unknown token"));
                }
                let ty = rpd.tokens.iter().find_map(|t| match t {
                    TokenD::Exact(a, _, t, _) => {
                        if a.to_string() == lit_v {
                            t.clone()
                        } else {
                            None
                        }
                    }
                    TokenD::Regex(a, _, t, _) => {
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
                if rpd.terms.contains(&id_v) {
                    let ty = rpd.tokens.iter().find_map(|t| match t {
                        TokenD::Exact(a, _, t, _) => {
                            if a.to_string() == id_v {
                                t.clone()
                            } else {
                                None
                            }
                        }
                        TokenD::Regex(a, _, t, _) => {
                            if a.to_string() == id_v {
                                t.clone()
                            } else {
                                None
                            }
                        }
                    });
                    symbols.push(RuleSymbolD::T(id.clone(), ty));
                } else {
                    symbols.push(RuleSymbolD::NT(id.clone()));
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
            return Err(syn::Error::new_spanned(
                r.left.clone(),
                "This rule has no final ast production rule",
            ));
        }
        a.last = true;
    } else {
        return Err(syn::Error::new_spanned(
            r.left.clone(),
            "This rule has no ast production rule",
        ));
    }
    out_rules.push(RuleD {
        left: r.left.clone(),
        symbols,
        ast_funcs,
    });
    Ok((out_rules, out_types))
}

pub fn process(
    rules: &[Rule],
    rpd: &RuleProcessingData,
) -> syn::Result<(Vec<RuleD>, Vec<(Ident, Type)>)> {
    let mut rs = Vec::new();
    let mut ts = Vec::new();
    let mut template_instances = Vec::new();
    for r in rules {
        for (i, sr) in r.subrules.iter().enumerate() {
            let (o_r, o_t) = process_rule(i, r, sr, rpd, &mut template_instances)?;
            rs.extend(o_r);
            ts.extend(o_t);
        }
    }
    Ok((rs, ts))
}

pub fn complete_nterms(
    types: Vec<(Ident, Type)>,
    after: &Rules,
) -> (Vec<String>, Vec<(Ident, Type)>) {
    let mut types = types;
    let mut nterms: Vec<String> = types.iter().map(|(i, _)| i.to_string()).collect();
    for r in &after.rules {
        if let None = nterms.iter().find(|s| s == &&r.left.to_string()) {
            nterms.push(r.left.to_string());
            types.push((
                r.left.clone(),
                syn::parse::<Type>(quote! {()}.into()).unwrap(),
            ));
        }
    }
    (nterms, types)
}
