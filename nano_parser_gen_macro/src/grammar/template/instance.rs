use super::*;

use proc_macro2::Ident;
use syn::{Error, Type};

use crate::{
    grammar::{AstFuncD, RuleD, RuleSymbolD, TokenD},
    parsing::{rules::RuleEntity, template::TemplateInstArg},
    preprocess_rust_code::convert_litteral,
};

pub struct TemplateInstance<'a> {
    pub template: &'a Template,
    pub name: Ident,
    pub args: Vec<TemplateInstArg>,
    pub depends_names: Vec<Ident>,
    pub ty: Type,
}

impl<'a> Clone for TemplateInstance<'a> {
    fn clone(&self) -> Self {
        Self {
            template: self.template,
            name: self.name.clone(),
            args: self.args.clone(),
            depends_names: self.depends_names.clone(),
            ty: self.ty.clone(),
        }
    }
}

impl<'a> std::fmt::Debug for TemplateInstance<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let ty = &self.ty;
        f.debug_struct("TemplateInstance")
            .field("template", self.template)
            .field("name", &self.name)
            .field("args", &self.args)
            .field("depends_names", &self.depends_names)
            .field("type", &format!("{}", quote::quote!(#ty)))
            .finish()
    }
}

impl<'a> TemplateInstance<'a> {
    pub fn rules(&self, terms: &[String], tokens: &[TokenD]) -> syn::Result<Vec<RuleD>> {
        let mut rules = Vec::new();
        for (i, r) in self.template.rules.iter().enumerate() {
            let mut symbols = Vec::new();
            let mut ast_funcs = Vec::new();
            let mut after = 0;
            let mut ast_func_id = 0;
            let items = r
                .iter()
                .map(|i| match i {
                    TemplateRuleItem::TSelf => RuleEntity::Ident(self.name.clone()),
                    TemplateRuleItem::Normal(n) => n.clone(),
                    TemplateRuleItem::TArg(i, _) => self.args.get(*i).unwrap().into(),
                    TemplateRuleItem::TDep(i) => {
                        RuleEntity::Ident(self.depends_names.get(*i).unwrap().clone())
                    }
                })
                .collect::<Vec<_>>();
            for e in items {
                match e {
                    RuleEntity::TemplateInst(_) => {
                        // instanciate template
                    }
                    RuleEntity::Epsilon(_) => {
                        symbols.push(RuleSymbolD::Epsilon);
                    }
                    RuleEntity::Exact(lit) => {
                        let lit_v = convert_litteral(&lit.value());
                        if !terms.contains(&lit_v) {
                            return Err(Error::new_spanned(lit, "Unknown token"));
                        }
                        let ty = tokens.iter().find_map(|t| match t {
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
                        if terms.contains(&id_v) {
                            let ty = tokens.iter().find_map(|t| match t {
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
                                &format!("{}_{}_{}", self.name, i, ast_func_id),
                                self.template.name.span(),
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
                        self.template.name.clone(),
                        "This template has no final ast production rule",
                    ));
                }
                a.last = true;
            } else {
                return Err(Error::new_spanned(
                    self.template.name.clone(),
                    "This template has no ast production rule",
                ));
            }
            rules.push(RuleD {
                left: self.name.clone(),
                symbols,
                ast_funcs,
            });
        }
        Ok(rules)
    }
}
