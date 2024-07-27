use super::*;

use proc_macro2::{Ident, TokenStream};
use syn::Type;

use crate::preprocess_rust_code::convert_litteral;
use crate::{
    grammar::TokenD,
    parsing::{
        rules::RuleEntity,
        template::{TemplateDecl, TemplateInstArg},
    },
};

#[derive(Clone)]
pub enum TemplateArg {
    Normal(TemplateInstArg),
    TArg(usize, TemplateParamType),
}

impl std::fmt::Debug for TemplateArg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Normal(tia) => write!(f, "{tia}"),
            Self::TArg(ta, _) => write!(f, "{ta}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub enum TemplateParamType {
    Sym,
    Tok,
    Nt,
    Ty,
}

#[derive(Debug, Clone)]
pub enum TemplateRuleItem {
    Normal(RuleEntity),
    TArg(usize, TemplateParamType),
    TSelf,
    TDep(usize),
}

pub type TemplateRule = Vec<TemplateRuleItem>;

#[derive(Debug, Clone)]
pub struct TemplateDepend {
    pub name: Ident,
    pub args: Vec<TemplateArg>,
}

#[derive(Debug, Clone)]
pub struct Template {
    pub out_type: TokenStream,
    pub name: Ident,
    pub param_count: usize,
    pub rules: Vec<TemplateRule>,
    pub depends: Vec<TemplateDepend>,
}

impl Template {
    pub fn new(decl: &TemplateDecl) -> Self {
        let value = decl.clone();
        let ty = value.ty;
        let out_type = quote::quote!( #ty );
        let params = value
            .params
            .iter()
            .enumerate()
            .map(|(i, p)| {
                (
                    i,
                    p.ident.clone(),
                    match &p.ty.to_string()[..] {
                        "ty" => TemplateParamType::Ty,
                        "tok" => TemplateParamType::Tok,
                        "sym" => TemplateParamType::Sym,
                        "nt" => TemplateParamType::Nt,
                        _ => panic!(
                            "Unexpected template param type, expected 'ty', 'tok', 'sym' or 'nt'"
                        ),
                    },
                )
            })
            .collect::<Vec<_>>();
        let out_type = crate::preprocess_rust_code::type_placeholders(out_type, &params);
        let mut depends = Vec::new();
        Template {
            out_type,
            name: value.name,
            param_count: value.params.len(),
            rules: value
                .subrules
                .into_iter()
                .map(|sr| {
                    sr.entities
                        .into_iter()
                        .map(|i| match i {
                            RuleEntity::Ident(i) => {
                                if let Some((id, _, t)) = params
                                    .iter()
                                    .find(|(_, b, _)| b.to_string() == i.to_string())
                                {
                                    TemplateRuleItem::TArg(*id, *t)
                                } else {
                                    TemplateRuleItem::Normal(RuleEntity::Ident(i))
                                }
                            }
                            RuleEntity::TemplateInst(ti) => {
                                let mut dep = TemplateDepend {
                                    name: ti.name.clone(),
                                    args: Vec::new(),
                                };
                                for a in ti.args {
                                    match a {
                                        TemplateInstArg::Ident(i) => {
                                            if let Some((id, _, t)) = params
                                                .iter()
                                                .find(|(_, b, _)| b.to_string() == i.to_string())
                                            {
                                                dep.args.push(TemplateArg::TArg(*id, *t));
                                            } else {
                                                dep.args.push(TemplateArg::Normal(
                                                    TemplateInstArg::Ident(i),
                                                ));
                                            }
                                        }
                                        o => dep.args.push(TemplateArg::Normal(o)),
                                    }
                                }
                                depends.push(dep);
                                TemplateRuleItem::TDep(depends.len() - 1)
                            }
                            re => TemplateRuleItem::Normal(re),
                        })
                        .collect()
                })
                .collect(),
            depends,
        }
    }

    pub fn instanciate<'a>(
        &'a self,
        args: Vec<TemplateInstArg>,
        templates: &'a [Template],
        nts: &[(Ident, Type)],
        tokens: &[TokenD],
    ) -> (TemplateInstance, Vec<TemplateInstance<'a>>) {
        assert!(args.len() == self.param_count);
        //TODO: check here that non-templateinstance nts exist.
        let mut name = String::from("__");
        name.push_str(&self.name.to_string());
        for a in &args {
            name.push('_');
            let arg = a.to_string();
            let arg = convert_litteral(&arg);
            name.push_str(&arg);
        }
        let name = Ident::new(&name, self.name.span());
        let mut depends_names = Vec::new();
        let mut depends_instances = Vec::new();
        for d in &self.depends {
            if let Some(t) = templates
                .iter()
                .find(|tm| tm.name.to_string() == d.name.to_string())
            {
                let (inst, o) = t.instanciate(
                    d.args
                        .iter()
                        .map(|a| match a {
                            TemplateArg::Normal(ta) => ta.clone(),
                            TemplateArg::TArg(i, _) => args[*i].clone(),
                        })
                        .collect(),
                    templates,
                    nts,
                    tokens,
                );
                depends_names.push(inst.name.clone());
                depends_instances.push(inst);
                depends_instances.extend(o);
            } else {
                panic!("NO TEMPLATE NAMED {}", d.name.to_string());
            }
        }

        (
            TemplateInstance {
                ty: crate::preprocess_rust_code::preprocess_for_type(
                    self.out_type.clone(),
                    &args,
                    nts,
                    tokens,
                ),
                name,
                args,
                template: self,
                depends_names,
            },
            depends_instances,
        )
    }
}
