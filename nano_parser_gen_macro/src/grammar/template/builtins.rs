use proc_macro2::{Ident, Span};

use crate::parsing::rules::RuleEntity;

use super::{Template, TemplateArg, TemplateDepend, TemplateParamType, TemplateRuleItem};

macro_rules! ident {
    ($name:expr) => {
        Ident::new($name, Span::call_site())
    };
}

macro_rules! epsilon {
    () => {
        TemplateRuleItem::Normal(RuleEntity::Epsilon(ident!("EPSILON")))
    };
}

macro_rules! ast {
    ($e:tt) => {
        TemplateRuleItem::Normal(RuleEntity::AstFunc(
            syn::parse::<syn::Expr>(quote::quote! { $e }.into()).unwrap(),
        ))
    };
}

macro_rules! targ {
    ($n:expr, $t:ident) => {
        TemplateRuleItem::TArg($n, TemplateParamType::$t)
    };
}

macro_rules! tdep {
    ($n:expr) => {
        TemplateRuleItem::TDep($n)
    };
}

macro_rules! tself {
    () => {
        TemplateRuleItem::TSelf
    };
}

macro_rules! dtarg {
    ($n:expr, $t:ident) => {
        TemplateArg::TArg($n, TemplateParamType::$t)
    };
}

macro_rules! _dn {
    ($n:expr) => {
        TemplateArg::Normal($n)
    };
}

pub fn builtin_templates() -> Vec<Template> {
    return vec![
        Template {
            out_type: quote::quote! { Option < $ sym 0usize > },
            name: ident!("Option"),
            param_count: 1,
            rules: vec![
                vec![targ!(0, Sym), ast!({ __return__ = Some(__mapping_0__) })],
                vec![epsilon!(), ast!({ __return__ = None })],
            ],
            depends: vec![],
        },
        Template {
            out_type: quote::quote! { bool },
            name: ident!("OptionB"),
            param_count: 1,
            rules: vec![
                vec![targ!(0, Sym), ast!({ __return__ = true })],
                vec![epsilon!(), ast!({ __return__ = false })],
            ],
            depends: vec![],
        },
        Template {
            out_type: quote::quote! { Vec < $ sym 0usize > },
            name: ident!("List"),
            param_count: 1,
            rules: vec![
                vec![
                    targ!(0, Sym),
                    tself!(),
                    ast!({
                        __mapping_1__.insert(0, __mapping_0__);
                        __return__ = __mapping_1__
                    }),
                ],
                vec![epsilon!(), ast!({ __return__ = Vec::new() })],
            ],
            depends: vec![],
        },
        Template {
            out_type: quote::quote! { Vec < $ sym 0usize > },
            name: ident!("List1"),
            param_count: 1,
            rules: vec![
                vec![
                    targ!(0, Sym),
                    tself!(),
                    ast!({
                        __mapping_1__.insert(0, __mapping_0__);
                        __return__ = __mapping_1__
                    }),
                ],
                vec![targ!(0, Sym), ast!({ __return__ = vec![__mapping_0__] })],
            ],
            depends: vec![],
        },
        Template {
            out_type: quote::quote! { Vec < $ sym 0usize > },
            name: ident!("SepList"),
            param_count: 2,
            rules: vec![
                vec![
                    targ!(0, Sym),
                    tdep!(0),
                    ast!({
                        __mapping_1__.insert(0, __mapping_0__);
                        __return__ = __mapping_1__
                    }),
                ],
                vec![epsilon!(), ast!({ __return__ = Vec::new() })],
            ],
            depends: vec![TemplateDepend {
                name: ident!("_SepList_sub"),
                args: vec![dtarg!(0, Sym), dtarg!(1, Sym)],
            }],
        },
        Template {
            out_type: quote::quote! { Vec < $ sym 0usize > },
            name: ident!("SepList1"),
            param_count: 2,
            rules: vec![
                vec![
                    targ!(0, Sym),
                    tdep!(0),
                    ast!({
                        __mapping_1__.insert(0, __mapping_0__);
                        __return__ = __mapping_1__
                    }),
                ],
                vec![targ!(0, Sym), ast!({ __return__ = vec![__mapping_0__] })],
            ],
            depends: vec![TemplateDepend {
                name: ident!("_SepList_sub"),
                args: vec![dtarg!(0, Sym), dtarg!(1, Sym)],
            }],
        },
        Template {
            out_type: quote::quote! { Vec < $ sym 0usize > },
            name: ident!("_SepList_sub"),
            param_count: 2,
            rules: vec![
                vec![
                    targ!(1, Sym),
                    targ!(0, Sym),
                    tself!(),
                    ast!({
                        __mapping_2__.insert(0, __mapping_1__);
                        __return__ = __mapping_2__
                    }),
                ],
                vec![epsilon!(), ast!({ __return__ = Vec::new() })],
            ],
            depends: vec![],
        },
        Template {
            out_type: quote::quote! { Vec < $ sym 0usize > },
            name: ident!("TermList"),
            param_count: 2,
            rules: vec![
                vec![
                    targ!(0, Sym),
                    targ!(1, Sym),
                    tself!(),
                    ast!({
                        __mapping_2__.insert(0, __mapping_0__);
                        __return__ = __mapping_2__
                    }),
                ],
                vec![epsilon!(), ast!({ __return__ = Vec::new() })],
            ],
            depends: vec![],
        },
        Template {
            out_type: quote::quote! { Vec < $ sym 0usize > },
            name: ident!("TermList1"),
            param_count: 1,
            rules: vec![
                vec![
                    targ!(0, Sym),
                    targ!(1, Sym),
                    tself!(),
                    ast!({
                        __mapping_2__.insert(0, __mapping_0__);
                        __return__ = __mapping_2__
                    }),
                ],
                vec![
                    targ!(0, Sym),
                    targ!(1, Sym),
                    ast!({ __return__ = vec![__mapping_0__] }),
                ],
            ],
            depends: vec![],
        },
    ];
}
