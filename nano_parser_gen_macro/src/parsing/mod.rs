pub mod decls;
pub mod rules;
pub mod template;

use crate::preprocess_rust_code::split_macro_input;

pub fn parse(
    input: proc_macro::TokenStream,
) -> syn::Result<(decls::GrammarContentDecls, rules::Rules)> {
    let (before, after) = split_macro_input(input);

    let decls = syn::parse::<decls::GrammarContentDecls>(before)?;
    let rules = syn::parse::<rules::Rules>(after)?;

    Ok((decls, rules))
}
