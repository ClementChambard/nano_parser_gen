extern crate proc_macro;

mod grammar;
mod parsing;
mod preprocess_rust_code;

use grammar::GrammarData;
use parsing::parse;
use proc_macro::TokenStream;

#[proc_macro]
pub fn grammar(input: TokenStream) -> TokenStream {
    let (decls, rules) = match parse(input) {
        Ok(a) => a,
        Err(e) => return e.to_compile_error().into(),
    };

    let gd = match GrammarData::new(decls, rules) {
        Ok(a) => a,
        Err(e) => return e.to_compile_error().into(),
    };

    gd.tokens().into()
}
