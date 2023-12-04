extern crate proc_macro;

mod ast;
mod grammar_data;
mod preprocess_rust_code;

use ast::{GrammarContentDecls, Rules};
use grammar_data::GrammarData;
use proc_macro::{TokenStream, TokenTree};
use syn::parse_macro_input;

#[proc_macro]
pub fn grammar(input: TokenStream) -> TokenStream {
    let mut seen_pc = false;
    let mut pos = None;
    for (i, token) in input.clone().into_iter().enumerate() {
        match token {
            TokenTree::Punct(p) => {
                let c = p.as_char();
                if c == '%' {
                    if seen_pc {
                        pos = Some(i);
                        break;
                    } else {
                        seen_pc = true;
                    }
                } else {
                    seen_pc = false;
                }
            }
            _ => {
                seen_pc = false;
            }
        }
    }
    let (before, after) = if let Some(pos) = pos {
        (
            input
                .clone()
                .into_iter()
                .take(pos - 1)
                .collect::<TokenStream>(),
            input.into_iter().skip(pos + 1).collect::<TokenStream>(),
        )
    } else {
        panic!("Couldn't find '%%' delimiter in grammar");
    };

    let decls = parse_macro_input!(before as GrammarContentDecls);
    let rules = parse_macro_input!(after as Rules);

    let gd = match GrammarData::new(decls, rules) {
        Ok(a) => a,
        Err(e) => return TokenStream::from(e.to_compile_error()),
    };
    gd.tokens().into()
}
