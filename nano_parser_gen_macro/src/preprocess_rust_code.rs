use proc_macro2::{Group, Ident, Span, TokenStream, TokenTree};
use quote::quote;
use syn::Type;

use crate::grammar::{TemplateParamType, TokenD};
use crate::parsing::template::TemplateInstArg;

pub fn split_macro_input(
    input: proc_macro::TokenStream,
) -> (proc_macro::TokenStream, proc_macro::TokenStream) {
    let mut seen_pc = false;
    let mut pos = None;
    for (i, token) in input.clone().into_iter().enumerate() {
        match token {
            proc_macro::TokenTree::Punct(p) => {
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
    if let Some(pos) = pos {
        (
            input
                .clone()
                .into_iter()
                .take(pos - 1)
                .collect::<proc_macro::TokenStream>(),
            input
                .into_iter()
                .skip(pos + 1)
                .collect::<proc_macro::TokenStream>(),
        )
    } else {
        panic!("Couldn't find '%%' delimiter in grammar");
    }
}

pub fn combine_spans(span1: Span, span2: Span) -> Span {
    // Combine the spans of the two tokens
    span1.join(span2).unwrap_or_else(|| {
        //  In case the spans cannot be joined, return a fallback span
        Span::call_site()
    })
}

pub fn preprocess_for_token(input: TokenStream) -> TokenStream {
    let mut result = TokenStream::new();
    let mut it = input.into_iter();
    while let Some(token) = it.next() {
        match token {
            TokenTree::Punct(p) => match p.as_char() {
                '$' => result.extend(Some(TokenTree::Ident(Ident::new("__s__", p.span())))),
                _ => result.extend(Some(TokenTree::Punct(p))),
            },
            TokenTree::Group(g) => {
                let inner_tokens = preprocess_for_token(g.stream());
                let delimiter = g.delimiter();
                let new_group = Group::new(delimiter, inner_tokens);
                result.extend(Some(TokenTree::Group(new_group)));
            }
            _ => {
                result.extend(Some(token));
            }
        }
    }
    result
}

pub fn type_placeholders(
    input: TokenStream,
    params: &[(usize, Ident, TemplateParamType)],
) -> TokenStream {
    let mut result = TokenStream::new();
    let mut it = input.into_iter();
    while let Some(token) = it.next() {
        match token {
            TokenTree::Ident(i) => {
                if let Some((id, n, t)) = params.iter().find(|(_, b, _)| b == &i.to_string()) {
                    let t_id = Ident::new(
                        match t {
                            TemplateParamType::Tok => "tok",
                            TemplateParamType::Nt => "nt",
                            TemplateParamType::Sym => "sym",
                            TemplateParamType::Ty => "ty",
                        },
                        n.span(),
                    );
                    result.extend(quote!($ #t_id #id));
                } else {
                    result.extend(Some(TokenTree::Ident(i)));
                }
            }
            TokenTree::Group(g) => {
                let inner_tokens = type_placeholders(g.stream(), params);
                let delimiter = g.delimiter();
                let new_group = Group::new(delimiter, inner_tokens);
                result.extend(Some(TokenTree::Group(new_group)));
            }
            _ => {
                result.extend(Some(token));
            }
        }
    }
    result
}

// TODO: better
pub fn preprocess_for_type(
    input: TokenStream,
    params: &[TemplateInstArg],
    types: &[(Ident, Type)],
    tokens: &[TokenD],
) -> Type {
    let mut result = TokenStream::new();
    let mut it = input.into_iter();
    while let Some(token) = it.next() {
        match token {
            TokenTree::Punct(p) => match p.as_char() {
                '$' => {
                    let Some(TokenTree::Ident(i)) = it.next() else { panic!("Expected an identifier in type placeholder"); };
                    let Some(TokenTree::Literal(l)) = it.next() else { panic!(); };
                    let n = l
                        .to_string()
                        .strip_suffix("usize")
                        .expect("Expected to have suffix 'usize' in type placeholder")
                        .parse::<usize>()
                        .expect("Expected a number in type placeholder");
                    let p = params[n].clone();
                    match (p, &i.to_string()[..]) {
                        (TemplateInstArg::Exact(l), "sym") | (TemplateInstArg::Exact(l), "tok") => {
                            let n = convert_litteral(&l.value());
                            let Some(tok) = tokens.iter().find(|t| t.get_term_name().to_string() == n) else {
                                panic!("Unknown token in template type");
                            };
                            match tok {
                                TokenD::Regex(_, _, Some(t), _)
                                | TokenD::Exact(_, _, Some(t), _) => {
                                    result.extend(quote!(#t));
                                }
                                _ => result.extend(quote!(())),
                            }
                        }
                        (TemplateInstArg::Ident(i), "sym") => {
                            if let Some(tok) = tokens
                                .iter()
                                .find(|t| t.get_term_name().to_string() == i.to_string())
                            {
                                match tok {
                                    TokenD::Regex(_, _, Some(t), _)
                                    | TokenD::Exact(_, _, Some(t), _) => {
                                        result.extend(quote!(#t));
                                    }
                                    _ => result.extend(quote!(())),
                                }
                            } else {
                                let Some((_, t)) = types.iter().find(|(i2, _)| i2.to_string() == i.to_string()) else {
                                    panic!("Unknown non terminal {}", i.to_string());
                                };
                                result.extend(quote!(#t));
                            }
                        }
                        (TemplateInstArg::Ident(i), "tok") => {
                            let Some(tok) = tokens.iter().find(|t| t.get_term_name().to_string() == i.to_string()) else {
                                panic!("Unknown token in template type");
                            };
                            match tok {
                                TokenD::Regex(_, _, Some(t), _)
                                | TokenD::Exact(_, _, Some(t), _) => {
                                    result.extend(quote!(#t));
                                }
                                _ => result.extend(quote!(())),
                            }
                        }
                        (TemplateInstArg::Ident(i), "nt") => {
                            let Some((_, t)) = types.iter().find(|(i2, _)| i2.to_string() == i.to_string()) else {
                                panic!("Unknown non terminal {}", i.to_string());
                            };
                            result.extend(quote!(#t));
                        }
                        (TemplateInstArg::Ident(i), "ty") => {
                            result.extend(Some(TokenTree::Ident(i)))
                        }
                        (TemplateInstArg::Type(t), "ty") => result.extend(quote!(#t)),
                        _ => panic!(),
                    }
                }
                _ => result.extend(Some(TokenTree::Punct(p))),
            },
            TokenTree::Group(g) => {
                let inner_tokens = preprocess_for_token(g.stream());
                let delimiter = g.delimiter();
                let new_group = Group::new(delimiter, inner_tokens);
                result.extend(Some(TokenTree::Group(new_group)));
            }
            _ => {
                result.extend(Some(token));
            }
        }
    }
    let result: proc_macro::TokenStream = result.into();
    let result = syn::parse::<Type>(result).unwrap();
    result
}

pub fn preprocess(input: TokenStream) -> TokenStream {
    let mut result = TokenStream::new();

    let mut it = input.into_iter();
    while let Some(token) = it.next() {
        match token {
            TokenTree::Punct(p) => match p.as_char() {
                '$' => {
                    if let Some(tok2) = it.next() {
                        match tok2 {
                            TokenTree::Punct(p2) => {
                                if p2.as_char() == '$' {
                                    result.extend(Some(TokenTree::Ident(Ident::new(
                                        "__return__",
                                        p.span(), // combine_spans(p.span(), p2.span()),
                                    ))));
                                } else {
                                    result.extend([TokenTree::Punct(p), TokenTree::Punct(p2)]);
                                }
                            }
                            TokenTree::Literal(l) => {
                                let ll = l.to_string().parse::<i32>();
                                match ll {
                                    Err(_) => {
                                        result.extend([TokenTree::Punct(p), TokenTree::Literal(l)])
                                    }
                                    Ok(ll) => result.extend(Some(TokenTree::Ident(Ident::new(
                                        &format!("__mapping_{ll}__"),
                                        p.span(), //combine_spans(p.span(), l.span()),
                                    )))),
                                }
                            }
                            _ => result.extend([TokenTree::Punct(p), tok2]),
                        }
                    } else {
                        result.extend(Some(TokenTree::Punct(p)));
                    }
                }
                '@' => {
                    if let Some(tok2) = it.next() {
                        if let TokenTree::Punct(p2) = tok2 {
                            if p2.as_char() == '@' {
                                result.extend(Some(TokenTree::Ident(Ident::new(
                                    "__parser_data__",
                                    p.span(), // combine_spans(p.span(), p2.span()),
                                ))));
                            } else {
                                result.extend([TokenTree::Punct(p), TokenTree::Punct(p2)]);
                            }
                        } else {
                            result.extend([TokenTree::Punct(p), tok2]);
                        }
                    } else {
                        result.extend(Some(TokenTree::Punct(p)));
                    }
                }
                _ => result.extend(Some(TokenTree::Punct(p))),
            },
            TokenTree::Group(g) => {
                let inner_tokens = preprocess(g.stream());
                let delimiter = g.delimiter();
                let new_group = Group::new(delimiter, inner_tokens);
                result.extend(Some(TokenTree::Group(new_group)));
            }
            _ => {
                result.extend(Some(token));
            }
        }
    }

    result
}

pub fn convert_litteral(input: &str) -> String {
    let mut result = String::new();

    for (i, c) in input.chars().enumerate() {
        match c {
            ' ' => result.push_str("Space"),
            '!' => result.push_str("Excl"),
            '"' => result.push_str("Dq"),
            '#' => result.push_str("Hash"),
            '$' => result.push_str("Dollar"),
            '%' => result.push_str("Percent"),
            '&' => result.push_str("And"),
            '\'' => result.push_str("SingleQuote"),
            '(' => result.push_str("Lparen"),
            ')' => result.push_str("Rparen"),
            '*' => result.push_str("Star"),
            '+' => result.push_str("Plus"),
            ',' => result.push_str("Comma"),
            '-' => result.push_str("Minus"),
            '.' => result.push_str("Dot"),
            '/' => result.push_str("Slash"),
            '0'..='9' => result.push(c),
            ':' => result.push_str("Colon"),
            ';' => result.push_str("Semicolon"),
            '<' => result.push_str("Lt"),
            '=' => result.push_str("Eq"),
            '>' => result.push_str("Gt"),
            '?' => result.push_str("Question"),
            '@' => result.push_str("At"),
            'A'..='Z' => result.push(c),
            '[' => result.push_str("Lbracket"),
            '\\' => result.push_str("Backslash"),
            ']' => result.push_str("Rbracket"),
            '^' => result.push_str("Caret"),
            '_' => result.push_str("Underscore"),
            '`' => result.push_str("Backtick"),
            'a'..='z' => {
                if i == 0 {
                    result.push(c.to_ascii_uppercase());
                } else {
                    result.push(c);
                }
            }
            '{' => result.push_str("Lbrace"),
            '|' => result.push_str("Pipe"),
            '}' => result.push_str("Rbrace"),
            '~' => result.push_str("Tilde"),
            _ => result.push_str("Unknown"), // Handle other characters as needed
        }
    }

    result
}
