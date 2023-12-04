use proc_macro2::{Group, Ident, Span, TokenStream, TokenTree};

pub fn combine_spans(span1: Span, span2: Span) -> Span {
    // Combine the spans of the two tokens
    span1.join(span2).unwrap_or_else(|| {
        //  In case the spans cannot be joined, return a fallback span
        Span::call_site()
    })
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
