fn unescape_string(input: &str) -> String {
    let mut output = String::new();
    let mut chars = input.chars().peekable();

    while let Some(ch) = chars.next() {
        if ch == '\\' {
            if let Some(next_ch) = chars.next() {
                match next_ch {
                    '\\' => output.push('\\'),
                    '\"' => output.push('\"'),
                    '\'' => output.push('\''),
                    'n' => output.push('\n'),
                    'r' => output.push('\r'),
                    't' => output.push('\t'),
                    '0' => output.push('\0'),
                    'x' => {
                        // Handle hexadecimal escape sequence (e.g., \x41 for 'A')
                        if let (Some(hex1), Some(hex2)) = (chars.next(), chars.next()) {
                            if let (Some(hex_digit1), Some(hex_digit2)) =
                                (hex1.to_digit(16), hex2.to_digit(16))
                            {
                                let byte_value = (hex_digit1 << 4) | hex_digit2;
                                if let Some(ascii_char) = std::char::from_u32(byte_value) {
                                    output.push(ascii_char);
                                } else {
                                    // Handle invalid hexadecimal value
                                    output.push_str("\\x");
                                    output.push(hex1);
                                    output.push(hex2);
                                }
                            } else {
                                // Handle invalid hexadecimal digits
                                output.push_str("\\x");
                                output.push(hex1);
                                output.push(hex2);
                            }
                        } else {
                            // Handle incomplete hexadecimal escape sequence
                            output.push_str("\\x");
                        }
                    }
                    'u' => {
                        // Handle Unicode escape sequence (e.g., \u{2665} for '❤')
                        if let (Some('{'), Some(mut unicode_ch)) = (chars.next(), chars.next()) {
                            let mut unicode_digits = String::new();
                            while unicode_ch != '}' {
                                unicode_digits.push(unicode_ch);
                                if let Some(ch) = chars.next() {
                                    unicode_ch = ch;
                                } else {
                                    // Handle incomplete Unicode escape sequence
                                    output.push_str("\\u");
                                    output.push('{');
                                    output.push_str(&unicode_digits);
                                    break;
                                }
                            }
                            if let Ok(unicode_value) = u32::from_str_radix(&unicode_digits, 16) {
                                if let Some(unicode_char) = std::char::from_u32(unicode_value) {
                                    output.push(unicode_char);
                                } else {
                                    // Handle invalid Unicode value
                                    output.push_str("\\u");
                                    output.push('{');
                                    output.push_str(&unicode_digits);
                                }
                            } else {
                                // Handle invalid hexadecimal value in Unicode escape sequence
                                output.push_str("\\u");
                                output.push('{');
                                output.push_str(&unicode_digits);
                            }
                        } else {
                            // Handle incomplete Unicode escape sequence
                            output.push_str("\\u");
                        }
                    }
                    _ => {
                        // Handle unknown escape sequences
                        output.push(ch);
                        output.push(next_ch);
                    }
                }
            } else {
                // Handle trailing backslash
                output.push(ch);
            }
        } else {
            output.push(ch);
        }
    }

    output
}

pub fn read_strlit(s: &str) -> String {
    let s = s.strip_prefix('"').unwrap().strip_suffix('"').unwrap();
    // escape characters

    // for now, simple replace for \ and "
    unescape_string(&s)
}
