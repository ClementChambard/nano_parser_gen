use super::lexer_struct::Lexer;
use super::source_file::SourceFile;
use super::Token;

pub struct Tokens<'l, T> {
    lexer: &'l Lexer<T>,
    source: SourceFile,
    position: usize,
    eof: bool,
}

impl<'l, K> Tokens<'l, K> {
    pub fn new(lexer: &'l Lexer<K>, source: &SourceFile) -> Self {
        Self {
            lexer,
            source: source.clone(),
            position: 0,
            eof: false,
        }
    }
}

impl<'l, K> Iterator for Tokens<'l, K> {
    type Item = Token<K>;

    fn next(&mut self) -> Option<Token<K>> {
        loop {
            if self.eof {
                return None;
            }
            if self.position == self.source.len() {
                self.eof = true;
                return Some(Token {
                    ty: self.lexer.eof().unwrap(),
                    loc: self
                        .source
                        .range_to_location(self.position..self.position + 1),
                });
            }

            let string = self.source.remaining(self.position);
            let match_set = self.lexer.regex_set().matches(string);
            let result = match_set
                .into_iter()
                .map(|i: usize| {
                    let m = self.lexer.regexes()[i].find(string).unwrap();
                    assert!(m.start() == 0);
                    (m.end(), i)
                })
                .next_back();
            let (len, i) = if let Some((a, b)) = result {
                (a, b)
            } else {
                panic!("ERROR lexer");
                // let loc = self
                //     .source
                //     .range_to_location(self.position..self.position + 1);
                // report_error(&loc, &format!("unknown start of token: {}", &string[0..1]));
                // self.position += 1;
                // return Some(Token {
                //     kind: self.lexer.error(),
                //     loc,
                //     text: string[0..1].to_string(),
                // });
            };

            let loc = self
                .source
                .range_to_location(self.position..self.position + len);
            let text = self.source.span(self.position..self.position + len);
            self.position += len;
            if let Some(t) = self.lexer.make(i, text) {
                return Some(Token { ty: t, loc });
            }
        }
    }
}
