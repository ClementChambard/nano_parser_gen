use super::lexer_struct::Lexer;

use regex::Error;
use regex::{Regex, RegexSet};

type Fnptr<T> = Option<Box<dyn Fn(&str) -> T>>;

pub struct LexerBuilder<'r, T> {
    regexes: Vec<&'r str>,
    kinds: Vec<Fnptr<T>>,
    escape: Vec<bool>,
    eof: Fnptr<T>,
    error: Fnptr<T>,
}

impl<'r, T> Default for LexerBuilder<'r, T> {
    fn default() -> Self {
        Self {
            regexes: Vec::new(),
            kinds: Vec::new(),
            escape: Vec::new(),
            eof: None,
            error: None,
        }
    }
}

impl<'r, T> LexerBuilder<'r, T> {
    pub fn token(mut self, re: &'r str, cons: Box<dyn Fn(&str) -> T>, escape: bool) -> Self {
        self.regexes.push(re);
        self.kinds.push(Some(cons));
        self.escape.push(escape);
        self
    }

    pub fn skip(mut self, re: &'r str) -> Self {
        self.regexes.push(re);
        self.kinds.push(None);
        self.escape.push(false);
        self
    }

    pub fn eof(mut self, kind: Box<dyn Fn(&str) -> T>) -> Self {
        self.eof = Some(kind);
        self
    }

    pub fn error(mut self, kind: Box<dyn Fn(&str) -> T>) -> Self {
        self.error = Some(kind);
        self
    }

    pub fn build(self) -> Result<Lexer<T>, Error> {
        let regexes = self.regexes.into_iter().zip(self.escape).map(|(r, e)| {
            if e {
                format!("^{}", &regex::escape(r))
            } else {
                format!("^{}", r)
            }
        });

        let regex_set = RegexSet::new(regexes)?;
        let mut regexes = Vec::new();
        for pattern in regex_set.patterns() {
            regexes.push(Regex::new(pattern)?);
        }

        Ok(Lexer::new(self.kinds, regexes, regex_set, self.eof))
    }
}
