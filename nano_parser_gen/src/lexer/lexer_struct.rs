use regex::{Regex, RegexSet};

use super::{builder::LexerBuilder, SourceFile, Tokens};

type Fnptr<T> = Option<Box<dyn Fn(&str) -> T>>;

pub struct Lexer<T> {
    kinds: Vec<Fnptr<T>>,
    regexes: Vec<Regex>,
    regex_set: RegexSet,
    eof: Fnptr<T>,
}

impl<T> Lexer<T> {
    pub fn builder<'r>() -> LexerBuilder<'r, T> {
        Default::default()
    }

    pub fn regex_set(&self) -> &RegexSet {
        &self.regex_set
    }

    pub fn regexes(&self) -> &[Regex] {
        &self.regexes
    }

    pub fn eof(&self) -> Option<T> {
        self.eof.as_ref().map(|f| f(""))
    }

    pub fn make(&self, i: usize, text: &str) -> Option<T> {
        if i > self.kinds.len() {
            panic!("OUT OF BOUNDS FOR LEXER::MAKE");
        }
        let k = self.kinds[i].as_ref()?;
        Some(k(text))
    }

    pub fn tokens<'l>(&'l self, source: &SourceFile) -> Tokens<'l, T> {
        Tokens::new(self, source)
    }

    pub fn new(
        kinds: Vec<Fnptr<T>>,
        regexes: Vec<Regex>,
        regex_set: RegexSet,
        eof: Fnptr<T>,
    ) -> Self {
        Self {
            kinds,
            regexes,
            regex_set,
            eof,
        }
    }
}
