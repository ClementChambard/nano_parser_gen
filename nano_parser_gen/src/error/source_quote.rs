use crate::lexer::Location;
use std::ops::Range;

use crossterm::style::Stylize;
pub use crossterm::style::{Attribute, Attributes, Color, ContentStyle};

pub fn note_style() -> ContentStyle {
    ContentStyle {
        attributes: Attributes::from(Attribute::Bold),
        foreground_color: Some(Color::Blue),
        ..Default::default()
    }
}

pub fn error_style() -> ContentStyle {
    ContentStyle {
        attributes: Attributes::from(Attribute::Bold),
        foreground_color: Some(Color::DarkRed),
        ..Default::default()
    }
}

pub fn no_style() -> ContentStyle {
    ContentStyle::new()
}

pub struct SourceQuote<'r> {
    filename: &'r str,
    lines: Vec<&'r str>,
    first_line: usize,
    highlights: Vec<SourceQuoteHighlight>,
    note: Option<String>,
}

impl<'r> SourceQuote<'r> {
    pub fn new(filename: &'r str, lines: Vec<&'r str>, first_line: usize) -> Self {
        Self {
            filename,
            lines,
            first_line,
            highlights: Vec::new(),
            note: None,
        }
    }

    pub fn note(mut self, note: String) -> Self {
        self.note = Some(note);
        self
    }

    pub fn highlight_raw(mut self, line: usize, range: Range<usize>) -> Self {
        self.highlights.push(SourceQuoteHighlight {
            line,
            range,
            underline: None,
            color: None,
            comment: None,
        });
        self
    }

    pub fn highlight(mut self, loc: &Location) -> Self {
        self.highlights.push(SourceQuoteHighlight {
            line: loc.line,
            range: loc.span.clone(),
            underline: None,
            color: None,
            comment: None,
        });
        self
    }

    pub fn color(mut self, color: ContentStyle) -> Self {
        self.highlights.last_mut().unwrap().color = Some(color);
        self
    }

    pub fn underline(mut self, underline: char, color: ContentStyle) -> Self {
        self.highlights.last_mut().unwrap().underline = Some((underline, color));
        self
    }

    pub fn comment(mut self, comment: String, color: ContentStyle) -> Self {
        self.highlights.last_mut().unwrap().comment = Some((comment, color));
        self
    }

    pub fn dump(self) {
        let line_n_nchar = (self.first_line + self.lines.len()).to_string().len();
        let note_style = note_style();
        if let Some(f) = self.highlights.first() {
            println!(
                "{}{} {}:{}:{}",
                " ".repeat(line_n_nchar),
                note_style.apply("-->"),
                self.filename,
                f.line,
                f.range.start
            );
            println!("{} {}", " ".repeat(line_n_nchar), note_style.apply('|'));
        }
        for (i, l) in self.lines.into_iter().enumerate() {
            let line_n = i + self.first_line;
            DecoratedLine {
                data: l,
                line_n,
                line_n_nchar,
                decorations: self
                    .highlights
                    .iter()
                    .filter(|l| l.line == line_n)
                    .map(|hd| LineDecoration {
                        color: hd.color,
                        underline: hd.underline,
                        comment: hd.comment.clone(),
                        range: hd.range.clone(),
                    })
                    .collect(),
            }
            .print();
        }
        if let Some(note) = self.note {
            println!("{} {}", " ".repeat(line_n_nchar), note_style.apply('|'));
            println!(
                "{} {} {} {}",
                " ".repeat(line_n_nchar),
                note_style.apply('='),
                "note:".bold(),
                note
            );
        }
        println!();
    }
}

pub struct DecoratedLine<'r> {
    line_n: usize,
    line_n_nchar: usize,
    data: &'r str,
    decorations: Vec<LineDecoration>,
}

impl<'r> DecoratedLine<'r> {
    fn print(&mut self) {
        self.decorations
            .sort_by(|a, b| a.range.start.partial_cmp(&b.range.start).unwrap());
        let mut line = String::new();
        let mut line_below = " ".repeat(self.line_n_nchar);
        line.push_str(&format!("{}", self.line_n));
        let missing_spaces = self.line_n_nchar - line.len();
        for _ in 0..missing_spaces {
            line.push(' ');
        }
        line_below.push_str(" | ");
        line_below = note_style().apply(line_below).to_string();
        line.push_str(" | ");
        line = note_style().apply(line).to_string();
        let mut line_of_text = String::new();
        let mut len = 0;
        for d in &self.decorations {
            line_of_text.push_str(&self.data[len..d.range.start]);
            if let Some(c) = d.color {
                line_of_text.push_str(&c.apply(&self.data[d.range.clone()]).to_string());
            } else {
                line_of_text.push_str(&self.data[d.range.clone()]);
            }
            len = d.range.end;
        }
        line_of_text.push_str(&self.data[len..self.data.len()]);

        let mut underlines = String::new();
        let mut len = 0;
        for d in &self.decorations {
            let spaces = d.range.start - len;
            if let Some((c, col)) = d.underline {
                underlines.push_str(&" ".repeat(spaces));
                underlines.push_str(
                    &col.apply(&String::from(c).repeat(d.range.end - d.range.start))
                        .to_string(),
                );
            } else {
                underlines.push_str(&" ".repeat(d.range.end - d.range.start + spaces));
            }
            len += d.range.end - d.range.start + spaces;
        }
        let mut add_lines = Vec::new();
        let mut add_lines_p = Vec::new();
        for (i, d) in self.decorations.iter().rev().enumerate() {
            if let Some((c, col)) = &d.comment {
                if i == 0 {
                    underlines.push(' ');
                    underlines.push_str(&col.apply(c).to_string());
                } else {
                    add_lines.push((c, col));
                    add_lines_p.insert(0, d.range.start);
                }
            }
        }
        let mut add_lines_content = Vec::new();
        for i in 0..add_lines.len() {
            let mut line = String::new();
            let mut len = 0;
            for j in 0..(add_lines_p.len() - i) {
                let spaces = " ".repeat(add_lines_p[j] - len);
                line.push_str(&spaces);
                len += add_lines_p[j] - len;
                if j == (add_lines_p.len() - i - 1) {
                    line.push_str(&add_lines[i].1.apply(add_lines[i].0).to_string());
                } else {
                    line.push_str(&add_lines[i].1.apply('|').to_string());
                    len += 1;
                }
            }
            add_lines_content.push(line);
        }
        println!("{}{}", line, line_of_text);
        if !underlines.is_empty() {
            println!("{}{}", line_below, underlines);
        }
        for al in add_lines_content {
            println!("{}{}", line_below, al);
        }
    }
}

pub struct LineDecoration {
    color: Option<ContentStyle>,
    underline: Option<(char, ContentStyle)>,
    comment: Option<(String, ContentStyle)>,
    range: Range<usize>,
}

pub struct SourceQuoteHighlight {
    line: usize,
    range: Range<usize>,
    underline: Option<(char, ContentStyle)>,
    color: Option<ContentStyle>,
    comment: Option<(String, ContentStyle)>,
}
