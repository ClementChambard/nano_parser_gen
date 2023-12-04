#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Location {
    pub line: usize,
    pub span: std::ops::Range<usize>,
}

impl Location {
    pub fn merge(&self, l2: &Location) -> Self {
        if self.line != l2.line {
            self.clone()
        } else {
            let min = self.span.start.min(l2.span.start);
            let max = self.span.end.max(l2.span.end);
            Location {
                line: self.line,
                span: min..max,
            }
        }
    }
}

impl std::fmt::Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.span.start)
    }
}

#[derive(Debug, Clone)]
pub struct SourceFile {
    pub filename: String,
    pub content: String,
    line_sizes: Vec<usize>,
}

impl SourceFile {
    fn calculate_line_sizes(content: &str) -> Vec<usize> {
        content.split('\n').map(|s| s.len() + 1).collect()
    }

    pub fn open(filename: &str) -> Result<Self, std::io::Error> {
        let content = std::fs::read_to_string(filename)?;
        Ok(Self {
            filename: filename.to_string(),
            line_sizes: Self::calculate_line_sizes(&content),
            content,
        })
    }

    pub fn remaining(&self, pos: usize) -> &str {
        &self.content[pos..]
    }

    pub fn span(&self, range: std::ops::Range<usize>) -> &str {
        &self.content[range]
    }

    pub fn len(&self) -> usize {
        self.content.len()
    }

    pub fn range_to_location(&self, range: std::ops::Range<usize>) -> Location {
        let len = range.end - range.start;
        let mut pos = range.start;
        let mut line = 0;
        for l in &self.line_sizes {
            if pos >= *l {
                line += 1;
                pos -= *l;
            } else {
                break;
            }
        }
        Location {
            line,
            span: pos..pos + len,
        }
    }

    pub fn get_line(&self, i: usize) -> &str {
        if i > self.line_sizes.len() {
            panic!("Out of range line {i}");
        }
        let start = self.line_sizes[..i].iter().sum();
        let len = self.line_sizes[i] - 1;
        &self.content[start..start + len]
    }
}

impl From<&str> for SourceFile {
    fn from(s: &str) -> SourceFile {
        SourceFile {
            filename: "dummy".to_owned(),
            content: s.to_owned(),
            line_sizes: Self::calculate_line_sizes(s),
        }
    }
}
