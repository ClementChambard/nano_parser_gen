pub mod source_quote;

use crossterm::style::{Color, Stylize};

pub fn error_header(s: &str) {
    println!(
        "{}{} {}",
        "error".bold().with(Color::DarkRed),
        ':'.bold(),
        s.bold()
    );
}
