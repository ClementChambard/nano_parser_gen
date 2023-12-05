mod ast;
mod parse;

fn main() {
    println!(
        "{:#?}",
        parse::parse_source(nano_parser_gen::lexer::SourceFile::from(
            r#"
// test comment 1
ecli { "a", "b" }
anmi { "a", "c" }

/* Test comment 2 */
sub main() {
    int c;
    @main_2(1, 2.0);
    delete;
}

sub main_2(int a, float b) {
    int c = 0;
    a = b - c + 3 + 4;
    ins_3();
    return;
}

"#,
        ))
    );
}
