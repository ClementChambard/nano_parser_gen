# Nano Parser Gen

A small parser generator written in rust using procedural macros.

## Installation

You can add Nano Parser Gen to your rust project using cargo.

```cargo new <project name>```

```cd <project name>```

```cargo add nano_parser_gen nano_parser_gen_macro```

## How to use

You will need to create a type that will be used to share data around while parsing.

It needs to derive `Clone` and contain at least a function `pub fn new() -> Self`.

Example:
```rust
#[derive(Clone)]
struct ParserData; // empty struct for now

impl ParserData {
    pub fn new() -> Self {
        Self {}
    }
}
```

If needed, define your ast nodes types.

They need to derive `clone` and `debug`.

You can now define the grammar of your language using the `grammar!` macro inside the crate `rust_parser_gen_macro`.

```rust
grammar! {

// set the parser data type (mandatory).
%parserdata ParserData

// define the tokens of the language
// with literals
%token "{"
%token "}"
// with a regex
%token kw_add = "add\\b"
// with a type and default conversion
%token <i32> int = "[0-9]+"
// with a type and custom conversion  (use '$' to refer to the parsed string)
%token <i32> custom_int = "custom_[0-9]+" => { $[7..].parse().unwrap() }
// ...

// define the sequence of characters that can be skipped
%skip "\\s+"           // skip spaces
%skip "//.*?(\\n|$)"   // single line comments
// ...

// define the types of the non terminal symbols
%type <i32> A B
// ...

// define the start symbol
%start A

// end the define section
%%

// the rules of the grammar

// rules for the 'A' non terminal symbol
A ::= "{" kw_add int B "}"
             // You need to create the AST node and store it in '$$'. For symbol 'A', it is a i32
             {
                 // get the AST value of the nth element with '$n' (0 indexed)
                 $$ = $2 + $3;
             }
    | <none>     // use <none> to indicate no tokens
             {
                 $$ = 0;
             }
    ;

// rules for the 'B' non terminal symbol
B ::= custom_int         { $$ = $0; }
    | kw_add int B       { $$ = $2 + $1; }
    ;

}
```

The macro defines the function `parse_source` that will parse a `SourceFile`.

`SourceFile`s can be constructed with `SourceFile::from(source: &str)` or `SourceFile::open(filename: &str)`.

Example:
```rust
fn main() {
    // This will print "15"
    println!("{}", parse_source(SourceFile::from(
        " {add 1 add 2 add 3 add 4   custom_5   }       // a comment"
    )));
}
```

## More examples

More examples can be found in this repo, with a more extensive use of the parser generator

## Other features

### Code blocks

Grammar rules for symbols are defined as follow:

Symbol ::= Rule1 | Rule2 | Rule3 ... Rule4 ;

Each rule is defined by its symbols (or '<none>') and must end with a code block, setting the '$$' variable as the output AST node of the symbol.

It can also contain code blocks in between rule symbols and if so, they will be executed between the parsing of those symbols.

Example:
```rust
Block ::= "{"
             { /* Code to execute when entering a block */ }
           BlockContent
             { /* Code to execute on block exit */ }
           "}"
             { $$ = $1; }
        ;
```

### What is ParserData

During the parsing process, there will be one instance of `ParserData` that can be accessed in a code block with `@@`.

Example
```rust
impl ParserData {
    pub fn enter_block(&self) { /*...*/ }
    pub fn exit_block(&self) { /*...*/ }
}

//...

Block ::= "{"
             { @@.enter_block(); }
           BlockContent
             { @@.exit_block(); }
           "}"
             { $$ = $1; }
        ;
```

### Rule templates

You can create rule templates to avoid code duplication.

One of the builtin template is List<T: sym>. It has type `Vec<TypeOfT>` and correspond to a potentially empty list of 'T'.

Possible way of implementing List<T: sym>:
```rust
// template params: `name: type`  with types  sym: any symbol,  tok: any token,  nt: any non terminal,  ty: a rust type
%template<Vec<T>> List<T: sym> ::= T List<T>       { $1.insert(0, $0); $$ = $1; }
                            | <none>               { $$ = Vec::new(); }
                            ; 

// ...

A ::= "{" List<int> "}" { $$ = $1; };
```

Builtin templates:
- Option<T: sym>               an optional symbol T                        type=`Option<TypeOfT>`
- OptionB<T: sym>              an optional symbol T                        type=`bool` (true if option is `Some`)
- List<T: sym>                 a list of symbol T                          type=`Vec<TypeOfT>`
- List1<T: sym>                a non empty list of symbol T                type=`Vec<TypeOfT>`
- SepList<T: sym, Sep: sym>    a Sep separated list of symbol T            type=`Vec<TypeOfT>`
- SepList1<T: sym, Sep: sym>   a non empyt Sep separated list of symbol T  type=`Vec<TypeOfT>`
