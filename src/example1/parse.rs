pub mod util;

use util::{BinExprPart, InstrSub, ParserData};

use super::ast;
use ast::{Instr, InstrType};

nano_parser_gen_macro::grammar! {

%parserdata ParserData

%token "{"
%token "}"
%token "("
%token ")"
%token "["
%token "]"
%token ","
%token ";"
%token ":"
%token "+"
%token "-"
%token "*"
%token "/"
%token "%"
%token "&"
%token "&&"
%token "|"
%token "||"
%token "^"
%token "."
%token "@"
%token "<"
%token "<="
%token ">"
%token ">="
%token "="
%token "=="
%token "!"
%token "!="

%token <String> id = "[a-zA-Z_][a-zA-Z_0-9]*"

%token kw_ecli     = "ecli\\b"
%token kw_anmi     = "anmi\\b"
%token kw_sub      = "sub\\b"
%token kw_if       = "if\\b"
%token kw_else     = "else\\b"
%token kw_goto     = "goto\\b"
%token kw_loop     = "loop\\b"
%token kw_break    = "break\\b"
%token kw_continue = "continue\\b"
%token kw_return   = "return\\b"
%token kw_delete   = "delete\\b"
%token kw_do       = "do\\b"
%token kw_while    = "while\\b"
%token kw_cos      = "cos\\b"
%token kw_sin      = "sin\\b"
%token kw_sqrt     = "sqrt\\b"
%token kw_int      = "int\\b"
%token kw_float    = "float\\b"
%token kw_async    = "async\\b"
%token kw_bh       = "BulletHandler\\b"

%token <i32>    instr = "ins_[0-9]+"              => { $[4..].parse().unwrap() }
%token <String> str   = "\"([^\\\\\"]|\\\\.)*?\"" => { nano_parser_gen::util::read_strlit($) }
%token <i32>    int   = "([1-9][0-9]*|0x[0-9a-fA-F]+|0b[01]+|0[0-7]*|'(\\\\.|[^\\\\'])')"
%token <f32>    float = "([0-9]*\\.[0-9]+(([eE][-+]?\\d+)|f)?|[0-9]+\\.[0-9]*(([eE][-+]?\\d+)|f)?|[0-9]+([eE][-+]?\\d+|f))"

%skip "\\s+"
%skip "//.*?(\\n|$)"
%skip "/\\*(.|\\n)*?\\*/"

%type <ast::Ecl>                Ecl
%type <ast::Param>              DefParam
%type <Vec<String>>             Anmi Ecli
%type <ast::Sub>                Sub
%type <ast::Instr>              Instr ElseIf
%type <Vec<ast::Instr>>         BlocInstr
%type <Box<ast::Instr>>         Else
%type <InstrSub>                Instr_sub
%type <i32>                     AsyncNumOpt Async
%type <u8>                      RankLabel
%type <ast::Expr>               Expr ExprOR ExprAN ExprBO ExprXO ExprBA ExprEQ Affect
%type <ast::Expr>               ExprCM ExprPM ExprTD ExprUN ExprPrimitive VarExpr MinusVarExpr
%type <BinExprPart>             ExprORp ExprANp ExprBOp ExprXOp ExprBAp ExprEQp ExprCMp ExprPMp ExprTDp

%start Ecl
%%

Ecl ::= Ecli Option<Anmi> List<Sub>              { $$ = ast::Ecl{ ecli: $0, anmi: $1.unwrap_or(Vec::new()), subs: $2 }; }
      | Anmi Option<Ecli> List<Sub>              { $$ = ast::Ecl{ anmi: $0, ecli: $1.unwrap_or(Vec::new()), subs: $2 }; }
      | List<Sub>                                { $$ = ast::Ecl{ anmi: Vec::new(), ecli: Vec::new(), subs: $0 }; }
      ;

Ecli ::= kw_ecli "{" SepList<str, ","> "}" { $$ = $2; };

Anmi ::= kw_anmi "{" SepList<str, ","> "}" { $$ = $2; };

DefParam ::= kw_int id                   { $$ = ast::Param::Int($1); }
           | kw_float id                 { $$ = ast::Param::Float($1); }
           ;

Sub ::= kw_sub id "(" SepList<DefParam, ","> ")"
        {
            @@.i = 0;
            @@.time = 0;
            @@.rank = 255;
            @@.push_scope();
            for p in &$3 {
                match p {
                    ast::Param::Int(s) => @@.add_int_alloced(s.clone()),
                    ast::Param::Float(s) => @@.add_float_alloced(s.clone()),
                }
            }
        }
        BlocInstr
        {
            $$ = ast::Sub::new($1, $3, $5, @@.max_offset());
            @@.pop_scope();
            @@.labels.clear();
        };

BlocInstr ::= {
                  @@.push_scope();
              }
              "{" List<Instr> "}"
              {
                  @@.pop_scope();
                  $$ = $1;
              };

Instr ::= id Instr_sub
        {
            $$ = match $1 {
                InstrSub::Call(e_l) => Instr::new(InstrType::Call(0, e_l), @@.time, @@.rank, @@.i), // todo: get name from $0
                InstrSub::Label => { @@.add_label($0); Instr::none() },
                InstrSub::Affect(e) => @@.assign_binding(&$0, e),
            };
            @@.i += 1;
        }
        | instr "(" SepList<Expr, ","> ")" ";" { $$ = Instr::new(InstrType::Call($0, $2), @@.time, @@.rank, @@.i); @@.i += 1; }
        | BlocInstr
        {
            $$ = if $0.is_empty() {
                Instr::none()
            } else {
                Instr::bloc($0)
            };
        }
        | int ":"
        {
            $$ = Instr::none();
            @@.time = $0 as usize;
        }
        | "+" int ":"
        {
            $$ = Instr::none();
            @@.time += $1 as usize;
        }
        | "-" int ":"
        {
            $$ = Instr::none();
            @@.time -= $1 as usize;
        }
        | "!" RankLabel
        {
            $$ = Instr::none();
            @@.rank = $1;
        }
        | kw_goto id "@" int ";"         { $$ = Instr::new(InstrType::Goto($1, $3), @@.time, @@.rank, @@.i); @@.i += 1; }
        | kw_loop BlocInstr              { $$ = Instr::new(InstrType::Loop($1), @@.time, @@.rank, @@.i); @@.i += 1; }
        | kw_break ";"                   { $$ = Instr::new(InstrType::Break, @@.time, @@.rank, @@.i); @@.i += 1; }
        | kw_continue ";"                { $$ = Instr::new(InstrType::Continue, @@.time, @@.rank, @@.i); @@.i += 1; }
        | kw_return ";"                  { $$ = Instr::new(InstrType::Return, @@.time, @@.rank, @@.i); @@.i += 1; }
        | kw_delete ";"                  { $$ = Instr::new(InstrType::Delete, @@.time, @@.rank, @@.i); @@.i += 1; }
        | ";"                            { $$ = Instr::none(); }
        | kw_int id
        {
            @@.add_int($1);
        }
        Option<Affect> ";"
        {
            $$ = match $2 {
                Some(a) => @@.assign_binding(&$1, a),
                None => Instr::none(),
            };
            @@.i += 1;
        }
        | kw_float id
        {
            @@.add_float($1);
        }
        Option<Affect> ";"
        {
            $$ = match $2 {
                Some(a) => @@.assign_binding(&$1, a),
                None => Instr::none(),
            };
            @@.i += 1;
        }
        | kw_while "(" Expr ")" BlocInstr  { $$ = Instr::new(InstrType::While($2, $4), @@.time, @@.rank, @@.i); @@.i += 1; }
        | kw_if "(" Expr ")" BlocInstr Option<Else> { $$ = Instr::new(InstrType::If($2, $4, $5), @@.time, @@.rank, @@.i); @@.i += 1; }
        | "@" id "(" SepList<Expr, ","> ")" Option<Async> ";" { $$ = Instr::new(InstrType::SubCall($1, $3, $5), @@.time, @@.rank, @@.i); @@.i += 1; }
        | kw_do BlocInstr kw_while "(" Expr ")" ";" { $$ = Instr::new(InstrType::DoWhile($4, $1), @@.time, @@.rank, @@.i); @@.i += 1; }
        ;

Async ::= kw_async AsyncNumOpt        { $$ = $1; };

AsyncNumOpt ::= "(" int ")"              { $$ = $1; }
              | <none>                   { $$ = -1; }
              ;

Affect ::= "=" Expr                   { $$ = $1; };

Else ::= kw_else ElseIf               { $$ = Box::new($1); };

ElseIf ::= BlocInstr                     { $$ = Instr::bloc($0); }
         | kw_if "(" Expr ")" BlocInstr Option<Else> { $$ = Instr::new(InstrType::If($2, $4, $5), @@.time, @@.rank, @@.i); @@.i += 1; }
         ;

Instr_sub ::= "(" SepList<Expr, ","> ")" ";"     { $$ = InstrSub::Call($1); }
            | ":"                        { $$ = InstrSub::Label; }
            | "=" Expr ";"               { $$ = InstrSub::Affect($1); }
            ;

RankLabel ::= id ":"                     { $$ = ast::rank_label_spec($0); }
            | "*" ":"                    { $$ = 255;  }
            ;

Expr ::= ExprOR                          { $$ = $0; };

ExprOR  ::= ExprAN ExprORp               { $$ = $1.merge($0); };
ExprORp ::= "||" ExprAN ExprORp          { $$ = $2.chain($1, ast::BinOp::Or); }
          | <none>                       { $$ = BinExprPart::None; }
          ;

ExprAN  ::= ExprBO ExprANp               { $$ = $1.merge($0); };
ExprANp ::= "&&" ExprBO ExprANp          { $$ = $2.chain($1, ast::BinOp::And); }
          | <none>                       { $$ = BinExprPart::None; }
          ;

ExprBO  ::= ExprXO ExprBOp               { $$ = $1.merge($0); };
ExprBOp ::= "|" ExprXO ExprBOp           { $$ = $2.chain($1, ast::BinOp::BinOr); }
          | <none>                       { $$ = BinExprPart::None; }
          ;

ExprXO  ::= ExprBA ExprXOp               { $$ = $1.merge($0); };
ExprXOp ::= "^" ExprBA ExprXOp           { $$ = $2.chain($1, ast::BinOp::Xor); }
          | <none>                       { $$ = BinExprPart::None; }
          ;

ExprBA  ::= ExprEQ ExprBAp               { $$ = $1.merge($0); };
ExprBAp ::= "&" ExprEQ ExprBAp           { $$ = $2.chain($1, ast::BinOp::BinAnd); }
          | <none>                       { $$ = BinExprPart::None; }
          ;

ExprEQ  ::= ExprCM ExprEQp               { $$ = $1.merge($0); };
ExprEQp ::= "==" ExprCM ExprEQp          { $$ = $2.chain($1, ast::BinOp::Eq); }
          | "!=" ExprCM ExprEQp          { $$ = $2.chain($1, ast::BinOp::Ne); }
          | <none>                       { $$ = BinExprPart::None; }
          ;

ExprCM  ::= ExprPM ExprCMp               { $$ = $1.merge($0); };
ExprCMp ::= "<" ExprPM ExprCMp           { $$ = $2.chain($1, ast::BinOp::Lt); }
          | "<=" ExprPM ExprCMp          { $$ = $2.chain($1, ast::BinOp::Le); }
          | ">" ExprPM ExprCMp           { $$ = $2.chain($1, ast::BinOp::Gt); }
          | ">=" ExprPM ExprCMp          { $$ = $2.chain($1, ast::BinOp::Ge); }
          | <none>                       { $$ = BinExprPart::None; }
          ;

ExprPM  ::= ExprTD ExprPMp               { $$ = $1.merge($0); };
ExprPMp ::= "+" ExprTD ExprPMp           { $$ = $2.chain($1, ast::BinOp::Add); }
          | "-" ExprTD ExprPMp           { $$ = $2.chain($1, ast::BinOp::Sub); }
          | <none>                       { $$ = BinExprPart::None; }
          ;

ExprTD  ::= ExprUN ExprTDp               { $$ = $1.merge($0); };
ExprTDp ::= "*" ExprUN ExprTDp           { $$ = $2.chain($1, ast::BinOp::Mul); }
          | "/" ExprUN ExprTDp           { $$ = $2.chain($1, ast::BinOp::Div); }
          | "%" ExprUN ExprTDp           { $$ = $2.chain($1, ast::BinOp::Mod); }
          | <none>                       { $$ = BinExprPart::None; }
          ;

ExprUN ::= "-" ExprPrimitive             { $$ = ast::Expr::Unary(ast::UnOp::Negate, Box::new($1)); }
         | "!" ExprPrimitive             { $$ = ast::Expr::Unary(ast::UnOp::Not, Box::new($1)); }
         | "+" ExprPrimitive             { $$ = $1; }
         | ExprPrimitive                 { $$ = $0; }
         ;

ExprPrimitive ::= int                    { $$ = ast::Expr::Int($0); }
                | float                  { $$ = ast::Expr::Float($0); }
                | str                    { $$ = ast::Expr::Str($0); }
                | id                     { $$ = @@.use_binding($0); }
                | "[" VarExpr "]"        { $$ = $1; }
                | "(" Expr ")"           { $$ = $1; }
                | kw_sin "(" Expr ")"    { $$ = ast::Expr::Unary(ast::UnOp::Sin, Box::new($2)); }
                | kw_cos "(" Expr ")"    { $$ = ast::Expr::Unary(ast::UnOp::Cos, Box::new($2)); }
                | kw_sqrt "(" Expr ")"   { $$ = ast::Expr::Unary(ast::UnOp::Sqrt, Box::new($2)); }
                ;

VarExpr ::= int                          { $$ = ast::Expr::VarInt($0); }
          | float                        { $$ = ast::Expr::VarFloat($0); }
          | "-" MinusVarExpr             { $$ = $1; }
          ;

MinusVarExpr ::= int                     { $$ = ast::Expr::VarInt(-$0); }
               | float                   { $$ = ast::Expr::VarFloat(-$0); }
               ;

}
