pub mod util;

use util::{BinExprPart, InstrSub, ParserData};

use super::ast;

parser_gen_macro::grammar! {

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

%token <String> insraw = "ins_[0-9]+"

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

%token <String> str   = "\"([^\\\\\"]|\\\\.)*?\""
%token <i32>    int   = "([1-9][0-9]*|0x[0-9a-fA-F]+|0b[01]+|0[0-7]*|'(\\\\.|[^\\\\'])')"
%token <f32>    float = "([0-9]*\\.[0-9]+(([eE][-+]?\\d+)|f)?|[0-9]+\\.[0-9]*(([eE][-+]?\\d+)|f)?|[0-9]+([eE][-+]?\\d+|f))"

%skip "\\s+"
%skip "//.*?(\\n|$)"
%skip "/\\*(.|\\n)*?\\*/"

%type <ast::Ecl>                Ecl
%type <ast::Param>              DefParam
%type <Vec<ast::Param>>         DefParam_list DefParam_list2
%type <Vec<String>>             Anmi Ecli Comma_sep_str2 Comma_sep_str_opt
%type <ast::Sub>                Sub
%type <Vec<ast::Sub>>           SubList
%type <ast::Instr>              Instr ElseIf
%type <Vec<ast::Instr>>         BlocInstr InstrList
%type <Option<Box<ast::Instr>>> OptElse
%type <InstrSub>                Instr_sub
%type <Option<i32>>             AsyncOpt
%type <i32>                     AsyncNumOpt
%type <u8>                      RankLabel
%type <ast::Expr>               Expr ExprOR ExprAN ExprBO ExprXO ExprBA ExprEQ
%type <ast::Expr>               ExprCM ExprPM ExprTD ExprUN ExprPrimitive VarExpr MinusVarExpr
%type <BinExprPart>             ExprORp ExprANp ExprBOp ExprXOp ExprBAp ExprEQp ExprCMp ExprPMp ExprTDp
%type <Option<ast::Expr>>       OptAffect
%type <Vec<ast::Expr>>          Param_list Param_list2

%start Ecl
%%
Comma_sep_str_opt ::= str Comma_sep_str2 { $1.insert(0, $0); $$ = $1; }
                    | <none>             { $$ = Vec::new(); }
                    ;

Comma_sep_str2 ::= "," str Comma_sep_str2  { $2.insert(0, $1); $$ = $2; }
                 | <none>                  { $$ = Vec::new(); }
                 ;

Param_list ::= Expr Param_list2          { $1.insert(0, $0); $$ = $1; }
             | <none>                    { $$ = Vec::new(); }
             ;

Param_list2 ::= "," Expr Param_list2     { $2.insert(0, $1); $$ = $2; }
              | <none>                   { $$ = Vec::new(); }
              ;

DefParam_list ::= DefParam DefParam_list2 { $1.insert(0, $0); $$ = $1; }
                | <none>                  { $$ = Vec::new(); }
                ;

DefParam_list2 ::= "," DefParam DefParam_list2 { $2.insert(0, $1); $$ = $2; }
                 | <none>                      { $$ = Vec::new(); }
                 ;

DefParam ::= kw_int id                   { $$ = ast::Param::Int($1); }
           | kw_float id                 { $$ = ast::Param::Float($1); }
           ;

Ecl ::= Ecli Anmi SubList                { $$ = ast::Ecl{ ecli: $0, anmi: $1, subs: $2 }; };

Ecli ::= kw_ecli "{" Comma_sep_str_opt "}" { $$ = $2; };

Anmi ::= kw_anmi "{" Comma_sep_str_opt "}" { $$ = $2; };

SubList ::= Sub SubList                  { $1.insert(0, $0); $$ = $1; }
          | <none>                       { $$ = Vec::new(); }
          ;

Sub ::= kw_sub id "(" DefParam_list ")"
        {
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
            $$ = ast::Sub{ name: $1, params: $3, bloc: $5, max_offset: @@.max_offset() * 4 };
            @@.pop_scope();
        };

BlocInstr ::= {
                  @@.push_scope();
              }
              "{" InstrList "}"
              {
                  @@.pop_scope();
                  $$ = $1.into_iter().filter(|i| i.is_some()).collect();
              };

InstrList ::= Instr InstrList            { $1.insert(0, $0); $$ = $1; }
            | <none>                     { $$ = Vec::new(); }
            ;

Instr ::= id Instr_sub
        {
            $$ = match $1 {
                InstrSub::Call(e_l) => ast::Instr::Call(0, e_l, @@.time, @@.rank), // todo: get name from $0
                InstrSub::Label => ast::Instr::Label($0),
                InstrSub::Affect(e) => @@.assign_binding(&$0, e),
            };
        }
        | insraw "(" Param_list ")" ";"
        {
            let i = $0[4..].parse::<i32>().unwrap();
            $$ = ast::Instr::Call(i, $2, @@.time, @@.rank);
        }
        | BlocInstr
        {
            $$ = if $0.is_empty() {
                ast::Instr::None
            } else {
                ast::Instr::Bloc($0)
            };
        }
        | int ":"
        {
            $$ = ast::Instr::None;
            @@.time = $0 as usize;
        }
        | "+" int ":"
        {
            $$ = ast::Instr::None;
            @@.time += $1 as usize;
        }
        | "-" int ":"
        {
            $$ = ast::Instr::None;
            @@.time -= $1 as usize;
        }
        | "!" RankLabel
        {
            $$ = ast::Instr::None;
            @@.rank = $1;
        }
        | kw_goto id "@" int ";"         { $$ = ast::Instr::Goto($1, $3, @@.time, @@.rank); }
        | kw_loop BlocInstr              { $$ = ast::Instr::Loop($1, @@.time, @@.rank); }
        | kw_break ";"                   { $$ = ast::Instr::Break(@@.time, @@.rank); }
        | kw_continue ";"                { $$ = ast::Instr::Continue(@@.time, @@.rank); }
        | kw_return ";"                  { $$ = ast::Instr::Return(@@.time, @@.rank); }
        | kw_delete ";"                  { $$ = ast::Instr::Delete(@@.time, @@.rank); }
        | ";"                            { $$ = ast::Instr::None; }
        | kw_int id
        {
            @@.add_int($1);
        }
        OptAffect ";"
        {
            $$ = match $2 {
                Some(a) => @@.assign_binding(&$1, a),
                None => ast::Instr::None,
            }
        }
        | kw_float id
        {
            @@.add_float($1);
        }
        OptAffect ";"
        {
            $$ = match $2 {
                Some(a) => @@.assign_binding(&$1, a),
                None => ast::Instr::None,
            }
        }
        | kw_while "(" Expr ")" BlocInstr  { $$ = ast::Instr::While($2, $4, @@.time, @@.rank); }
        | kw_if "(" Expr ")" BlocInstr OptElse { $$ = ast::Instr::If($2, $4, $5, @@.time, @@.rank); }
        | "@" id "(" Param_list ")" AsyncOpt ";" { $$ = ast::Instr::SubCall($1, $3, $5, @@.time, @@.rank); }
        | kw_do BlocInstr kw_while "(" Expr ")" ";" { $$ = ast::Instr::DoWhile($4, $1, @@.time, @@.rank); }
        ;

AsyncOpt ::= kw_async AsyncNumOpt        { $$ = Some($1); }
           | <none>                      { $$ = None; }
           ;

AsyncNumOpt ::= "(" int ")"              { $$ = $1; }
              | <none>                   { $$ = -1; }
              ;

OptAffect ::= "=" Expr                   { $$ = Some($1); }
            | <none>                     { $$ = None; }
            ;

OptElse ::= kw_else ElseIf               { $$ = Some(Box::new($1)); }
          | <none>                       { $$ = None; }
          ;

ElseIf ::= BlocInstr                     { $$ = ast::Instr::Bloc($0); }
         | kw_if "(" Expr ")" BlocInstr OptElse { $$ = ast::Instr::If($2, $4, $5, @@.time, @@.rank); }
         ;

Instr_sub ::= "(" Param_list ")" ";"     { $$ = InstrSub::Call($1); }
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
