(* Lexer for the K3 Programming Language *)

{
    open K3Parser
    open K3.AST

                let init_line lexbuf =
                  let pos = lexbuf.Lexing.lex_curr_p in
                  lexbuf.Lexing.lex_curr_p <- { pos with
                    Lexing.pos_lnum = 1;
                    Lexing.pos_bol = 0;
                  }

                let advance_line lexbuf =
                  let pos = lexbuf.Lexing.lex_curr_p in
                  lexbuf.Lexing.lex_curr_p <- { pos with
                    Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
                    Lexing.pos_bol = pos.Lexing.pos_cnum;
                  }

let string_error lexbuf msg =
    let pos = lexbuf.Lexing.lex_curr_p in
    let linenum = pos.Lexing.pos_lnum in
    let column = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
    Printf.sprintf "Error on line %d character %d : %s" linenum column msg

}

let whitespace = [' ' '\t']
let newline    = "\n\r" | '\n' | '\r'

let digit = ['0'-'9']
let integer = digit+
let real = digit+ '.' digit*

let identifier = ['_''a'-'z''A'-'Z']['_''a'-'z''A'-'Z''0'-'9']*
let ip4 = digit+ '.' digit+ '.' digit+ '.' digit+

let sl_comment     = "//"[^'\n' '\r']*
let ml_comment_st  = "/*"
let ml_comment_end = "*/"

rule tokenize = parse
    | whitespace    { tokenize lexbuf }
    | newline       { advance_line lexbuf; tokenize lexbuf }
    | eof           { EOF }

    | sl_comment    { tokenize lexbuf }
    | ml_comment_st { comment 1 lexbuf }

    | "expected" { EXPECTED }
    | "network"  { NETWORK }

    | "declare"  { DECLARE }
    | "foreign"  { FOREIGN }
    | "trigger"  { TRIGGER }
    | "role"     { ROLE }
    | "default"  { DEFAULT }

    | "source"   { SOURCE }
    | "sink"     { SINK }
    | "file"     { FILE }
    | "socket"   { SOCKET }
    | "random"   { RANDOM }
    | "stream"   { STREAM }
    | "bindflow"     { BINDFLOW }
    | "pattern"  { PATTERN }
    | "consume"  { CONSUME }

    | "()"      { UNIT }
    | '_'       { UNKNOWN }
    | "nothing" { NOTHING }

    | "true"  { BOOL true }
    | "false" { BOOL false }

    | "ind"   { INDIRECT }

    | real as value    { FLOAT (float_of_string value) }
    | integer as value { INTEGER (int_of_string value) }

    | '"' (([^'"']|"\\\"")* as s) '"'  { STRING s }

    | '(' { LPAREN }
    | ')' { RPAREN }
    | ',' { COMMA }
    | ';' { SEMICOLON }

    | '.'  { PERIOD }
    | '{'  { LBRACE }
    | '}'  { RBRACE }
    | "{|" { LBRACEBAR }
    | "|}" { RBRACEBAR }
    | '['  { LBRACKET }
    | ']'  { RBRACKET }
    | "[|"  { LBRACKETBAR }
    | "|]"  { RBRACKETBAR }
    | '|'  { BAR }

    | '-' { NEG }
    | '+' { PLUS }
    | '*' { TIMES }
    | '/' { DIVIDE }
    | '%' { MODULO }
    | '#' { HASH }

    | '&' { AND }
    | '|' { OR }
    | '!' { NOT }

    | '<'  { LT }
    | "==" { EQ }
    | "<=" { LEQ }
    | "!=" { NEQ }

    | '>'  { GT }
    | ">=" { GEQ }

    | "->"  { RARROW }
    | "<-"  { LARROW }
    | "<->" { LRARROW }
    | "=>"  { RASSOC }
    | ':'   { COLON }
    | '\\'  { BACKSLASH }

    | '?'  { QUESTION }
    | '='  { GETS }
    | ":=" { COLONGETS }

    | "++" { CONCAT }

    | "do" { DO }

    | "unit"   { TYPE TUnit }
    | "bool"   { TYPE TBool }
    | "byte"   { TYPE TByte }
    | "int"    { TYPE TInt }
    | "date"    { TYPE TDate }
    | "float"  { TYPE TFloat }
    | "string" { TYPE TString }
    | "address" { TYPE TAddress }
    | "unknown" { TYPE TUnknown }
    | "top" { TYPE TTop }

    | "maybe"  { MAYBE }
    | "ref"    { REF }
    | "just"   { JUST }

    | "range"  { RANGE }

    | "map"       { MAP }
    | "iterate"   { ITERATE }
    | "filter"    { FILTER }
    | "flatten"   { FLATTEN }
    | "fold"      { AGGREGATE }
    | "groupby"   { GROUPBYAGGREGATE }
    | "sort"      { SORT }

    | "peek" { PEEK }

    | "if"   { IF }
    | "then" { THEN }
    | "else" { ELSE }

    | "case" { CASE }
    | "of"   { OF }

    | "let"  { LET }
    | "in"   { IN }
    | "bind" { BIND }
    | "as"   { AS }

    | "send" { SEND }

    | "insert" { INSERT }
    | "update" { UPDATE }
    | "delete" { DELETE }

    | '@' { ANNOTATE }

    | "effect"        { EFFECT }
    | "parallel"      { PARALLEL }

    | identifier as name { IDENTIFIER (name) }
    | ip4 as ip { IP(ip) }
    | _ as c { failwith (string_error lexbuf (Printf.sprintf "Unrecognized character: %c" c))}

and comment depth = parse
| ml_comment_st  { raise (Failure ("nested comments are invalid")) }
| ml_comment_end { tokenize lexbuf }
| eof            { raise (Failure ("hit end of file in a comment")) }
| _              { comment depth lexbuf }
