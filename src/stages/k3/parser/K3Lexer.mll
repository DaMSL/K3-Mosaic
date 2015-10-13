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

    | "sexpected" { EXPECTED }
    | "snetwork"  { NETWORK }

    | "declare"  { DECLARE }
    | "foreign"  { FOREIGN }
    | "trigger"  { TRIGGER }
    | "srole"    { ROLE }
    | "sdefault" { DEFAULT }

    | "ssource"   { SOURCE }
    | "ssink"     { SINK }
    | "sfile"     { FILE }
    | "socket"   { SOCKET }
    | "srandom"  { RANDOM }
    | "stream"   { STREAM }
    | "bindflow" { BINDFLOW }
    | "spattern"  { PATTERN }
    | "sconsume"  { CONSUME }

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
    | "[#"  { LBRACKETHASH }
    | "#]"  { RBRACKETHASH }
    | "[|"  { LBRACKETBAR }
    | "|]"  { RBRACKETBAR }
    | "[:"  { LBRACKETCOLON }
    | ":]"  { RBRACKETCOLON }
    | "{:"  { LBRACECOLON }
    | ":}"  { RBRACECOLON }
    | "{<"  { LBRACELT }
    | ">}"  { RBRACELT }
    | "[<"  { LBRACKETLT }
    | "[>="  { LBRACKETGEQ }
    | "[<="  { LBRACKETLEQ }
    | "[>"  { LBRACKETGT }
    | ">]"  { RBRACKETLT }
    | "[?"  { LBRACKETQ }
    | "?]"  { RBRACKETQ }
    | "|"   { BAR }

    | '-' { NEG }
    | '+' { PLUS }
    | '*' { TIMES }
    | '/' { DIVIDE }
    | '%' { MODULO }
    | '#' { HASH }

    | '&' { AND }
    | "||" { OR }
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
    | ':'   { COLON }
    | '\\'  { BACKSLASH }

    | '='  { GETS }
    | ":=" { COLONGETS }

    | "++" { CONCAT }

    | "do" { DO }

    | "unit"    { TYPE TUnit }
    | "bool"    { TYPE TBool }
    | "byte"    { TYPE TByte }
    | "int"     { TYPE TInt }
    | "date"    { TYPE TDate }
    | "float"   { TYPE TFloat }
    | "string"  { TYPE TString }
    | "address" { TYPE TAddress }
    | "unknown" { TYPE TUnknown }
    | "top"     { TYPE TTop }

    | "maybe"  { MAYBE }
    | "mut"    { MUT }
    | "just"   { JUST }

    | "range"  { RANGE }

    | "map"       { MAP }
    | "iterate"   { ITERATE }
    | "filter"    { FILTER }
    | "flatten"   { FLATTEN }
    | "fold"      { AGGREGATE }
    | "vfold"     { AGGREGATEV }
    | "groupby"   { GROUPBYAGGREGATE }
    | "sort"      { SORT }
    | "csize"     { SIZE }
    | "equijoin"  { EQUIJOIN }

    | "at"      { AT }
    | "at_with" { AT_WITH }
    | "min_with" { MIN_WITH }
    | "peek" { PEEK }
    | "peek_with_vid" { PEEK_WITH_VID }

    | "if"   { IF }
    | "then" { THEN }
    | "else" { ELSE }

    | "case" { CASE }
    | "of"   { OF }

    | "let"  { LET }
    | "in"   { IN }
    | "bind" { BIND }
    | "as"   { AS }
    | "ignore" { IGNORE }

    | "send" { SEND }

    | "insert" { INSERT }
    | "insert_at" {INSERT_AT }
    | "set_all" { SET_ALL }
    | "extend" { EXTEND }
    | "upsert_with" { UPSERT_WITH }
    | "upsert_with_before" { UPSERT_WITH_BEFORE }
    | "update_suffix" { UPDATE_SUFFIX }
    | "update" { UPDATE }
    | "update_at_with" { UPDATE_AT_WITH }
    | "delete" { DELETE }
    | "delete_prefix" { DELETE_PREFIX }
    | "delete_at" {DELETE_AT }
    | "clear_all"     { CLEAR_ALL }
    | "filter_geq"    { FILTERGEQ }
    | "filter_gt"     { FILTERGT }
    | "filter_lt"     { FILTERLT }
    | "filter_leq"    { FILTERLEQ }

    | "poly_iter"     { POLY_ITER }
    | "poly_iter_tag" { POLY_ITER_TAG }
    | "poly_fold"     { POLY_FOLD }
    | "poly_fold_tag" { POLY_FOLD_TAG }
    | "poly_at" { POLY_AT }
    | "poly_at_with" { POLY_AT_WITH }
    | "poly_insert" { POLY_INSERT }


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
