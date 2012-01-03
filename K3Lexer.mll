(* Lexer for the K3 Programming Language *)

{
    open K3Parser
    open K3
}

let whitespace = [' ' '\t' '\n']

let digit = ['0'-'9']
let integer = digit+
let real = digit+ '.' digit+

let identifier = ['_''a'-'z''A'-'Z']['_''a'-'z''A'-'Z''0'-'9']*

rule tokenize = parse
    | whitespace { tokenize lexbuf }
    | eof { EOF }

    | integer as value { INTEGER (int_of_string value) }
    | real as value { FLOAT (float_of_string value) }
    | '"' (([^'"']|"\\\"")* as s) '"'  { STRING s }
    | "true" { BOOL true }
    | "false" { BOOL false }

    | '(' { LPAREN }
    | ')' { RPAREN }
    | ',' { COMMA }

    | '+' { PLUS }
    | '*' { TIMES }
    | '-' { NEG }

    | identifier as name {
        IDENTIFIER (name)
    }
