(* Lexer for the K3 Programming Language *)

{
    open K3Parser
    open K3
}

let whitespace = [' ' '\t' '\n']

let digit = ['0'-'9']
let integer = digit+

rule tokenize = parse
    | whitespace { tokenize lexbuf }
    | eof { EOF }

    | integer as value { INTEGER (int_of_string value) }
