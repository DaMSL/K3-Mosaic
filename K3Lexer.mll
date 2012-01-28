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
    | ';' { SEMICOLON }

    | '{' { LBRACE }
    | '}' { RBRACE }
    | "{|" { LBRACEBAR }
    | "|}" { RBRACEBAR }
    | '[' { LBRACKET }
    | ']' { RBRACKET }

    | '+' { PLUS }
    | '*' { TIMES }
    | '-' { NEG }

    | '&' { AND }
    | '|' { OR }
    | '!' { NOT }

    | '<' { LT }
    | "==" { EQ }
    | "<=" { LEQ }
    | "!=" { NEQ }

    | "->" { RARROW }
    | "<->" { LRARROW }
    | ':' { COLON }
    | '\\' { BACKSLASH }

    | '?' { QUESTION }
    | '=' { GETS }

    | "++" { CONCAT }

    | "do" { DO }

    | "bool" { TYPE TBool }
    | "int" { TYPE TInt }
    | "float" { TYPE TFloat }
    | "string" { TYPE TString }

    | "map" { MAP }
    | "iterate" { ITERATE }
    | "filtermap" { FILTERMAP }
    | "flatten" { FLATTEN }
    | "fold" { AGGREGATE }
    | "groupby" { GROUPBYAGGREGATE }
    | "sort" { SORT }
    | "rank" { RANK }
    | "head" { HEAD }
    | "tail" { TAIL }

    | "if" { IF }
    | "then" { THEN }
    | "else" { ELSE }

    | "send" { SEND }

    | '@' { ANNOTATE }

    | "declare" { DECLARE }
    | "foreign" { FOREIGN }
    | "trigger" { TRIGGER }

    | identifier as name {
        IDENTIFIER (name)
    }
