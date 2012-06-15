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

let comment = '#' [^ '\n']* '\n'

rule tokenize = parse
    | whitespace { tokenize lexbuf }
    | eof { EOF }
    | comment { tokenize lexbuf }

    | "declare" { DECLARE }
    | "foreign" { FOREIGN }
    | "trigger" { TRIGGER }
    | "consume" { CONSUME }

    | "()" { UNIT }
    | '_' { UNKNOWN }
    | "nothing" { NOTHING }
    | "true" { BOOL true }
    | "false" { BOOL false }
    | integer as value { INTEGER (int_of_string value) }
    | real as value { FLOAT (float_of_string value) }
    | '"' (([^'"']|"\\\"")* as s) '"'  { STRING s }

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

    | '-' { NEG }
    | '+' { PLUS }
    | '*' { TIMES }
    | '/' { DIVIDE }
    | '%' { MODULO }

    | '&' { AND }
    | '|' { OR }
    | '!' { NOT }

    | '<' { LT }
    | "==" { EQ }
    | "<=" { LEQ }
    | "!=" { NEQ }

    | '>' { GT }
    | ">=" { GEQ }

    | "->" { RARROW }
    | "<-" { LARROW }
    | "<->" { LRARROW }
    | ':' { COLON }
    | '\\' { BACKSLASH }

    | '?' { QUESTION }
    | '=' { GETS }
    | ":=" { COLONGETS }

    | "++" { CONCAT }

    | "do" { DO }

    | "unit" { TYPE TUnit }
    | "bool" { TYPE TBool }
    | "byte" { TYPE TByte }
    | "int" { TYPE TInt }
    | "float" { TYPE TFloat }
    | "string" { TYPE TString }
    | "maybe" { MAYBE }
    | "ref" { REF }
    | "just" { JUST }

    | "range" { RANGE }

    | "map" { MAP }
    | "iterate" { ITERATE }
    | "filtermap" { FILTERMAP }
    | "flatten" { FLATTEN }
    | "fold" { AGGREGATE }
    | "groupby" { GROUPBYAGGREGATE }
    | "sort" { SORT }

    | "if" { IF }
    | "then" { THEN }
    | "else" { ELSE }

    | "send" { SEND }

    | "insert" { INSERT }
    | "update" { UPDATE }
    | "delete" { DELETE }

    | '@' { ANNOTATE }

    | identifier as name {
        IDENTIFIER (name)
    }
