type token =
  | TYPE of (Type.type_t)
  | INTEGER of (int)
  | ID of (string)
  | CONST_STRING of (string)
  | CONST_FLOAT of (float)
  | EQ
  | NE
  | LT
  | LE
  | GT
  | SUM
  | MINUS
  | PRODUCT
  | COMMA
  | LPAREN
  | RPAREN
  | LBRACK
  | RBRACK
  | PERIOD
  | COLON
  | DOLLAR
  | ON
  | SYSTEM
  | READY
  | QUERY
  | IF
  | IF0
  | ELSE
  | ITERATE
  | LAMBDA
  | APPLY
  | MAP
  | FLATTEN
  | AGGREGATE
  | GROUPBYAGGREGATE
  | MEMBER
  | LOOKUP
  | SLICE
  | FILTER
  | SINGLETON
  | COMBINE
  | CREATE
  | TABLE
  | STREAM
  | FROM
  | SOCKET
  | FILE
  | PIPE
  | FIXEDWIDTH
  | DELIMITED
  | LINE
  | VARSIZE
  | OFFSET
  | ADJUSTBY
  | SETVALUE
  | PCUPDATE
  | PCVALUEUPDATE
  | PCELEMENTREMOVE
  | EXTERNALLAMBDA
  | INT
  | UNIT
  | FLOAT
  | COLLECTION
  | STRINGTYPE
  | CHAR
  | VARCHAR
  | DATE
  | IN
  | OUT
  | EOSTMT
  | EOF
  | LBRACE
  | RBRACE
  | BOUND
  | ARROW

val dbtoasterK3Program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf ->  K3.prog_t 
val statement :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf ->  K3.expr_t 
val mapDeclaration :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf ->  K3.map_t 
