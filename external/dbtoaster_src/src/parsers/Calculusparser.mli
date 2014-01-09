type token =
  | TYPE of (Type.type_t)
  | ID of (string)
  | STRING of (string)
  | INT of (int)
  | FLOAT of (float)
  | BOOL of (bool)
  | EQ
  | NEQ
  | LT
  | LTE
  | GT
  | GTE
  | VARCHAR
  | CHAR
  | DATE
  | EOF
  | EOSTMT
  | CREATE
  | TABLE
  | STREAM
  | LPAREN
  | RPAREN
  | LBRACKET
  | RBRACKET
  | LBRACE
  | RBRACE
  | COMMA
  | COLON
  | PLUS
  | TIMES
  | MINUS
  | DIVIDE
  | POUND
  | AS
  | FROM
  | ON
  | DO
  | SYSTEM
  | READY
  | DECLARE
  | QUERY
  | MAP
  | PARTIAL
  | INITIALIZED
  | FILE
  | SOCKET
  | FIXEDWIDTH
  | LINE
  | DELIMITED
  | AGGSUM
  | LIFT
  | SETVALUE
  | INCREMENT
  | EXISTS
  | CORRECT
  | WITH
  | FOR

val statementList :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf ->  Schema.t * (string * Calculus.expr_t) list 
val calculusExpr :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf ->  Calculus.expr_t 
val mapProgram :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf ->  M3.prog_t 
val mapTriggerStmt :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf ->  Plan.stmt_t 
