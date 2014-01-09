type token =
  | TYPE of (Type.type_t)
  | ID of (string)
  | STRING of (string)
  | INT of (int)
  | FLOAT of (float)
  | DATE
  | CHAR
  | VARCHAR
  | TRUE
  | FALSE
  | EQ
  | NE
  | LT
  | LE
  | GT
  | GE
  | SUM
  | MINUS
  | PRODUCT
  | DIVIDE
  | AND
  | OR
  | NOT
  | BETWEEN
  | LIKE
  | COMMA
  | LPAREN
  | RPAREN
  | PERIOD
  | AS
  | JOIN
  | INNER
  | OUTER
  | LEFT
  | RIGHT
  | ON
  | NATURAL
  | EXISTS
  | IN
  | SOME
  | ALL
  | UNION
  | CREATE
  | TABLE
  | FROM
  | USING
  | SELECT
  | WHERE
  | GROUP
  | BY
  | HAVING
  | ORDER
  | SOCKET
  | FILE
  | FIXEDWIDTH
  | VARSIZE
  | OFFSET
  | ADJUSTBY
  | SETVALUE
  | LINE
  | DELIMITED
  | EXTRACT
  | LIST
  | DISTINCT
  | CASE
  | WHEN
  | ELSE
  | THEN
  | END
  | FUNCTION
  | RETURNS
  | EXTERNAL
  | POSTGRES
  | RELATION
  | PIPE
  | ASC
  | DESC
  | SOURCE
  | ARGS
  | INSTANCE
  | TUPLE
  | ADAPTOR
  | BINDINGS
  | STREAM
  | EOSTMT
  | EOF
  | SUMAGG
  | COUNTAGG
  | AVGAGG
  | MAXAGG
  | MINAGG
  | INCLUDE
  | INTERVAL

val dbtoasterSqlFile :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf ->  Sql.file_t 
val dbtoasterSqlStmtList :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf ->  Sql.t list 
val expression :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf ->  Sql.expr_t 
