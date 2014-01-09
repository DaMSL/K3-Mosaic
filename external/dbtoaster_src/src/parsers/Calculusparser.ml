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

open Parsing;;
let _ = parse_error;;
# 2 "src/parsers/Calculusparser.mly"
open Type
open Constants
open Calculus
open Arithmetic
open Plan

let incorporate_stmt ?(old = (Schema.empty_db (), [])) (rel, query) =
   let (old_rels,old_queries) = old in
   begin match rel with 
      | Some(name, schema, reltype, source, adaptor) ->
         Schema.add_rel old_rels ~source:source ~adaptor:adaptor
                        (name, schema, reltype)
      | None    -> ()
   end;
   (old_rels, query @ old_queries)
;;

type map_metadata = 
   MapIsQuery | MapIsPartial | MapInitializedAtStart

# 84 "src/parsers/Calculusparser.ml"
let yytransl_const = [|
  263 (* EQ *);
  264 (* NEQ *);
  265 (* LT *);
  266 (* LTE *);
  267 (* GT *);
  268 (* GTE *);
  269 (* VARCHAR *);
  270 (* CHAR *);
  271 (* DATE *);
    0 (* EOF *);
  272 (* EOSTMT *);
  273 (* CREATE *);
  274 (* TABLE *);
  275 (* STREAM *);
  276 (* LPAREN *);
  277 (* RPAREN *);
  278 (* LBRACKET *);
  279 (* RBRACKET *);
  280 (* LBRACE *);
  281 (* RBRACE *);
  282 (* COMMA *);
  283 (* COLON *);
  284 (* PLUS *);
  285 (* TIMES *);
  286 (* MINUS *);
  287 (* DIVIDE *);
  288 (* POUND *);
  289 (* AS *);
  290 (* FROM *);
  291 (* ON *);
  292 (* DO *);
  293 (* SYSTEM *);
  294 (* READY *);
  295 (* DECLARE *);
  296 (* QUERY *);
  297 (* MAP *);
  298 (* PARTIAL *);
  299 (* INITIALIZED *);
  300 (* FILE *);
  301 (* SOCKET *);
  302 (* FIXEDWIDTH *);
  303 (* LINE *);
  304 (* DELIMITED *);
  305 (* AGGSUM *);
  306 (* LIFT *);
  307 (* SETVALUE *);
  308 (* INCREMENT *);
  309 (* EXISTS *);
  310 (* CORRECT *);
  311 (* WITH *);
  312 (* FOR *);
    0|]

let yytransl_block = [|
  257 (* TYPE *);
  258 (* ID *);
  259 (* STRING *);
  260 (* INT *);
  261 (* FLOAT *);
  262 (* BOOL *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\006\000\006\000\007\000\007\000\009\000\
\009\000\010\000\010\000\012\000\012\000\011\000\011\000\011\000\
\014\000\015\000\015\000\015\000\016\000\016\000\016\000\017\000\
\017\000\013\000\013\000\013\000\013\000\013\000\013\000\008\000\
\018\000\018\000\019\000\019\000\020\000\020\000\020\000\020\000\
\020\000\020\000\021\000\021\000\021\000\002\000\005\000\005\000\
\005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
\005\000\005\000\005\000\028\000\028\000\028\000\028\000\028\000\
\028\000\026\000\026\000\027\000\027\000\031\000\031\000\030\000\
\030\000\022\000\022\000\022\000\022\000\022\000\025\000\025\000\
\029\000\029\000\023\000\023\000\024\000\024\000\003\000\032\000\
\032\000\033\000\033\000\033\000\033\000\034\000\037\000\037\000\
\037\000\037\000\036\000\035\000\038\000\038\000\038\000\038\000\
\038\000\040\000\039\000\039\000\004\000\041\000\041\000\000\000\
\000\000\000\000\000\000"

let yylen = "\002\000\
\002\000\003\000\003\000\001\000\001\000\006\000\008\000\001\000\
\001\000\000\000\001\000\002\000\004\000\003\000\004\000\003\000\
\002\000\002\000\002\000\002\000\003\000\004\000\001\000\003\000\
\005\000\001\000\004\000\004\000\001\000\001\000\001\000\005\000\
\000\000\001\000\001\000\003\000\003\000\003\000\003\000\003\000\
\002\000\001\000\001\000\001\000\001\000\001\000\003\000\003\000\
\003\000\003\000\002\000\003\000\001\000\008\000\001\000\001\000\
\005\000\005\000\004\000\001\000\001\000\001\000\001\000\001\000\
\001\000\003\000\004\000\001\000\005\000\001\000\000\000\007\000\
\010\000\001\000\001\000\001\000\001\000\004\000\000\000\001\000\
\001\000\003\000\003\000\001\000\008\000\008\000\002\000\002\000\
\001\000\002\000\002\000\001\000\002\000\006\000\000\000\002\000\
\002\000\002\000\005\000\004\000\003\000\003\000\003\000\012\000\
\009\000\001\000\003\000\000\000\003\000\001\000\001\000\002\000\
\002\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\112\000\
\000\000\004\000\005\000\000\000\077\000\075\000\076\000\074\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\113\000\
\000\000\053\000\043\000\044\000\045\000\055\000\056\000\000\000\
\000\000\000\000\000\000\114\000\000\000\000\000\000\000\000\000\
\092\000\000\000\000\000\000\000\115\000\000\000\008\000\009\000\
\000\000\000\000\001\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\042\000\000\000\000\000\000\000\000\000\000\000\000\000\070\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\090\000\087\000\088\000\091\000\093\000\000\000\000\000\
\110\000\111\000\000\000\000\000\000\000\002\000\003\000\026\000\
\000\000\000\000\031\000\066\000\000\000\000\000\000\000\000\000\
\080\000\083\000\000\000\047\000\000\000\000\000\000\000\000\000\
\000\000\060\000\061\000\062\000\063\000\064\000\065\000\052\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\106\000\101\000\102\000\103\000\000\000\
\000\000\096\000\097\000\098\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\067\000\
\000\000\078\000\000\000\000\000\000\000\037\000\000\000\000\000\
\000\000\000\000\000\000\059\000\000\000\000\000\000\000\000\000\
\000\000\000\000\100\000\000\000\000\000\011\000\032\000\000\000\
\000\000\000\000\082\000\000\000\058\000\000\000\000\000\057\000\
\000\000\069\000\000\000\000\000\000\000\107\000\000\000\000\000\
\028\000\027\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\072\000\000\000\034\000\
\000\000\000\000\000\000\000\000\000\000\013\000\000\000\000\000\
\007\000\000\000\085\000\000\000\086\000\054\000\000\000\000\000\
\000\000\000\000\000\000\000\000\036\000\000\000\105\000\000\000\
\000\000\000\000\014\000\000\000\000\000\016\000\073\000\000\000\
\020\000\018\000\019\000\000\000\017\000\015\000\000\000\000\000\
\104\000\000\000\021\000\000\000\000\000\022\000\000\000\000\000\
\025\000"

let yydgoto = "\005\000\
\008\000\024\000\036\000\143\000\025\000\009\000\010\000\011\000\
\049\000\173\000\217\000\174\000\101\000\235\000\236\000\245\000\
\252\000\207\000\208\000\209\000\026\000\027\000\028\000\029\000\
\104\000\030\000\031\000\124\000\105\000\032\000\073\000\038\000\
\039\000\040\000\041\000\042\000\138\000\043\000\144\000\133\000\
\091\000"

let yysindex = "\100\001\
\041\255\141\255\040\255\019\255\000\000\074\255\255\254\000\000\
\003\000\000\000\000\000\150\255\000\000\000\000\000\000\000\000\
\054\255\141\255\006\255\112\255\141\255\079\255\086\255\000\000\
\077\000\000\000\000\000\000\000\000\000\000\000\000\000\061\255\
\072\255\133\255\118\255\000\000\107\255\126\000\040\255\148\255\
\000\000\169\255\145\255\083\255\000\000\030\255\000\000\000\000\
\190\255\191\255\000\000\011\000\153\255\196\255\138\255\186\255\
\068\255\149\255\180\255\184\255\193\255\112\255\112\255\194\255\
\000\000\077\000\203\255\141\255\141\255\141\255\141\255\000\000\
\235\255\224\255\224\255\208\255\013\255\139\255\139\255\218\255\
\234\255\000\000\000\000\000\000\000\000\000\000\019\255\138\255\
\000\000\000\000\141\255\240\255\219\255\000\000\000\000\000\000\
\251\255\252\255\000\000\000\000\015\000\026\000\032\000\041\000\
\000\000\000\000\034\000\000\000\141\255\138\255\138\255\100\255\
\082\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\112\255\112\255\112\255\112\255\196\255\046\000\077\000\077\000\
\077\000\141\255\048\000\000\000\000\000\000\000\000\000\025\000\
\139\255\000\000\000\000\000\000\019\255\196\255\063\000\053\000\
\077\000\079\000\141\255\078\000\089\000\065\000\196\255\000\000\
\086\000\000\000\167\255\092\000\093\000\000\000\082\000\082\000\
\082\000\220\255\094\000\000\000\200\255\069\255\141\255\067\000\
\096\000\019\255\000\000\138\255\099\000\000\000\000\000\100\000\
\101\000\196\255\000\000\196\255\000\000\103\000\104\000\000\000\
\102\000\000\000\077\000\141\255\008\255\000\000\105\000\091\000\
\000\000\000\000\106\000\107\000\112\255\112\255\141\255\077\000\
\196\255\196\255\079\000\096\255\110\000\000\000\112\000\000\000\
\062\000\113\000\233\255\114\000\070\000\000\000\124\000\146\255\
\000\000\196\255\000\000\112\255\000\000\000\000\084\000\243\254\
\020\255\131\000\020\255\115\000\000\000\196\255\000\000\095\000\
\135\000\097\000\000\000\138\000\020\255\000\000\000\000\085\000\
\000\000\000\000\000\000\122\000\000\000\000\000\243\254\070\255\
\000\000\098\000\000\000\123\000\143\000\000\000\121\000\146\000\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\044\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\002\000\000\000\000\000\000\000\000\000\000\000\000\000\018\000\
\000\000\109\000\000\000\000\000\000\000\000\000\151\001\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\129\000\000\000\000\000\
\000\000\056\000\000\000\000\000\017\255\000\000\000\000\000\000\
\000\000\004\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\109\000\109\000\109\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\128\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\001\000\033\000\000\000\000\000\000\000\187\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\206\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\129\000\000\000\016\000\050\000\
\059\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\109\000\000\000\000\000\000\000\000\000\129\000\000\000\000\000\
\005\000\133\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\226\255\232\255\
\088\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\128\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\129\000\000\000\129\000\000\000\000\000\000\000\000\000\
\000\000\000\000\139\000\000\000\000\000\000\000\136\000\017\000\
\000\000\000\000\000\000\000\000\137\000\137\000\000\000\140\000\
\129\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\141\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\129\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\019\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\142\000\000\000\
\000\000"

let yygindex = "\000\000\
\107\001\195\255\000\000\156\001\248\255\000\000\045\000\000\000\
\000\000\000\000\000\000\214\000\215\255\097\255\000\000\000\000\
\164\000\223\000\202\000\014\000\245\255\000\000\238\255\000\000\
\163\255\083\000\008\000\000\000\209\255\026\001\000\000\129\001\
\000\000\000\000\000\000\000\000\242\255\063\255\255\000\095\001\
\000\000"

let yytablesize = 426
let yytable = "\058\000\
\029\000\046\000\051\000\051\000\109\000\103\000\126\000\059\000\
\065\000\057\000\094\000\046\000\066\000\106\000\136\000\049\000\
\006\000\068\000\023\000\080\000\044\000\033\000\232\000\084\000\
\084\000\084\000\084\000\084\000\084\000\201\000\231\000\163\000\
\030\000\064\000\102\000\102\000\060\000\084\000\050\000\084\000\
\035\000\084\000\084\000\084\000\084\000\084\000\084\000\037\000\
\169\000\048\000\065\000\065\000\137\000\249\000\078\000\079\000\
\006\000\006\000\050\000\202\000\127\000\128\000\129\000\139\000\
\140\000\233\000\234\000\238\000\156\000\157\000\061\000\250\000\
\084\000\056\000\033\000\112\000\113\000\246\000\034\000\007\000\
\089\000\090\000\145\000\037\000\195\000\175\000\196\000\072\000\
\108\000\100\000\251\000\047\000\048\000\035\000\046\000\069\000\
\070\000\071\000\067\000\074\000\155\000\075\000\088\000\179\000\
\054\000\068\000\102\000\212\000\076\000\065\000\065\000\065\000\
\065\000\061\000\013\000\014\000\015\000\016\000\103\000\081\000\
\158\000\165\000\082\000\102\000\228\000\083\000\017\000\121\000\
\122\000\123\000\191\000\062\000\102\000\019\000\159\000\160\000\
\161\000\162\000\096\000\215\000\216\000\063\000\012\000\013\000\
\014\000\015\000\016\000\102\000\226\000\227\000\097\000\098\000\
\099\000\096\000\061\000\017\000\132\000\132\000\187\000\102\000\
\018\000\102\000\019\000\085\000\020\000\097\000\098\000\099\000\
\087\000\053\000\021\000\054\000\077\000\100\000\078\000\079\000\
\055\000\046\000\137\000\200\000\078\000\079\000\102\000\213\000\
\086\000\065\000\065\000\181\000\107\000\022\000\211\000\092\000\
\093\000\023\000\069\000\070\000\071\000\061\000\109\000\102\000\
\114\000\115\000\116\000\117\000\118\000\119\000\110\000\081\000\
\065\000\081\000\111\000\240\000\041\000\041\000\041\000\041\000\
\041\000\041\000\120\000\055\000\186\000\121\000\122\000\123\000\
\125\000\131\000\041\000\069\000\070\000\071\000\041\000\041\000\
\039\000\039\000\039\000\039\000\039\000\039\000\038\000\038\000\
\038\000\038\000\038\000\038\000\184\000\135\000\039\000\121\000\
\122\000\123\000\039\000\039\000\038\000\222\000\130\000\142\000\
\038\000\038\000\141\000\146\000\069\000\070\000\071\000\029\000\
\029\000\029\000\029\000\029\000\029\000\147\000\148\000\149\000\
\029\000\046\000\052\000\051\000\109\000\029\000\046\000\029\000\
\051\000\029\000\029\000\006\000\029\000\029\000\029\000\049\000\
\006\000\068\000\023\000\150\000\049\000\071\000\068\000\030\000\
\030\000\030\000\030\000\030\000\030\000\068\000\068\000\068\000\
\030\000\007\000\029\000\151\000\152\000\030\000\154\000\030\000\
\029\000\030\000\030\000\084\000\030\000\030\000\030\000\153\000\
\084\000\048\000\164\000\166\000\068\000\068\000\048\000\084\000\
\084\000\084\000\050\000\167\000\044\000\171\000\170\000\050\000\
\172\000\176\000\030\000\044\000\044\000\044\000\178\000\220\000\
\030\000\121\000\122\000\123\000\177\000\084\000\040\000\040\000\
\040\000\040\000\040\000\040\000\001\000\002\000\003\000\004\000\
\069\000\070\000\071\000\180\000\040\000\121\000\122\000\123\000\
\040\000\040\000\182\000\183\000\185\000\188\000\189\000\192\000\
\193\000\194\000\197\000\198\000\204\000\224\000\225\000\199\000\
\205\000\206\000\203\000\218\000\219\000\221\000\237\000\230\000\
\223\000\239\000\242\000\244\000\247\000\248\000\241\000\254\000\
\243\000\255\000\000\001\250\000\253\000\095\000\089\000\079\000\
\108\000\010\000\099\000\094\000\012\000\033\000\095\000\045\000\
\214\000\035\000\024\000\001\001\210\000\229\000\168\000\084\000\
\190\000\134\000"

let yycheck = "\018\000\
\000\000\000\000\000\000\000\000\000\000\053\000\068\000\002\001\
\020\000\018\000\000\000\004\000\021\000\055\000\002\001\000\000\
\000\000\000\000\000\000\034\000\002\001\035\001\003\001\007\001\
\008\001\009\001\010\001\011\001\012\001\022\001\224\000\125\000\
\000\000\020\000\053\000\054\000\031\001\021\001\040\001\023\001\
\054\001\025\001\026\001\000\000\028\001\029\001\030\001\003\000\
\142\000\000\000\062\000\063\000\040\001\247\000\042\001\043\001\
\017\001\017\001\000\000\052\001\069\000\070\000\071\000\078\000\
\079\000\046\001\047\001\227\000\110\000\111\000\002\001\002\001\
\056\001\020\001\035\001\062\000\063\000\237\000\039\001\039\001\
\051\001\052\001\091\000\039\000\178\000\147\000\180\000\027\001\
\021\001\021\001\021\001\018\001\019\001\054\001\087\000\028\001\
\029\001\030\001\020\001\028\001\109\000\030\001\020\001\151\000\
\022\001\020\001\125\000\201\000\037\001\121\000\122\000\123\000\
\124\000\002\001\003\001\004\001\005\001\006\001\166\000\002\001\
\021\001\130\000\016\001\142\000\218\000\000\000\015\001\028\001\
\029\001\030\001\172\000\020\001\151\000\022\001\121\000\122\000\
\123\000\124\000\001\001\044\001\045\001\030\001\002\001\003\001\
\004\001\005\001\006\001\166\000\003\001\004\001\013\001\014\001\
\015\001\001\001\002\001\015\001\074\000\075\000\167\000\178\000\
\020\001\180\000\022\001\016\001\024\001\013\001\014\001\015\001\
\024\001\020\001\030\001\022\001\040\001\021\001\042\001\043\001\
\027\001\170\000\040\001\188\000\042\001\043\001\201\000\202\000\
\016\001\197\000\198\000\021\001\003\001\049\001\199\000\002\001\
\002\001\053\001\028\001\029\001\030\001\002\001\050\001\218\000\
\007\001\008\001\009\001\010\001\011\001\012\001\027\001\021\001\
\220\000\023\001\027\001\230\000\007\001\008\001\009\001\010\001\
\011\001\012\001\025\001\027\001\021\001\028\001\029\001\030\001\
\022\001\002\001\021\001\028\001\029\001\030\001\025\001\026\001\
\007\001\008\001\009\001\010\001\011\001\012\001\007\001\008\001\
\009\001\010\001\011\001\012\001\025\001\038\001\021\001\028\001\
\029\001\030\001\025\001\026\001\021\001\021\001\020\001\022\001\
\025\001\026\001\041\001\020\001\028\001\029\001\030\001\007\001\
\008\001\009\001\010\001\011\001\012\001\051\001\020\001\020\001\
\016\001\016\001\016\001\016\001\016\001\021\001\021\001\023\001\
\021\001\025\001\026\001\017\001\028\001\029\001\030\001\016\001\
\016\001\016\001\016\001\021\001\021\001\020\001\021\001\007\001\
\008\001\009\001\010\001\011\001\012\001\028\001\029\001\030\001\
\016\001\039\001\050\001\026\001\021\001\021\001\021\001\023\001\
\056\001\025\001\026\001\016\001\028\001\029\001\030\001\023\001\
\021\001\016\001\021\001\020\001\051\001\052\001\021\001\028\001\
\029\001\030\001\016\001\051\001\021\001\025\001\016\001\021\001\
\002\001\004\001\050\001\028\001\029\001\030\001\022\001\026\001\
\056\001\028\001\029\001\030\001\004\001\050\001\007\001\008\001\
\009\001\010\001\011\001\012\001\001\000\002\000\003\000\004\000\
\028\001\029\001\030\001\022\001\021\001\028\001\029\001\030\001\
\025\001\026\001\023\001\023\001\023\001\051\001\023\001\021\001\
\021\001\021\001\020\001\020\001\034\001\056\001\003\001\026\001\
\023\001\023\001\026\001\022\001\021\001\021\001\004\001\052\001\
\023\001\023\001\004\001\002\001\056\001\020\001\048\001\021\001\
\048\001\003\001\026\001\002\001\051\001\041\001\000\000\023\001\
\025\001\021\001\016\001\016\001\021\001\021\001\052\000\004\000\
\203\000\021\001\021\001\000\001\198\000\220\000\141\000\039\000\
\170\000\075\000"

let yynames_const = "\
  EQ\000\
  NEQ\000\
  LT\000\
  LTE\000\
  GT\000\
  GTE\000\
  VARCHAR\000\
  CHAR\000\
  DATE\000\
  EOF\000\
  EOSTMT\000\
  CREATE\000\
  TABLE\000\
  STREAM\000\
  LPAREN\000\
  RPAREN\000\
  LBRACKET\000\
  RBRACKET\000\
  LBRACE\000\
  RBRACE\000\
  COMMA\000\
  COLON\000\
  PLUS\000\
  TIMES\000\
  MINUS\000\
  DIVIDE\000\
  POUND\000\
  AS\000\
  FROM\000\
  ON\000\
  DO\000\
  SYSTEM\000\
  READY\000\
  DECLARE\000\
  QUERY\000\
  MAP\000\
  PARTIAL\000\
  INITIALIZED\000\
  FILE\000\
  SOCKET\000\
  FIXEDWIDTH\000\
  LINE\000\
  DELIMITED\000\
  AGGSUM\000\
  LIFT\000\
  SETVALUE\000\
  INCREMENT\000\
  EXISTS\000\
  CORRECT\000\
  WITH\000\
  FOR\000\
  "

let yynames_block = "\
  TYPE\000\
  ID\000\
  STRING\000\
  INT\000\
  FLOAT\000\
  BOOL\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'statement) in
    Obj.repr(
# 53 "src/parsers/Calculusparser.mly"
                                   ( incorporate_stmt _1 )
# 486 "src/parsers/Calculusparser.ml"
               :  Schema.t * (string * Calculus.expr_t) list ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'statement) in
    Obj.repr(
# 54 "src/parsers/Calculusparser.mly"
                                   ( incorporate_stmt _1 )
# 493 "src/parsers/Calculusparser.ml"
               :  Schema.t * (string * Calculus.expr_t) list ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'statement) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 :  Schema.t * (string * Calculus.expr_t) list ) in
    Obj.repr(
# 55 "src/parsers/Calculusparser.mly"
                                   ( incorporate_stmt ~old:_3 _1 )
# 501 "src/parsers/Calculusparser.ml"
               :  Schema.t * (string * Calculus.expr_t) list ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'relStatement) in
    Obj.repr(
# 58 "src/parsers/Calculusparser.mly"
                              ( (Some(_1), []) )
# 508 "src/parsers/Calculusparser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'queryStatement) in
    Obj.repr(
# 59 "src/parsers/Calculusparser.mly"
                              ( (None,     [_1]) )
# 515 "src/parsers/Calculusparser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'tableOrStream) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'emptyFieldList) in
    Obj.repr(
# 63 "src/parsers/Calculusparser.mly"
   ( (String.uppercase _3, _5, _2, Schema.NoSource, ("",[])) )
# 524 "src/parsers/Calculusparser.ml"
               : 'relStatement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : 'tableOrStream) in
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : 'emptyFieldList) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'sourceStmt) in
    Obj.repr(
# 65 "src/parsers/Calculusparser.mly"
   ( (String.uppercase _3, _5, _2, fst _8, snd _8) )
# 534 "src/parsers/Calculusparser.ml"
               : 'relStatement))
; (fun __caml_parser_env ->
    Obj.repr(
# 68 "src/parsers/Calculusparser.mly"
         ( Schema.TableRel  )
# 540 "src/parsers/Calculusparser.ml"
               : 'tableOrStream))
; (fun __caml_parser_env ->
    Obj.repr(
# 69 "src/parsers/Calculusparser.mly"
         ( Schema.StreamRel )
# 546 "src/parsers/Calculusparser.ml"
               : 'tableOrStream))
; (fun __caml_parser_env ->
    Obj.repr(
# 72 "src/parsers/Calculusparser.mly"
                              ( [] )
# 552 "src/parsers/Calculusparser.ml"
               : 'emptyFieldList))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'fieldList) in
    Obj.repr(
# 73 "src/parsers/Calculusparser.mly"
                              ( _1 )
# 559 "src/parsers/Calculusparser.ml"
               : 'emptyFieldList))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'dbtType) in
    Obj.repr(
# 76 "src/parsers/Calculusparser.mly"
                              ( [_1,_2] )
# 567 "src/parsers/Calculusparser.ml"
               : 'fieldList))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'dbtType) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'fieldList) in
    Obj.repr(
# 77 "src/parsers/Calculusparser.mly"
                              ( (_1,_2)::_4 )
# 576 "src/parsers/Calculusparser.ml"
               : 'fieldList))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'bytestreamParams) in
    Obj.repr(
# 81 "src/parsers/Calculusparser.mly"
  ( (Schema.FileSource(_2, fst _3), snd _3) )
# 584 "src/parsers/Calculusparser.ml"
               : 'sourceStmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'bytestreamParams) in
    Obj.repr(
# 83 "src/parsers/Calculusparser.mly"
  ( (Schema.SocketSource(Unix.inet_addr_of_string _2, _3, fst _4), snd _4) )
# 593 "src/parsers/Calculusparser.ml"
               : 'sourceStmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'bytestreamParams) in
    Obj.repr(
# 85 "src/parsers/Calculusparser.mly"
  ( (Schema.SocketSource(Unix.inet_addr_any, _2, fst _3), snd _3) )
# 601 "src/parsers/Calculusparser.ml"
               : 'sourceStmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'framingStmt) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'adaptorStmt) in
    Obj.repr(
# 88 "src/parsers/Calculusparser.mly"
                            ( (_1, _2) )
# 609 "src/parsers/Calculusparser.ml"
               : 'bytestreamParams))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 91 "src/parsers/Calculusparser.mly"
                                    ( Schema.FixedSize(_2) )
# 616 "src/parsers/Calculusparser.ml"
               : 'framingStmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 92 "src/parsers/Calculusparser.mly"
                                    ( Schema.Delimited("\n") )
# 622 "src/parsers/Calculusparser.ml"
               : 'framingStmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 93 "src/parsers/Calculusparser.mly"
                                    ( Schema.Delimited(_1) )
# 629 "src/parsers/Calculusparser.ml"
               : 'framingStmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 96 "src/parsers/Calculusparser.mly"
                                   ( (String.lowercase _1, []) )
# 636 "src/parsers/Calculusparser.ml"
               : 'adaptorStmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'adaptorParams) in
    Obj.repr(
# 97 "src/parsers/Calculusparser.mly"
                                   ( (String.lowercase _1, _3) )
# 644 "src/parsers/Calculusparser.ml"
               : 'adaptorStmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 98 "src/parsers/Calculusparser.mly"
                                   ( (String.lowercase _1, []) )
# 651 "src/parsers/Calculusparser.ml"
               : 'adaptorStmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 101 "src/parsers/Calculusparser.mly"
                                           ( [(String.lowercase _1,_3)] )
# 659 "src/parsers/Calculusparser.ml"
               : 'adaptorParams))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'adaptorParams) in
    Obj.repr(
# 102 "src/parsers/Calculusparser.mly"
                                           ( (String.lowercase _1,_3)::_5 )
# 668 "src/parsers/Calculusparser.ml"
               : 'adaptorParams))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Type.type_t) in
    Obj.repr(
# 105 "src/parsers/Calculusparser.mly"
                            ( _1 )
# 675 "src/parsers/Calculusparser.ml"
               : 'dbtType))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 106 "src/parsers/Calculusparser.mly"
                            ( TString )
# 682 "src/parsers/Calculusparser.ml"
               : 'dbtType))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 107 "src/parsers/Calculusparser.mly"
                            ( TString )
# 689 "src/parsers/Calculusparser.ml"
               : 'dbtType))
; (fun __caml_parser_env ->
    Obj.repr(
# 108 "src/parsers/Calculusparser.mly"
                            ( TString )
# 695 "src/parsers/Calculusparser.ml"
               : 'dbtType))
; (fun __caml_parser_env ->
    Obj.repr(
# 109 "src/parsers/Calculusparser.mly"
                            ( TString )
# 701 "src/parsers/Calculusparser.ml"
               : 'dbtType))
; (fun __caml_parser_env ->
    Obj.repr(
# 110 "src/parsers/Calculusparser.mly"
                            ( TDate    )
# 707 "src/parsers/Calculusparser.ml"
               : 'dbtType))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 :  Calculus.expr_t ) in
    Obj.repr(
# 113 "src/parsers/Calculusparser.mly"
                                         ( (_3, _5) )
# 715 "src/parsers/Calculusparser.ml"
               : 'queryStatement))
; (fun __caml_parser_env ->
    Obj.repr(
# 116 "src/parsers/Calculusparser.mly"
                                 ( [] )
# 721 "src/parsers/Calculusparser.ml"
               : 'emptyValueExprList))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'valueExprList) in
    Obj.repr(
# 117 "src/parsers/Calculusparser.mly"
                                 ( _1 )
# 728 "src/parsers/Calculusparser.ml"
               : 'emptyValueExprList))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'valueExpr) in
    Obj.repr(
# 120 "src/parsers/Calculusparser.mly"
                                 ( [_1] )
# 735 "src/parsers/Calculusparser.ml"
               : 'valueExprList))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'valueExpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'valueExprList) in
    Obj.repr(
# 121 "src/parsers/Calculusparser.mly"
                                 ( _1::_3)
# 743 "src/parsers/Calculusparser.ml"
               : 'valueExprList))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'valueExpr) in
    Obj.repr(
# 124 "src/parsers/Calculusparser.mly"
                              ( _2 )
# 750 "src/parsers/Calculusparser.ml"
               : 'valueExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'valueExpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'valueExpr) in
    Obj.repr(
# 125 "src/parsers/Calculusparser.mly"
                              ( ValueRing.mk_prod [_1; _3] )
# 758 "src/parsers/Calculusparser.ml"
               : 'valueExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'valueExpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'valueExpr) in
    Obj.repr(
# 126 "src/parsers/Calculusparser.mly"
                              ( ValueRing.mk_sum  [_1; _3] )
# 766 "src/parsers/Calculusparser.ml"
               : 'valueExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'valueExpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'valueExpr) in
    Obj.repr(
# 127 "src/parsers/Calculusparser.mly"
                              ( ValueRing.mk_sum  [_1; ValueRing.mk_neg _3] )
# 774 "src/parsers/Calculusparser.ml"
               : 'valueExpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'valueExpr) in
    Obj.repr(
# 128 "src/parsers/Calculusparser.mly"
                              ( match _2 with
                                | ValueRing.Val(AConst(c)) ->
                                     Arithmetic.mk_const
                                        (Constants.Math.prod c (CInt(-1)))  
                                | _ -> ValueRing.mk_neg _2 )
# 785 "src/parsers/Calculusparser.ml"
               : 'valueExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'valueLeaf) in
    Obj.repr(
# 133 "src/parsers/Calculusparser.mly"
                              ( _1 )
# 792 "src/parsers/Calculusparser.ml"
               : 'valueExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'constant) in
    Obj.repr(
# 136 "src/parsers/Calculusparser.mly"
                              ( Arithmetic.mk_const _1 )
# 799 "src/parsers/Calculusparser.ml"
               : 'valueLeaf))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'variable) in
    Obj.repr(
# 137 "src/parsers/Calculusparser.mly"
                              ( Arithmetic.mk_var _1 )
# 806 "src/parsers/Calculusparser.ml"
               : 'valueLeaf))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'functionDefn) in
    Obj.repr(
# 138 "src/parsers/Calculusparser.mly"
                              ( ValueRing.mk_val _1 )
# 813 "src/parsers/Calculusparser.ml"
               : 'valueLeaf))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 :  Calculus.expr_t ) in
    Obj.repr(
# 141 "src/parsers/Calculusparser.mly"
                              ( (* Calculus.strip_calc_metadata *) _1 )
# 820 "src/parsers/Calculusparser.ml"
               :  Calculus.expr_t ))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 :  Calculus.expr_t ) in
    Obj.repr(
# 144 "src/parsers/Calculusparser.mly"
                                  ( _2 )
# 827 "src/parsers/Calculusparser.ml"
               :  Calculus.expr_t ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 :  Calculus.expr_t ) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 :  Calculus.expr_t ) in
    Obj.repr(
# 146 "src/parsers/Calculusparser.mly"
                                  ( CalcRing.mk_prod [_1; _3] )
# 835 "src/parsers/Calculusparser.ml"
               :  Calculus.expr_t ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 :  Calculus.expr_t ) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 :  Calculus.expr_t ) in
    Obj.repr(
# 148 "src/parsers/Calculusparser.mly"
                                  ( CalcRing.mk_sum [_1; _3] )
# 843 "src/parsers/Calculusparser.ml"
               :  Calculus.expr_t ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 :  Calculus.expr_t ) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 :  Calculus.expr_t ) in
    Obj.repr(
# 150 "src/parsers/Calculusparser.mly"
                                  ( CalcRing.mk_sum [_1; CalcRing.mk_neg _3] )
# 851 "src/parsers/Calculusparser.ml"
               :  Calculus.expr_t ))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 :  Calculus.expr_t ) in
    Obj.repr(
# 151 "src/parsers/Calculusparser.mly"
                                  ( CalcRing.mk_neg _2 )
# 858 "src/parsers/Calculusparser.ml"
               :  Calculus.expr_t ))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'valueExpr) in
    Obj.repr(
# 152 "src/parsers/Calculusparser.mly"
                                  ( Calculus.mk_value _2 )
# 865 "src/parsers/Calculusparser.ml"
               :  Calculus.expr_t ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'valueLeaf) in
    Obj.repr(
# 153 "src/parsers/Calculusparser.mly"
                                  ( Calculus.mk_value _1 )
# 872 "src/parsers/Calculusparser.ml"
               :  Calculus.expr_t ))
; (fun __caml_parser_env ->
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'emptyVariableList) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 :  Calculus.expr_t ) in
    Obj.repr(
# 155 "src/parsers/Calculusparser.mly"
                                  ( Calculus.mk_aggsum _4 _7 )
# 880 "src/parsers/Calculusparser.ml"
               :  Calculus.expr_t ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'relationDefn) in
    Obj.repr(
# 156 "src/parsers/Calculusparser.mly"
                                  ( let (reln, relv) = _1 in
                                    Calculus.mk_rel reln relv )
# 888 "src/parsers/Calculusparser.ml"
               :  Calculus.expr_t ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'externalDefn) in
    Obj.repr(
# 158 "src/parsers/Calculusparser.mly"
                                  ( let (en, iv, ov, et, em) = _1 in
                                    Calculus.mk_external en iv ov et em )
# 896 "src/parsers/Calculusparser.ml"
               :  Calculus.expr_t ))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'valueExpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'comparison) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'valueExpr) in
    Obj.repr(
# 161 "src/parsers/Calculusparser.mly"
                                  ( Calculus.mk_cmp _3 _2 _4 )
# 905 "src/parsers/Calculusparser.ml"
               :  Calculus.expr_t ))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'variable) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 :  Calculus.expr_t ) in
    Obj.repr(
# 163 "src/parsers/Calculusparser.mly"
  ( let (var_n, var_t) = _2 in 
   let (e_type) = 
      if Debug.active "PARSE-CALC-WITH-FLOAT-VARS" then var_t
      else Calculus.type_of_expr _4 in
   let target = (var_n, e_type) in
      Calculus.mk_lift target _4 )
# 918 "src/parsers/Calculusparser.ml"
               :  Calculus.expr_t ))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 :  Calculus.expr_t ) in
    Obj.repr(
# 170 "src/parsers/Calculusparser.mly"
                                    ( Calculus.mk_exists _3 )
# 925 "src/parsers/Calculusparser.ml"
               :  Calculus.expr_t ))
; (fun __caml_parser_env ->
    Obj.repr(
# 174 "src/parsers/Calculusparser.mly"
      ( Type.Eq  )
# 931 "src/parsers/Calculusparser.ml"
               : 'comparison))
; (fun __caml_parser_env ->
    Obj.repr(
# 174 "src/parsers/Calculusparser.mly"
                         ( Type.Neq )
# 937 "src/parsers/Calculusparser.ml"
               : 'comparison))
; (fun __caml_parser_env ->
    Obj.repr(
# 174 "src/parsers/Calculusparser.mly"
                                            ( Type.Lt  )
# 943 "src/parsers/Calculusparser.ml"
               : 'comparison))
; (fun __caml_parser_env ->
    Obj.repr(
# 175 "src/parsers/Calculusparser.mly"
      ( Type.Lte )
# 949 "src/parsers/Calculusparser.ml"
               : 'comparison))
; (fun __caml_parser_env ->
    Obj.repr(
# 175 "src/parsers/Calculusparser.mly"
                         ( Type.Gt  )
# 955 "src/parsers/Calculusparser.ml"
               : 'comparison))
; (fun __caml_parser_env ->
    Obj.repr(
# 175 "src/parsers/Calculusparser.mly"
                                            ( Type.Gte )
# 961 "src/parsers/Calculusparser.ml"
               : 'comparison))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 178 "src/parsers/Calculusparser.mly"
                                                    ( (_1, []) )
# 968 "src/parsers/Calculusparser.ml"
               : 'relationDefn))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'variableList) in
    Obj.repr(
# 179 "src/parsers/Calculusparser.mly"
                                                    ( (_1, _3) )
# 976 "src/parsers/Calculusparser.ml"
               : 'relationDefn))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'externalDefnWithoutMeta) in
    Obj.repr(
# 182 "src/parsers/Calculusparser.mly"
                          ( _1 )
# 983 "src/parsers/Calculusparser.ml"
               : 'externalDefn))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'externalDefnWithoutMeta) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'optionalColon) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 :  Calculus.expr_t ) in
    Obj.repr(
# 183 "src/parsers/Calculusparser.mly"
                                                                      (
      let (name, ivars, ovars, extType, _) = _1 in
         (name, ivars, ovars, extType, (Some(_4)))
   )
# 995 "src/parsers/Calculusparser.ml"
               : 'externalDefn))
; (fun __caml_parser_env ->
    Obj.repr(
# 189 "src/parsers/Calculusparser.mly"
        ( () )
# 1001 "src/parsers/Calculusparser.ml"
               : 'optionalColon))
; (fun __caml_parser_env ->
    Obj.repr(
# 190 "src/parsers/Calculusparser.mly"
        ( () )
# 1007 "src/parsers/Calculusparser.ml"
               : 'optionalColon))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'emptyVariableList) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'emptyVariableList) in
    Obj.repr(
# 194 "src/parsers/Calculusparser.mly"
                                 ( (_1, _3, _6, TFloat, None) )
# 1016 "src/parsers/Calculusparser.ml"
               : 'externalDefnWithoutMeta))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 9 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 7 : 'dbtType) in
    let _6 = (Parsing.peek_val __caml_parser_env 4 : 'emptyVariableList) in
    let _9 = (Parsing.peek_val __caml_parser_env 1 : 'emptyVariableList) in
    Obj.repr(
# 196 "src/parsers/Calculusparser.mly"
                                 ( (_1, _6, _9, _3, None) )
# 1026 "src/parsers/Calculusparser.ml"
               : 'externalDefnWithoutMeta))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 199 "src/parsers/Calculusparser.mly"
            ( CBool(_1) )
# 1033 "src/parsers/Calculusparser.ml"
               : 'constant))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 200 "src/parsers/Calculusparser.mly"
            ( CInt(_1) )
# 1040 "src/parsers/Calculusparser.ml"
               : 'constant))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 201 "src/parsers/Calculusparser.mly"
            ( CFloat(_1) )
# 1047 "src/parsers/Calculusparser.ml"
               : 'constant))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 202 "src/parsers/Calculusparser.mly"
            ( CString(_1) )
# 1054 "src/parsers/Calculusparser.ml"
               : 'constant))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 203 "src/parsers/Calculusparser.mly"
                                ( Constants.parse_date _3 )
# 1061 "src/parsers/Calculusparser.ml"
               : 'constant))
; (fun __caml_parser_env ->
    Obj.repr(
# 206 "src/parsers/Calculusparser.mly"
                              ( [] )
# 1067 "src/parsers/Calculusparser.ml"
               : 'emptyVariableList))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'variableList) in
    Obj.repr(
# 207 "src/parsers/Calculusparser.mly"
                              ( _1 )
# 1074 "src/parsers/Calculusparser.ml"
               : 'emptyVariableList))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'variable) in
    Obj.repr(
# 210 "src/parsers/Calculusparser.mly"
                              ( [_1] )
# 1081 "src/parsers/Calculusparser.ml"
               : 'variableList))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'variable) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'variableList) in
    Obj.repr(
# 211 "src/parsers/Calculusparser.mly"
                              ( _1::_3 )
# 1089 "src/parsers/Calculusparser.ml"
               : 'variableList))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'dbtType) in
    Obj.repr(
# 214 "src/parsers/Calculusparser.mly"
                   ( (_1, _3) )
# 1097 "src/parsers/Calculusparser.ml"
               : 'variable))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 215 "src/parsers/Calculusparser.mly"
                   ( (_1, TFloat) )
# 1104 "src/parsers/Calculusparser.ml"
               : 'variable))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'dbtType) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'emptyValueExprList) in
    Obj.repr(
# 219 "src/parsers/Calculusparser.mly"
         ( AFn(_2, _7, _4) )
# 1113 "src/parsers/Calculusparser.ml"
               : 'functionDefn))
; (fun __caml_parser_env ->
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'dbtType) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'emptyValueExprList) in
    Obj.repr(
# 221 "src/parsers/Calculusparser.mly"
         ( AFn("/", _7, _4) )
# 1121 "src/parsers/Calculusparser.ml"
               : 'functionDefn))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'mapProgramStatementList) in
    Obj.repr(
# 228 "src/parsers/Calculusparser.mly"
                              (
      let (rels, views, triggers, queries) = _1 in
         let db = Schema.empty_db () in (
            List.iter (fun (name, schema, reltype, source, adaptor) -> 
               Schema.add_rel db ~source:source ~adaptor:adaptor 
                                 (name, schema, reltype)
            ) rels;
            let prog = M3.init db in
               List.iter (M3.add_view prog) views;
               List.iter (fun (n,d) -> M3.add_query prog n d) queries;
               List.iter (fun (e,sl) -> 
                  List.iter (fun s -> M3.add_stmt prog e s) sl
               ) triggers;
               prog
         )
   )
# 1143 "src/parsers/Calculusparser.ml"
               :  M3.prog_t ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'mapProgramStatement) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'mapProgramStatementList) in
    Obj.repr(
# 247 "src/parsers/Calculusparser.mly"
      (  let (orels, oviews, otriggers, oqueries) = _2 in
         let (nrels, nviews, ntriggers, nqueries) = _1 in
            (  nrels @ orels,          nviews @ oviews, 
               ntriggers @ otriggers,  nqueries @ oqueries) 
      )
# 1155 "src/parsers/Calculusparser.ml"
               : 'mapProgramStatementList))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'mapProgramStatement) in
    Obj.repr(
# 253 "src/parsers/Calculusparser.mly"
      (  _1 )
# 1162 "src/parsers/Calculusparser.ml"
               : 'mapProgramStatementList))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'relStatement) in
    Obj.repr(
# 256 "src/parsers/Calculusparser.mly"
                      ( ([_1], [], [], []) )
# 1169 "src/parsers/Calculusparser.ml"
               : 'mapProgramStatement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'mapView) in
    Obj.repr(
# 257 "src/parsers/Calculusparser.mly"
                      ( ([], [fst _1], [], snd _1) )
# 1176 "src/parsers/Calculusparser.ml"
               : 'mapProgramStatement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'mapTrigger) in
    Obj.repr(
# 258 "src/parsers/Calculusparser.mly"
                      ( ([], [], [_1], []) )
# 1183 "src/parsers/Calculusparser.ml"
               : 'mapProgramStatement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'mapQuery) in
    Obj.repr(
# 259 "src/parsers/Calculusparser.mly"
                      ( ([], [], [], [_1]) )
# 1190 "src/parsers/Calculusparser.ml"
               : 'mapProgramStatement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'mapViewMetadata) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'externalDefnWithoutMeta) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 :  Calculus.expr_t ) in
    Obj.repr(
# 263 "src/parsers/Calculusparser.mly"
                           ( 
      let (name, ivars, ovars, dtype, meta) = _4 in
      (  {
            Plan.ds_name = Plan.mk_ds_name name (ivars,ovars) dtype;
            Plan.ds_definition = _6
         },
         if List.mem MapIsQuery _2
         then [name, Calculus.mk_external name ivars ovars dtype meta] 
         else []
      )
   )
# 1209 "src/parsers/Calculusparser.ml"
               : 'mapView))
; (fun __caml_parser_env ->
    Obj.repr(
# 276 "src/parsers/Calculusparser.mly"
                              ( [] )
# 1215 "src/parsers/Calculusparser.ml"
               : 'mapViewMetadata))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'mapViewMetadata) in
    Obj.repr(
# 277 "src/parsers/Calculusparser.mly"
                              ( MapIsQuery::_2 )
# 1222 "src/parsers/Calculusparser.ml"
               : 'mapViewMetadata))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'mapViewMetadata) in
    Obj.repr(
# 278 "src/parsers/Calculusparser.mly"
                              ( MapIsPartial::_2 )
# 1229 "src/parsers/Calculusparser.ml"
               : 'mapViewMetadata))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'mapViewMetadata) in
    Obj.repr(
# 279 "src/parsers/Calculusparser.mly"
                              ( MapInitializedAtStart::_2 )
# 1236 "src/parsers/Calculusparser.ml"
               : 'mapViewMetadata))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 :  Calculus.expr_t ) in
    Obj.repr(
# 282 "src/parsers/Calculusparser.mly"
                                            ( (_3, _5) )
# 1244 "src/parsers/Calculusparser.ml"
               : 'mapQuery))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'mapEvent) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'mapTriggerStmtList) in
    Obj.repr(
# 285 "src/parsers/Calculusparser.mly"
                                            ( (_1, _3) )
# 1252 "src/parsers/Calculusparser.ml"
               : 'mapTrigger))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'schemaRelationDefn) in
    Obj.repr(
# 288 "src/parsers/Calculusparser.mly"
                                  ( Schema.InsertEvent(_3) )
# 1259 "src/parsers/Calculusparser.ml"
               : 'mapEvent))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'schemaRelationDefn) in
    Obj.repr(
# 289 "src/parsers/Calculusparser.mly"
                                  ( Schema.DeleteEvent(_3) )
# 1266 "src/parsers/Calculusparser.ml"
               : 'mapEvent))
; (fun __caml_parser_env ->
    Obj.repr(
# 290 "src/parsers/Calculusparser.mly"
                                  ( Schema.SystemInitializedEvent )
# 1272 "src/parsers/Calculusparser.ml"
               : 'mapEvent))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 10 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 8 : 'emptyVariableList) in
    let _7 = (Parsing.peek_val __caml_parser_env 5 : 'emptyVariableList) in
    let _10 = (Parsing.peek_val __caml_parser_env 2 : 'variable) in
    let _12 = (Parsing.peek_val __caml_parser_env 0 : 'mapEvent) in
    Obj.repr(
# 293 "src/parsers/Calculusparser.mly"
                                  (
         Schema.CorrectiveUpdate(_2, _4, _7, _10, _12)
  )
# 1285 "src/parsers/Calculusparser.ml"
               : 'mapEvent))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : 'emptyVariableList) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'variable) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'mapEvent) in
    Obj.repr(
# 297 "src/parsers/Calculusparser.mly"
               (
         Schema.CorrectiveUpdate(_2, [], _4, _7, _9)
  )
# 1297 "src/parsers/Calculusparser.ml"
               : 'mapEvent))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'relationDefn) in
    Obj.repr(
# 302 "src/parsers/Calculusparser.mly"
               (
   let (reln, relv) = _1 in (reln, relv, Schema.StreamRel)
)
# 1306 "src/parsers/Calculusparser.ml"
               : 'schemaRelationDefn))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 :  Plan.stmt_t ) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'mapTriggerStmtList) in
    Obj.repr(
# 307 "src/parsers/Calculusparser.mly"
                                           ( _1 :: _3 )
# 1314 "src/parsers/Calculusparser.ml"
               : 'mapTriggerStmtList))
; (fun __caml_parser_env ->
    Obj.repr(
# 308 "src/parsers/Calculusparser.mly"
                                           ( [] )
# 1320 "src/parsers/Calculusparser.ml"
               : 'mapTriggerStmtList))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'externalDefn) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'mapTriggerType) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 :  Calculus.expr_t ) in
    Obj.repr(
# 311 "src/parsers/Calculusparser.mly"
                                              (
      let (en, iv, ov, et, em) = _1 in 
      Calculus.sanity_check_variable_names _3;
      {  target_map  = Calculus.mk_external en iv ov et em;
         update_type = _2;
         update_expr = _3
      }
   )
# 1336 "src/parsers/Calculusparser.ml"
               :  Plan.stmt_t ))
; (fun __caml_parser_env ->
    Obj.repr(
# 321 "src/parsers/Calculusparser.mly"
            ( ReplaceStmt )
# 1342 "src/parsers/Calculusparser.ml"
               : 'mapTriggerType))
; (fun __caml_parser_env ->
    Obj.repr(
# 322 "src/parsers/Calculusparser.mly"
            ( UpdateStmt  )
# 1348 "src/parsers/Calculusparser.ml"
               : 'mapTriggerType))
(* Entry statementList *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry calculusExpr *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry mapProgram *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry mapTriggerStmt *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let statementList (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf :  Schema.t * (string * Calculus.expr_t) list )
let calculusExpr (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 2 lexfun lexbuf :  Calculus.expr_t )
let mapProgram (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 3 lexfun lexbuf :  M3.prog_t )
let mapTriggerStmt (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 4 lexfun lexbuf :  Plan.stmt_t )
