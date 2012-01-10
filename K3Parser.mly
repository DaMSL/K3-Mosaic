/* Parser for the K3 Programming Language */

%{
    open K3
    open Tree

    let uuid = ref 1

    let get_uuid () = let t = !uuid in uuid := !uuid + 1; t

    let mkexpr tag children = match children with
        | [] -> Leaf(get_uuid(), tag)
        | _  -> Node(get_uuid(), tag, children)

    let rec build_collection exprs ctype = match exprs with
        | [] -> mkexpr (Empty(ctype)) []
        | [e] -> mkexpr (Singleton(ctype)) [e]
        | e :: es -> mkexpr Combine [mkexpr (Singleton(ctype)) [e]; build_collection es ctype]
%}

%token EOF

%token <int> INTEGER
%token <float> FLOAT
%token <string> STRING
%token <bool> BOOL

%token <K3.type_t> TYPE

%token LPAREN RPAREN COMMA SEMICOLON

%token LBRACE RBRACE LBRACEBAR RBRACEBAR LBRACKET RBRACKET

%token PLUS MINUS NEG
%token TIMES

%token LT EQ LEQ NEQ

%token <string> IDENTIFIER

%start line
%type <int K3.expr_t> line

%left LT EQ LEQ NEQ

%left PLUS MINUS
%left TIMES

%right NEG

%%

line: expr EOF { $1 }

type_expr:
    | TYPE { $1 }

expr:
    | LPAREN tuple RPAREN { $2 }
    | constant { mkexpr (Const($1)) [] }
    | identifier { mkexpr (Var($1, TUnknown)) [] }

    | arithmetic { $1 }
    | predicate { $1 }
    | collection { $1 }

tuple:
    | expr_list { if List.length $1 == 1 then List.hd $1 else mkexpr Tuple $1 }

expr_list:
    | expr { [$1] }
    | expr COMMA expr_list { $1 :: $3 }

expr_seq:
    | tuple { [$1] }
    | tuple SEMICOLON expr_seq { $1 :: $3 }

constant:
    | INTEGER { CInt($1) }
    | FLOAT { CFloat($1) }
    | STRING { CString($1) }
    | BOOL { CBool($1) }

identifier:
    | IDENTIFIER { $1 }

arithmetic:
    | NEG expr { mkexpr Neg [$2] }
    | expr PLUS expr { mkexpr Add [$1; $3] }
    | expr NEG expr %prec MINUS { mkexpr Add [$1; mkexpr Neg [$3]] }
    | expr TIMES expr { mkexpr Mult [$1; $3] }

predicate:
    | expr LT expr { mkexpr Lt [$1; $3] }
    | expr EQ expr { mkexpr Eq [$1; $3] }
    | expr LEQ expr { mkexpr Leq [$1; $3] }
    | expr NEQ expr { mkexpr Neq [$1; $3] }

collection:
    | LBRACE RBRACE { mkexpr (Empty(TCollection(TSet, TUnknown))) [] }
    | LBRACEBAR RBRACEBAR { mkexpr (Empty(TCollection(TBag, TUnknown))) [] }
    | LBRACKET RBRACKET { mkexpr (Empty(TCollection(TList, TUnknown))) [] }

    | LBRACE expr_seq RBRACE { build_collection $2 (TCollection(TSet, TUnknown)) }
    | LBRACEBAR expr_seq RBRACEBAR { build_collection $2 (TCollection(TBag, TUnknown)) }
    | LBRACKET expr_seq RBRACKET { build_collection $2 (TCollection(TList, TUnknown)) }
