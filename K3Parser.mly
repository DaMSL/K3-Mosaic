/* Parser for the K3 Programming Language */

%{
    open K3
    open Tree

    let uuid = ref 1

    let get_uuid () = let t = !uuid in uuid := !uuid + 1; t

    let mkleaf tag = Leaf(get_uuid(), tag)
    let mknode tag children = Node(get_uuid(), tag, children)
%}

%token EOF

%token <int> INTEGER
%token <float> FLOAT
%token <string> STRING
%token <bool> BOOL

%token LPAREN RPAREN COMMA

%token PLUS MINUS NEG
%token TIMES

%token <string> IDENTIFIER

%start line
%type <int K3.expr_t> line

%left PLUS MINUS
%left TIMES

%right NEG

%%

line: expr EOF { $1 }

expr:
    | LPAREN tuple RPAREN { $2 }
    | constant { mkleaf(Const($1)) }
    | identifier { mkleaf(Var($1, TUnknown)) }

    | arithmetic { $1 }

tuple:
    | expr_list { if List.length $1 == 1 then List.hd $1 else mknode Tuple $1 }

expr_list:
    | expr { [$1] }
    | expr COMMA expr_list { $1 :: $3 }

constant:
    | INTEGER { CInt($1) }
    | FLOAT { CFloat($1) }
    | STRING { CString($1) }
    | BOOL { CBool($1) }

identifier:
    | IDENTIFIER { $1 }

arithmetic:
    | NEG expr { mknode Neg [$2] }
    | expr PLUS expr { mknode Add [$1; $3] }
    | expr NEG expr %prec MINUS { mknode Add [$1; mknode Neg [$3]] }
    | expr TIMES expr { mknode Mult [$1; $3] }
