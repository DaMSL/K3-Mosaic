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

%token <string> IDENTIFIER

%start line
%type <int K3.expr_t> line

%%

line: expr EOF { $1 }

expr:
    | constant { mkleaf(Const($1)) }
    | identifier { mkleaf(Var($1, TUnknown)) }

constant:
    | INTEGER { CInt($1) }
    | FLOAT { CFloat($1) }
    | STRING { CString($1) }
    | BOOL { CBool($1) }

identifier:
    | IDENTIFIER { $1 }
