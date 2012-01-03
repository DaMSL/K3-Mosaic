/* Parser for the K3 Programming Language */

%{
    open K3
    open Tree

    let uuid = ref 1

    let get_uuid () = let t = !uuid in uuid := !uuid + 1; t
%}

%token EOF

%token <int> INTEGER

%start line
%type <int K3.expr_t> line

%%

line: expr EOF { $1 }

expr:
    | constant { Leaf(get_uuid(), Const($1)) }

constant:
    | INTEGER { CInt($1) }
