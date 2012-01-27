/* Parser for the K3 Programming Language */

%{
    open K3
    open Tree

    let uuid = ref 1

    let get_uuid () = let t = !uuid in uuid := !uuid + 1; t

    let globals = ref []

    let mkexpr tag children = match children with
        | [] -> Leaf(get_uuid(), tag)
        | _  -> Node(get_uuid(), tag, children)

    let rec build_collection exprs ctype = match exprs with
        | [] -> mkexpr (Empty(ctype)) []
        | [e] -> mkexpr (Singleton(ctype)) [e]
        | e :: es -> mkexpr Combine [mkexpr (Singleton(ctype)) [e]; build_collection es ctype]

    let rec accumulate_slice_indices exprs i = match exprs with
        | [] -> []
        | Leaf(_, Var("_", _)) :: es -> accumulate_slice_indices es (i + 1)
        | e :: es -> (e, i) :: accumulate_slice_indices es (i + 1)
%}

%token <int> INTEGER
%token <float> FLOAT
%token <string> STRING
%token <bool> BOOL

%token EOF

%token <K3.type_t> TYPE

%token LPAREN RPAREN COMMA SEMICOLON

%token LBRACE RBRACE LBRACEBAR RBRACEBAR LBRACKET RBRACKET

%token PLUS MINUS NEG
%token TIMES

%token CONCAT

%token AND OR NOT

%token LT EQ LEQ NEQ

%token LRARROW RARROW

%token COLON
%token BACKSLASH

%token QUESTION

%token GETS

%token DO

%token MAP ITERATE FILTERMAP FLATTEN
%token AGGREGATE GROUPBYAGGREGATE
%token SORT RANK

%token HEAD TAIL

%token IF THEN ELSE

%token SEND

%token ANNOTATE

%token <string> IDENTIFIER

%token DECLARE
%token TRIGGER

%start program
%type <int K3.program_t> program

%right RARROW
%right LRARROW

%right CONCAT

%right IF THEN ELSE

%left LT EQ LEQ NEQ

%left OR
%left AND

%left PLUS MINUS
%left TIMES

%left ANNOTATE

%right NEG NOT

%left COLON

%%

program:
    | EOF { [Declaration(Trigger("on_start", ATuple([]), [], !globals))] }
    | statement program { $1 :: $2 }

statement:
    | global { Declaration($1) }
    | trigger { Declaration($1) }

global:
    | DECLARE identifier SEMICOLON { Global(fst $2, snd $2) }
    | DECLARE identifier GETS expr SEMICOLON {
        globals := !globals @ [Assign(fst $2, $4)]; Global(fst $2, snd $2)
    }

trigger:
    | TRIGGER IDENTIFIER arg LBRACE effect_seq RBRACE {
        let names, effects = List.split $5 in
        let declarations = List.filter (
            function i -> match i with
                | ("_", TUnknown) -> false
                | _ -> true
            ) names in
        Trigger($2, $3, declarations, effects)
    }

effect:
    | DECLARE identifier GETS expr { ($2, Assign(fst $2, $4)) }
    | identifier GETS expr { (("_", TUnknown), Assign(fst $1, $3)) }
    | expr { (("_", TUnknown), Mutate($1)) }

effect_seq:
    | effect SEMICOLON { [$1] }
    | effect SEMICOLON effect_seq { $1 :: $3 }

type_expr:
    | TYPE { $1 }

    | LBRACE type_tuple RBRACE { TCollection(TSet, $2) }
    | LBRACEBAR type_tuple RBRACEBAR { TCollection(TBag, $2) }
    | LBRACKET type_tuple RBRACKET { TCollection(TList, $2) }

    | LPAREN type_tuple RPAREN { $2 }

    | type_expr RARROW type_expr { TFunction($1, $3) }

type_tuple:
    | type_expr_list { if List.length $1 == 1 then List.hd $1 else TTuple($1) }

type_expr_list:
    | type_expr { [$1] }
    | type_expr COMMA type_expr_list { $1 :: $3 }

expr:
    | LPAREN tuple RPAREN { $2 }
    | constant { mkexpr (Const($1)) [] }
    | identifier { mkexpr (Var(fst $1, snd $1)) [] }

    | arithmetic { $1 }
    | predicate { $1 }
    | collection { $1 }
    | collection_op { $1 }

    | conditional { $1 }

    | lambda { $1 }

    | access { $1 }

    | block { $1 }
    | annotation { $1 }

    | SEND LPAREN address COMMA tuple RPAREN { mkexpr (Send($3)) [$5] }

    | IDENTIFIER LPAREN tuple RPAREN {
        mkexpr Apply [mkexpr (Var($1, TUnknown)) []; $3]
    }

arg:
    | identifier { AVar(fst $1, snd $1) }
    | LPAREN arg_list RPAREN  {
        if List.length $2 == 1 then
            let arg = List.hd $2 in
            AVar(fst arg, snd arg)
        else ATuple($2)
    }

arg_list:
    | identifier { [($1)] }
    | identifier COMMA arg_list { $1 :: $3 }

tuple:
    | expr_list { if List.length $1 == 1 then List.hd $1 else mkexpr Tuple $1 }

expr_list:
    | expr { [$1] }
    | expr COMMA expr_list { $1 :: $3 }

expr_seq:
    | tuple { [$1] }
    | tuple SEMICOLON expr_seq { $1 :: $3 }

address:
    | IDENTIFIER { Local($1) }

constant:
    | INTEGER { CInt($1) }
    | FLOAT { CFloat($1) }
    | STRING { CString($1) }
    | BOOL { CBool($1) }

identifier:
    | IDENTIFIER { ($1, TUnknown) }
    | IDENTIFIER COLON type_expr { ($1, $3) }

arithmetic:
    | NEG expr { mkexpr Neg [$2] }
    | expr PLUS expr { mkexpr Add [$1; $3] }
    | expr NEG expr %prec MINUS { mkexpr Add [$1; mkexpr Neg [$3]] }
    | expr TIMES expr { mkexpr Mult [$1; $3] }

    | NOT expr { mkexpr Neg [$2] }
    | expr OR expr { mkexpr Add [$1;$3] }
    | expr AND expr { mkexpr Mult [$1;$3] }

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

collection_op:
    | expr CONCAT expr {
        mkexpr Combine [$1;$3]
    }

    | MAP LPAREN expr COMMA expr RPAREN {
        mkexpr Map [$3;$5]
    }

    | ITERATE LPAREN expr COMMA expr RPAREN {
        mkexpr Iterate [$3;$5]
    }

    | FILTERMAP LPAREN expr COMMA expr COMMA expr RPAREN {
        mkexpr FilterMap [$3;$5;$7]
    }

    | FLATTEN LPAREN expr RPAREN {
        mkexpr Flatten [$3]
    }

    | AGGREGATE LPAREN expr COMMA expr COMMA expr RPAREN {
        mkexpr Aggregate [$3;$5;$7]
    }

    | GROUPBYAGGREGATE LPAREN expr COMMA expr COMMA expr COMMA expr RPAREN {
        mkexpr GroupByAggregate [$3;$5;$7;$9]
    }

    | SORT LPAREN expr COMMA expr RPAREN {
        mkexpr Sort [$3;$5]
    }

    | RANK LPAREN expr COMMA expr RPAREN {
        mkexpr Rank [$3;$5]
    }

    | HEAD LPAREN expr RPAREN { mkexpr Head [$3] }
    | TAIL LPAREN expr RPAREN { mkexpr Tail [$3] }

conditional:
    | IF expr THEN expr ELSE expr {
        mkexpr IfThenElse [$2;$4;$6]
    }

lambda:
    | BACKSLASH arg RARROW expr { mkexpr (Lambda($2)) [$4] }
    | BACKSLASH arg LRARROW arg RARROW expr { mkexpr (AssocLambda($2, $4)) [$6] }

access:
    | expr LBRACKET tuple RBRACKET {
        match $3 with
            | Leaf(_, Var("_", _)) -> mkexpr Lookup [$1;$3]
            | Node(_, Tuple, keys) ->
                let values, indices = List.split (accumulate_slice_indices keys 0) in
                if List.length indices == List.length keys
                then mkexpr Lookup [$1;$3]
                else mkexpr (Slice(indices)) ($1 :: values)
            | _ -> mkexpr Lookup [$1;$3]
    }
    | expr LBRACKET tuple RBRACKET QUESTION { mkexpr Member [$1;$3] }
    | expr LBRACKET tuple RBRACKET GETS expr { mkexpr Update [$1;$3;$6] }

block: DO LBRACE expr_seq RBRACE { mkexpr Block $3 }

annotation:
    | expr ANNOTATE expr { $1 }
