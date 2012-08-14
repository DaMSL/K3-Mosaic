/* Parser for the K3 Programming Language */

%{
    open K3.AST
    open K3.Annotation
    open Tree

    let uuid = ref 1

    let get_uuid () = let t = !uuid in uuid := !uuid + 1; t

    let globals = ref []

    let mkexpr tag children = match children with
        | [] -> Leaf(((get_uuid(), tag), []))
        | _  -> Node(((get_uuid(), tag), []), children)

    let rec build_collection exprs ctype = match exprs with
        | [] -> mkexpr (Empty(ctype)) []
        | [e] -> mkexpr (Singleton(ctype)) [e]
        | e :: es -> mkexpr Combine [mkexpr (Singleton(ctype)) [e]; build_collection es ctype]

    let contained_unknown_type = TContained(TImmutable(TUnknown,[]))

    let mk_unknown_collection t_c = TIsolated(TImmutable(TCollection(t_c, contained_unknown_type),[]))

%}

%token EXPECTED
%token DECLARE FOREIGN TRIGGER ROLE DEFAULT
%token CONSUME BIND SOURCE PATTERN FILE 

%token UNIT UNKNOWN NOTHING
%token <int> INTEGER
%token <float> FLOAT
%token <string> STRING
%token <bool> BOOL
%token MAYBE JUST
%token REF
%token RANGE

%token EOF

%token <K3.AST.base_type_t> TYPE

%token LPAREN RPAREN COMMA SEMICOLON

%token LBRACE RBRACE LBRACEBAR RBRACEBAR LBRACKET RBRACKET

%token NEG PLUS MINUS TIMES DIVIDE MODULO

%token CONCAT

%token AND OR NOT LT EQ LEQ NEQ GT GEQ

%token BACKSLASH LRARROW RARROW LARROW

%token COLON

%token QUESTION
%token INSERT UPDATE DELETE

%token GETS COLONGETS

%token DO

%token MAP ITERATE FILTERMAP FLATTEN
%token AGGREGATE GROUPBYAGGREGATE
%token SORT RANK

%token PEEK

%token IF THEN ELSE

%token SEND

%token ANNOTATE
%token INDEX UNIQUE ORDERED SORTED EFFECT PARALLEL

%token <string> IDENTIFIER

%start program
%start expression_test
%start expr

%type <K3.AST.program_t> program
%type <K3Util.expression_test list> expression_test
%type <K3.AST.expr_t> expr

%right RARROW
%right LRARROW

%right CONCAT

%right IF THEN ELSE

%left LT EQ LEQ NEQ GT GEQ

%left OR
%left AND

%left PLUS MINUS
%left TIMES DIVIDE MODULO

%left ANNOTATE

%right NEG NOT

%left COLON

%%

program:
    | declaration { [$1, []] }
    | declaration program { ($1, []) :: $2 }
;

expression_test:
    | expr EXPECTED expr                   { [[], $1, $3] }
    | program expr EXPECTED expr           { [$1, $2, $4] }
    | expression_test expression_test      { $1@$2 }
;

declaration:
    | DECLARE IDENTIFIER COLON type_expr { Global($2, $4, None) }
    | DECLARE IDENTIFIER COLON type_expr GETS expr { Global($2, $4, Some $6) }
    
    | FOREIGN IDENTIFIER COLON type_expr { Foreign($2, $4) }

    | TRIGGER IDENTIFIER arg LBRACE RBRACE GETS expr { Trigger($2, $3, [], $7) }
    | TRIGGER IDENTIFIER arg LBRACE value_typed_identifier_list RBRACE GETS expr {
      let locals = List.map (fun (id,t) -> (id,t,[])) $5
      in Trigger($2, $3, locals, $8)
    }

    | ROLE IDENTIFIER LBRACE stream_program RBRACE { Role($2, $4) }
    | DEFAULT ROLE IDENTIFIER                      { DefaultRole($3) }
;

/* Annotations */
annotations:
    | annotation                   { [$1] }
    | annotation SEMICOLON annotations { $1::$3 }
;

annotation:
    | data_annotation      { ((fun (r, a) -> Data(r,a)) $1) }
    | control_annotation   { ((fun (r, a) -> Control(r,a)) $1) }
;

data_annotation:
    | positions RARROW positions       { Constraint,  FunDep($1, $3) }
    | INDEX LPAREN positions RPAREN    { Hint,        Index($3) }
    | UNIQUE LPAREN positions RPAREN   { Constraint,  Unique($3) }
    | ORDERED LPAREN positions RPAREN  { Constraint,  Ordered($3) }
    | SORTED LPAREN positions RPAREN   { Constraint,  Sorted($3) }
;

control_annotation:
    | EFFECT LPAREN identifier_list RPAREN   { Constraint, Effect($3) }
    | PARALLEL LPAREN INTEGER RPAREN         { Hint,       Parallel($3) }
;

positions: integer_list { $1 };


/* Stream programs */
stream_program:
    | stream_statement { [$1] }
    | stream_statement stream_program { $1 :: $2 }
;

stream_statement:
    | stream      { Stream($1) }
    | instruction { Instruction($1) }

    | BIND IDENTIFIER RARROW IDENTIFIER                   { Bind($2, $4) }
    | BIND SOURCE IDENTIFIER RARROW TRIGGER IDENTIFIER    { Bind($3, $6) }
;

instruction:
    | CONSUME IDENTIFIER { Consume($2) }
;

stream :
    | SOURCE IDENTIFIER COLON type_expr
        GETS FILE LPAREN IDENTIFIER COMMA STRING RPAREN    {
          let format =
            match String.lowercase($8) with
              | "csv" -> CSV | "json" -> JSON
              | _ -> raise Parsing.Parse_error
          in Source($2, $4, (File($10), format))
      }

    | PATTERN IDENTIFIER GETS stream_pattern { Derived($2, $4) }
;

stream_pattern:
    | IDENTIFIER                       { Terminal($1) }
    | LPAREN stream_pattern RPAREN     { $2 }

    | stream_pattern QUESTION          { Optional($1) }
    | stream_pattern TIMES             { Repeat($1, UntilEOF) }
    
    | stream_pattern OR stream_pattern {
        let unwrap_choice x = match x with Choice(l) -> l | _ -> [x]
        in Choice((unwrap_choice $1)@(unwrap_choice $3))
      }
    
    | stream_pattern stream_pattern {
        let unwrap_seq x = match x with Sequence(l) -> l | _ -> [x]
        in Sequence((unwrap_seq $1)@(unwrap_seq $2))
      }
;

/* Types */
type_expr:
    | function_type_expr { TFunction(fst $1, snd $1) }
    | isolated_value_type_expr { TValue($1) }
    | LPAREN type_expr RPAREN { $2 }
;

function_type_expr: isolated_value_type_expr RARROW isolated_value_type_expr { ($1, $3) };

isolated_value_type_expr: isolated_mutable_type_expr { TIsolated($1) };
contained_value_type_expr: contained_mutable_type_expr { TContained($1) };

isolated_mutable_type_expr:
    | isolated_base_type_expr { let a,b = $1 in TImmutable(a,b) }
    | REF isolated_base_type_expr { let a,b = $2 in TMutable(a,b) }
;

contained_mutable_type_expr:
    | contained_base_type_expr { let a,b = $1 in TImmutable(a,b) }
    | REF contained_base_type_expr { let a,b = $2 in TMutable(a,b) }
;

isolated_base_type_expr:
    | TYPE { $1, [] }
    | LPAREN isolated_base_type_tuple RPAREN { $2, [] }
    | annotated_collection_type      { $1 }
    | MAYBE isolated_value_type_expr { TMaybe($2), [] }
;

contained_base_type_expr:
    | TYPE { $1, [] }
    | LPAREN contained_base_type_tuple RPAREN { $2, [] }
    | annotated_collection_type       { $1 }
    | MAYBE contained_value_type_expr { TMaybe($2), [] }
;

isolated_base_type_tuple:
    | isolated_value_type_expr COMMA isolated_value_type_expr_list {
        TTuple($1 :: $3)
    }
;

contained_base_type_tuple:
    | contained_value_type_expr COMMA contained_value_type_expr_list {
        TTuple($1 :: $3)
    }
;

isolated_value_type_expr_list:
    | isolated_value_type_expr { [$1] }
    | isolated_value_type_expr COMMA isolated_value_type_expr_list { $1 :: $3 }
;

contained_value_type_expr_list:
    | contained_value_type_expr { [$1] }
    | contained_value_type_expr COMMA contained_value_type_expr_list { $1 :: $3 }
;

annotated_collection_type:
    | collection_type                         { $1,[] }
    | collection_type ANNOTATE LBRACE annotations RBRACE  { $1, $4 }
;

collection_type:
    | LBRACE contained_value_type_expr RBRACE { TCollection(TSet, $2) }
    | LBRACEBAR contained_value_type_expr RBRACEBAR { TCollection(TBag, $2) }
    | LBRACKET contained_value_type_expr RBRACKET { TCollection(TList, $2) }
;

/* Expressions */
expr:
    | LPAREN tuple RPAREN { $2 }
    | block { $1 }

    | JUST expr { mkexpr Just [$2] }

    | constant { mkexpr (Const($1)) [] }
    | collection { $1 }
    | range { $1 }
    | variable { mkexpr (Var($1)) [] }
    | arithmetic { $1 }
    | predicate { $1 }
    | conditional { $1 }
    | lambda { $1 }
    | access { $1 }
    | transformers { $1 }
    | mutation { $1 }

    | SEND LPAREN IDENTIFIER COMMA address COMMA tuple RPAREN {
        mkexpr Send [mkexpr (Const(CTarget($3))) []; mkexpr (Const($5)) []; $7]
      }
    | expr LPAREN tuple RPAREN { mkexpr Apply [$1; $3] }
;

expr_list:
    | expr { [$1] }
    | expr COMMA expr_list { $1 :: $3 }
;

expr_seq:
    | expr                    { [$1] }
    | expr SEMICOLON expr_seq { $1 :: $3 }
;

tuple:
    | expr_list { if List.length $1 == 1 then List.hd $1 else mkexpr Tuple $1 }
;

value_typed_identifier:
    | IDENTIFIER COLON isolated_value_type_expr { ($1, $3) }
;

value_typed_identifier_list:
    | value_typed_identifier { [($1)] }
    | value_typed_identifier COMMA value_typed_identifier_list { $1 :: $3 }
;

arg:
    | UNKNOWN { AIgnored } 
    | value_typed_identifier { AVar(fst $1, snd $1) }
    | JUST arg { AMaybe($2) }
    | LPAREN arg_list RPAREN  {
        if List.length $2 == 1 then List.hd $2
        else ATuple($2)
    }
;

arg_list:
    | arg { [($1)] } 
    | arg COMMA arg_list { $1 :: $3 }

constant:
    | UNKNOWN { CUnknown }
    | UNIT { CUnit }
    | NOTHING { CNothing }
    | BOOL { CBool($1) }
    | INTEGER { CInt($1) }
    | FLOAT { CFloat($1) }
    | STRING { CString($1) }
;

range:
    | LBRACE expr COLON expr COLON expr RBRACE { mkexpr (Range(TSet)) [$2; $4; $6] }
    | LBRACEBAR expr COLON expr COLON expr RBRACEBAR { mkexpr (Range(TBag)) [$2; $4; $6] }
    | LBRACKET expr COLON expr COLON expr RBRACKET { mkexpr (Range(TList)) [$2; $4; $6] }
;

collection:
    | LBRACE RBRACE { mkexpr (Empty(mk_unknown_collection TSet)) [] }
    | LBRACEBAR RBRACEBAR { mkexpr (Empty(mk_unknown_collection TBag)) [] }
    | LBRACKET RBRACKET { mkexpr (Empty(mk_unknown_collection TList)) [] }

    | LBRACE expr_seq RBRACE { build_collection $2 (mk_unknown_collection TSet) }
    | LBRACEBAR expr_seq RBRACEBAR { build_collection $2 (mk_unknown_collection TBag) }
    | LBRACKET expr_seq RBRACKET { build_collection $2 (mk_unknown_collection TList) }
;

variable:
    | IDENTIFIER { $1 }
;

address:
    | IDENTIFIER COLON INTEGER { CAddress($1,$3) }
;

arithmetic:
    | NEG expr { mkexpr Neg [$2] }
    | expr PLUS expr { mkexpr Add [$1; $3] }
    | expr NEG expr %prec MINUS { mkexpr Add [$1; mkexpr Neg [$3]] }
    | expr TIMES expr { mkexpr Mult [$1; $3] }
    | expr DIVIDE expr { mkexpr Apply [mkexpr (Var("/")) []; mkexpr Tuple [$1; $3]] }
    | expr MODULO expr { mkexpr Apply [mkexpr (Var("%")) []; mkexpr Tuple [$1; $3]] }
;

predicate:
    | expr LT expr { mkexpr Lt [$1; $3] }
    | expr EQ expr { mkexpr Eq [$1; $3] }
    | expr LEQ expr { mkexpr Leq [$1; $3] }
    | expr NEQ expr { mkexpr Neq [$1; $3] }

    | expr GT expr { mkexpr Neg [mkexpr Leq [$1; $3]] }
    | expr GEQ expr { mkexpr Neg [mkexpr Lt [$1; $3]] }
;

conditional:
    | IF expr THEN expr ELSE expr { mkexpr IfThenElse [$2; $4; $6] }
;

lambda:
    | BACKSLASH arg RARROW expr { mkexpr (Lambda($2)) [$4] }
;

access:
    | expr LBRACKET tuple RBRACKET { mkexpr Slice [$1; $3] }
    | PEEK LPAREN expr RPAREN { mkexpr Peek [$3] }
;

mutation:
    | INSERT LPAREN expr COMMA expr RPAREN { mkexpr Insert [$3; $5] }
    | UPDATE LPAREN expr COMMA expr, COMMA expr RPAREN { mkexpr Update [$3; $5; $7] }
    | DELETE LPAREN expr COMMA expr RPAREN { mkexpr Delete [$3; $5] }
    | expr LARROW expr { mkexpr Assign [$1; $3] }
    | expr GETS expr { mkexpr Assign [$1; $3] }
;

transformers:
    | expr CONCAT expr { mkexpr Combine [$1; $3] }
    | MAP LPAREN expr COMMA expr RPAREN { mkexpr Map [$3; $5] }
    | ITERATE LPAREN expr COMMA expr RPAREN { mkexpr Iterate [$3; $5] }
    | FILTERMAP LPAREN expr COMMA expr COMMA expr RPAREN { mkexpr FilterMap [$3; $5; $7] }
    | FLATTEN LPAREN expr RPAREN { mkexpr Flatten [$3] }
    | AGGREGATE LPAREN expr COMMA expr COMMA expr RPAREN { mkexpr Aggregate [$3; $5; $7] }
    | GROUPBYAGGREGATE LPAREN expr COMMA expr COMMA expr COMMA expr RPAREN {
        mkexpr GroupByAggregate [$3; $5; $7; $9]
    }
    | SORT LPAREN expr COMMA expr RPAREN { mkexpr Sort [$3; $5] }
;

block:
    | DO LBRACE expr_seq RBRACE { mkexpr Block $3 }
;

/* Sequence primitives */
integer_list:
    | INTEGER                       { [$1] }
    | INTEGER COMMA integer_list    { $1::$3 }
;

identifier_list:
    | IDENTIFIER                       { [$1] }
    | IDENTIFIER COMMA identifier_list { $1::$3 } 
;
