/* K3 Programming Language Parser  */

%{
  open Util
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

  let parse_format s = match String.lowercase s with
    | "csv" -> CSV | "json" -> JSON
    | _ -> raise Parsing.Parse_error

  let numerrors = ref 0

  let print_error msg = 
      incr numerrors; 
      let pos = Parsing.symbol_start_pos() in 
      let linenum = pos.Lexing.pos_lnum in 
      let column = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in 
      Printf.printf "Error on line %d character %d : " linenum column;
      print_endline msg;
      if !numerrors > 20 then raise Exit else raise Parsing.Parse_error
        
  (* Predefined errors *)
  let missing_paren ?(side="left") = print_error ("Missing "^side^" parenthesis")

  let id_error () = print_error("Expected identifier")
  let type_error () = print_error("Expected type expression")
  
  let address_error ip port = print_error("Invalid address "^ip^":"^(string_of_int port))

  let op_error op_class i = 
    let op_type = match i with 1 -> " unary" | 2 -> " binary" | 3 -> " ternary" | _ -> ""
    in print_error("Invalid"^op_type^" "^op_class^" operator syntax")

  let arith_error i = op_error "arithmetic" i
  let comp_error () = op_error "comparison" 2
  
  let cond_error cond_class =
    print_error ("Invalid conditional "^cond_class^" error")

  let lambda_error error_class = 
    print_error ("Invalid lambda "^error_class^" expression")

  let assign_error assign_class =
    print_error ("Invalid "^assign_class^" assignment RHS expression")

  let positional_error value_class i =
    let i_str = match i with 
        | 1 -> "first"   | 2 -> "second" | 3 -> "third"
        | 4 -> "fourth"  | 5 -> "second" | 6 -> "third"
        | 7 -> "seventh" | 8 -> "eighth" | 9 -> "ninth"
        | _ -> string_of_int i
    in print_error ("Expected "^value_class^" as "^i_str^" argument")
  
  let coll_error i = positional_error "collection" i
  let coll_lambda_error fn_class i = positional_error (fn_class^" function") i
  let value_error i = positional_error "value" i

  let expr_error () = print_error("Expected expression")
  
  let flow_program_error () = print_error("Expected flow program")

%}

%token NETWORK EXPECTED
%token DECLARE FOREIGN TRIGGER ROLE DEFAULT
%token CONSUME BIND SOURCE SINK PATTERN FILE SOCKET RANDOM STREAM

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

%token IF THEN ELSE LET IN

%token SEND

%token ANNOTATE
%token RASSOC EFFECT PARALLEL

%token <string> IDENTIFIER IP

%start program
%start program_test
%start expression_test
%start expr

%type <K3.AST.program_t>            program
%type <K3.AST.program_test_t>       program_test
%type <K3.AST.program_test_t>       expression_test
%type <K3.AST.expr_t>               expr

%right RARROW
%right LRARROW

%right IF THEN ELSE

%right CONCAT

%left OR
%left AND

%left LT EQ LEQ NEQ GT GEQ

%left PLUS MINUS
%left TIMES DIVIDE MODULO

%left ANNOTATE

%right NEG NOT

%left COLON

%%

program :
    | declaration         { if !numerrors>=1 then raise Exit else [$1, []] }
    | declaration program { ($1, []) :: $2 }
;

declaration :
    | DECLARE IDENTIFIER COLON type_expr { Global($2, $4, None) }
    | DECLARE IDENTIFIER COLON type_expr GETS expr { Global($2, $4, Some $6) }

    | FOREIGN IDENTIFIER COLON type_expr { Foreign($2, $4) }

    | flow_program  { Flow($1) }

    | ROLE IDENTIFIER LBRACE flow_program RBRACE   { Role($2, $4) }
    | DEFAULT ROLE IDENTIFIER                      { DefaultRole($3) }

    /* Error handling */
    | DECLARE IDENTIFIER COLON type_expr GETS error { expr_error() }
    | DECLARE IDENTIFIER COLON error                { type_error() }
    | DECLARE error                                 { id_error() }
    
    | FOREIGN IDENTIFIER COLON error { type_error() }
    | FOREIGN error                  { id_error() }

    | ROLE IDENTIFIER LBRACE error { flow_program_error() }
    | ROLE IDENTIFIER error        { flow_program_error() }
    | ROLE error                   { id_error() }

    | DEFAULT ROLE error           { id_error() }
;

/* Flow programs */
flow_program :
    | flow_statement { [$1, []] }
    | flow_statement flow_program { ($1,[]) :: $2 }
;

flow_statement :
    | resource      { $1 }
    | instruction   { Instruction($1) }

    | TRIGGER IDENTIFIER arg LBRACE RBRACE GETS expr { Sink(Code($2, $3, [], $7)) }
    | TRIGGER IDENTIFIER arg LBRACE value_typed_identifier_list RBRACE GETS expr {
      let locals = List.map (fun (id,t) -> (id,t,[])) $5
      in Sink(Code($2, $3, locals, $8))
    }

    | BIND IDENTIFIER RARROW IDENTIFIER                   { Bind($2, $4) }
    | BIND SOURCE IDENTIFIER RARROW TRIGGER IDENTIFIER    { Bind($3, $6) }

    /* Error handling */

    | TRIGGER IDENTIFIER arg LBRACE RBRACE GETS error { print_error("Error in trigger body") }

    | TRIGGER IDENTIFIER arg LBRACE value_typed_identifier_list RBRACE GETS error {
        print_error("Error in trigger body")
      }

    | TRIGGER IDENTIFIER arg LBRACE error { print_error("Expected list of local declarations") }
    | TRIGGER error                       { print_error("Invalid trigger") }

    | BIND IDENTIFIER RARROW error                 { print_error("Invalid bind target") }
    | BIND SOURCE IDENTIFIER RARROW TRIGGER error  { print_error("Invalid bind target") }
    | BIND error                                   { print_error("Invalid bind source") }
;

instruction :
    | CONSUME IDENTIFIER { Consume($2) }
;

resource :
    | SOURCE IDENTIFIER COLON type_expr GETS handle {
        let channel_type, channel_format = $6
        in Source(Resource($2, Handle($4, channel_type, channel_format)))
      }

    | SOURCE IDENTIFIER COLON type_expr GETS stream {
        Source(Resource($2, Stream($4, $6)))
      }

    | SOURCE PATTERN IDENTIFIER GETS resource_pattern { 
        Source(Resource($3, Pattern($5)))
      }

    | SINK IDENTIFIER COLON type_expr GETS handle {
        let channel_type, channel_format = $6
        in Sink(Resource($2, Handle($4, channel_type, channel_format)))
      }
      
    | SINK PATTERN IDENTIFIER GETS resource_pattern {
        Sink(Resource($3, Pattern($5)))
      }
;

handle :
    | FILE LPAREN STRING COMMA IDENTIFIER RPAREN
      { File($3), parse_format $5 }

    | SOCKET LPAREN STRING COMMA INTEGER COMMA IDENTIFIER RPAREN
      { Network($3, $5), parse_format $7 }
;

stream :
    | STREAM LPAREN expr RPAREN  { ConstStream($3) }

    | RANDOM LPAREN INTEGER RPAREN { RandomStream($3) }


resource_pattern :
    | IDENTIFIER                       { Terminal($1) }
    | LPAREN resource_pattern RPAREN     { $2 }

    | resource_pattern QUESTION          { Optional($1) }
    | resource_pattern TIMES             { Repeat($1, UntilEOF) }
    
    | resource_pattern OR resource_pattern {
        let unwrap_choice x = match x with Choice(l) -> l | _ -> [x]
        in Choice((unwrap_choice $1)@(unwrap_choice $3))
      }
    
    | resource_pattern resource_pattern {
        let unwrap_seq x = match x with Sequence(l) -> l | _ -> [x]
        in Sequence((unwrap_seq $1)@(unwrap_seq $2))
      }
;


/* Annotations */
annotations :
    | annotation                   { [$1] }
    | annotation SEMICOLON annotations { $1::$3 }
;

annotation :
    | data_annotation      { ((fun (r, a) -> Data(r,a)) $1) }
    | control_annotation   { ((fun (r, a) -> Control(r,a)) $1) }
;

data_annotation :
    | positions RARROW positions       { Constraint,  FunDep($1, Positions($3)) }
    | positions RARROW TIMES           { Constraint,  FunDep($1, Element) }
    | positions RASSOC positions       { Constraint,  MVFunDep($1, Positions($3)) }
    | positions RASSOC TIMES           { Constraint,  MVFunDep($1, Element) }
    | "key" LPAREN positions RPAREN    { Constraint,  FunDep($3, Element) }
    | "index" LPAREN positions RPAREN  { Constraint,  MVFunDep($3, Element) }
    | "unique" LPAREN positions RPAREN { Constraint,  Unique($3) }
    | "ordered" LPAREN positions RPAREN  { Constraint,  Ordered($3) }
    | "sequential"                     { Hint,        Sequential }
    | "randomaccess"                   { Hint,        RandomAccess }
;

control_annotation :
    | EFFECT LPAREN identifier_list RPAREN   { Constraint, Effect($3) }
    | PARALLEL LPAREN INTEGER RPAREN         { Hint,       Parallel($3) }
;

positions : integer_list { $1 };


/* Types */
type_expr :
    | function_type_expr { TFunction(fst $1, snd $1) }
    | isolated_value_type_expr { TValue($1) }
    | LPAREN type_expr RPAREN { $2 }
;

function_type_expr : isolated_value_type_expr RARROW isolated_value_type_expr { ($1, $3) };

isolated_value_type_expr  : isolated_mutable_type_expr  { TIsolated($1) };
contained_value_type_expr : contained_mutable_type_expr { TContained($1) };

isolated_mutable_type_expr :
    | isolated_base_type_expr { let a,b = $1 in TImmutable(a,b) }
    | REF isolated_base_type_expr { let a,b = $2 in TMutable(a,b) }
;

contained_mutable_type_expr :
    | contained_base_type_expr { let a,b = $1 in TImmutable(a,b) }
    | REF contained_base_type_expr { let a,b = $2 in TMutable(a,b) }
;

isolated_base_type_expr :
    | TYPE { $1, [] }
    | LPAREN isolated_base_type_tuple RPAREN { $2, [] }
    | annotated_collection_type      { $1 }
    | MAYBE isolated_value_type_expr { TMaybe($2), [] }
;

contained_base_type_expr :
    | TYPE { $1, [] }
    | LPAREN contained_base_type_tuple RPAREN { $2, [] }
    | annotated_collection_type       { $1 }
    | MAYBE contained_value_type_expr { TMaybe($2), [] }
;

isolated_base_type_tuple :
    | isolated_value_type_expr COMMA isolated_value_type_expr_list {
        TTuple($1 :: $3)
    }
;

contained_base_type_tuple :
    | contained_value_type_expr COMMA contained_value_type_expr_list {
        TTuple($1 :: $3)
    }
;

contained_value_type_tuple :
    | contained_value_type_expr COMMA contained_value_type_expr_list {
        TContained(TImmutable(TTuple($1 :: $3), []))
    }

isolated_value_type_expr_list :
    | isolated_value_type_expr { [$1] }
    | isolated_value_type_expr COMMA isolated_value_type_expr_list { $1 :: $3 }
;

contained_value_type_expr_list :
    | contained_value_type_expr { [$1] }
    | contained_value_type_expr COMMA contained_value_type_expr_list { $1 :: $3 }
;

annotated_collection_type :
    | collection_type                                     { $1,[] }
    | collection_type ANNOTATE LBRACE annotations RBRACE  { $1, $4 }
;

collection_type :
    | LBRACE contained_value_type_tuple RBRACE { TCollection(TSet, $2) }
    | LBRACEBAR contained_value_type_tuple RBRACEBAR { TCollection(TBag, $2) }
    | LBRACKET contained_value_type_tuple RBRACKET { TCollection(TList, $2) }
    | LBRACE contained_value_type_expr RBRACE { TCollection(TSet, $2) }
    | LBRACEBAR contained_value_type_expr RBRACEBAR { TCollection(TBag, $2) }
    | LBRACKET contained_value_type_expr RBRACKET { TCollection(TList, $2) }
;

/* Expressions */
expr :
    | LPAREN tuple RPAREN { $2 }
    | block { $1 }

    | JUST expr { mkexpr Just [$2] }
    | NOTHING COLON isolated_value_type_expr { mkexpr (Nothing($3)) [] }

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
    | SEND LPAREN IDENTIFIER COMMA access COMMA tuple RPAREN {
        mkexpr Send [mkexpr (Const(CTarget($3))) []; $5; $7]
      }
    | SEND LPAREN IDENTIFIER COMMA variable COMMA tuple RPAREN {
        mkexpr Send [mkexpr (Const(CTarget($3))) []; mkexpr (Var $5) []; $7]
      }

    /* Function application and let notation */
    | expr LPAREN tuple RPAREN    { mkexpr Apply [$1; $3] }
    | LET arg GETS expr IN expr   { mkexpr Apply [mkexpr (Lambda $2) [$6]; $4] }

    /* TODO: more error handling */
    | SEND LPAREN IDENTIFIER COMMA address COMMA error { print_error("Invalid send argument") }
    | SEND LPAREN IDENTIFIER COMMA error { print_error("Invalid send address") }
    | SEND LPAREN error { print_error("Invalid send target") }
    | SEND error { print_error("Invalid send syntax") }

    | expr LPAREN error { print_error("Function application error") }
    
    | LET arg GETS expr IN error   { print_error "Let body error" }
    | LET arg GETS error           { print_error "Let binding target error" }
    | LET error                    { print_error "Let binding error" }
;

expr_list :
    | expr                 { [$1] }
    | expr COMMA expr_list { $1 :: $3 }
;

expr_seq :
    | tuple                    { [$1] }
    | tuple SEMICOLON expr_seq { $1 :: $3 }
;

tuple :
    | expr_list { if List.length $1 == 1 then List.hd $1 else mkexpr Tuple $1 }
;

value_typed_identifier :
    | IDENTIFIER COLON isolated_value_type_expr { ($1, $3) }
    | IDENTIFIER COLON error                    { type_error() }

;

value_typed_identifier_list :
    | value_typed_identifier                                   { [($1)] }
    | value_typed_identifier COMMA value_typed_identifier_list { $1 :: $3 }
;

arg :
    | UNKNOWN { AIgnored } 
    | value_typed_identifier  { AVar(fst $1, snd $1) }
    | JUST arg                { AMaybe($2) }
    | LPAREN arg_list RPAREN  { if List.length $2 == 1 then List.hd $2 else ATuple($2) }
;

arg_list :
    | arg                { [($1)] } 
    | arg COMMA arg_list { $1 :: $3 }

constant :
    | UNKNOWN   { CUnknown }
    | UNIT      { CUnit }
    | BOOL      { CBool($1) }
    | INTEGER   { CInt($1) }
    | FLOAT     { CFloat($1) }
    | STRING    { CString($1) }
    | address   { $1 }
;

range :
    | LBRACE expr COLON COLON expr COLON COLON expr RBRACE { mkexpr (Range(TSet)) [$2; $5; $8] }
    | LBRACEBAR expr COLON COLON expr COLON COLON expr RBRACEBAR { mkexpr (Range(TBag)) [$2; $5; $8] }
    | LBRACKET expr COLON COLON expr COLON COLON expr RBRACKET { mkexpr (Range(TList)) [$2; $5; $8] }
;

collection :
    | LBRACE RBRACE COLON isolated_value_type_expr { build_collection [] $4 }
    | LBRACEBAR RBRACEBAR COLON isolated_value_type_expr { build_collection [] $4 }
    | LBRACKET RBRACKET COLON isolated_value_type_expr{ build_collection [] $4 }
    | LBRACE RBRACE error { print_error "missing type for empty set"}
    | LBRACEBAR RBRACEBAR error { print_error "missing type for empty bag"}
    | LBRACKET RBRACKET error { print_error "missing type for empty list"}

    | LBRACE expr_seq RBRACE { build_collection $2 (mk_unknown_collection TSet) }
    | LBRACEBAR expr_seq RBRACEBAR { build_collection $2 (mk_unknown_collection TBag) }
    | LBRACKET expr_seq RBRACKET { build_collection $2 (mk_unknown_collection TList) }
;

variable :
    | IDENTIFIER { $1 }
;

address :
    | IDENTIFIER COLON INTEGER { CAddress($1,$3) }
    | IP COLON INTEGER { 
        let parts = Str.split (Str.regexp_string ".") $1 in
        let valid = List.for_all (fun x -> (int_of_string x) < 256) parts in
        if valid then CAddress(String.concat "." parts, $3)
        else address_error $1 $3
      }
;

arithmetic :
    | NEG expr { mkexpr Neg [$2] }
    | expr PLUS expr { mkexpr Add [$1; $3] }
    | expr NEG expr %prec MINUS { mkexpr Add [$1; mkexpr Neg [$3]] }
    | expr TIMES expr { mkexpr Mult [$1; $3] }
    | expr DIVIDE expr { mkexpr Apply [mkexpr (Var("/")) []; mkexpr Tuple [$1; $3]] }
    | expr MODULO expr { mkexpr Apply [mkexpr (Var("%")) []; mkexpr Tuple [$1; $3]] }

    /* Error handling */
    | NEG error         { arith_error 1 }
    | expr PLUS error   { arith_error 2 }
    | expr TIMES error  { arith_error 2 }
    | expr DIVIDE error { arith_error 2 }
    | expr MODULO error { arith_error 2 }
;

predicate :
    | NOT expr { mkexpr Neg [$2] }
    | expr AND expr { mkexpr Mult [$1; $3] }
    | expr OR expr { mkexpr Add [$1; $3] }
    | expr LT expr { mkexpr Lt [$1; $3] }
    | expr EQ expr { mkexpr Eq [$1; $3] }
    | expr LEQ expr { mkexpr Leq [$1; $3] }
    | expr NEQ expr { mkexpr Neq [$1; $3] }
    | expr GT expr { mkexpr Neg [mkexpr Leq [$1; $3]] }
    | expr GEQ expr { mkexpr Neg [mkexpr Lt [$1; $3]] }

    /* Error handling */
    | expr LT error  { comp_error() }
    | expr EQ error  { comp_error() }
    | expr LEQ error { comp_error() }
    | expr NEQ error { comp_error() }
    | expr GT error  { comp_error() }
    | expr GEQ error { comp_error() }
;

conditional :
    | IF expr THEN expr ELSE expr { mkexpr IfThenElse [$2; $4; $6] }

    /* Error handling */
    | IF expr THEN expr ELSE error { cond_error "else branch" }
    | IF expr THEN error           { cond_error "then branch" }
    | IF error                     { cond_error "predicate" }
;

lambda :
     | BACKSLASH arg RARROW expr { mkexpr (Lambda($2)) [$4] }
     /* Alternative syntax for indicating non-tuple output */
     | BACKSLASH arg RASSOC expr { mkexpr (Lambda($2)) [$4] }

     /* Error handling */
     | BACKSLASH arg RARROW error { lambda_error "body" }
     | BACKSLASH error            { lambda_error "argument" }
;

access :
    | expr LBRACKET tuple RBRACKET { mkexpr Slice [$1; $3] }
    | PEEK LPAREN expr RPAREN { mkexpr Peek [$3] }
;

mutation :
    /* Inserts, deletes and sends use a vararg function syntax for their value/payload */
    | INSERT LPAREN expr COMMA tuple RPAREN { mkexpr Insert [$3; $5] }
    | DELETE LPAREN expr COMMA tuple RPAREN { mkexpr Delete [$3; $5] }

    /* Updates must explicitly specify their new/old value as a tuple */
    | UPDATE LPAREN expr COMMA expr COMMA expr RPAREN { mkexpr Update [$3; $5; $7] }

    | expr LARROW expr { mkexpr Assign [$1; $3] }
    | expr GETS expr { mkexpr Assign [$1; $3] }
    | expr COLONGETS expr { mkexpr Assign [$1;$3] }

    /* Error handling */
    | INSERT LPAREN expr error { value_error 2 }
    | INSERT LPAREN error      { coll_error 1 }
    | UPDATE LPAREN expr error { value_error 2 }
    | UPDATE LPAREN error      { coll_error 1 }
    | DELETE LPAREN expr error { value_error 2 }
    | DELETE LPAREN error      { coll_error 1 }
    | expr LARROW error        { assign_error "reference" }
    | expr GETS error          { assign_error "copy" }
    | expr COLONGETS error     { assign_error "refcopy" }
;

transformers :
    | expr CONCAT expr                                    { mkexpr Combine [$1; $3] }
    | MAP LPAREN expr COMMA expr RPAREN                   { mkexpr Map [$3; $5] }
    | ITERATE LPAREN expr COMMA expr RPAREN               { mkexpr Iterate [$3; $5] }
    | FILTERMAP LPAREN expr COMMA expr COMMA expr RPAREN  { mkexpr FilterMap [$3; $5; $7] }
    | FLATTEN LPAREN expr RPAREN                          { mkexpr Flatten [$3] }
    | AGGREGATE LPAREN expr COMMA expr COMMA expr RPAREN  { mkexpr Aggregate [$3; $5; $7] }
    | GROUPBYAGGREGATE LPAREN expr COMMA expr COMMA expr COMMA expr RPAREN {
        mkexpr GroupByAggregate [$3; $5; $7; $9]
    }
    | SORT LPAREN expr COMMA expr RPAREN { mkexpr Sort [$3; $5] }

    /* Error handling */
    | expr CONCAT error { print_error("Expected expression for combine") }
    
    | MAP LPAREN expr error { coll_error 2 }
    | MAP LPAREN error      { coll_lambda_error "map" 1 }
    | MAP error             { print_error("Invalid map syntax") }
    
    | ITERATE LPAREN expr error { coll_error 2 }
    | ITERATE LPAREN error      { coll_lambda_error "iterate" 1 }
    | ITERATE error             { print_error("Invalid iterate syntax") }
    
    | FILTERMAP LPAREN expr COMMA expr error { coll_error 3 }
    | FILTERMAP LPAREN expr error            { coll_lambda_error "filtermap map" 2 }
    | FILTERMAP LPAREN error                 { coll_lambda_error "filtermap filter" 1 }
    | FILTERMAP error                        { print_error("Invalid filtermap syntax") }
    
    | FLATTEN LPAREN error { print_error("Expected a nested collection") }
    
    | AGGREGATE LPAREN expr COMMA expr error { coll_error 3 }
    | AGGREGATE LPAREN expr error            { value_error 2 }
    | AGGREGATE LPAREN error                 { coll_lambda_error "aggregate" 1 }
    | AGGREGATE error                        { print_error("Invalid fold syntax") }
    
    | GROUPBYAGGREGATE LPAREN expr COMMA expr COMMA expr error { coll_error 4 }
    | GROUPBYAGGREGATE LPAREN expr COMMA expr error            { value_error 3 }
    | GROUPBYAGGREGATE LPAREN expr error                       { coll_lambda_error "group-by aggregate" 2 }
    | GROUPBYAGGREGATE LPAREN error                            { coll_lambda_error "grouping" 1 }
    | GROUPBYAGGREGATE error                                   { print_error("Invalid groupby syntax") }
;

block :
    | DO LBRACE expr_seq RBRACE { mkexpr Block $3 }
;

/* Sequence primitives */
integer_list :
    | INTEGER                       { [$1] }
    | INTEGER COMMA integer_list    { $1::$3 }
;

identifier_list :
    | IDENTIFIER                       { [$1] }
    | IDENTIFIER COMMA identifier_list { $1::$3 } 
;


/* Testing */
program_test :
    | program EXPECTED named_expr_list     { ProgTest ($1, $3) }
    | program NETWORK EXPECTED named_expr_list { NetworkTest ($1, $4) }
    | program EXPECTED error               { print_error "invalid expected value list" }
    | program NETWORK EXPECTED error       { print_error "invalid expected value list" }
    | program error                        { print_error "no expected values specified for program test" }
;

expression_test_list :
    | expr EXPECTED check_expr                       { [[], $1, $3] }
    | program expr EXPECTED check_expr               { [$1, $2, $4] }
    | expression_test_list SEMICOLON expression_test_list { $1@$3 }
    | expr EXPECTED error                  { print_error "invalid expected expression"}
;

expression_test :
    | expression_test_list                          { ExprTest $1 }

named_expr_list :
    | expr GETS check_expr                          { [$1, $3] }           
    | expr GETS check_expr COMMA named_expr_list    { $5@[$1, $3] }
    | expr GETS error { print_error "invalid check expression"}
;

check_expr :
    | expr          { InlineExpr($1) }
    | FILE STRING   { FileExpr($2) }
;

