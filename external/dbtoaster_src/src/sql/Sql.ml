(**
   Internal representation of a SQL parse tree.  The first representation in 
   the compiler pipeline.  Not all features of SQL are supported. In particular,
   only SUM aggregates are available, and there is no HAVING clause.

{[
   CREATE TABLE name(col1 type1[, ...]) 
   FROM type 
   
   SELECT   gb_term1[, ...], SUM(agg1)[, ...]
   FROM     rel
   WHERE    condition
   GROUP BY 
]}
*)

open Type
open Constants

(**
   A generic exception pertaining to SQL.
   
   The first string is detailed information about the error or ""
*)
exception SqlException of string * string
(**
   An error that occurs during parsing.
   The string is an message, the integer is a 
*)
exception SQLParseError of string * Lexing.position
(**
   A feature of SQL that is unsupported is encountered.
*)
exception FeatureUnsupported of string



(**/**)
let error msg                 = raise (SqlException("", msg))
(**/**)

(**
   A SQL variable.  The first field is the (optional) relation or 
   pseudorelation that the variable is associated with.  Distinct from 
   [Type.var_t], because of this optional relation association.
*)
type sql_var_t = string option * string * type_t

(**
   A SQL schema (list of variables)
*)
type schema_t = sql_var_t list

(**
   A SQL table (analogous to individual elements of [Schema.t]).
   
   [Schema.t] is effectively a set of [Sql.table_t]s grouped by source.
*)
type table_t = string * schema_t * Schema.rel_type_t *
               (Schema.source_t * Schema.adaptor_t)

(**
   Sql arithmetic operations
*)
type arith_t = Sum | Prod | Sub | Div

(**
   Sql aggregate types
*)
type agg_t = SumAgg | CountAgg of sql_var_t list option | AvgAgg

type select_option_t = 
   | Select_Distinct

(**
   A Sql expression (which appears in either the target clause, or as part of a 
   comparison condition)
*)
type expr_t = 
 | Const      of const_t                    (** A constant value *)
 | Var        of sql_var_t                  (** A variable *)
 | SQLArith   of expr_t * arith_t * expr_t  (** A binary arithmetic operation *)
 | Negation   of expr_t                     (** Negation of an expression *)
 | NestedQ    of select_t                   (** A single-target nested query 
                                                (which evaluates to a constant 
                                                value) *)
 | ExternalFn of string * expr_t list       (** A reference to an external 
                                                function. *)
 | Aggregate  of agg_t * expr_t             (** An aggregate operator.  This
                                                only makes sense if this
                                                expression appears in the target
                                                (or having) clause(s). *)
 | Case of (cond_t * expr_t) list * expr_t  (** A branching expression.  A list 
                                                of conditions to be evaluated in
                                                order, and an else clause to be 
                                                evaluated if nothing else is 
                                                true. *)

(**
   A member of the target clause (a named Sql expression).  The result of the 
   query will include a column with the indicated name with values based on the 
   provided expression.
*)
and target_t = string * expr_t

(**
   A member of the where clause (or technically, having clauses, if they were 
   supported).  This is a boolean query.
*)
and cond_t   = 
 | Comparison of expr_t * cmp_t * expr_t (** A comparison between two 
                                             expressions *)
 | And        of cond_t * cond_t         (** A conjunction between two 
                                             conditions *)
 | Or         of cond_t * cond_t         (** A disjunction between two
                                             conditions *)
 | Not        of cond_t                  (** The negation of a condition *)
 | Exists     of select_t                (** An existential subquery *)
 | ConstB     of bool                    (** A boolean constant *)
 | Like       of expr_t * string         (** Partial string match *)
 | InList     of expr_t * const_t list   (** Finite list membership test *)

(**
   A member of a from clause.  This can either be a relation, or a nested 
   subquery
*)
and source_t = 
 | Table of string                       (** A table *)
 | SubQ  of select_t                     (** A subquery *)

(**
   A named member of a from clause.
*)
and labeled_source_t = string * source_t

(**
   A full SQL [SELECT] query.  Consists of [target clause] x [from clause] x 
   [where clause] x [group by clause].  A non-group-by query has an empty group-
   by clause.
*)
and select_t =
   | Union of select_t * select_t
   | Select of
   (* SELECT      *) target_t list *
   (* FROM        *) labeled_source_t list *
   (* WHERE       *) cond_t *
   (* GROUP BY    *) sql_var_t list *
   (* HAVING      *) cond_t *
   (* etc...      *) select_option_t list

(**
   A SQL statement.  This can be either a [CREATE TABLE] statement or a [SELECT]
   statement 
*)
type t = 
 | Create_Table of table_t
 | SelectStmt   of select_t

(**
   The content of a SQL file.  This includes all the tables defined in the file
   and all select statements appearing in the file
*)
type file_t = table_t list * select_t list

(**
   An abstract sql entity (any type in sql)
   
   This type is used primarilly for error handling to provide context for
   error messages.  
*)
type sql_entity_t = 
   | SqlStatement  of t
   | SqlQuery      of select_t
   | SqlExpression of expr_t
   | SqlCondition  of cond_t
   | NoSqlEntity
(**
   An error caused by an invalid Sql expression
*)
exception InvalidSql of
   string * string * sql_entity_t list

(**/**)
(** Utilities for creating InvalidSqlExpressions *)

let sql_stack:sql_entity_t list ref = ref [];;

let push_sql_stack (entity:sql_entity_t): unit =
   sql_stack := entity :: !sql_stack

let pop_sql_stack (): unit =
   if !sql_stack = [] then failwith "Popping an empty sql stack"
   else sql_stack := List.tl !sql_stack

let evaluate_with_entity (entity:sql_entity_t) expr =
   push_sql_stack entity;
   let ret = expr () in
   pop_sql_stack();
   ret

let invalid_sql ?(detail = "") msg =
   raise (InvalidSql(msg,detail,!sql_stack))

(**/**)

(* Construction Helpers *)
(**
   Generate a SQL file from a provided statememnt.
   @param stmt  A SQL statement
   @return      A SQL file containing only [stmt]
*)
let mk_file (stmt:t): file_t =
   match stmt with
    | Create_Table(table) -> ([table], [])
    | SelectStmt(select)  -> ([], [select])

(**
   Merge the content of two SQL files.
   @param lhs  A SQL file
   @param rhs  A SQL file
   @return     A SQL file containing everything in both [lhs] and [rhs]
*)
let merge_files (lhs:file_t) (rhs:file_t): file_t =
   ((fst lhs) @ (fst rhs), (snd lhs) @ (snd rhs))

(**
   An empty SQL file
*)
let empty_file:file_t = ([],[])

(**
   Add a statement at the end of a SQL file
   @param stmt A SQL statement
   @param file A SQL file
   @return     A SQL file containing [stmt] and everything in [file]
*)
let add_to_file (stmt:t) (file:file_t): file_t =
   merge_files file (mk_file stmt)

(**
   Add a statement at the beginning a SQL file
   @param stmt A SQL statement
   @param file A SQL file
   @return     A SQL file containing [stmt] and everything in [file]
*)
let add_to_file_first (stmt:t) (file:file_t): file_t =
   merge_files (mk_file stmt) file 


(**
   Create a conjunction between two terms.  If either term is a boolean
   constant, then evaluate the conjunction in-place.
   @param lhs  A condition
   @param rhs  A condition
   @return     [lhs]^[rhs] if neither is a boolean constant, [false] if either
               is [false], or if either is [true] then return the other.
*)
let mk_and (lhs:cond_t) (rhs:cond_t): cond_t =
   match lhs with
    | ConstB(true) -> rhs
    | ConstB(false) -> ConstB(false)
    | _ -> (
      match rhs with
       | ConstB(true) -> lhs
       | ConstB(false) -> ConstB(false)
       | _ -> And(lhs, rhs)
    )

(**
   Create a disjunction between two terms.  If either term is a boolean
   constant, then evaluate the disjunction in-place.
   @param lhs  A condition
   @param rhs  A condition
   @return     [lhs]V[rhs] if neither is a boolean constant, [true] if either
               is [true], or if either is [false] then return the other.
*)
let mk_or (lhs:cond_t) (rhs:cond_t): cond_t =
   match lhs with
    | ConstB(true) -> ConstB(true)
    | ConstB(false) -> rhs
    | _ -> (
      match rhs with
       | ConstB(true) -> ConstB(true)
       | ConstB(false) -> lhs
       | _ -> Or(lhs, rhs)
    )

(**** Printing ****)
(**
   Produce the (Sqlparser-compatible) symbol of the specified arithmetic 
   operator.
   @param op  An arithmetic operator
   @return    The symbol corresponding to [op]
*)
let string_of_arith_op (op:arith_t): string =
   match op with
      | Sum -> "+" | Prod -> "*" | Sub -> "-" | Div -> "/"

(**
   Produce the (Sqlparser-compatible) name of the specified aggregate function 
   (without parenthesis).
   @param agg   An aggregate function
   @return      The function name of [var]
*)
let string_of_agg (agg:agg_t): string =
   match agg with SumAgg   -> "SUM"
                | CountAgg(fields) -> "COUNT"
                | AvgAgg   -> "AVG"

(**
   Produce the (Sqlparser-compatible) fully qualified (if possible) name of the
   specified variable.
   @param var  A SQL variable
   @return     The fully qualified (if possble) string form of [var]
*)
let string_of_var ((s,v,_):sql_var_t): string =
   match s with Some(source) -> source^"."^v | None -> v

(**/**)
let mk_sym = FreshVariable.declare_class "sql/Sql" "sql"
(**/**)

(**
   Produce a name for the expression.  This is used primarilly by Sqlparser to
   generate unique target names for expressions.  If the expression is a single 
   variable, then the name of the variable is the name of the expression.  
   Otherwise, we generate a name based on the root level operand.
   @param expr  A SQL expression
   @return      A unique name for the expression
*)
let name_of_expr (expr:expr_t): string =
   let arbitrary basename = mk_sym ~inline:("_"^basename) () in
   begin match expr with
      | Const(c)            -> arbitrary "constant"
      | Var(_,vn,_)         -> vn
      | SQLArith(_,_,_)
      | ExternalFn(_,_)
      | Case(_)              
      | Negation(_)         -> arbitrary "expression"
      | NestedQ(_)          -> arbitrary "nested_query"
      | Aggregate(a,_)      -> arbitrary (
                                    (String.lowercase (string_of_agg a))^
                                    "_aggregate"
                               )
   end
   
(** 
   Produce the (SQL-compatible) string representation of a constant.
   @param a   A constant
   @return    The SQL-compatible string-representation of [a]
*)
let string_of_const (const:Constants.const_t):string =
   match const with
     | CBool(true)  -> "1"
     | CBool(false) -> "0"
     | CInt(av) -> string_of_int av
     | CFloat(av) -> string_of_float av
     | CString(av) -> "'"^av^"'"   
     | CDate _     -> Constants.sql_of_const const
     | CInterval _ -> Constants.sql_of_const const

(**
   Produce the (Sqlparser-compatible) representation of the specified SQL 
   expression.
   @param expr  A SQL expression
   @return      The string representation of [expr]
*)
let rec string_of_expr (expr:expr_t): string =
   match expr with
      | Const(c) -> string_of_const c
      | Var(v) -> string_of_var(v)
      | SQLArith(a,op,b) -> "("^(string_of_expr a)^")"^
                              (string_of_arith_op op)^
                              "("^(string_of_expr b)^")"
      | Negation(a) -> "-("^(string_of_expr a)^")"
      | NestedQ(q) -> "("^(string_of_select q)^")"
      | Aggregate(CountAgg(None), _) -> "COUNT(*)"
      | Aggregate(CountAgg(Some(fields)), _) -> 
         "COUNT( DISTINCT "^
         (String.concat "" (List.map (fun x -> (string_of_var x)^" ") fields))^
         ")"
      | Aggregate(agg,a) -> (string_of_agg agg)^"("^
                            (string_of_expr a)^")"
      | ExternalFn(fn,fargs) -> fn^"("^(ListExtras.string_of_list ~sep:", "
                                          string_of_expr fargs)^")"
      | Case(cases, else_branch) -> 
         "CASE "^(String.concat " " (List.map (fun (c,e) -> 
               "WHEN "^(string_of_cond c)^" THEN "^(string_of_expr e)) cases))^
         " ELSE "^(string_of_expr else_branch)

(**
   Produce the (Sqlprser-compatible) representation of the specified SQL 
   condition
   @param cond  A SQL condition
   @return      The string representation of [cond]
*)
and string_of_cond (cond:cond_t): string =
   match cond with 
      | Comparison(a,cmp,b) -> "("^(string_of_expr a)^")"^
                                   (string_of_cmp cmp)^
                               "("^(string_of_expr b)^")"
      | And(a,b)      -> "("^(string_of_cond a)^") AND ("^(string_of_cond b)^")"
      | Or(a,b)       -> "("^(string_of_cond a)^") OR ("^(string_of_cond b)^")"
      | Not(c)        -> "NOT "^(string_of_cond c)
      | Exists(s)     -> "EXISTS ("^(string_of_select s)^")"
      | Like(e, s)    -> "("^(string_of_expr e)^") LIKE '"^s^"'"
      | InList(e, l)  -> "("^(string_of_expr e)^") IN VALUES ("^
                         (ListExtras.string_of_list ~sep:"," string_of_const l)^
                        ")"
      | ConstB(true)  -> "TRUE"
      | ConstB(false) -> "FALSE"

(**
   Produce the (Sqlparser-compatible) representation of the specified SQL 
   [SELECT] query.
   @param stmt  A SQL [SELECT] statement
   @return      The string representation of [stmt]
*)
and string_of_select (stmt:select_t): string =
   match stmt with
   | Union(s1, s2) -> (string_of_select s1) ^ " UNION " ^ (string_of_select s2)
   | Select(target, from, where, gb, having, opts) ->
      "SELECT "^
      (if List.mem Select_Distinct opts then "DISTINCT " else "")^
      (ListExtras.string_of_list ~sep:", " 
                                 (fun (n,e) -> (string_of_expr e)^" AS "^n)
                                 target)^
      (if List.length from > 0 then
            " FROM "^(ListExtras.string_of_list ~sep:", " (fun (n,s) -> 
               (match s with 
                  | Table(t) -> t
                  | SubQ(s) -> "("^(string_of_select s)^")"
               )^" AS "^n
            ) from)
         else "")^
      (if where <> ConstB(true) then
            " WHERE "^(string_of_cond where)
         else "")^
      (if List.length gb > 0 then
            " GROUP BY "^(ListExtras.string_of_list ~sep:", " string_of_var gb)
         else "")^
      (if having <> ConstB(true) then
            " HAVING "^(string_of_cond having)
         else "")
(*      ^*)
(*   ";" *)

(**
   Produce the (Sqlparser-compatible) representation of the specified [CREATE
   TABLE] statement.
   @param stmt   A SQL [CREATE TABLE] statement
   @return       The string representation of [stmt]
*)
let string_of_table ((name, vars, reltype, (source, adaptor)):table_t): string =
   "CREATE "^(if reltype == Schema.TableRel then "TABLE" else "STREAM")^
   " "^name^"("^
      (ListExtras.string_of_list ~sep:", " (fun (_,v,t) ->
         v^" "^(string_of_type t)
      ) vars)^
      ");"

(* Misc Utility *)
(**
   Given a list of tables, return the table with the specified name
   @param t       The table to find in [tables]
   @param tables  A list of tables
   @return        The table in [tables] with name [t]
   @raise SqlException If [tables] contains no table named [t]
*)
let find_table (t:string) (tables:table_t list): table_t =
   try 
      List.find (fun (t2,_,_,_) -> t = t2) tables
   with Not_found -> 
      invalid_sql ("Undefined table: '"^t^"'")

;;

(**
   Compute the type of a SQL expression.
   @param strict  (optional) If explicitly set to false, expr_type will not 
                  attempt to coerce TAny to an exact type.  Normally, this would
                  raise an error whenever this coersion is not possible, which
                  is the case during parsing.
   @param expr    A SQL expression
   @param tables  The database schema (a list of all tables)
   @param sources All members of the [FROM] clause in the [SELECT] statement
                  in the context of which [expr] is being evaluated.
   @return        The type of [expr]
*)
let rec expr_type ?(strict = true) (expr:expr_t) (tables:table_t list) 
                  (sources:labeled_source_t list): type_t =
   let return_if_numeric st msg = 
      begin match st with | TInt | TFloat -> st
                          | TAny when not strict -> st
                          | _ -> invalid_sql (msg^(string_of_type st))
      end
   in
   let rcr e = expr_type ~strict:strict e tables sources in
   evaluate_with_entity (SqlExpression(expr)) (fun () ->
      match expr with
         | Const(c)        -> type_of_const c
         | Var(v)          -> var_type ~strict:strict v tables sources
         | SQLArith(_,Div,_) -> TFloat
         | SQLArith(a,_,b) -> (escalate_type (rcr a) (rcr b))
         | Negation(subexp) ->
            return_if_numeric (rcr subexp) "Negation of "
         | NestedQ(stmt) -> 
            let subsch = select_schema ~strict:strict ~parent_sources:sources
                                       tables stmt in
               if (List.length subsch) != 1 then
                  invalid_sql "Nested query must return a single value"
               else 
                  let (_,_,t) = List.hd subsch in
                     t
         | Aggregate(agg,subexp) -> 
            begin match agg with
               (* Sum takes its type from the nested expression *)
               | SumAgg -> return_if_numeric (rcr subexp) "Aggregate of "
               (* Count ignores the nested expression *)
               | CountAgg _ -> TInt
               (* Average must have numeric inputs but always returns float*)
               | AvgAgg -> 
                  let _ = return_if_numeric (rcr subexp) "Aggregate of " 
                  in TFloat
            end
         | ExternalFn(fn,fargs) ->
            let arg_types = List.map rcr fargs in
            begin try 
               if (not strict) && (List.exists (fun x -> x = TAny) arg_types) 
               then TAny else
                  Functions.infer_type fn arg_types
            with 
               | Not_found -> invalid_sql ("Undeclared Function '"^fn^"'")
               | Functions.InvalidFunctionArguments _ ->
                     invalid_sql ("Invalid function arguments ("^
                                  (ListExtras.string_of_list ~sep:", "
                                                             string_of_type 
                                                             arg_types)^")")
            end
         | Case(cases, else_branch) ->
            Type.escalate_type_list 
               ((expr_type else_branch tables sources) :: 
                  (List.map (fun (_,x) -> expr_type x tables sources) cases))
   )
    
(**
   Compute the schema of a labeled source.  Like source_schema, but the schema
   variables will be bound to the labeled identifier.
   
   @param strict  (optional) If explicitly set to false, schema elements with 
                  type TAny will not be coerced to normal types.  Normally,
                  an error would be raised if this coersion is not possible,
                  which is the case during parsing.
   @param parent_souces (optional) A list of named sources, which are in-scope 
                        when [stmt] is evaluated
   @param tables  The database schema (a list of all tables)
   @param source  The labeled source
   @return        The schema (types/names) of the result of [stmt]
*)
and labeled_source_schema ?(strict=true) ?(parent_sources = []) 
                          (tables:table_t list) 
                          ((label,source):labeled_source_t): schema_t =
   Debug.print "LOG-SQL" (fun () -> "[SQL] Labeled sources of : "^label);
   List.map (fun (_,varn,vart) -> ((Some(label)),varn,vart))
            (source_schema ~strict:strict ~parent_sources:parent_sources
                           tables source)

(**
   Compute the schema of a source

   @param strict  (optional) If explicitly set to false, schema elements with 
                  type TAny will not be coerced to normal types.  Normally,
                  an error would be raised if this coersion is not possible,
                  which is the case during parsing.
   @param parent_souces (optional) A list of named sources, which are in-scope 
                        when [stmt] is evaluated
   @param tables  The database schema (a list of all tables)
   @param source  The source
   @return        The schema (types/names) of the result of [stmt]
*)
and source_schema ?(strict=true) ?(parent_sources = []) (tables:table_t list) 
                  (source:source_t): schema_t =
   match source with 
      | Table(table_name) -> 
         let (_,sch,_,_) = 
            List.find (fun (cmp_name,_,_,_) -> cmp_name=table_name) tables
         in sch
      | SubQ(stmt) -> select_schema ~strict:strict 
                                    ~parent_sources:parent_sources 
                                    tables 
                                    stmt

(**
   Compute the schema of a [SELECT] statement
   @param strict  (optional) If explicitly set to false, schema elements with 
                  type TAny will not be coerced to normal types.  Normally,
                  an error would be raised if this coersion is not possible,
                  which is the case during parsing.
   @param parent_souces (optional) A list of named sources, which are in-scope 
                        when [stmt] is evaluated
   @param tables  The database schema (a list of all tables)
   @param stmt    The [SELECT] statement
   @return        The schema (types/names) of the result of [stmt]
*)
and select_schema ?(strict=true) ?(parent_sources = []) (tables:table_t list) 
                  (stmt:select_t): schema_t =
   match stmt with
   | Union(s1, s2) ->
      let rcr stmt = 
         select_schema ~strict:strict ~parent_sources:parent_sources tables stmt 
      in
      let sch1, sch2 = (rcr s1, rcr s2) in
      if sch1 = sch2 then
         sch1
      else
         invalid_sql "Schemas do not match"
   | Select(targets, sources, _, _, _, _) ->
       evaluate_with_entity (SqlQuery(stmt)) (fun () ->
          List.map (fun (name, expr) -> 
             (  None, name, expr_type ~strict:strict expr tables 
                                      (sources@parent_sources))
          ) targets
       )

(**
   Find the member of a [FROM] clause that a specified SQL variable's name 
   should be associated with.
   @param v       A SQL variable's name
   @param tables  The database schema (a list of all tables)
   @param sources All members of the [FROM] clause in the [SELECT] statement
                  in the context of which the variable named [v] is being 
                  evaluated.
   @return        The element of [sources] that has a schema with a column
                  named [v]
*)
and source_for_var_name (v:string) (tables:table_t list) 
                        (sources:labeled_source_t list): labeled_source_t =
   Debug.print "LOG-SQL-BIND" (fun () ->
      "Tracking source for variable '"^v^"' in: "^
      (ListExtras.ocaml_of_list fst sources)
   );
   let candidates =
      List.find_all (fun (sn,sd) ->
         let sch = 
            match sd with
               | Table(t) -> 
                  Debug.print "LOG-SQL-BIND" 
                     (fun () -> "  (table "^t^":"^sn^")");
                  let (_,sch,_,_) = find_table t tables in sch
               | SubQ(s) -> 
                  Debug.print "LOG-SQL-BIND" (fun () -> "  (query "^sn^")");
                  select_schema ~parent_sources:
                     (List.filter (fun (cmp_name,_) -> cmp_name <> sn) sources)
                     tables s
         in
            List.exists (fun (_,v2,_) -> v2 = v) sch
      ) sources
   in
      if List.length candidates = 0 then
         invalid_sql ("Unbound variable '"^v^"'")
      else if List.length candidates > 1 then
         invalid_sql ("Ambiguous variable '"^v^"'; Provide an explicit source")
      else
         List.hd candidates
(**
   Find the member of a [FROM] clause that a specified variable should be 
   associated with.  If the variable is already associated with a source, the
   source name is validated and the corresponding source is returned.
   @param var    A SQL variable
   @param tables  The database schema (a list of all tables)
   @param sources All members of the [FROM] clause in the [SELECT] statement
                  in the context of which [var] is being evaluated.
   @return        The element of [sources] that [var] is associated with.
*)
and source_for_var ((s,v,_):sql_var_t) (tables:table_t list) 
                   (sources:labeled_source_t list): labeled_source_t =
   match s with 
      | Some(sn) -> 
         if List.mem_assoc sn sources then
            (sn,List.assoc sn sources)
         else
            invalid_sql ~detail:("Valid sources: "^
                                 (ListExtras.ocaml_of_list fst sources))
                        ("Unbound source '"^sn^"'")
      | None -> source_for_var_name v tables sources
      
(**
   Obtain the type of the specified variable.  If the variable has the wildcard
   type [TAny] and the optional [strict] parameter isn't explicitly set to 
   false, attempt to dereference the variable and obtain the variable's type 
   from schema information.
   @param strict  (optional) If explicitly set to false, schema elements with 
                  type TAny will not be coerced to normal types.  Normally,
                  an error would be raised if this coersion is not possible,
                  which is the case during parsing.
   @param v       A SQL variable
   @param tables  The database schema (a list of all tables)
   @param sources All members of the [FROM] clause in the [SELECT] statement
                  in the context of which [v] is being evaluated.
   @return        The variable's type as encoded in [v], or if that type is 
                  [TAny], the variable's type information from the schema.
*)
and var_type ?(strict = true) (v:sql_var_t) (tables:table_t list) 
             (sources:labeled_source_t list): type_t =
   let (_,vn,t) = v in
   match t with
      | TAny when strict -> 
         (* Try to dereference the variable and figure out its type *)
         let (source_name,source) = source_for_var v tables sources in
         let sch = (
            match source with 
               | Table(table) -> 
                     let (_,sch,_,_) = find_table table tables in sch
               | SubQ(subq) -> 
                     select_schema ~parent_sources:sources tables subq 
         )
         in
         (
            try 
               let (_,_,t) = List.find (fun (_,vn2,_) -> vn2 = vn) sch in t
            with Not_found ->
               invalid_sql ~detail:(
                  "Schema of '"^source_name^"' is : "^
                  (ListExtras.string_of_list ~sep:", " string_of_var sch)
               ) ("Unbound variable '"^(string_of_var v)^"'")
         )
      | _ -> t

;;

(**
   Ensure that all variables in a SQL [SELECT] statement have been associated
   with one of the sources in the statement or one of its parents.  This is
   a deep rewriting.  Also ensures that sources are uniquely named in 
   a given scope.
   @param parent_sources (optional) A list of sources in the context of which
                         [stmt] is being evaluated.
   @param stmt           A SQL [SELECT] statement.
   @param tables         The database schema (a list of all tables)
   @return               [stmt] recursively rewritten with all of its variables 
                         bound to a source in the [FROM] clause of an enclosing
                         [SELECT] statement.
*)
let rec bind_select_vars ?(parent_sources = [])
                         (query:select_t)
                         (tables:table_t list): select_t =
   match query with
   | Union(s1, s2) ->
      let rcr stmt = 
         bind_select_vars ~parent_sources:parent_sources stmt tables 
      in 
      Union(rcr s1, rcr s2)
   | Select(targets,inner_sources,conds,gb,having,opts) ->
      let sources = parent_sources @ inner_sources in
      let rcr_c c = bind_cond_vars c tables sources in
      let rcr_e e = bind_expr_vars e tables sources in
      let rcr_q ?(qn = "") q = 
         (* When recurring on a subquery, we filter out the subquery itself *)
         bind_select_vars 
            ~parent_sources:(List.filter (fun (sn,_) -> sn <> qn) sources)
            q tables 
      in
      Debug.print "LOG-SQL-BIND" (fun () ->
         "[SQL-BIND] Binding query with sources: "^
         (ListExtras.ocaml_of_list fst sources)^"\n\t"^
         (string_of_select query)
      );
      let mapped_targets = 
         List.map (fun (tn,te) -> (tn,rcr_e te)) targets
      in
         evaluate_with_entity (SqlQuery(query)) (fun () -> Select(
            mapped_targets,
            List.map (fun (sn,s) -> 
               if List.length (List.filter (fun x -> x = sn)
                                 (List.map fst sources)) > 1 then (
                  invalid_sql ("Duplicate source name: '"^sn^"'")
               );
               (sn, (match s with Table(_) -> s 
                                | SubQ(subq) -> SubQ(rcr_q ~qn:sn subq)))
            ) inner_sources,
            rcr_c conds,
            List.map (fun (s,v,t) -> 
               if (s = None) && (List.mem_assoc v mapped_targets)
               then 
                  begin match (List.assoc v mapped_targets) with
                     | Var(ts,tv,tt) when tv = v -> (ts, v, tt)
                     | _ -> (None, v, expr_type ~strict:true 
                                        (List.assoc v mapped_targets) 
                                        tables sources)
                  end
               else 
                 ((Some(fst (source_for_var (s,v,t) tables sources))),
                  v,
                  var_type (s,v,t) tables sources)
            ) gb,
            rcr_c having,
            opts)
         )

(**
   Ensure that all variables in a SQL condition have been associated with
   one of the sources in the statement or one of its parents.  Also
   ensures that sources are uniquely named in a given scope.
   @param cond    A SQL condition
   @param tables  The database schema (a list of all tables)
   @param sources All members of the [FROM] clause in the [SELECT] statement
                  in the context of which [cond] is being evaluated.
   @return        [cond] recursively rewritten with all of its variables bound
                  to a source in [sources] or (as appropriate) a source nested
                  within [cond].
*)
and bind_cond_vars (cond:cond_t) (tables:table_t list)
                   (sources:labeled_source_t list): cond_t =
   let rcr_c c = bind_cond_vars c tables sources in
   let rcr_e e = bind_expr_vars e tables sources in
   let rcr_q q = bind_select_vars ~parent_sources:sources q tables in
   Debug.print "LOG-SQL-BIND" (fun () ->
      "[SQL-BIND] Binding condition with sources: "^
      (ListExtras.ocaml_of_list fst sources)^"\n\t"^
      (string_of_cond cond)
   );
   evaluate_with_entity (SqlCondition(cond)) (fun () ->
      match cond with
         | Comparison(a,op,b) -> Comparison(rcr_e a,op,rcr_e b)
         | And(a,b) -> And(rcr_c a,rcr_c b)
         | Or(a,b)  -> Or(rcr_c a,rcr_c b)
         | Not(a)   -> Not(rcr_c a)
         | Exists(q) -> Exists(rcr_q q)
         | ConstB(_) -> cond
         | Like(e,s) -> Like(rcr_e e, s)
         | InList(e,l) -> InList(rcr_e e, l)
   )
   
(**
   Ensure that all variables in a SQL expression have been associated with
   one of the sources in the statement or one of its parents.  Also
   ensures that sources are uniquely named in a given scope.
   @param expr    A SQL expression
   @param tables  The database schema (a list of all tables)
   @param sources All members of the [FROM] clause in the [SELECT] statement
                  in the context of which [expr] is being evaluated.
   @return        [expr] recursively rewritten with all of its variables bound
                  to a source in [sources] or (as appropriate) a source nested
                  within [expr].
*)
and bind_expr_vars (expr:expr_t) (tables:table_t list) 
                   (sources:labeled_source_t list): expr_t =
   let rcr_c c = bind_cond_vars c tables sources in
   let rcr_e e = bind_expr_vars e tables sources in
   let rcr_q q = bind_select_vars ~parent_sources:sources q tables in
   Debug.print "LOG-SQL-BIND" (fun () ->
      "[SQL-BIND] Binding expression with sources: "^
      (ListExtras.ocaml_of_list fst sources)^"\n\t"^
      (string_of_expr expr)
   );
   let bind_var (s,v,t) = 
      (Some(fst (source_for_var (s,v,t) tables sources))),
      v,
      var_type (s,v,t) tables sources
   in
   evaluate_with_entity (SqlExpression(expr)) (fun () ->
      match expr with 
         | Const(_) -> expr
         | Var(v) -> Var(bind_var v)
         | SQLArith(a,op,b) -> SQLArith(rcr_e a,op,rcr_e b)
         | Negation(a) -> Negation(rcr_e a)
         | NestedQ(q) -> NestedQ(rcr_q q)
         | Aggregate(CountAgg(Some(fields)),a) -> 
            Aggregate(CountAgg(Some(List.map bind_var fields)), rcr_e a)
         | Aggregate(agg,a) -> Aggregate(agg, rcr_e a)
         | ExternalFn(fn,fargs) -> ExternalFn(fn, List.map rcr_e fargs)
         | Case(cases, else_branch) -> 
            Case(List.map (fun (c,e) -> (rcr_c c, rcr_e e)) cases, 
                 rcr_e else_branch)
   )

(**
   Determine whether the indicated expression requires an aggregate computation.
   @param expr   A SQL expression
   @return       True if evaluating [expr] requires an aggregate computation.
*)
let rec is_agg_expr (expr:expr_t): bool =
   match expr with 
      | Const(_) -> false
      | Var(_) -> false
      | SQLArith(a,_,b) -> (is_agg_expr a) || (is_agg_expr b)
      | Negation(e) -> is_agg_expr e
      | NestedQ(_) -> false
      | Aggregate(_,_) -> true
      | ExternalFn(fn,fargs) -> 
         List.fold_left (||) false (List.map is_agg_expr fargs)
      | Case(cases, else_branch) -> 
         List.fold_left (||) (is_agg_expr else_branch) 
                        (List.map (fun (_,x) -> is_agg_expr x) cases)

(**
   Determine whether the indicated SQL query is an aggregate query.
   @param stmt   A SQL [SELECT] statement
   @return       True if [stmt] describes an aggregate query
*)
let rec is_agg_query (stmt): bool =
   match stmt with 
   (* TODO: Check whether this is the correct behavior for UNIONs *)
   | Union(s1, s2) -> (is_agg_query s1) && (is_agg_query s2)
   | Select(targets, _, _, _, _, _) -> 
      List.exists is_agg_expr (List.map snd targets)
;;

let rec push_down_nots (cond:cond_t): cond_t =
   let rcr = push_down_nots in
   begin match cond with
      | Not(Not(c)) -> rcr c
      | Not(Comparison(e1, cmp, e2)) ->
         Comparison(e1, inverse_of_cmp cmp, e2)
      | Not(And(c1,c2)) -> rcr (Or((Not(c1)),(Not(c2))))
      | Not(Or(c1,c2)) -> rcr (And((Not(c1)),(Not(c2))))
      | Not(ConstB(c)) -> ConstB(not c)
      | And(c1,c2) -> And(rcr c1, rcr c2)
      | Or(c1,c2) -> Or(rcr c1, rcr c2)
      | _ -> cond
   end

;;

let rec rewrite_having_expr (q_name:string) 
                            (tables:table_t list)
                            (sources:labeled_source_t list)
                            (expr:expr_t): (expr_t * (string * expr_t) list) =
   let rcr_c c = rewrite_having_cond q_name tables sources c in
   let rcr e = rewrite_having_expr q_name tables sources e in
   match expr with 
   | Const(_) -> (expr, [])
   | Var(_) -> (expr, [])
   | SQLArith(a,op,b) -> 
      let ra, ral = rcr a in 
      let rb, rbl = rcr b in
      (SQLArith(ra, op, rb), ral @ rbl)
   | Negation(e) -> 
      let re, rel = rcr e in
      (Negation(re), rel)
   | NestedQ(q) -> (expr, [])
   | Aggregate(_,_) ->
      let t = expr_type expr tables sources in 
      let n = mk_sym ~inline:("_agg") () in
      (Var(Some(q_name),n,t), [n, expr])
   | ExternalFn(fn,fargs) -> 
      let rfargs = List.map rcr fargs in
      let rfargsl = List.fold_left (@) [] (List.map snd rfargs) in
      (ExternalFn(fn, List.map fst rfargs), rfargsl)
   | Case(cases, else_branch) ->
      let rc, rcl = 
         List.fold_left 
            (fun (rcases, rcasesl) (cond, expr) -> 
               let rcond, rcondl = rcr_c cond in
               let re, rel = rcr expr in
               (rcases @ [(rcond, re)], rcasesl @ rcondl @ rel)) 
            ([], []) 
            cases
      in 
      let re, rel = rcr else_branch in
      (Case(rc, re), rcl @ rel)

and rewrite_having_cond (q_name:string)
                        (tables:table_t list)
                        (sources:labeled_source_t list)
                        (cond:cond_t): (cond_t * (string * expr_t) list) =
   let rcr_c c = rewrite_having_cond q_name tables sources c in
   let rcr_e e = rewrite_having_expr q_name tables sources e in
   match cond with
   | Comparison(a,op,b) ->
      let ra, ral = rcr_e a in 
      let rb, rbl = rcr_e b in
      (Comparison(ra, op, rb), ral @ rbl)
   | And(a,b) ->
      let ra, ral = rcr_c a in 
      let rb, rbl = rcr_c b in
      (And(ra, rb), ral @ rbl)
   | Or(a,b) -> 
      let ra, ral = rcr_c a in 
      let rb, rbl = rcr_c b in
      (Or(ra, rb), ral @ rbl)
   | Not(e) -> 
      let re, rel = rcr_c e in
      (Not(re), rel)
   | Exists(s) -> cond, []
   | ConstB(_) -> cond, []
   | Like(e,s) -> 
      let re, rel = rcr_e e in
      (Like(re, s), rel)
   | InList(e, l) -> 
      let re, rel = rcr_e e in
      (InList(re, l), rel)

and rewrite_having_query (tables:table_t list)  
                         (query:select_t) =
   Debug.print "LOG-REWRITE-HAVING" (fun () -> "[SQL] Rewriting having in: " ^
                                               (string_of_select query));
   let rewritten_query =
      match query with
      | Select(_, _, _, _, ConstB(true), _) -> query
      | Select(targets,sources,cond,gb,having,opts) ->
         let inner_query_name = mk_sym ~inline:("_inner") () in
         let h_replaced_aggs, h_agg_tgs = 
            rewrite_having_cond inner_query_name tables sources having 
         in
         let inner_query = 
            Select(targets @ h_agg_tgs,sources,cond,gb,ConstB(true),opts) 
         in
         let outer_targets =
            List.map (fun (n,e) ->
                        let t = expr_type e tables sources in
                        let v = Var(Some(inner_query_name), n, t) in
                        let te = 
                           if is_agg_expr e then Aggregate(SumAgg, v) else v 
                        in
                        (n, te)) 
                     targets 
         in
         let outer_gb =
            List.fold_left (fun outer_gb (_, e) ->
                              match e with
                              | Var(v) -> v :: outer_gb  
                              | _      -> outer_gb)
                           [] outer_targets         
         in 
         Select(outer_targets,
                [(inner_query_name,SubQ(inner_query))],
                h_replaced_aggs,
                outer_gb,
                ConstB(true),
                []) 
      | _ -> query 
   in 
   Debug.print "LOG-REWRITE-HAVING" 
               (fun () -> "[SQL] Rewritten query: "^
                          (string_of_select rewritten_query));
   rewritten_query

(**
   Expand all targets of type * and *.* and something.*.  Does not recur into
   nested select statements.
   @param stmt    A SQL [SELECT] statement
   @param tables  The database schema (a list of all tables)
   @return        [stmt] rewritten with all targets consisting of AVar(_,*,_)
                  replaced by the corresponding wildcard expansion
*)
let rec expand_wildcard_targets (tables:table_t list)
                                (query:select_t) =
   match query with
   | Union(s1, s2) -> 
      let rcr stmt = expand_wildcard_targets tables stmt in
      Union(rcr s1, rcr s2)
   | Select(targets,sources,cond,gb,having,opts) ->
      Debug.print "LOG-SQL" (fun () -> "[SQL] Expanding query: "^
                                       (string_of_select query));
      let expanded_q = 
         evaluate_with_entity (SqlQuery(query)) (fun () -> Select(
            List.flatten (List.map (fun (tname, texpr) ->
               match texpr with
                  | Var(None, "*", _) ->
                     List.map 
                        (fun expr -> (name_of_expr (Var(expr)), (Var(expr)))) (
                           List.flatten (List.map (labeled_source_schema tables) 
                                                  sources)
                        )
                  | Var(Some(source), "*", _) ->
                     List.map (fun expr -> (name_of_expr (Var(expr)), (Var(expr))))
                              (labeled_source_schema tables
                                 (source,(List.assoc source sources)))
                  | _ -> [tname,texpr]
                  
            ) targets),
            sources, cond, gb, having, opts))
      in
         Debug.print "LOG-SQL" (fun () -> "[SQL] Expanded query: "^
                                          (string_of_select expanded_q));
         expanded_q
(**/**)
let global_table_defs:(string * table_t) list ref = ref []
;;
let reset_table_defs () = 
   (*print_endline ("Resetting tables; Old Tables Are: "^(
      string_of_list0 ", " fst !global_table_defs));*)
   global_table_defs := []
;;

(**
   This is to support include directives in SQL.  Specifically, in order to
   allow the parser to recur into itself, we need definitions from both 
   Sqlparser and Sqllexer.  However, these definitions are not available in the 
   parser itself.  To break the dependency cycle, we use a reference here. The 
   reference is instantiated at the end of Sqllexer. *)
let parse_file:((string -> t list) ref) = ref (fun _ -> []) 
(**/**)

(**
   The string representation of an abstract SQL entity.
*)
let string_of_sql_entity (entity:sql_entity_t): string =
   match entity with 
   | SqlStatement(Create_Table(table)) -> string_of_table table
   | SqlStatement(SelectStmt(stmt))
   | SqlQuery(stmt)                    -> string_of_select stmt
   | SqlExpression(expr)               -> string_of_expr expr
   | SqlCondition(cond)                -> string_of_cond cond
   | NoSqlEntity                       -> "<< unknown >>"

(**
   Generate a human-parseable error message and detail message for an InvalidSql
   exception.
*)
let string_of_invalid_sql (msg, detail, stack): (string * string) =
   if stack = [] then (msg, detail) else
   let immediate_entity = List.hd stack in
   begin match immediate_entity with
      | SqlExpression _ | SqlCondition _ -> 
         let rec string_of_outermost_terms last_entity curr_stack =
            begin match curr_stack with
               | [] -> "\n\tin "^(string_of_sql_entity last_entity)
               | (SqlQuery(q))::_ -> 
                  "\n\tin "^(string_of_sql_entity last_entity)^
                  "\n\tin "^(string_of_select q)
               | hd::rest -> string_of_outermost_terms hd rest
            end
         in (msg^(string_of_outermost_terms immediate_entity (List.tl stack)),
             detail)
         
      | _ -> (msg ^"\n\tin " ^(string_of_sql_entity immediate_entity), detail)
   end
