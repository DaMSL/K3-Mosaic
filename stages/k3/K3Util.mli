open K3

(* Stringification *)

val string_of_address: address_t -> string
val string_of_container_type: container_type_t -> string
val string_of_base_type: base_type_t -> string
val string_of_mutable_type: mutable_type_t -> string
val string_of_value_type: value_type_t -> string
val string_of_type: type_t -> string
val string_of_const: constant_t -> string
val string_of_arg: arg_t -> string
val string_of_expr_tag: expr_tag_t -> string list -> string
val string_of_expr: 'a expr_t -> string

val string_of_declaration: 'a declaration_t -> string
val string_of_instruction: instruction_t -> string
val string_of_statement: 'a statement_t -> string
val string_of_program: 'a program_t -> string


(* AST accessors *)

(* Returns all variables in an expression *)
val vars_of_expr      : 'a expr_t -> 'a expr_t list

(* Returns the free variables in an expression *)
val free_vars_of_expr : 'a expr_t -> 'a expr_t list

(* Returns whether e2 is directly contained in e1 *)
val contains_expr : 'a expr_t -> 'a expr_t -> bool

(* Returns all subexpressions matching a given predicate *)
val filter_expr :
  ('a expr_t -> bool) -> 'a expr_t -> 'a expr_t list

(* Substitutes any occurrences of the given bindings in an expression,
 * in a bottom-up, capture-avoiding fashion.
 * Assumes substitution function domain and range are (subtree) disjoint.
 *)
val substitute_expr :
  ('a expr_t * 'a expr_t) list -> 'a expr_t
  -> 'a expr_t * (int * int) list
  
(* Linearizes (i.e. flattens) an expression tree to its constituent
 * subexpressions, in an order given by its first argument.
 * The first argument linearizes a single node and is of the form:
 *   child linearizations -> node -> linearization  *)
val linearize_expr :
  ('a expr_t list list -> 'a expr_t -> 'a expr_t list) -> 'a expr_t
  -> 'a expr_t list

val pre_order_linearization : 'a expr_t list list -> 'a expr_t -> 'a expr_t list
val post_order_linearization : 'a expr_t list list -> 'a expr_t -> 'a expr_t list
