open K3.AST
open K3.Annotation

(* AST *)

val id_of_expr : expr_t -> int 
val tag_of_expr : expr_t -> expr_tag_t
val meta_of_expr : expr_t -> annotation_t

(* Variable id extraction *)
val vars_of_arg : arg_t -> id_t list
val typed_vars_of_arg : arg_t -> (id_t * value_type_t) list
val id_of_var : expr_t -> id_t

val tuple_type_of_args : value_type_t list -> type_t
val tuple_type_of_arg : arg_t -> type_t

(* Predicates *)
val is_const : expr_t -> bool
val is_var : expr_t -> bool
val is_var_match : id_t -> expr_t -> bool

(* Bindings *)
val arg_of_lambda : expr_t -> arg_t option
val vars_of_lambda : expr_t -> id_t list
val typed_vars_of_lambda : expr_t -> (id_t * value_type_t) list

(* Type tags *)
val signature_of_type : type_t -> string
val type_of_signature : string -> type_t

(* AST testers *)
val is_peek : expr_t -> bool

(* AST constructors / destructors *)
val decompose_aggregate : expr_t -> expr_t * expr_t * expr_t
val decompose_apply : expr_t -> expr_t * expr_t
val decompose_block : expr_t -> expr_t list
val decompose_filter_map : expr_t -> expr_t * expr_t * expr_t
val decompose_gbagg : expr_t -> expr_t * expr_t * expr_t * expr_t
val decompose_ifthenelse : expr_t -> expr_t * expr_t * expr_t
val decompose_insert : expr_t -> expr_t * expr_t
val decompose_iterate : expr_t -> expr_t * expr_t
val decompose_lambda : expr_t -> expr_t
val decompose_map : expr_t -> expr_t * expr_t
val decompose_peek : expr_t -> expr_t
val decompose_send : expr_t -> expr_t * expr_t * (expr_t list)
val decompose_tuple : expr_t -> expr_t list

(* Declaration accessors *)
val globals_of_program  : program_t -> program_t
val flows_of_program    : program_t -> program_t

val global_of_program   : id_t -> program_t -> declaration_t * annotation_t

(* Flow program accessors *)
val sources_of_flow    : flow_program_t -> flow_endpoint_t list
val sinks_of_flow      : flow_program_t -> flow_endpoint_t list
val generators_of_flow : flow_program_t -> flow_endpoint_t list
val triggers_of_flow   : flow_program_t -> flow_endpoint_t list

val sources_of_program    : program_t -> flow_endpoint_t list
val sinks_of_program      : program_t -> flow_endpoint_t list
val generators_of_program : program_t -> flow_endpoint_t list
val triggers_of_program   : program_t -> flow_endpoint_t list

val trigger_of_program  : id_t -> program_t -> flow_endpoint_t


(* Returns all variables in an expression *)
val vars_of_expr      : expr_t -> expr_t list

(* Returns the free variables in an expression *)
val free_vars_of_expr : expr_t -> expr_t list

(* Returns whether e2 is directly contained in e1 *)
val contains_expr : expr_t -> expr_t -> bool

(* Returns all subexpressions matching a given predicate *)
val filter_expr :
  (expr_t -> bool) -> expr_t -> expr_t list

(* Substitutes any occurrences of the given bindings in an expression,
 * in a bottom-up, capture-avoiding fashion.
 * Assumes substitution function domain and range are (subtree) disjoint.
 *)
val substitute_expr :
  (expr_t * expr_t) list -> expr_t
  -> expr_t * (int * int) list
  
(* Linearizes (i.e. flattens) an expression tree to its constituent
 * subexpressions, in an order given by its first argument.
 * The first argument linearizes a single node and is of the form:
 *   child linearizations -> node -> linearization  *)
val linearize_expr :
  (expr_t list list -> expr_t -> expr_t list) -> expr_t
  -> expr_t list

val pre_order_linearization : expr_t list list -> expr_t -> expr_t list
val post_order_linearization : expr_t list list -> expr_t -> expr_t list

val renumber_ast_ids : expr_t -> int ref -> expr_t
val renumber_program_ids : program_t -> program_t
