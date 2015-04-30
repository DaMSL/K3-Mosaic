open Util
open K3.AST
open K3.Annotation

(* AST *)

val id_of_expr : expr_t -> int
val tag_of_expr : expr_t -> expr_tag_t
val meta_of_expr : expr_t -> annotation_t

val details_of_expr : expr_t -> int * expr_tag_t * annotation_t * expr_t list
val expr_of_details : int -> expr_tag_t -> annotation_t -> expr_t list -> expr_t

(* Variable id extraction *)
val vars_of_arg : arg_t -> id_t list
val typed_vars_of_arg : arg_t -> (id_t * type_t) list
val id_of_var : expr_t -> id_t

(* Predicates *)
val is_const : expr_t -> bool
val is_var : expr_t -> bool
val is_var_match : id_t -> expr_t -> bool

(* Bindings *)
val arg_of_lambda : expr_t -> arg_t option
val vars_of_lambda : expr_t -> id_t list
val typed_vars_of_lambda : expr_t -> (id_t * type_t) list

(* AST testers *)
val is_peek : expr_t -> bool

(* AST constructors / destructors *)
val decompose_add : expr_t -> expr_t * expr_t
val decompose_aggregate : expr_t -> expr_t * expr_t * expr_t
val decompose_apply : expr_t -> expr_t * expr_t
val decompose_assign : expr_t -> id_t * expr_t
val decompose_block : expr_t -> expr_t list
val decompose_combine : expr_t -> expr_t * expr_t
val decompose_const : expr_t -> constant_t
val decompose_caseof : expr_t -> expr_t * expr_t * expr_t
val decompose_bind : expr_t -> expr_t * id_t * expr_t
val decompose_delete : expr_t -> id_t * expr_t
val decompose_eq : expr_t -> expr_t * expr_t
val decompose_filter : expr_t -> expr_t * expr_t
val decompose_flatten : expr_t -> expr_t
val decompose_gbagg : expr_t -> expr_t * expr_t * expr_t * expr_t
val decompose_ifthenelse : expr_t -> expr_t * expr_t * expr_t
val decompose_insert : expr_t -> id_t * expr_t
val decompose_iterate : expr_t -> expr_t * expr_t
val decompose_just : expr_t -> expr_t
val decompose_lambda : expr_t -> arg_t * expr_t
val decompose_leq : expr_t -> expr_t * expr_t
val decompose_let : expr_t -> id_t list * expr_t * expr_t
val decompose_lt : expr_t -> expr_t * expr_t
val decompose_map : expr_t -> expr_t * expr_t
val decompose_mult : expr_t -> expr_t * expr_t
val decompose_neg : expr_t -> expr_t
val decompose_neq : expr_t -> expr_t * expr_t
val decompose_peek : expr_t -> expr_t
val decompose_range : expr_t -> expr_t * expr_t * expr_t
val decompose_send : expr_t -> expr_t * expr_t * (expr_t list)
val decompose_singleton : expr_t -> expr_t
val decompose_slice : expr_t -> expr_t * expr_t
val decompose_sliceidx : expr_t -> expr_t * expr_t
val decompose_sort : expr_t -> expr_t * expr_t
val decompose_size : expr_t -> expr_t
val decompose_subscript : expr_t -> int * expr_t
val decompose_tuple : expr_t -> expr_t list
val decompose_update : expr_t -> id_t * expr_t * expr_t
val decompose_var : expr_t -> id_t
val decompose_indirect : expr_t -> expr_t

val decompose_role : declaration_t * 'a -> id_t * flow_program_t
val decompose_trig : flow_statement_t * 'a -> id_t * arg_t * expr_t
val decompose_global_fn : declaration_t * 'a -> id_t * type_t * expr_t


(* decompose if we have a tuple, otherwise return e *)
val extract_if_tuple : expr_t -> expr_t list

(* Declaration accessors *)
val is_global : declaration_t * 'a -> bool
val is_foreign : declaration_t * 'a -> bool
val is_flow : declaration_t * 'a -> bool
val is_role : declaration_t * 'a -> bool
val is_def_role : declaration_t * 'a -> bool

(* Declaration accessors *)
val globals_of_program  : program_t -> program_t
val global_values_of_program  : program_t -> program_t
val global_functions_of_program  : program_t -> program_t
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

val id_of_code : flow_endpoint_t -> id_t
val expr_of_code : flow_endpoint_t -> expr_t
val args_of_code : flow_endpoint_t -> arg_t

val id_of_role : declaration_t * 'a -> id_t

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

(* expression -> starting number -> expression *)
val renumber_expr_ids : start:int -> expr_t -> int * expr_t
val renumber_program_ids : ?start:int -> program_t -> int * program_t
val renumber_test_program_ids :
  ?start:int -> program_test_t -> int * program_test_t

(* Attach a type annotation to an expr *)
val attach_type : type_t -> expr_t -> expr_t

(* Get the expression list inside a tuple if any *)
val unwrap_tuple : expr_t -> expr_t list

(* Fold over all expression trees in a program (triggers, globals) *)
val fold_over_exprs : ('a -> expr_t -> 'a) -> 'a -> program_t -> 'a

val filter_by_index_t : ?anti_set:IntSet.t * 'a -> index_t -> 'a list -> 'a list

