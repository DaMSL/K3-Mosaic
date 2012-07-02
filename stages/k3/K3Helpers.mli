(* Utility functions to enable easy manipulation of K3 AST trees *)

(* easy access to the int type *)
val t_int : K3.value_type_t
val t_int_mut : K3.value_type_t

(* wrap in a list *)
val wrap_tlist : K3.value_type_t -> K3.value_type_t
val wrap_tlist_mut : K3.value_type_t -> K3.value_type_t
val wrap_ttuple : K3.value_type_t list -> K3.value_type_t
val wrap_ttuple_mut : K3.value_type_t list -> K3.value_type_t

(* simple functions that enable easy construction of AST trees *)
val mk_const : K3.constant_t -> ((int * K3.expr_tag_t) * int) Tree.tree_t
val mk_var : K3.id_t -> ((int * K3.expr_tag_t) * int) Tree.tree_t
val mk_tuple :
  ((int * K3.expr_tag_t) * int) Tree.tree_t list ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t
val mk_just :
  ((int * K3.expr_tag_t) * int) Tree.tree_t ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t
val mk_empty : K3.value_type_t -> ((int * K3.expr_tag_t) * int) Tree.tree_t
val mk_singleton :
  K3.value_type_t ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t
val mk_combine :
  ((int * K3.expr_tag_t) * int) Tree.tree_t ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t
val mk_range :
  K3.container_type_t ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t
val mk_add :
  ((int * K3.expr_tag_t) * int) Tree.tree_t ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t
val mk_sub :
  ((int * K3.expr_tag_t) * int) Tree.tree_t ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t
val mk_or :
  ((int * K3.expr_tag_t) * int) Tree.tree_t ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t
val mk_mult :
  ((int * K3.expr_tag_t) * int) Tree.tree_t ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t
val mk_and :
  ((int * K3.expr_tag_t) * int) Tree.tree_t ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t
val mk_neg :
  ((int * K3.expr_tag_t) * int) Tree.tree_t ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t
val mk_not :
  ((int * K3.expr_tag_t) * int) Tree.tree_t ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t
val mk_eq :
  ((int * K3.expr_tag_t) * int) Tree.tree_t ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t
val mk_lt :
  ((int * K3.expr_tag_t) * int) Tree.tree_t ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t
val mk_neq :
  ((int * K3.expr_tag_t) * int) Tree.tree_t ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t
val mk_leq :
  ((int * K3.expr_tag_t) * int) Tree.tree_t ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t
val mk_geq :
  ((int * K3.expr_tag_t) * int) Tree.tree_t ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t
val mk_gt :
  ((int * K3.expr_tag_t) * int) Tree.tree_t ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t
val mk_lambda :
  K3.arg_t ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t
val mk_apply :
  ((int * K3.expr_tag_t) * int) Tree.tree_t ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t
val mk_block :
  ((int * K3.expr_tag_t) * int) Tree.tree_t list ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t
val mk_iter :
  ((int * K3.expr_tag_t) * int) Tree.tree_t ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t
val mk_if :
  ((int * K3.expr_tag_t) * int) Tree.tree_t ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t
val mk_map :
  ((int * K3.expr_tag_t) * int) Tree.tree_t ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t
val mk_filtermap :
  ((int * K3.expr_tag_t) * int) Tree.tree_t ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t
val mk_flatten :
  ((int * K3.expr_tag_t) * int) Tree.tree_t ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t
val mk_agg :
  ((int * K3.expr_tag_t) * int) Tree.tree_t ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t
val mk_gbagg :
  ((int * K3.expr_tag_t) * int) Tree.tree_t ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t
val mk_sort :
  ((int * K3.expr_tag_t) * int) Tree.tree_t ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t
val mk_slice :
  ((int * K3.expr_tag_t) * int) Tree.tree_t ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t
val mk_insert :
  ((int * K3.expr_tag_t) * int) Tree.tree_t ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t
val mk_delete :
  ((int * K3.expr_tag_t) * int) Tree.tree_t ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t
val mk_update :
  ((int * K3.expr_tag_t) * int) Tree.tree_t ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t
val mk_peek :
  ((int * K3.expr_tag_t) * int) Tree.tree_t ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t
val mk_assign :
  ((int * K3.expr_tag_t) * int) Tree.tree_t ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t
val mk_send :
  ((int * K3.expr_tag_t) * int) Tree.tree_t ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t

(* extract only the types from a list of (id, type) *)
val extract_arg_types : ('a * 'b) list -> 'b list

(* extract only the names from a list of (id, type) *)
val extract_arg_names : ('a * 'b) list -> 'a list

(* take a list of ids and convert it to a list of vars *)
val convert_names_to_vars :
  K3.id_t list -> ((int * K3.expr_tag_t) * int) Tree.tree_t list
val strip_args : K3.arg_t -> (K3.id_t * K3.value_type_t) list

(* macro to check if a collection has a specific member *)
val mk_has_member :
  ((int * K3.expr_tag_t) * int) Tree.tree_t ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t ->
  K3.value_type_t -> ((int * K3.expr_tag_t) * int) Tree.tree_t

(* macro to create a global function *)
val mk_global_fn : 
  K3.id_t ->
  (K3.id_t * K3.value_type_t) list ->
  K3.value_type_t list ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t -> int K3.declaration_t

(* macro to create an associative lambda ie a lambda with 2 args *)
val mk_assoc_lambda :
  K3.arg_t ->
  K3.arg_t ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t

(* macro to create a regular functional let structure *)
val mk_let :
  K3.id_t ->
  K3.value_type_t ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t

(* macro to make a 'let many', where many values are assigned simultaneously *)
val mk_let_many :
  (K3.id_t * K3.value_type_t) list ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t

(* macro to destruct a tuple, take only the first members, and add to other ids
 * and types *)
val mk_reduced_tuple :
  K3.id_t ->
  K3.value_type_t list ->
  int ->
  (K3.id_t * K3.value_type_t) list ->
  ((int * K3.expr_tag_t) * int) Tree.tree_t

