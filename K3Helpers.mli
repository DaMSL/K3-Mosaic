val meta : int
val mk_const : K3.constant_t -> (int, K3.expr_tag_t) Tree.tree_t
val mk_var : K3.id_t -> (int, K3.expr_tag_t) Tree.tree_t
val mk_tuple :
  (int, K3.expr_tag_t) Tree.tree_t list -> (int, K3.expr_tag_t) Tree.tree_t
val mk_just :
  (int, K3.expr_tag_t) Tree.tree_t -> (int, K3.expr_tag_t) Tree.tree_t
val mk_empty : K3.value_type_t -> (int, K3.expr_tag_t) Tree.tree_t
val mk_singleton :
  K3.value_type_t ->
  (int, K3.expr_tag_t) Tree.tree_t -> (int, K3.expr_tag_t) Tree.tree_t
val mk_combine :
  (int, K3.expr_tag_t) Tree.tree_t ->
  (int, K3.expr_tag_t) Tree.tree_t -> (int, K3.expr_tag_t) Tree.tree_t
val mk_range :
  K3.collection_type_t ->
  (int, K3.expr_tag_t) Tree.tree_t ->
  (int, K3.expr_tag_t) Tree.tree_t ->
  (int, K3.expr_tag_t) Tree.tree_t -> (int, K3.expr_tag_t) Tree.tree_t
val mk_add :
  (int, K3.expr_tag_t) Tree.tree_t ->
  (int, K3.expr_tag_t) Tree.tree_t -> (int, K3.expr_tag_t) Tree.tree_t
val mk_or :
  (int, K3.expr_tag_t) Tree.tree_t ->
  (int, K3.expr_tag_t) Tree.tree_t -> (int, K3.expr_tag_t) Tree.tree_t
val mk_mult :
  (int, K3.expr_tag_t) Tree.tree_t ->
  (int, K3.expr_tag_t) Tree.tree_t -> (int, K3.expr_tag_t) Tree.tree_t
val mk_and :
  (int, K3.expr_tag_t) Tree.tree_t ->
  (int, K3.expr_tag_t) Tree.tree_t -> (int, K3.expr_tag_t) Tree.tree_t
val mk_neg :
  (int, K3.expr_tag_t) Tree.tree_t -> (int, K3.expr_tag_t) Tree.tree_t
val mk_not :
  (int, K3.expr_tag_t) Tree.tree_t -> (int, K3.expr_tag_t) Tree.tree_t
val mk_eq :
  (int, K3.expr_tag_t) Tree.tree_t ->
  (int, K3.expr_tag_t) Tree.tree_t -> (int, K3.expr_tag_t) Tree.tree_t
val mk_lt :
  (int, K3.expr_tag_t) Tree.tree_t ->
  (int, K3.expr_tag_t) Tree.tree_t -> (int, K3.expr_tag_t) Tree.tree_t
val mk_neq :
  (int, K3.expr_tag_t) Tree.tree_t ->
  (int, K3.expr_tag_t) Tree.tree_t -> (int, K3.expr_tag_t) Tree.tree_t
val mk_leq :
  (int, K3.expr_tag_t) Tree.tree_t ->
  (int, K3.expr_tag_t) Tree.tree_t -> (int, K3.expr_tag_t) Tree.tree_t
val mk_geq :
  (int, K3.expr_tag_t) Tree.tree_t ->
  (int, K3.expr_tag_t) Tree.tree_t -> (int, K3.expr_tag_t) Tree.tree_t
val mk_gt :
  (int, K3.expr_tag_t) Tree.tree_t ->
  (int, K3.expr_tag_t) Tree.tree_t -> (int, K3.expr_tag_t) Tree.tree_t
val mk_lambda :
  K3.arg_t ->
  (int, K3.expr_tag_t) Tree.tree_t -> (int, K3.expr_tag_t) Tree.tree_t
val mk_apply :
  (int, K3.expr_tag_t) Tree.tree_t ->
  (int, K3.expr_tag_t) Tree.tree_t -> (int, K3.expr_tag_t) Tree.tree_t
val mk_block :
  (int, K3.expr_tag_t) Tree.tree_t list -> (int, K3.expr_tag_t) Tree.tree_t
val mk_iter :
  (int, K3.expr_tag_t) Tree.tree_t ->
  (int, K3.expr_tag_t) Tree.tree_t -> (int, K3.expr_tag_t) Tree.tree_t
val mk_ifthenelse :
  (int, K3.expr_tag_t) Tree.tree_t ->
  (int, K3.expr_tag_t) Tree.tree_t ->
  (int, K3.expr_tag_t) Tree.tree_t -> (int, K3.expr_tag_t) Tree.tree_t
val mk_map :
  (int, K3.expr_tag_t) Tree.tree_t ->
  (int, K3.expr_tag_t) Tree.tree_t -> (int, K3.expr_tag_t) Tree.tree_t
val mk_filtermap :
  (int, K3.expr_tag_t) Tree.tree_t ->
  (int, K3.expr_tag_t) Tree.tree_t ->
  (int, K3.expr_tag_t) Tree.tree_t -> (int, K3.expr_tag_t) Tree.tree_t
val mk_flatten :
  (int, K3.expr_tag_t) Tree.tree_t -> (int, K3.expr_tag_t) Tree.tree_t
val mk_aggregate :
  (int, K3.expr_tag_t) Tree.tree_t ->
  (int, K3.expr_tag_t) Tree.tree_t ->
  (int, K3.expr_tag_t) Tree.tree_t -> (int, K3.expr_tag_t) Tree.tree_t
val mk_groupbyaggregate :
  (int, K3.expr_tag_t) Tree.tree_t ->
  (int, K3.expr_tag_t) Tree.tree_t ->
  (int, K3.expr_tag_t) Tree.tree_t ->
  (int, K3.expr_tag_t) Tree.tree_t -> (int, K3.expr_tag_t) Tree.tree_t
val mk_sort :
  (int, K3.expr_tag_t) Tree.tree_t ->
  (int, K3.expr_tag_t) Tree.tree_t -> (int, K3.expr_tag_t) Tree.tree_t
val mk_slice :
  (int, K3.expr_tag_t) Tree.tree_t ->
  (int, K3.expr_tag_t) Tree.tree_t -> (int, K3.expr_tag_t) Tree.tree_t
val mk_insert :
  (int, K3.expr_tag_t) Tree.tree_t ->
  (int, K3.expr_tag_t) Tree.tree_t -> (int, K3.expr_tag_t) Tree.tree_t
val mk_delete :
  (int, K3.expr_tag_t) Tree.tree_t ->
  (int, K3.expr_tag_t) Tree.tree_t -> (int, K3.expr_tag_t) Tree.tree_t
val mk_update :
  (int, K3.expr_tag_t) Tree.tree_t ->
  (int, K3.expr_tag_t) Tree.tree_t ->
  (int, K3.expr_tag_t) Tree.tree_t -> (int, K3.expr_tag_t) Tree.tree_t
val mk_peek :
  (int, K3.expr_tag_t) Tree.tree_t -> (int, K3.expr_tag_t) Tree.tree_t
val mk_assigntoref :
  (int, K3.expr_tag_t) Tree.tree_t ->
  (int, K3.expr_tag_t) Tree.tree_t -> (int, K3.expr_tag_t) Tree.tree_t
val mk_send :
  (int, K3.expr_tag_t) Tree.tree_t ->
  (int, K3.expr_tag_t) Tree.tree_t -> (int, K3.expr_tag_t) Tree.tree_t
val extract_arg_types : ('a * 'b) list -> 'b list
val extract_arg_names : ('a * 'b) list -> 'a list
val convert_names_to_vars :
  K3.id_t list -> (int, K3.expr_tag_t) Tree.tree_t list
val strip_args : K3.arg_t -> (K3.id_t * K3.value_type_t) list
val mk_has_member :
  (int, K3.expr_tag_t) Tree.tree_t ->
  (int, K3.expr_tag_t) Tree.tree_t ->
  K3.value_type_t -> (int, K3.expr_tag_t) Tree.tree_t
val mk_assoc_lambda :
  K3.arg_t ->
  K3.arg_t ->
  (int, K3.expr_tag_t) Tree.tree_t -> (int, K3.expr_tag_t) Tree.tree_t
val mk_let :
  K3.id_t ->
  K3.value_type_t ->
  (int, K3.expr_tag_t) Tree.tree_t ->
  (int, K3.expr_tag_t) Tree.tree_t -> (int, K3.expr_tag_t) Tree.tree_t
val mk_let_many :
  (K3.id_t * K3.value_type_t) list ->
  (int, K3.expr_tag_t) Tree.tree_t ->
  (int, K3.expr_tag_t) Tree.tree_t -> (int, K3.expr_tag_t) Tree.tree_t
val wrap_tlist : K3.value_type_t -> K3.value_type_t
val wrap_ttuple : K3.value_type_t list -> K3.value_type_t
