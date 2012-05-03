val mk_const : 'a -> K3.constant_t -> ('a, K3.expr_tag_t) Tree.tree_t
val mk_var : 'a -> K3.id_t -> ('a, K3.expr_tag_t) Tree.tree_t
val mk_tuple :
  'a ->
  ('a, K3.expr_tag_t) Tree.tree_t list -> ('a, K3.expr_tag_t) Tree.tree_t
val mk_just :
  'a -> ('a, K3.expr_tag_t) Tree.tree_t -> ('a, K3.expr_tag_t) Tree.tree_t
val mk_empty : 'a -> K3.value_type_t -> ('a, K3.expr_tag_t) Tree.tree_t
val mk_singleton :
  'a ->
  K3.value_type_t ->
  ('a, K3.expr_tag_t) Tree.tree_t -> ('a, K3.expr_tag_t) Tree.tree_t
val mk_combine :
  'a ->
  ('a, K3.expr_tag_t) Tree.tree_t ->
  ('a, K3.expr_tag_t) Tree.tree_t -> ('a, K3.expr_tag_t) Tree.tree_t
val mk_range :
  'a ->
  K3.collection_type_t ->
  ('a, K3.expr_tag_t) Tree.tree_t ->
  ('a, K3.expr_tag_t) Tree.tree_t ->
  ('a, K3.expr_tag_t) Tree.tree_t -> ('a, K3.expr_tag_t) Tree.tree_t
val mk_add :
  'a ->
  ('a, K3.expr_tag_t) Tree.tree_t ->
  ('a, K3.expr_tag_t) Tree.tree_t -> ('a, K3.expr_tag_t) Tree.tree_t
val mk_mult :
  'a ->
  ('a, K3.expr_tag_t) Tree.tree_t ->
  ('a, K3.expr_tag_t) Tree.tree_t -> ('a, K3.expr_tag_t) Tree.tree_t
val mk_neg :
  'a -> ('a, K3.expr_tag_t) Tree.tree_t -> ('a, K3.expr_tag_t) Tree.tree_t
val mk_eq :
  'a ->
  ('a, K3.expr_tag_t) Tree.tree_t ->
  ('a, K3.expr_tag_t) Tree.tree_t -> ('a, K3.expr_tag_t) Tree.tree_t
val mk_lt :
  'a ->
  ('a, K3.expr_tag_t) Tree.tree_t ->
  ('a, K3.expr_tag_t) Tree.tree_t -> ('a, K3.expr_tag_t) Tree.tree_t
val mk_neq :
  'a ->
  ('a, K3.expr_tag_t) Tree.tree_t ->
  ('a, K3.expr_tag_t) Tree.tree_t -> ('a, K3.expr_tag_t) Tree.tree_t
val mk_leq :
  'a ->
  ('a, K3.expr_tag_t) Tree.tree_t ->
  ('a, K3.expr_tag_t) Tree.tree_t -> ('a, K3.expr_tag_t) Tree.tree_t
val mk_lambda :
  'a ->
  K3.arg_t ->
  ('a, K3.expr_tag_t) Tree.tree_t -> ('a, K3.expr_tag_t) Tree.tree_t
val mk_apply :
  'a ->
  ('a, K3.expr_tag_t) Tree.tree_t ->
  ('a, K3.expr_tag_t) Tree.tree_t -> ('a, K3.expr_tag_t) Tree.tree_t
val mk_block :
  'a ->
  ('a, K3.expr_tag_t) Tree.tree_t list -> ('a, K3.expr_tag_t) Tree.tree_t
val mk_iterate :
  'a ->
  ('a, K3.expr_tag_t) Tree.tree_t ->
  ('a, K3.expr_tag_t) Tree.tree_t -> ('a, K3.expr_tag_t) Tree.tree_t
val mk_ifthenelse :
  'a ->
  ('a, K3.expr_tag_t) Tree.tree_t ->
  ('a, K3.expr_tag_t) Tree.tree_t ->
  ('a, K3.expr_tag_t) Tree.tree_t -> ('a, K3.expr_tag_t) Tree.tree_t
val mk_map :
  'a ->
  ('a, K3.expr_tag_t) Tree.tree_t ->
  ('a, K3.expr_tag_t) Tree.tree_t -> ('a, K3.expr_tag_t) Tree.tree_t
val mk_filtermap :
  'a ->
  ('a, K3.expr_tag_t) Tree.tree_t ->
  ('a, K3.expr_tag_t) Tree.tree_t ->
  ('a, K3.expr_tag_t) Tree.tree_t -> ('a, K3.expr_tag_t) Tree.tree_t
val mk_flatten :
  'a -> ('a, K3.expr_tag_t) Tree.tree_t -> ('a, K3.expr_tag_t) Tree.tree_t
val mk_aggregate :
  'a ->
  ('a, K3.expr_tag_t) Tree.tree_t ->
  ('a, K3.expr_tag_t) Tree.tree_t ->
  ('a, K3.expr_tag_t) Tree.tree_t -> ('a, K3.expr_tag_t) Tree.tree_t
val mk_groupbyaggregate :
  'a ->
  ('a, K3.expr_tag_t) Tree.tree_t ->
  ('a, K3.expr_tag_t) Tree.tree_t ->
  ('a, K3.expr_tag_t) Tree.tree_t ->
  ('a, K3.expr_tag_t) Tree.tree_t -> ('a, K3.expr_tag_t) Tree.tree_t
val mk_sort :
  'a ->
  ('a, K3.expr_tag_t) Tree.tree_t ->
  ('a, K3.expr_tag_t) Tree.tree_t -> ('a, K3.expr_tag_t) Tree.tree_t
val mk_slice :
  'a ->
  ('a, K3.expr_tag_t) Tree.tree_t ->
  ('a, K3.expr_tag_t) Tree.tree_t -> ('a, K3.expr_tag_t) Tree.tree_t
val mk_insert :
  'a ->
  ('a, K3.expr_tag_t) Tree.tree_t ->
  ('a, K3.expr_tag_t) Tree.tree_t -> ('a, K3.expr_tag_t) Tree.tree_t
val mk_delete :
  'a ->
  ('a, K3.expr_tag_t) Tree.tree_t ->
  ('a, K3.expr_tag_t) Tree.tree_t -> ('a, K3.expr_tag_t) Tree.tree_t
val mk_update :
  'a ->
  ('a, K3.expr_tag_t) Tree.tree_t ->
  ('a, K3.expr_tag_t) Tree.tree_t ->
  ('a, K3.expr_tag_t) Tree.tree_t -> ('a, K3.expr_tag_t) Tree.tree_t
val mk_peek :
  'a -> ('a, K3.expr_tag_t) Tree.tree_t -> ('a, K3.expr_tag_t) Tree.tree_t
val mk_assigntoref :
  'a ->
  ('a, K3.expr_tag_t) Tree.tree_t ->
  ('a, K3.expr_tag_t) Tree.tree_t -> ('a, K3.expr_tag_t) Tree.tree_t
val mk_send :
  'a ->
  ('a, K3.expr_tag_t) Tree.tree_t ->
  ('a, K3.expr_tag_t) Tree.tree_t -> ('a, K3.expr_tag_t) Tree.tree_t
