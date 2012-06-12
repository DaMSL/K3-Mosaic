type 'a tree_t
    = Leaf of 'a
    | Node of 'a * 'a tree_t list

val decompose_tree : 'a tree_t -> 'a * 'a tree_t list
val recompose_tree : 'a * 'a tree_t list -> 'a tree_t

val data_of_tree : 'a tree_t -> 'a
val sub_of_tree : 'a tree_t -> 'a tree_t list

val sub_tree     : 'a tree_t -> 'a tree_t list
val rebuild_tree : 'a tree_t list -> 'a tree_t -> 'a tree_t
val fold_tree    : 
  ('td -> 'a tree_t -> 'td) ->
  ('td -> 'bu list -> 'a tree_t -> 'bu) ->
  'td -> 'bu -> 'a tree_t -> 'bu

val string_of_tree : ('a -> string list -> string) -> 'a tree_t -> string
