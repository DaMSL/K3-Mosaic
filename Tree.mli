(* A generic tree *)
type ('a, 'tag) tree_t
    = Leaf of 'a * 'tag
    | Node of 'a * 'tag * ('a, 'tag) tree_t list

(* Tree decomposition *)
val decompose_tree : ('a, 'tag) tree_t -> ('a * 'tag) * (('a, 'tag) tree_t list)
val recompose_tree : ('a * 'tag) * (('a, 'tag) tree_t list) -> ('a, 'tag) tree_t

val data_of_tree : ('a, 'tag) tree_t -> ('a * 'tag)
val sub_of_tree : ('a, 'tag) tree_t -> ('a, 'tag) tree_t list

val sub_tree     : ('a, 'tag) tree_t -> ('a, 'tag) tree_t list
val rebuild_tree : ('a, 'tag) tree_t list -> ('a, 'tag) tree_t -> ('a, 'tag) tree_t 
val fold_tree    : 
  ('td -> ('a, 'tag) tree_t -> 'td) -> 
  ('td -> 'bu list -> ('a, 'tag) tree_t -> 'bu) ->
  'td -> 'bu -> ('a, 'tag) tree_t -> 'bu

val string_of_tree :
  ('a -> string list -> string)
  -> ('tag -> string list -> string)
  -> ('a, 'tag) tree_t -> string
