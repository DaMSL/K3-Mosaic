type 'a tree_t
    = Leaf of 'a
    | Node of 'a * 'a tree_t list

val string_of_tree : ('a -> string list -> string) -> 'a tree_t -> string

(* Tree constructors, destructors *)

val mk_tree : 'a * 'a tree_t list -> 'a tree_t
val decompose_tree : 'a tree_t -> 'a * 'a tree_t list
val recompose_tree : 'a tree_t -> 'a tree_t list -> 'a tree_t

(* Tree accessors *)

val node_data : 'a tree_t -> 'a
val tree_data : 'a tree_t -> 'a list
val sub_tree  : 'a tree_t -> 'a tree_t list

(* Tree traversals *)

val fold_tree : 
  ('td -> 'a tree_t -> 'td) ->
  ('td -> 'bu list -> 'a tree_t -> 'bu) ->
  'td -> 'bu -> 'a tree_t -> 'bu

val fold_tree_thread : 
  ('td -> 'a tree_t -> 'td) ->
  ('td * 'bu list -> 'a tree_t -> 'td * 'bu) ->
  'td -> 'bu -> 'a tree_t -> 'td * 'bu

(* Trees with tuple metadata *)
val prepend_tree : ('a -> 'b) -> 'a tree_t -> ('b * 'a) tree_t
val append_tree  : ('a -> 'b) -> 'a tree_t -> ('a * 'b) tree_t
val project_tree : ('a * 'b) tree_t -> 'b tree_t

val fst_data : ('a * 'b) tree_t -> 'a
val snd_data : ('a * 'b) tree_t -> 'b

(* Generic tree labelling *)

val label_tree : 'a tree_t -> (int * 'a) tree_t
val unlabel_tree : (int * 'a) tree_t -> 'a tree_t
val label_of_node : (int * 'a) tree_t -> int
val label_of_tree : (int * 'a) tree_t -> int list
