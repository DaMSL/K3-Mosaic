(* low-precedence function application. Saves ( ) *)
val (@:) : ('a -> 'b) -> 'a -> 'b

val compose :('b -> 'c) -> ('a -> 'b) -> ('a -> 'c)
val id_fn : 'a -> 'a
val (|-) : ('b -> 'c) -> ('a -> 'b) -> ('a -> 'c)

(* take the first x values of a list *)
val list_take : int -> 'a list -> 'a list

(* take the last x values of a list *)
val list_take_end : int -> 'a list -> 'a list

(* drop the first x values of a list *)
val list_drop : int -> 'a list -> 'a list

(* drop the last x values of a list *)
val list_drop_end : int -> 'a list -> 'a list

(* combine two lists into one list with tuples *)
val list_zip : 'a list -> 'b list -> ('a * 'b) list

(* take the head of a list *)
val list_head : 'a list -> 'a

(* take the last member of a list *)
val list_last : 'a list -> 'a

val compose_fn : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b

(* fold left until the predicate (2nd arg) is true *)
val foldl_until : ('a -> 'b -> 'a) -> ('a -> 'b -> bool) -> 'a -> 'b list -> 'a

val read_file : string -> string

(* create a numerical range from a to b *)
val create_range : int -> int -> int list

(* create a numerical range corresponding to the length of a list *)
val create_corr_range : int -> 'a list -> int list

(* insert an index for each member of a list *)
val insert_index_fst : int -> 'a list -> (int * 'a) list

(* insert an index for each member of a list, in snd place *)
val insert_index_snd : int -> 'a list -> ('a * int) list

(* unique members of a list *)
val nub : 'a list -> 'a list
