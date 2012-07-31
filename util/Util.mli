(* low-precedence function application. Saves ( ) *)
val (@:) : ('a -> 'b) -> 'a -> 'b

(* create a numerical range from a to b *)
val create_range : int -> int -> int list

(* take the first x values of a list *)
val list_take : int -> 'a list -> 'a list

(* drop the first x values of a list *)
val list_drop : int -> 'a list -> 'a list

(* combine two lists into one list with tuples *)
val list_zip : 'a list -> 'b list -> ('a * 'b) list

val compose_fn : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
val read_file : string -> string
