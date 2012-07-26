val (@:) : ('a -> 'b) -> 'a -> 'b
val create_range : int -> int -> int list
val list_take : 'a list -> int -> 'a list
val list_zip : 'a list -> 'b list -> ('a * 'b) list
val compose_fn : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
val read_file : string -> string