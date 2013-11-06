(* low-precedence function application. Saves ( ) *)
val foi : int -> float
val iof : float -> int
val soi : int -> string
val sof : float -> string
val ios : string -> int
val fos : string -> float
val bos : string -> bool
val sob : bool -> string

type ('a, 'b) either_t = Left of 'a | Right of 'b

val (@:) : ('a -> 'b) -> 'a -> 'b

val compose :('b -> 'c) -> ('a -> 'b) -> ('a -> 'c)
val id_fn : 'a -> 'a
val (|-) : ('b -> 'c) -> ('a -> 'b) -> ('a -> 'c)

(* whether a list is empty *)
val null : 'a list -> bool

(* like List.nth *)
val at : 'a list -> int -> 'a

(* take the first elements in a tuple for a whole list *)
val fst_many : ('a * 'b) list -> 'a list

(* take the second elements in a tuple for a whole list *)
val snd_many : ('a * 'b) list -> 'b list

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
val hd : 'a list -> 'a

(* take the tail of a list *)
val tl : 'a list -> 'a list

(* take the last member of a list *)
val list_last : 'a list -> 'a

(* will only remove one instance of x in xs (as opposed to filter) *)
val list_remove : 'a -> 'a list -> 'a list

val compose_fn : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b

(* fold left until the predicate (2nd arg) is true *)
val foldl_until : ('a -> 'b -> ('a, 'a) either_t ) -> 'a -> 'b list -> 'a

val read_file_lines : string -> string list

val read_file : string -> string

val write_file : string -> string -> unit

(* create a numerical range from a to b *)
val create_range : ?step:int -> int -> int -> int list

(* create a numerical range corresponding to the length of a list *)
val create_corr_range : int -> 'a list -> int list

(* insert an index for each member of a list *)
val insert_index_fst : int -> 'a list -> (int * 'a) list

(* insert an index for each member of a list, in snd place *)
val insert_index_snd : int -> 'a list -> ('a * int) list

(* tail recursive, so more efficient than mapping alone *)
val list_map : ('a -> 'b) -> 'a list -> 'b list

(* get an index with every item in a map *)
val list_mapi : (int * 'a -> 'b) -> 'a list -> 'b list

(* a cross between a map and a fold. Can only map the current list, but also
 * gets another value to play with, and no need to project out the temporary
 * value *)
val mapfold : ('b -> 'a -> 'b * 'c) -> 'b -> 'a list -> 'b * 'c list

val filter_map : ('a -> 'b option) -> 'a list -> 'b list

(* calls f on its output over and over, num times *)
val iterate : ('a -> 'a) -> 'a -> int -> 'a

(* calls f on its output over and over again until p is true *)
val iterate_until : ('a -> ('b, 'a) either_t) -> 'a -> 'b

(* repeat a function many times, building a list from indices *)
(* do this without instantiating the index list *)
val list_populate : (int -> 'a) -> int -> int -> 'a list

(* transform a list into a list of lists of i elements *)
(* if there aren't enough elements to fill the last list, it's filled as much as
 * possible *)
val list_bunch : int -> 'a list -> 'a list list

(* intersperse 2 lists together. When one runs out, continue with the other *)
val list_intersperse : 'a list -> 'a list -> 'a list

(* functions without exceptions *)
val list_find : ('a -> bool) -> 'a list -> 'a option
val find : ('a -> 'b -> 'c) -> 'a -> 'b -> 'c option

(* find the maximum of a list after it's been modified by a function *)
val list_max_op : ('a -> 'b) -> 'a list -> 'a * 'b
val list_min_op: ('a -> 'b) -> 'a list -> 'a * 'b

val list_min : 'a list -> 'a
val list_max : 'a list -> 'a

(* modify/add to an association list generically *)
val assoc_modify : ('b option -> 'b option) -> 'a -> ('a * 'b) list -> ('a * 'b) list

val assoc_join : ('a * 'b) list -> ('a * 'c) list -> ('a * ('b * 'c)) list

val cartesian_product : 'a list -> 'b list -> ('a * 'b) list

(* unique members of a list *)
val nub : 'a list -> 'a list

val array_find : ('a -> bool) -> 'a array -> int * 'a

(* map an array to a list *)
val array_map : ('a -> 'b) -> 'a array -> 'b list

(* wrap an expression in some *)
val some : 'a -> 'a option

val is_some : 'a option -> bool

(* unwrap a some. Fail if not a Some *)
val unwrap_some : 'a option -> 'a

(* flatten a list of maybes into a list *)
val flatten_some : 'a option list -> 'a list

(* --- String functions --- *)
val lines : string -> string list

val unlines : string list -> string

val words : string -> string list

val unwords : string list -> string

val str_take : int -> string -> string

val str_drop : int -> string -> string

val str_take_end : int -> string -> string

val str_drop_end : int -> string -> string

(* --- regexp helpers --- *)

(* returns a list of groups of a regexp *)
val r_groups : string -> r:string -> n:int -> string option list

(* takes regexp, str *)
val r_split : string -> string -> string list

(* regexp -> str -> bool *)
val r_match : string -> string -> bool


(* --- other stuff ---- *)
val make_lst : 'a -> int -> 'a list
