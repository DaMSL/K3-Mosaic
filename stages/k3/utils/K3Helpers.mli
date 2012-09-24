(* Utility functions to enable easy manipulation of K3 AST trees *)
open K3.AST

(* easy access to the int type *)
val canonical : base_type_t -> value_type_t
val t_int : value_type_t
val t_int_mut : value_type_t
val t_float : value_type_t
val t_float_mut : value_type_t

(* wrap in a list *)
val wrap_tlist : value_type_t -> value_type_t
val wrap_tlist_mut : value_type_t -> value_type_t
val wrap_ttuple : value_type_t list -> value_type_t
val wrap_ttuple_mut : value_type_t list -> value_type_t
val wrap_args : (id_t * value_type_t) list -> arg_t
val wrap_args_maybe : (id_t * value_type_t) list -> arg_t

(* simple functions that enable easy construction of AST trees *)
val mk_const : constant_t -> expr_t
val mk_var : id_t -> expr_t
val mk_tuple : expr_t list -> expr_t
val mk_just : expr_t -> expr_t

val mk_empty : value_type_t -> expr_t
val mk_singleton : value_type_t -> expr_t -> expr_t
val mk_combine : expr_t -> expr_t -> expr_t
val mk_range : container_type_t -> expr_t -> expr_t -> expr_t -> expr_t

val mk_add : expr_t -> expr_t -> expr_t
val mk_sub : expr_t -> expr_t -> expr_t
val mk_or : expr_t -> expr_t -> expr_t
val mk_mult : expr_t -> expr_t -> expr_t
val mk_and : expr_t -> expr_t -> expr_t
val mk_neg : expr_t -> expr_t

val mk_not : expr_t -> expr_t
val mk_eq : expr_t -> expr_t -> expr_t
val mk_lt : expr_t -> expr_t -> expr_t
val mk_neq : expr_t -> expr_t -> expr_t
val mk_leq : expr_t -> expr_t -> expr_t
val mk_geq : expr_t -> expr_t -> expr_t
val mk_gt : expr_t -> expr_t -> expr_t

val mk_lambda : arg_t -> expr_t -> expr_t
val mk_apply : expr_t -> expr_t -> expr_t
val mk_block : expr_t list -> expr_t
val mk_iter : expr_t -> expr_t -> expr_t
val mk_if : expr_t -> expr_t -> expr_t -> expr_t

val mk_map : expr_t -> expr_t -> expr_t
val mk_filtermap : expr_t -> expr_t -> expr_t -> expr_t
val mk_flatten : expr_t -> expr_t
val mk_agg : expr_t -> expr_t -> expr_t -> expr_t
val mk_gbagg : expr_t -> expr_t -> expr_t -> expr_t -> expr_t
val mk_sort : expr_t -> expr_t -> expr_t

val mk_peek : expr_t -> expr_t
val mk_slice : expr_t -> expr_t -> expr_t
val mk_insert : expr_t -> expr_t -> expr_t
val mk_delete : expr_t -> expr_t -> expr_t
val mk_update : expr_t -> expr_t -> expr_t -> expr_t

val mk_assign : expr_t -> expr_t -> expr_t
val mk_send : expr_t -> expr_t -> expr_t -> expr_t

(* extract only the types from a list of (id, type) *)
val extract_arg_types : ('a * 'b) list -> 'b list

(* extract only the names from a list of (id, type) *)
val extract_arg_names : ('a * 'b) list -> 'a list

(* take a list of ids and convert it to a list of vars *)
val ids_to_vars :
  id_t list -> expr_t list

(* macro to check if a collection has a specific member *)
val mk_has_member :
  expr_t ->
  expr_t ->
  value_type_t -> expr_t

(* macro to create a global value *)
val mk_global_val : id_t -> value_type_t -> declaration_t

(* macro to create a global function *)
val mk_global_fn : 
  id_t ->
  (id_t * value_type_t) list ->
  value_type_t list ->
  expr_t -> declaration_t

(* macro to declare a foreign function *)
val mk_foreign_fn :
  id_t -> value_type_t -> value_type_t -> declaration_t

(* macro to create an associative lambda ie a lambda with 2 args *)
val mk_assoc_lambda :
  arg_t ->
  arg_t ->
  expr_t ->
  expr_t

(* macro to create a regular functional let structure *)
val mk_let :
  id_t ->
  value_type_t ->
  expr_t ->
  expr_t ->
  expr_t

(* macro to make a 'let many', where many values are assigned simultaneously *)
val mk_let_many :
  (id_t * value_type_t) list ->
  expr_t ->
  expr_t ->
  expr_t

(* data type to manipulate tuples *)
type tuple_pat = Position of int | ExternVar of id_t | Unknown

(* create a string pattern of a tuple, for manipulation
 * specify the length of the pattern *)
val tuple_make_pattern: value_type_t list -> tuple_pat list

(* take and drop from a pattern, replacing the values taken/dropped with
 * unknowns for slices *)
val slice_pat_take: int -> tuple_pat list -> tuple_pat list

val slice_pat_drop: int -> tuple_pat list -> tuple_pat list

(* given an integer and a prefix, create an id for the internals of a tuple *)
val int_to_temp_id: int -> string -> id_t

(* destruct a tuple, at which point parts are available via ids made by
 * int_to_temp_id *)
val mk_destruct_tuple: id_t -> value_type_t list -> string -> expr_t -> expr_t

(* create K3 code to build a tuple using a pattern containing portions of the
 * old tuple and ids of new variables. The pattern is produced by
 * tuple_make_pattern
 * tuple name -> type list -> pattern
 * *)
val mk_rebuild_tuple: id_t -> value_type_t list -> tuple_pat list -> expr_t

