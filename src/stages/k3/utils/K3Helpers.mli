(* Utility functions to enable easy manipulation of K3 AST trees *)
open K3.AST
open K3.Annotation

val mk_no_anno : 'a -> 'a * annotation_t
(* Wrap with sorted annotation. Give int list of positions to sort by *)
val mk_anno_sort : 'a * annotation_t -> int list -> 'a * annotation_t

(* easy access to K3 types *)
val canonical : base_type_t -> value_type_t
val t_bool : value_type_t
val t_int : value_type_t
val t_int_mut : value_type_t
val t_float : value_type_t
val t_float_mut : value_type_t
val t_byte : value_type_t
val t_string : value_type_t
val t_unit : value_type_t
val t_unknown : value_type_t

(* easy type for addresses *)
val t_addr : value_type_t

(* K3 types for various things *)
val t_trig_id : value_type_t
val t_stmt_id : value_type_t
val t_map_id : value_type_t
val t_vid : value_type_t
val t_vid_mut : value_type_t

(* create a global vid comparison function *)
type vid_op = VEq | VNeq | VGt | VLt | VGeq | VLeq
val mk_global_vid_op : id_t -> vid_op -> declaration_t * annotation_t

(* wrap in a specific type *)
val wrap_tlist : value_type_t -> value_type_t
val wrap_tlist_mut : value_type_t -> value_type_t
val wrap_tset : value_type_t -> value_type_t
val wrap_tset_mut : value_type_t -> value_type_t
val wrap_tbag : value_type_t -> value_type_t
val wrap_tbag_mut : value_type_t -> value_type_t
val wrap_ttuple : value_type_t list -> value_type_t
val wrap_ttuple_mut : value_type_t list -> value_type_t
val wrap_tmaybe : value_type_t -> value_type_t 
val wrap_tmaybes : value_type_t list -> value_type_t list
val wrap_tfunc : value_type_t -> value_type_t -> type_t
val wrap_args : (id_t * value_type_t) list -> arg_t
val wrap_args_maybe : (id_t * value_type_t) list -> arg_t

(* simple functions that enable easy construction of AST trees *)
val mk_const : constant_t -> expr_t
val mk_const_int : int -> expr_t
val mk_var : id_t -> expr_t
val mk_tuple : expr_t list -> expr_t
val mk_just : expr_t -> expr_t
val mk_nothing : value_type_t -> expr_t

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
val mk_deref : expr_t -> expr_t
val mk_send : expr_t -> expr_t -> expr_t -> expr_t

(* smart role constructors *)
val mk_const_stream : id_t -> value_type_t -> expr_t list -> flow_statement_t *
    annotation_t
val mk_random_stream : id_t -> value_type_t -> int -> flow_statement_t * annotation_t
val mk_file_handle : id_t -> value_type_t -> string -> ?is_json:bool -> bool ->
    flow_statement_t * annotation_t
val mk_net_handle : id_t -> value_type_t -> address -> ?is_json:bool -> bool ->
    flow_statement_t * annotation_t
val mk_bind : id_t -> id_t -> flow_statement_t * annotation_t
val mk_consume : id_t -> flow_statement_t * annotation_t
val mk_role : id_t -> flow_program_t -> declaration_t * annotation_t

(* extract only the types from a list of (id, type) *)
val extract_arg_types : ('a * 'b) list -> 'b list

(* extract only the names from a list of (id, type) *)
val extract_arg_names : ('a * 'b) list -> 'a list

(* take a list of ids and convert it to a list of vars *)
val ids_to_vars :
  id_t list -> expr_t list

(* check if a collection is empty *)
val mk_is_empty : expr_t -> value_type_t -> expr_t

(* macro to check if a collection has a specific member *)
val mk_has_member :
  expr_t ->
  expr_t ->
  value_type_t -> expr_t

(* macro to create a trigger *)
val mk_code_sink : id_t -> arg_t -> (id_t * value_type_t * annotation_t) list 
  -> expr_t -> flow_statement_t * annotation_t

(* macro to create a global value *)
val mk_global_val : id_t -> value_type_t -> declaration_t * annotation_t

val mk_global_val_init : 
  id_t -> value_type_t -> expr_t -> declaration_t * annotation_t

(* macro to generate a global function with more control over args *)
val mk_global_fn_raw:
  id_t -> arg_t -> value_type_t -> value_type_t -> expr_t -> 
    declaration_t * annotation_t

(* macro to create a global function *)
val mk_global_fn : 
  id_t -> (id_t * value_type_t) list -> value_type_t list ->
  expr_t -> declaration_t * annotation_t

(* macro to declare a foreign function *)
val mk_foreign_fn :
  id_t -> value_type_t -> value_type_t -> declaration_t * annotation_t

(* macro to declare a flow *)
val mk_flow : flow_program_t -> declaration_t * annotation_t

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

(* macro similar to fst *)
val mk_fst: value_type_t list -> expr_t -> expr_t

(* macro similar to snd *)
val mk_snd: value_type_t list -> expr_t -> expr_t

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
val int_to_temp_id: string -> int -> id_t

(* create a consistent range with which to refer to tuple ids when destructing
 * tuples *)
val mk_tuple_range: 'a list -> int list

(* turn a list of types to a list of made up ids and corresponding types *)
val types_to_ids_types : string -> value_type_t list -> (string * value_type_t) list

(* destruct a tuple, at which point parts are available via ids made by
 * int_to_temp_id *)
val mk_destruct_tuple: id_t -> value_type_t list -> string -> expr_t -> expr_t

(* create K3 code to build a tuple using a pattern containing portions of the
 * old tuple and ids of new variables. The pattern is produced by
 * tuple_make_pattern
 * tuple name -> type list -> pattern
 * *)
val mk_rebuild_tuple: id_t -> value_type_t list -> tuple_pat list -> expr_t

(* unwrap maybe values by creating an inner values with postfix "_unwrap" *)
val mk_unwrap_maybe: (id_t * value_type_t) list -> expr_t -> expr_t

(* id function for maps. No need fill in a tuple creation *)
val mk_id: value_type_t list -> expr_t

