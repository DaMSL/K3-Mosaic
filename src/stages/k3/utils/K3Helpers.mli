(* Utility functions to enable easy manipulation of K3 AST trees *)
open K3.AST
open K3.Annotation

val mk_no_anno : 'a -> 'a * annotation_t
(* Wrap with sorted annotation. Give int list of positions to sort by *)
val mk_anno_sort : 'a * annotation_t -> int list -> 'a * annotation_t

(* easy access to K3 types *)
val canonical : base_type_t -> value_type_t
val t_bool : value_type_t
val t_bool_mut: value_type_t
val t_int : value_type_t
val t_int_mut : value_type_t
val t_date : value_type_t
val t_float : value_type_t
val t_float_mut : value_type_t
val t_byte : value_type_t
val t_string : value_type_t
val t_unit : value_type_t
val t_unknown : value_type_t

(* easy type for addresses *)
val t_addr : value_type_t
val t_addr_mut : value_type_t

(* K3 types for various things *)
val t_trig_id : value_type_t
val t_stmt_id : value_type_t
val t_map_id : value_type_t
val t_vid : value_type_t
val t_vid_mut : value_type_t

(* create a global vid comparison function *)
type vid_op = VEq | VNeq | VGt | VLt | VGeq | VLeq
val mk_global_vid_op : id_t -> vid_op -> declaration_t * annotation_t

(* preserve isolation while operating on underlying type *)
val preserve_iso : value_type_t -> (mutable_type_t -> mutable_type_t) -> value_type_t

(* preserve mutability while operating on underlying base type & annotation *)
val preserve_mut : mutable_type_t -> (base_type_t -> base_type_t) -> mutable_type_t

(* convert a type to mutable *)
val mut : value_type_t -> value_type_t


(* wrap in a specific type *)
val wrap_ttuple : value_type_t list -> value_type_t
val wrap_ttuple_mut : value_type_t list -> value_type_t
val wrap_tlist : value_type_t -> value_type_t
(* a version that wraps in a tuple if necessary *)
val wrap_tlist' : value_type_t list -> value_type_t
val wrap_tlist_mut : value_type_t -> value_type_t
val wrap_tlist_mut' : value_type_t list -> value_type_t
val wrap_tset : value_type_t -> value_type_t
val wrap_tset' : value_type_t list -> value_type_t
val wrap_tset_mut : value_type_t -> value_type_t
val wrap_tset_mut' : value_type_t list -> value_type_t
val wrap_tbag : value_type_t -> value_type_t
val wrap_tbag' : value_type_t list -> value_type_t
val wrap_tbag_mut : value_type_t -> value_type_t
val wrap_tbag_mut' : value_type_t list -> value_type_t
val wrap_tmap : value_type_t -> value_type_t
val wrap_tmap' : value_type_t list -> value_type_t
val wrap_tmap_mut : value_type_t -> value_type_t
val wrap_tmap_mut' : value_type_t list -> value_type_t
val wrap_tmmap : index_t list -> value_type_t -> value_type_t
val wrap_tmmap' : index_t list -> value_type_t list -> value_type_t
val wrap_tmmap_mut : index_t list -> value_type_t -> value_type_t
val wrap_tmmap_mut' : index_t list -> value_type_t list -> value_type_t
val wrap_tind : value_type_t -> value_type_t
val wrap_tind_mut : value_type_t -> value_type_t
val wrap_tmaybe : value_type_t -> value_type_t
val wrap_tmaybes : value_type_t list -> value_type_t list
val wrap_tfunc : value_type_t -> value_type_t -> type_t

(* wrap a single layer of arguments *)
val wrap_args : (id_t * value_type_t) list -> arg_t

(* wrap multiple layers of arguments *)
val wrap_args_deep : arg_t list -> arg_t

(* wrap arguments, turning maybe types to maybe argument types *)
val wrap_args_maybe : (id_t * value_type_t) list -> arg_t

(* Unwrap functions for types *)
(* returns mutability and type *)
val unwrap_vtype : value_type_t -> bool * base_type_t

val unwrap_col : base_type_t -> container_type_t * value_type_t

val unwrap_vcol : value_type_t -> bool * (container_type_t * value_type_t)

val unwrap_tval : type_t -> value_type_t

val unwrap_tcol : type_t -> bool * (container_type_t * value_type_t)

(* unwrap a tuple type and return its list. If not a ttuple, return as singleton *)
val unwrap_ttuple : value_type_t -> value_type_t list

(* simple functions that enable easy construction of AST trees *)
val mk_const : constant_t -> expr_t
val mk_cint : int -> expr_t
val mk_cfloat : float -> expr_t
val mk_cstring : string -> expr_t
val mk_cbool : bool -> expr_t
val mk_ctarget : id_t -> expr_t
val mk_cunknown : expr_t
val mk_cunit : expr_t
val mk_caddress : address -> expr_t

val mk_var : id_t -> expr_t
val mk_tuple : ?force:bool -> expr_t list -> expr_t
val mk_just : expr_t -> expr_t
val mk_nothing : value_type_t -> expr_t
val mk_nothing_m : value_type_t -> expr_t

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
val mk_lambda' : (id_t * value_type_t) list -> expr_t -> expr_t
val mk_apply : expr_t -> expr_t -> expr_t
val mk_block : expr_t list -> expr_t
val mk_iter : expr_t -> expr_t -> expr_t
val mk_if : expr_t -> expr_t -> expr_t -> expr_t
val mk_case : expr_t -> id_t -> expr_t -> expr_t -> expr_t
val mk_case_rev : expr_t -> id_t -> expr_t -> expr_t -> expr_t

val mk_map : expr_t -> expr_t -> expr_t
val mk_filter : expr_t -> expr_t -> expr_t
val mk_flatten : expr_t -> expr_t
val mk_agg : expr_t -> expr_t -> expr_t -> expr_t
val mk_gbagg : expr_t -> expr_t -> expr_t -> expr_t -> expr_t
val mk_sort : expr_t -> expr_t -> expr_t

val mk_peek : expr_t -> expr_t
val mk_slice : expr_t -> expr_t -> expr_t
val mk_slice' : expr_t -> expr_t list -> expr_t
(* int list list: specify index to use
   expr_t: list of integer values specifying GT, LT, EQ *)
val mk_slice_idx : int list list -> expr_t -> expr_t -> expr_t -> expr_t
val mk_slice_idx' : int list list -> expr_t list -> expr_t -> expr_t list -> expr_t
val mk_insert : expr_t -> expr_t -> expr_t
val mk_delete : expr_t -> expr_t -> expr_t
val mk_update : expr_t -> expr_t -> expr_t -> expr_t

val mk_ind : expr_t -> expr_t
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

(* take a list of ids and convert it to a list of vars *)
val ids_to_vars : id_t list -> expr_t list

(* take a list of vars and convert it to ids *)
val vars_to_ids : expr_t list -> id_t list

(* check if a collection is empty *)
val mk_is_empty : expr_t -> value_type_t -> expr_t

(* macro to check if a collection has a specific member *)
val mk_has_member : expr_t -> expr_t -> value_type_t -> expr_t

val mk_has_member' : expr_t -> expr_t list -> value_type_t -> expr_t

(* macro to create a trigger *)
val mk_code_sink : id_t -> arg_t -> (id_t * value_type_t * annotation_t) list
  -> expr_t -> flow_statement_t * annotation_t
(* use lists instead of needing wrap_args *)
val mk_code_sink' : id_t -> (id_t * value_type_t) list -> (id_t * value_type_t * annotation_t) list
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
val mk_assoc_lambda : arg_t -> arg_t -> expr_t -> expr_t

(* same macro, but auto-calls wrap_args *)
val mk_assoc_lambda' : (id_t * value_type_t) list -> (id_t * value_type_t) list -> expr_t -> expr_t

(* macro to create a regular functional let structure *)
val mk_let : id_t -> value_type_t -> expr_t -> expr_t -> expr_t

(* macro to make a 'let many', where many values are assigned simultaneously *)
val mk_let_many :
  (id_t * value_type_t) list -> expr_t -> expr_t -> expr_t

(* macro to make a deep-matching let statement *)
val mk_let_deep : arg_t -> expr_t -> expr_t -> expr_t

val mk_let_deep' : (id_t * value_type_t) list -> expr_t -> expr_t -> expr_t

(* like fst, but for a tuple of any size *)
val project_from_tuple : value_type_t list -> expr_t -> total:int -> choice:int -> expr_t

(* macro similar to fst *)
val mk_fst: value_type_t list -> expr_t -> expr_t

(* macro similar to snd *)
val mk_snd: value_type_t list -> expr_t -> expr_t

(* like fst, but for a collection with a tuple of any size *)
val project_from_col : value_type_t list -> expr_t -> total:int -> choice:int -> expr_t

(* macro similar to fst but for a collection *)
val mk_fst_many: value_type_t list -> expr_t -> expr_t

(* macro similar to snd but for a collection *)
val mk_snd_many: value_type_t list -> expr_t -> expr_t

(* given an integer and a prefix, create an id for the internals of a tuple *)
val int_to_temp_id: string -> int -> id_t

(* create a consistent range with which to refer to tuple ids when destructing
 * tuples *)
val mk_tuple_range: ?first:int -> 'a list -> int list

(* turn a list of types to a list of made up ids and corresponding types *)
val types_to_ids_types : ?first:int -> string -> value_type_t list -> (string * value_type_t) list

(* destruct a tuple, at which point parts are available via ids made by
 * int_to_temp_id *)
val mk_destruct_tuple: id_t -> value_type_t list -> string -> expr_t -> expr_t

(* Create K3 code to rebuild a tuple
 * A lambda determines how to shuffle the ids of the tuple, or insert external ids
 * *)
val mk_rebuild_tuple: ?prefix:id_t -> id_t -> value_type_t list -> (id_t list -> id_t list) -> expr_t

(* unwrap maybe values by creating an inner values with postfix "_unwrap" *)
val mk_unwrap_maybe: (id_t * value_type_t) list -> expr_t -> expr_t

(* id function for maps. No need fill in a tuple creation *)
val mk_id: value_type_t list -> expr_t

(* Convert between k3 representation and ocaml representation of containers *)
val list_of_k3_container : expr_t -> expr_t list
val k3_container_of_list : value_type_t -> expr_t list -> expr_t

(* convert arg to value type *)
val value_type_of_arg: arg_t -> value_type_t

(* convert the type of a collection *)
val mk_convert_col : value_type_t -> value_type_t -> expr_t -> expr_t
