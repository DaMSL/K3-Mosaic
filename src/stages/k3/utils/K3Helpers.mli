(* Utility functions to enable easy manipulation of K3 AST trees *)
open Util
open K3.AST
open K3.Annotation

val mk_no_anno : 'a -> 'a * annotation_t

(* easy access to K3 types *)
val canonical : base_type_t -> type_t
val t_bool : type_t
val t_bool_mut: type_t
val t_int : type_t
val t_int_mut : type_t
val t_date : type_t
val t_float : type_t
val t_float_mut : type_t
val t_byte : type_t
val t_string : type_t
val t_unit : type_t
val t_unknown : type_t
val t_top : type_t

(* easy type for addresses *)
val t_addr : type_t
val t_addr_mut : type_t

val t_alias : id_t -> type_t

val t_bool_vector : type_t
val t_int_vector : type_t
val t_bitset : type_t

(* K3 types for various things *)
val t_trig_id : type_t
val t_stmt_id : type_t
val t_stmt_map_id : type_t
val t_map_id : type_t
val t_vid : type_t

val vid_increment : ?vid_expr:expr_t -> unit -> expr_t
val min_vid_k3 : expr_t
val sys_init_vid_k3 : expr_t
val start_vid_k3 : expr_t

(* convert a type to mutable *)
val mut : type_t -> type_t
val immut : type_t -> type_t

(* wrap in a specific type *)
val wrap_ttuple : type_t list -> type_t
val wrap_ttuple_mut : type_t list -> type_t
val wrap_tcol : container_type_t -> type_t -> type_t
val wrap_tlist : type_t -> type_t
(* a version that wraps in a tuple if necessary *)
val wrap_tlist' : type_t list -> type_t
val wrap_tvector : type_t -> type_t
val wrap_tvector' : type_t list -> type_t
val wrap_tset : type_t -> type_t
val wrap_tset' : type_t list -> type_t
val wrap_tbag : type_t -> type_t
val wrap_tbag' : type_t list -> type_t
val wrap_tmap : type_t -> type_t
val wrap_tmap' : type_t list -> type_t
val wrap_tsortedmap : type_t -> type_t
val wrap_tsortedmap' : type_t list -> type_t
val wrap_tsortedset : type_t -> type_t
val wrap_tsortedset' : type_t list -> type_t
val wrap_tvmap : ?idx:IntSetSet.t -> type_t -> type_t
val wrap_tvmap' : ?idx:IntSetSet.t -> type_t list -> type_t
val wrap_tpolyq : poly_tags -> type_t
val wrap_tuniqpolyq : poly_tags -> type_t
val wrap_t_calc : type_t -> type_t
val wrap_t_calc' : type_t list -> type_t
val wrap_tind : type_t -> type_t
val wrap_tind_mut : type_t -> type_t
val wrap_tmaybe : type_t -> type_t
val wrap_tmaybes : type_t list -> type_t list
(* our own version of maybes that uses tuples instead *)
val wrap_tupmaybe : type_t -> type_t
val wrap_tupmaybes : type_t list -> type_t list
val wrap_tfunc : type_t list -> type_t -> type_t

(* wrap a single layer of arguments *)
val wrap_args : (id_t * type_t) list -> arg_t

(* wrap multiple layers of arguments *)
val wrap_args_deep : (id_t * type_t) list -> arg_t

(* wrap arguments, turning maybe types to maybe argument types *)
val wrap_args_maybe : (id_t * type_t) list -> arg_t

(* Unwrap functions for types *)
(* returns mutability and type *)
val unwrap_col : base_type_t -> container_type_t * type_t

val unwrap_tcol : type_t -> container_type_t * type_t

val unwrap_tfun : type_t -> type_t list * type_t

val unwrap_t : type_t -> base_type_t

(* unwrap a tuple type and return its list. If not a ttuple, return as singleton *)
val unwrap_ttuple : type_t -> type_t list

val unwrap_tind : type_t -> type_t

val is_tind : type_t -> bool

(* simple functions that enable easy construction of AST trees *)
val mk_const : constant_t -> expr_t
val mk_cint : int -> expr_t
val mk_cfloat : float -> expr_t
val mk_cstring : string -> expr_t
val mk_cbool : bool -> expr_t
val mk_ctrue : expr_t
val mk_cfalse : expr_t
val mk_ctarget : id_t -> expr_t
val mk_cunknown : expr_t
val mk_cunit : expr_t
val mk_caddress : address -> expr_t

val mk_var : id_t -> expr_t
val mk_ignore : expr_t -> expr_t
val mk_tuple : ?force:bool -> expr_t list -> expr_t
val mk_just : expr_t -> expr_t
val mk_nothing : type_t -> expr_t
val mk_nothing_m : type_t -> expr_t

val mk_empty : type_t -> expr_t
val mk_singleton : type_t -> expr_t list -> expr_t
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
val mk_lambda' : (id_t * type_t) list -> expr_t -> expr_t
(* deep matching on tuples *)
val mk_lambda'' : (id_t * type_t) list -> expr_t -> expr_t
val mk_lambda2 : arg_t -> arg_t -> expr_t -> expr_t
val mk_lambda2' : (id_t * type_t) list -> (id_t * type_t) list -> expr_t -> expr_t
val mk_lambda2'' : (id_t * type_t) list -> (id_t * type_t) list -> expr_t -> expr_t
val mk_lambda2d' : (id_t * type_t) list list -> (id_t * type_t) list list -> expr_t -> expr_t
val mk_lambda3 : arg_t -> arg_t -> arg_t -> expr_t -> expr_t
val mk_lambda3' : (id_t * type_t) list -> (id_t * type_t) list -> (id_t * type_t) list -> expr_t -> expr_t
val mk_lambda4 : arg_t -> arg_t -> arg_t -> arg_t -> expr_t -> expr_t
val mk_lambda4' : (id_t * type_t) list -> (id_t * type_t) list -> (id_t * type_t) list -> (id_t * type_t) list -> expr_t -> expr_t
val mk_apply : expr_t -> expr_t list -> expr_t
val mk_apply' : id_t -> expr_t list -> expr_t
val mk_block : expr_t list -> expr_t
val mk_iter : expr_t -> expr_t -> expr_t
val mk_iter' : expr_t -> id_t -> expr_t
val mk_if : expr_t -> expr_t -> expr_t -> expr_t
val mk_if_eq : expr_t -> expr_t -> expr_t -> expr_t -> expr_t
val mk_case_sn : expr_t -> id_t -> expr_t -> expr_t -> expr_t
val mk_case_ns : expr_t -> id_t -> expr_t -> expr_t -> expr_t
val mk_bind : expr_t -> id_t -> expr_t -> expr_t
val mk_deref : expr_t -> expr_t

val mk_map : expr_t -> expr_t -> expr_t
val mk_filter : expr_t -> expr_t -> expr_t
val mk_flatten : expr_t -> expr_t
val mk_agg : expr_t -> expr_t -> expr_t -> expr_t
val mk_aggv : expr_t -> expr_t -> expr_t -> expr_t
val mk_agg_fst : expr_t -> expr_t -> expr_t
val mk_gbagg : expr_t -> expr_t -> expr_t -> expr_t -> expr_t
val mk_sort : expr_t -> expr_t -> expr_t
val mk_equijoin : expr_t -> expr_t -> expr_t -> expr_t -> expr_t -> expr_t -> expr_t
val mk_size : expr_t -> expr_t
val mk_subscript : int -> expr_t -> expr_t

val mk_peek : expr_t -> expr_t
val mk_peek_with_vid : expr_t -> expr_t -> expr_t -> expr_t
val mk_at_with : ?error:expr_t -> expr_t -> expr_t -> expr_t -> expr_t
val mk_at_with' : ?error:expr_t -> id_t -> expr_t -> expr_t -> expr_t
val mk_at : expr_t -> expr_t -> expr_t
val mk_at' : id_t -> expr_t -> expr_t
val mk_min_with : expr_t -> expr_t -> expr_t -> expr_t
val mk_is_member : expr_t -> expr_t -> expr_t
val mk_is_member' : id_t -> expr_t -> expr_t

val mk_peek' : id_t -> expr_t
val mk_slice : expr_t -> expr_t list -> expr_t
val mk_slice' : id_t -> expr_t list -> expr_t
(* int list list: specify index to use
   expr_t: list of integer values specifying GT, LT, EQ *)
val mk_slice_lt : expr_t -> expr_t list -> expr_t
val mk_slice_lt' : id_t -> expr_t list -> expr_t
val mk_slice_leq : expr_t -> expr_t list -> expr_t
val mk_slice_leq' : id_t -> expr_t list -> expr_t
val mk_slice_gt : expr_t -> expr_t list -> expr_t
val mk_slice_gt' : id_t -> expr_t list -> expr_t
val mk_slice_geq : expr_t -> expr_t list -> expr_t
val mk_slice_geq' : id_t -> expr_t list -> expr_t
val mk_insert : ?path:int list -> id_t -> expr_t list -> expr_t
val mk_insert_at : ?path: int list -> id_t -> expr_t -> expr_t list -> expr_t
val mk_extend : ?path:int list -> id_t -> expr_t -> expr_t
val mk_upsert_with : ?path:int list -> id_t -> expr_t list -> expr_t -> expr_t -> expr_t
val mk_upsert_with_before : ?path:int list -> id_t -> expr_t list -> expr_t -> expr_t -> expr_t
val mk_delete : ?path:int list -> id_t -> expr_t list -> expr_t
val mk_delete_block : ?path:int list -> id_t -> expr_t list -> expr_t
val mk_delete_prefix : ?path:int list -> id_t -> expr_t list -> expr_t
val mk_delete_all_prefix : ?path:int list -> id_t -> expr_t -> expr_t
val mk_delete_at : ?path:int list -> id_t -> expr_t -> expr_t
val mk_delete_with : ?path:int list -> id_t -> expr_t list -> expr_t -> expr_t -> expr_t
val mk_pop : ?path:int list -> id_t -> expr_t
val mk_clear_all : ?path:int list -> id_t -> expr_t
val mk_clear_all_block : ?path:int list -> id_t -> expr_t
val mk_update : ?path:int list -> id_t -> expr_t list -> expr_t list -> expr_t
val mk_update_suffix : ?path:int list -> id_t -> expr_t list -> expr_t -> expr_t
val mk_update_at_with : ?path:int list -> id_t -> expr_t -> expr_t -> expr_t
val mk_update_slice : id_t -> expr_t list -> expr_t -> expr_t
val mk_filter_gt : expr_t -> expr_t list -> expr_t
val mk_filter_gt' : id_t -> expr_t list -> expr_t
val mk_filter_geq : expr_t -> expr_t list -> expr_t
val mk_filter_geq' : id_t -> expr_t list -> expr_t
val mk_filter_lt : expr_t -> expr_t list -> expr_t
val mk_filter_lt' : id_t -> expr_t list -> expr_t
val mk_filter_leq : expr_t -> expr_t list -> expr_t
val mk_filter_leq' : id_t -> expr_t list -> expr_t

val mk_ind : expr_t -> expr_t
val mk_assign : ?path:int list -> id_t -> expr_t -> expr_t
val mk_send : id_t -> expr_t -> expr_t list -> expr_t
val mk_send_raw: expr_t -> expr_t -> expr_t -> expr_t

(* smart role constructors *)
val mk_const_stream : id_t -> type_t -> expr_t list -> flow_statement_t *
    annotation_t
val mk_random_stream : id_t -> type_t -> int -> flow_statement_t * annotation_t
val mk_file_handle : id_t -> type_t -> string -> ?is_json:bool -> bool ->
    flow_statement_t * annotation_t
val mk_net_handle : id_t -> type_t -> address -> ?is_json:bool -> bool ->
    flow_statement_t * annotation_t
val mk_bind_role : id_t -> id_t -> flow_statement_t * annotation_t
val mk_consume : id_t -> flow_statement_t * annotation_t
val mk_role : id_t -> flow_program_t -> declaration_t * annotation_t
val mk_typedef : id_t -> type_t -> declaration_t * annotation_t

(* take a list of ids and convert it to a list of vars *)
val ids_to_vars : id_t list -> expr_t list
val ids_to_vars' : (id_t * 'a) list -> expr_t list

(* take a list of vars and convert it to ids *)
val vars_to_ids : expr_t list -> id_t list

(* check if a collection is empty *)
val mk_is_empty : expr_t -> y:expr_t -> n:expr_t -> expr_t

(* macro to check if a collection has a specific member *)
val mk_has_member : expr_t -> expr_t list -> type_t -> expr_t

(* macro to create a trigger *)
val mk_code_sink : id_t -> arg_t -> (id_t * type_t * annotation_t) list
  -> expr_t -> flow_statement_t * annotation_t
(* use lists instead of needing wrap_args *)
val mk_code_sink' : id_t -> (id_t * type_t) list -> (id_t * type_t * annotation_t) list
  -> expr_t -> flow_statement_t * annotation_t

(* macro to create a global value *)
val mk_global_val : id_t -> type_t -> declaration_t * annotation_t

val mk_global_val_init :
  id_t -> type_t -> expr_t -> declaration_t * annotation_t

(* macro to generate a global function with more control over args *)
val mk_global_fn_raw:
  id_t -> arg_t -> type_t list -> type_t -> expr_t -> declaration_t * annotation_t

(* macro to create a global function *)
val mk_global_fn :
  ?wr_all:bool -> ?wr_arg:int list -> id_t -> (id_t * type_t) list -> type_t list -> expr_t -> declaration_t * annotation_t

(* macro to declare a foreign function *)
val mk_foreign_fn :
  id_t -> type_t list -> type_t -> declaration_t * annotation_t

(* macro to declare a foreign function given a type *)
val mk_foreign_short : id_t -> type_t -> declaration_t * annotation_t

(* macro to declare a flow *)
val mk_flow : flow_program_t -> declaration_t * annotation_t

(* macro to create an associative lambda ie a lambda with 2 args *)
val mk_assoc_lambda : arg_t -> arg_t -> expr_t -> expr_t

(* same macro, but auto-calls wrap_args *)
val mk_assoc_lambda' : (id_t * type_t) list -> (id_t * type_t) list -> expr_t -> expr_t

(* let structure *)
val mk_let : id_t list -> expr_t -> expr_t -> expr_t
val mk_let_block : id_t list -> expr_t -> expr_t list -> expr_t

(* polyqueue functions *)
(* lambda returns (idx,off) *)
val mk_poly_iter: expr_t -> expr_t -> expr_t -> expr_t -> expr_t
val mk_poly_iter': ?unique:bool -> expr_t -> expr_t
val mk_poly_fold: expr_t -> expr_t -> expr_t -> expr_t
val mk_poly_iter_tag: string -> expr_t -> expr_t -> expr_t -> expr_t -> expr_t
val mk_poly_iter_tag':?unique:bool -> string -> expr_t -> expr_t
val mk_poly_fold_tag: string -> expr_t -> expr_t -> expr_t -> expr_t -> expr_t -> expr_t
val mk_poly_fold_tag': ?unique:bool -> string -> expr_t -> expr_t -> expr_t
val mk_poly_at: string -> expr_t -> expr_t -> expr_t -> expr_t
val mk_poly_at': ?unique:bool -> string -> expr_t
val mk_poly_at_with: string -> expr_t -> expr_t -> expr_t -> expr_t -> expr_t -> expr_t
val mk_poly_at_with': ?unique:bool -> string -> expr_t -> expr_t
val mk_poly_insert: ?path:int list -> string -> id_t -> expr_t list -> expr_t
val mk_poly_insert_block: ?path:int list -> string -> id_t -> expr_t list -> expr_t
val mk_poly_tag_at: expr_t -> expr_t -> expr_t
val mk_poly_skip: string -> expr_t -> expr_t -> expr_t -> expr_t
val mk_poly_skip': ?unique:bool -> string -> expr_t
val mk_poly_skip_block: ?unique:bool -> string -> expr_t list -> expr_t
val mk_poly_skip_all: string -> expr_t -> expr_t -> expr_t -> expr_t
val mk_poly_skip_all': ?unique:bool -> string -> expr_t
val mk_poly_skip_all_block: ?unique:bool -> string -> expr_t list -> expr_t
val mk_poly_unpack : expr_t -> expr_t
val mk_poly_reserve : ?path:int list -> string -> expr_t -> expr_t -> expr_t -> expr_t

(* macro similar to fst *)
val mk_fst: expr_t -> expr_t
val mk_fst': id_t -> expr_t

(* macro similar to snd *)
val mk_snd: expr_t -> expr_t
val mk_snd': id_t -> expr_t

val mk_thd: expr_t -> expr_t
val mk_thd': id_t -> expr_t

(* like fst, but for a collection with a tuple of any size *)
val project_from_col : type_t list -> expr_t -> choice:int -> expr_t

(* macro similar to fst but for a collection *)
val mk_fst_many: type_t list -> expr_t -> expr_t

(* macro similar to snd but for a collection *)
val mk_snd_many: type_t list -> expr_t -> expr_t

(* given an integer and a prefix, create an id for the internals of a tuple *)
val int_to_temp_id: string -> int -> id_t

(* turn a list of types to a list of made up ids and corresponding types *)
val types_to_ids_types : ?first:int -> string -> type_t list -> (string * type_t) list

(* destruct a tuple, at which point parts are available via ids made by
 * int_to_temp_id *)
val mk_destruct_tuple: id_t -> type_t list -> string -> expr_t -> expr_t

(* Create K3 code to rebuild a tuple
 * A lambda determines how to shuffle the ids of the tuple, or insert external ids
 * *)
val mk_rebuild_tuple: ?prefix:id_t -> id_t -> type_t list -> (id_t list -> id_t list) -> expr_t

(* id function for maps. No need fill in a tuple creation *)
val mk_id: type_t list -> expr_t

(* Convert between k3 representation and ocaml representation of containers *)
val list_of_k3_container : expr_t -> expr_t list
val k3_container_of_list : type_t -> expr_t list -> expr_t

(* convert arg to value type *)
val type_of_arg: arg_t -> type_t list

(* convert the type of a collection *)
val mk_convert_col : type_t -> type_t -> expr_t -> expr_t

val mk_convert_col_dst : type_t -> container_type_t -> expr_t -> expr_t
val mk_convert_col' : type_t -> container_type_t -> expr_t -> expr_t

val mk_peek_or_zero : ?zero:expr_t -> expr_t -> expr_t

val mk_peek_or_error : string -> expr_t -> expr_t

val mk_lookup : expr_t -> expr_t list -> expr_t
val mk_lookup' : id_t -> expr_t list -> expr_t

val default_value_of_t : type_t -> expr_t

(* data structure record to standardize manipulation *)
type data_struct = { id: string;
                     e: (string * type_t) list;
                     ee: (string * type_t) list list;
                     t: type_t;
                     init: expr_t option;
                     (* init that isn't used right away *)
                     d_init: expr_t option;
                     map_id: int option;
                     global: bool;
                     vid: bool;
                   }

val create_ds : ?e:(string * type_t) list -> ?ee:(string * type_t) list list -> ?init:expr_t -> ?d_init:expr_t -> ?map_id:int -> ?global:bool -> ?vid:bool -> string -> type_t -> data_struct

val decl_global : data_struct -> declaration_t * annotation_t
val delayed_init : data_struct -> expr_t

(* add to id,type list *)
val id_t_add : string -> (string * 'a) list -> (string * 'a) list

(* modify values of id_t format and convert any remaining values to vars *)
val modify_e : (string * 'a) list -> (string * expr_t) list -> expr_t list

(* find the index of a member of an id_t *)
val index_e : (string * 'a) list -> string -> int

(* easy access to unit argument for functions/triggers *)
val unit_arg : (string * type_t) list
val unknown_arg : (string * type_t) list

(* easy to use error function macro *)
val mk_error : string -> expr_t
val mk_error' : expr_t -> expr_t

val mk_size_slow : data_struct -> expr_t

val mk_min_max : string -> expr_t -> type_t -> (expr_t -> expr_t -> expr_t) -> expr_t -> (id_t * type_t) list-> id_t -> expr_t

val mk_pop_sim : ?cond:expr_t -> string -> string -> expr_t -> expr_t -> expr_t

val mk_incr : ?n:expr_t -> string -> expr_t
val mk_decr : ?n:expr_t -> string -> expr_t

(* delete using a slice with unknowns *)
val mk_delete_one : data_struct -> expr_t list -> expr_t

val mk_upsert_with_sim : data_struct -> id_t -> k:expr_t list -> default:expr_t -> v:expr_t -> expr_t

val mk_delete_with_cond : data_struct -> id_t -> k:expr_t list -> delcond:expr_t -> v:expr_t -> expr_t

val mk_counter : id_t -> data_struct

val mk_bool_ds : ?init:expr_t -> id_t -> data_struct

val mk_barrier :
  ?args:(id_t * type_t) list ->
  ?pre:expr_t list ->
  ?reusable:bool ->
  id_t ->
  ctr:id_t ->
  total:expr_t ->
  after:expr_t ->
  flow_statement_t * annotation_t

val mk_id_fn : data_struct -> expr_t
val mk_id_fn' : type_t list -> expr_t

val mk_case_tup_sn : expr_t -> id_t -> expr_t -> expr_t -> expr_t
val mk_case_tup_ns : expr_t -> id_t -> expr_t -> expr_t -> expr_t
val mk_tup_just : expr_t -> expr_t
val mk_tup_nothing : type_t -> expr_t
val mk_is_tup_nothing : expr_t -> expr_t

(* insert and var block, for usual insertion in a lambda *)
(* @tuple: return a tuple starting with this expr list *)
val mk_insert_block : ?path:int list -> id_t -> expr_t list -> expr_t
val mk_insert_at_block : ?path:int list -> id_t -> expr_t -> expr_t list -> expr_t
val mk_set_all : ?path:int list -> id_t -> expr_t list -> expr_t
val mk_set_all_block : ?path:int list -> id_t -> expr_t list -> expr_t
val mk_extend_block : ?path:int list -> id_t -> expr_t -> expr_t
val mk_upsert_with_block : ?path:int list -> id_t -> expr_t list -> expr_t -> expr_t -> expr_t
val mk_update_at_with_block : ?path:int list -> id_t -> expr_t -> expr_t -> expr_t

(* easy filtering over vectors *)
val mk_filter_cnt : expr_t -> data_struct -> expr_t

(* loop over bitmaps as in route and shuffle *)
(* @all: all values (even false) or just trues *)
val mk_iter_bitmap : ?idx:string -> expr_t -> expr_t -> expr_t
val mk_iter_bitmap' : ?idx:string -> expr_t -> id_t -> expr_t
val mk_agg_bitmap : ?idx:string -> ?move:bool -> (id_t * type_t) list -> expr_t -> expr_t -> expr_t -> expr_t
val mk_agg_bitmap' : ?idx:string -> ?move:bool -> (id_t * type_t) list -> expr_t -> expr_t -> id_t -> expr_t

val mk_check_tag : int -> expr_t -> expr_t -> expr_t -> expr_t -> expr_t
val mk_check_tag' : ?unique:bool -> int -> expr_t -> expr_t
val mk_if_tag : int -> expr_t -> expr_t -> expr_t -> expr_t -> expr_t -> expr_t
val mk_if_tag' : ?unique:bool -> int -> expr_t -> expr_t -> expr_t
val mk_if_poly_end_ny : ?unique:bool -> expr_t -> expr_t -> expr_t

val mk_concat : expr_t -> expr_t -> expr_t
val mk_print : expr_t -> expr_t
val mk_mod : expr_t -> expr_t -> expr_t
val mk_divi : expr_t -> expr_t -> expr_t
val mk_divf : expr_t -> expr_t -> expr_t
val mk_soi: expr_t -> expr_t
