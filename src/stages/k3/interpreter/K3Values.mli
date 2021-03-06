open Util
open K3.AST

exception RuntimeError of int * string * string

(* Interpreter representation of values *)

module IdMap : Map.S with type key = id_t

val add_from_list : 'a IdMap.t -> (id_t * 'a) list -> 'a IdMap.t

val map_modify : ('a option -> 'a option) -> id_t -> 'a IdMap.t -> 'a IdMap.t

module rec ValueMap : sig include NearMap.S with type key = Value.value_t end

and ValueVMap : sig include IVMap.S with type key = Value.value_t
                                    and  type vid = Value.value_t end

and ValueBag : sig include IBag.S with type elt = Value.value_t end

and ValueSet : sig include ISet.S with type elt = Value.value_t end

and ValueSSet : sig include NearSet.S with type elt = Value.value_t end

and Value : sig
  type eval_t = VDeclared of value_t ref | VTemp of value_t
  and foreign_func_t = env_t -> env_t * eval_t
  (* arguments to a function/trigger *)
  (* an env_t is global values and frames (functional environment) *)
  and local_env_t = value_t list IdMap.t
  and global_env_t = (value_t ref) IdMap.t
  (* trigger env is where we store the trigger functions. These functions take the
  * address,
  * scheduler_state (parametrized here to prevent circular inclusion), the
  * environment, value_t of arguments, and produce unit *)
  and trigger_env_t = (address -> env_t -> value_t list -> unit) IdMap.t
  and env_t = {
        triggers:trigger_env_t;
        globals:global_env_t;
        locals:local_env_t;
        accessed:StrSet.t ref;
        type_aliases:(id_t, type_t) Hashtbl.t;
        stack:id_t list;
      }
  and fun_typ = FLambda | FGlobal of id_t | FTrigger of id_t
  and poly_inner = {
    data: (value_t * string * value_t) IntMap.t;
              (* itag, stag, value *)
    tags: poly_tags;
    sets: ValueSet.t IntMap.t; (* tag -> set of values for checking uniqueness *)
    unique: bool;
  }
  and value_t
      = VMax
      | VMin
      | VUnknown
      | VUnit
      | VBool of bool
      | VInt of int
      | VFloat of float
      | VByte of char
      | VString of string
      | VTuple of value_t list
      | VOption of value_t option
      | VSet of ValueSet.t
      | VBag of ValueBag.t
      | VList of value_t IList.t
      | VVector of value_t IntMap.t * int * value_t
      | VMap of value_t ValueMap.t
      | VSortedMap of value_t ValueMap.t
      | VSortedSet of ValueSSet.t
      | VVMap of value_t ValueVMap.t
      | VPolyQueue of poly_inner
      | VFunction of fun_typ * arg_t * local_env_t * expr_t (* closure *)
      | VForeignFunction of id_t * arg_t * foreign_func_t
      | VAddress of address
      | VTarget of id_t
      | VIndirect of value_t ref
end

and ValueComp : sig val compare_v : Value.value_t -> Value.value_t -> int
                     val hash : Value.value_t -> int
                     val reset_counter : unit -> unit
                     val get_counter : unit -> int
                 end

open Value

val default_env : env_t

(* if we have 2 collection values that match *)
val matching_collections : value_t -> value_t -> bool

(* Value comparison *)
val equal_values : value_t -> value_t -> bool
val compare_values : (int -> int -> bool) -> value_t -> value_t -> bool

(* Find inequalities and put their locations in a list *)
val find_inequality : value_t -> value_t -> int option

val unwrap_vtuple : value_t -> value_t list
val wrap_vtuple : value_t list -> value_t

(* Value stringification *)
val repr_of_value : value_t -> string
val string_of_value : ?mark_points:int list -> value_t -> string

(* functions to drop out of logs *)
val drop_fn_prefixes : string list

(* Environment stringification *)
val print_env : ?skip_functions:bool ->
                ?skip_empty:bool ->
                ?accessed_only:bool -> env_t -> unit

val string_of_env : ?skip_functions:bool ->
                    ?skip_empty:bool ->
                    ?accessed_only:bool -> env_t -> string

(* Conversion between values and other types *)
val value_of_const : constant_t -> value_t
val value_of_const_expr : expr_t -> value_t
val type_of_value : int -> value_t -> type_t
(* val expr_of_value : int -> value_t -> expr_t *)
val is_vmap : value_t -> bool
val strip_vid : value_t -> value_t

(* Universal collection functions *)
type 'a t_err_fn = (string -> string -> 'a)
val v_peek : ?vid:bool -> value_t option t_err_fn -> value_t -> value_t option
val v_combine : value_t t_err_fn -> value_t -> value_t -> value_t
val v_fold : 'a t_err_fn -> ('a -> value_t -> 'a) -> 'a -> value_t -> 'a
val v_fold_v : value_t -> 'a t_err_fn -> ('a -> value_t -> 'a) -> 'a -> value_t -> 'a
val v_fold_all : 'a t_err_fn -> ('a -> value_t -> value_t -> 'a) -> 'a -> value_t -> 'a
val v_fold_poly : 'a t_err_fn -> (int -> value_t * string * value_t -> 'a -> 'a) -> 'a -> value_t -> 'a
val v_traverse_poly : (int * 'a) t_err_fn -> (int -> value_t * string * value_t -> 'a -> int * 'a) -> int -> 'a -> value_t -> int * 'a
val v_fold_poly_tag : 'a t_err_fn -> value_t -> string -> (int -> value_t * string * value_t -> 'a -> 'a) -> 'a -> value_t -> 'a
val v_set_all : value_t t_err_fn -> value_t -> value_t -> value_t
val v_iter : unit t_err_fn -> (value_t -> unit) -> value_t -> unit
val v_insert : ?vidkey:value_t -> ?tag:string -> value_t t_err_fn -> value_t -> value_t -> value_t
val v_insert_at : value_t t_err_fn -> value_t -> value_t -> value_t -> value_t
val v_delete : value_t t_err_fn -> value_t -> value_t -> value_t
val v_delete_all_prefix : value_t t_err_fn -> value_t -> value_t -> value_t
val v_delete_prefix : value_t t_err_fn -> value_t -> value_t -> value_t
val v_pop : value_t t_err_fn -> value_t -> value_t
val v_update : ?vidkey:value_t -> value_t t_err_fn -> value_t -> value_t -> value_t -> value_t
val v_update_suffix : value_t t_err_fn -> value_t -> (value_t -> value_t) -> value_t -> value_t
val v_empty : value_t t_err_fn -> value_t -> value_t
(* container, elem type *)
val v_col_of_t : ?elem:((id_t, type_t) Hashtbl.t * type_t) -> container_type_t -> value_t
val v_of_t : (id_t, type_t) Hashtbl.t -> ?id:string -> type_t -> value_t
val v_sort : value_t t_err_fn -> (value_t -> value_t -> int) -> value_t -> value_t
val v_size : value_t t_err_fn -> value_t -> value_t
val v_singleton : value_t t_err_fn -> (id_t, type_t) Hashtbl.t -> value_t -> container_type_t -> type_t -> value_t
val v_slice : value_t t_err_fn -> value_t -> value_t -> value_t
val v_slice_op : [`LT|`LEQ|`GT|`GEQ|`EQ] -> value_t t_err_fn -> value_t -> value_t -> value_t
val v_filter_op : value_t t_err_fn -> [`LT|`LEQ|`GT|`GEQ|`EQ] -> value_t -> value_t -> value_t
val v_at : ?extend:bool -> ?tag:string -> ?get_stag:bool -> ?get_itag:bool -> value_t option t_err_fn -> value_t -> value_t -> value_t option
val v_min : value_t option t_err_fn -> value_t -> value_t option
val v_is_empty : value_t t_err_fn -> value_t -> value_t
val v_mem : value_t t_err_fn -> value_t -> value_t -> value_t
