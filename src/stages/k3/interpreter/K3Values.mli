open K3.AST

exception RuntimeError of int * string

(* Interpreter representation of values *)

module IdMap : Map.S with type key = id_t

val add_from_list : 'a IdMap.t -> (id_t * 'a) list -> 'a IdMap.t

val map_modify : ('a option -> 'a option) -> id_t -> 'a IdMap.t -> 'a IdMap.t

module rec ValueMap : sig include NearMap.S with type key = Value.value_t end

and ValueMMap : sig include IMultimap.S with type elt = Value.value_t end

and Value : sig
  type eval_t = VDeclared of value_t ref | VTemp of value_t
  and foreign_func_t = env_t -> env_t * eval_t
  (* arguments to a function/trigger *)
  and frame_t = (id_t * value_t) list
  (* an env_t is global values and frames (functional environment) *)
  and env_t = (value_t ref) IdMap.t * (frame_t list)
  and bag_t = value_t list
  and vindex_t = index_t * value_t ValueMap.t
  and value_t
    = VUnknown
    | VUnit
    | VBool of bool
    | VInt of int
    | VFloat of float
    | VByte of char
    | VString of string
    | VTuple of value_t list
    | VOption of value_t option
    | VSet of value_t list
    | VBag of value_t list
    | VList of value_t list
    | VMap of value_t ValueMap.t
    | VMultimap of ValueMMap.t
    | VFunction of arg_t * expr_t
    | VForeignFunction of arg_t * foreign_func_t
    | VAddress of address
    | VTarget of id_t
    | VIndirect of value_t ref
end

open Value

(* trigger env is where we store the trigger functions. These functions take the
 * address,
 * scheduler_state (parametrized here to prevent circular inclusion), the
 * environment, value_t of arguments, and produce unit *)
type trigger_env_t = (address -> env_t -> value_t -> unit) IdMap.t

type program_env_t = trigger_env_t * env_t

(* Value comparison *)
val equal_values : ?neq:bool -> value_t -> value_t -> bool

(* Find inequalities and put their locations in a list *)
val find_inequality : value_t -> value_t -> int list

(* Value sorting for consistency *)
val sort_values : value_t -> value_t

(* Value stringification *)
val repr_of_value : value_t -> string
val string_of_value : ?mark_points:int list -> value_t -> string

(* Environment stringification *)
val print_env : bool -> env_t -> unit
val print_program_env : program_env_t -> unit

val string_of_env : ?skip_functions:bool -> env_t -> string
val string_of_program_env : program_env_t -> string

(* Conversion between values and other types *)
val value_of_const : constant_t -> value_t
val value_of_const_expr : expr_t -> value_t
val type_of_value : int -> value_t -> value_type_t
val expr_of_value : int -> value_t -> expr_t

(* Universal collection functions *)
type t_err_fn = (string -> string -> unit)
val v_peek : t_err_fn -> value_t -> value_t
val v_combine : t_err_fn -> value_t -> value_t -> value_t
val v_fold : t_err_fn -> ('a -> value_t -> 'a) -> value_t -> 'a
val v_iter : t_err_fn -> (value_t -> unit) -> value_t -> unit
val v_iter2 : t_err_fn -> (value_t -> value_t -> unit) -> value_t -> value_t -> unit
val v_insert : t_err_fn -> value_t -> value_t -> value_t
val v_delete : t_err_fn -> value_t -> value_t -> value_t
val v_update : t_err_fn -> value_t -> value_t -> value_t -> value_t
val v_empty : t_err_fn -> ?no_multimap : bool -> value_t -> value_t
val v_empty_of_t : container_type_t -> value_t
val v_sort : t_err_fn -> (value_t -> value_t -> value_t) -> value_t -> value_t
val v_singleton : t_err_fn -> value_t -> container_type_t -> value_t
val v_slice : t_err_fn -> value_t -> value_t -> value_t
val v_slice_idx : t_err_fn -> value_t -> value_t -> value_t -> value_t
