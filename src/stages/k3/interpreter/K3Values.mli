open Util
open K3.AST

exception RuntimeError of int * string

(* Interpreter representation of values *)

module IdMap : Map.S with type key = id_t

val add_from_list : 'a IdMap.t -> (id_t * 'a) list -> 'a IdMap.t

val map_modify : ('a option -> 'a option) -> id_t -> 'a IdMap.t -> 'a IdMap.t

module rec ValueMap : sig include NearMap.S with type key = Value.value_t end

and ValueMMap : sig include IMultimap.S with type elt = Value.value_t end

and ValueBag : sig include IBag.S with type elt = Value.value_t end

and Value : sig
  type eval_t = VDeclared of value_t ref | VTemp of value_t
  and foreign_func_t = env_t -> env_t * eval_t
  (* arguments to a function/trigger *)
  (* an env_t is global values and frames (functional environment) *)
  and local_env_t = value_t list IdMap.t
  and global_env_t = (value_t ref) IdMap.t
  and env_t = global_env_t * local_env_t
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
      | VSet of value_t ISet.t
      | VBag of ValueBag.t
      | VList of value_t IList.t
      | VMap of value_t ValueMap.t
      | VMultimap of ValueMMap.t
      | VFunction of arg_t * local_env_t * expr_t (* closure *)
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

(* trigger env is where we store the trigger functions. These functions take the
 * address,
 * scheduler_state (parametrized here to prevent circular inclusion), the
 * environment, value_t of arguments, and produce unit *)
type trigger_env_t = (address -> env_t -> value_t -> unit) IdMap.t

type program_env_t = trigger_env_t * env_t

(* Value comparison *)
val equal_values : value_t -> value_t -> bool
val compare_values : (int -> int -> bool) -> value_t -> value_t -> bool

(* Find inequalities and put their locations in a list *)
val find_inequality : value_t -> value_t -> int option

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
val type_of_value : int -> value_t -> type_t
val expr_of_value : int -> value_t -> expr_t

(* Universal collection functions *)
type 'a t_err_fn = (string -> string -> 'a)
val v_peek : value_t option t_err_fn -> value_t -> value_t option
val v_combine : value_t t_err_fn -> value_t -> value_t -> value_t
val v_fold : 'a t_err_fn -> ('a -> value_t -> 'a) -> 'a -> value_t -> 'a
val v_iter : unit t_err_fn -> (value_t -> unit) -> value_t -> unit
val v_insert : value_t t_err_fn -> value_t -> value_t -> value_t
val v_delete : value_t t_err_fn -> value_t -> value_t -> value_t
val v_update : value_t t_err_fn -> value_t -> value_t -> value_t -> value_t
val v_empty : value_t t_err_fn -> ?no_multimap : bool -> value_t -> value_t
val v_empty_of_t : container_type_t -> value_t
val v_sort : value_t t_err_fn -> (value_t -> value_t -> int) -> value_t -> value_t
val v_singleton : value_t t_err_fn -> value_t -> container_type_t -> value_t
val v_slice : value_t t_err_fn -> value_t -> value_t -> value_t
val v_slice_idx : value_t t_err_fn -> index_t -> comp_t -> value_t -> value_t -> value_t
