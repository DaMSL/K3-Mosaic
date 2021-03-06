(* Utility functions to get data out of the specialized K3 program data
 * structure *)

open Util
open K3.AST
open K3Helpers

exception Bad_data of string

(* Data structure describing the original K3 program *)
type stmt_id_t = int
type trig_id_t = int
type trig_name_t = string
type map_id_t = int
type map_var_binding_t = id_t * int
type stmt_data_t
  (* stmt_id, trig_id, lhs_map, lhs_map_binding, rhs_maps_with_bindings, update/replace *)
  = {stmt: stmt_id_t;
     trig: trig_id_t;
     lmap: map_id_t;
     lmap_binds: map_var_binding_t list;
     rmap_binds: (map_id_t * map_var_binding_t list) list;
     is_update: bool
    }

type trig_data_t
  (* trig_id, name, bound_args, stmts *)
  = {trig: trig_id_t;
     trig_nm:string;
     args: (id_t * type_t) list;
     stmts: stmt_id_t list;
    }

  (* map_id, map_name, parameters *)
type map_data_t =
  {map: map_id_t;
   map_nm: string;
   map_types: type_t list;
  }
type prog_data_t = trig_data_t list * stmt_data_t list * map_data_t list

(* Stringification *)
val string_of_binding: map_var_binding_t -> string
val string_of_bindings: map_var_binding_t list -> string
val string_of_map: (map_id_t * map_var_binding_t list) -> string

val string_of_map_data: map_data_t -> string
val string_of_stmt_data: stmt_data_t -> string
val string_of_trig_data: trig_data_t -> string
val string_of_prog_data: prog_data_t -> string

(* Utility functions using this data structure *)
val is_delete_t : trig_name_t -> bool
val is_insert_t : trig_name_t -> bool
val remove_trig_prefix : trig_name_t -> trig_name_t
val get_trig_list : ?corrective:bool -> ?sys_init:bool -> ?delete:bool -> prog_data_t -> trig_name_t list
val for_all_trigs : ?corrective:bool -> ?sys_init:bool -> ?delete:bool -> prog_data_t -> (trig_name_t -> 'a) -> 'a list
val get_stmt_list : prog_data_t -> stmt_id_t list
val get_trig_stmt_list : ?corrective:bool -> ?sys_init:bool -> ?delete:bool -> prog_data_t -> (trig_name_t * stmt_id_t) list
val get_max_stmt : prog_data_t -> stmt_id_t
val for_all_stmts : prog_data_t -> (stmt_id_t -> 'a) -> 'a list
val get_map_list : prog_data_t -> map_id_t list
val get_maps_with_keys : prog_data_t -> map_id_t list
val for_all_maps : prog_data_t -> (map_id_t -> 'a) -> 'a list
val for_all_rhs_lhs_maps : prog_data_t -> (map_id_t * map_id_t -> 'a) -> 'a list
val find_trigger : prog_data_t -> trig_name_t -> trig_data_t
val find_map : prog_data_t -> map_id_t -> map_data_t
val find_map_by_name : prog_data_t -> string -> map_data_t
val find_stmt : prog_data_t -> stmt_id_t -> stmt_data_t
val trigger_id_for_name : prog_data_t -> trig_name_t -> trig_id_t
val trigger_name_for_id : prog_data_t -> trig_id_t -> trig_name_t
val args_of_t : prog_data_t -> trig_name_t -> (id_t * type_t) list
val s_and_over_stmts_in_t :
  prog_data_t ->
  (prog_data_t -> stmt_id_t -> 'a list) -> trig_name_t -> (stmt_id_t * 'a) list
val stmt_has_rhs_map : prog_data_t -> stmt_id_t -> map_id_t -> bool
val stmts_with_rhs_maps_in_t : prog_data_t -> trig_name_t -> stmt_id_t list
val stmts_without_rhs_maps_in_t : prog_data_t -> trig_name_t -> stmt_id_t list
val rhs_maps_of_stmt : prog_data_t -> stmt_id_t -> map_id_t list
val stmts_rhs_maps : prog_data_t -> (stmt_id_t * map_id_t) list
val stmts_lhs_maps : prog_data_t -> (stmt_id_t * map_id_t) list
val for_all_stmts_rhs_maps : prog_data_t -> (stmt_id_t * map_id_t -> 'a) -> 'a list
val lhs_map_of_stmt : prog_data_t -> stmt_id_t -> map_id_t
val rhs_lhs_of_stmt : prog_data_t -> stmt_id_t -> (map_id_t * map_id_t) list
val find_lmap_bindings_in_stmt :
  prog_data_t -> stmt_id_t -> map_id_t -> map_var_binding_t list
val find_rmap_bindings_in_stmt :
  prog_data_t -> stmt_id_t -> map_id_t -> map_var_binding_t list
val find_map_bindings_in_stmt :
  prog_data_t -> stmt_id_t -> map_id_t -> map_var_binding_t list
val map_name_of : prog_data_t -> map_id_t -> string
val map_id_of_name : prog_data_t -> string -> map_id_t
(* change a map name to a buffer map name *)
val buf_of_stmt_map : int -> string -> string
val buf_of_stmt_map_id: prog_data_t -> int -> map_id_t -> string
val map_types_for : prog_data_t -> map_id_t -> type_t list
val map_types_no_val_for : prog_data_t -> map_id_t -> type_t list

(* add a vid to the types *)
val map_types_add_v : type_t list -> type_t list

(* map types including the vid *)
val map_types_with_v_for : prog_data_t -> map_id_t -> type_t list

(* get map names with map ids *)
val map_names_ids_of_stmt : prog_data_t -> stmt_id_t -> (string * map_id_t) list

(* construct a representation of ids and types for a map *)
val map_ids_types_for : ?prefix:string -> prog_data_t -> map_id_t ->
    (string * type_t) list

(* get ids and types with no value *)
val map_ids_types_no_val_for : ?prefix:string -> prog_data_t -> map_id_t ->
    (string * type_t) list

(* add a vid of any kind (var, whatever) to a list. Allows consistent placement
 *)
val map_add_v : 'a -> 'a list -> 'a list

(* add a vid to map ids. This allows consistent placement of vids in the
 * strucutres *)
val map_ids_add_v : ?vid:string -> string list -> string list

(*add a vid to the ids,types*)
val map_ids_types_add_v : ?vid:string -> (string * type_t) list ->
    (string * type_t) list

(* construct a list of ids, types including the vid *)
val map_ids_types_with_v_for : ?prefix:string -> ?vid:string -> prog_data_t ->
    map_id_t -> (string * type_t) list

(* because we add the vid first, we need to modify the numbering of arguments in
* the key. It's easier to control this in one place *)
val adjust_key_id_for_v : int -> int
val stmts_of_t : prog_data_t -> trig_name_t -> stmt_id_t list
val trigger_of_stmt : prog_data_t -> stmt_id_t -> trig_name_t

val var_list_from_bound : ?rmap_id:map_id_t ->
  prog_data_t -> stmt_id_t -> map_id_t -> (K3.AST.id_t * K3.AST.type_t) list

(* returns a k3 list of maybes that has the relevant map pattern, as well as a route pat_idx *)
val key_pat_from_bound : prog_data_t -> (map_id_t, int IntSetMap.t) Hashtbl.t -> stmt_id_t -> map_id_t -> (expr_t list * int)

val get_shuffle_pat_idx : prog_data_t -> (map_id_t, int IntSetMap.t) Hashtbl.t -> stmt_id_t -> map_id_t -> map_id_t -> int

(* returns a k3 list of variables or CUnknown. Can't use same types as partial_key *)
val slice_key_from_bound : prog_data_t -> stmt_id_t -> map_id_t -> expr_t list

(* return a binding pattern for a stmt of (left_index, right_index) list
 * showing how a lhs map variable corresponds to a rhs variable
 * starting at 0 index *)
val get_map_bindings_in_stmt : prog_data_t -> stmt_id_t -> map_id_t -> map_id_t
-> int IntMap.t

(* whether we have a complex, interlocking loop pattern in this stmt *)
val stmt_many_loop_vars : prog_data_t -> stmt_id_t -> IntSet.t option

type freevar_info = {
                      lmap_free: map_id_t * (id_t * int) list;
                      lmap_bound: map_id_t * (id_t * int) list;
                      rmaps_free: (map_id_t * (id_t * int) list) list;
                      rmaps_bound: (map_id_t * (id_t * int) list) list;
                    }

val free_bound_vars : prog_data_t -> stmt_id_t -> freevar_info

val is_opt_route_stmt : ?info:freevar_info -> prog_data_t -> stmt_id_t -> bool

(* dump info about the program *)
val dump_info : prog_data_t -> string

val nonempty_rmaps_of_stmt : prog_data_t -> stmt_id_t -> map_id_t list

(* get the max number of stmts per trigger in the program *)
val max_stmts_per_trig : prog_data_t -> int

val stmt_map_ids : prog_data_t -> (int * (stmt_id_t * map_id_t)) list
