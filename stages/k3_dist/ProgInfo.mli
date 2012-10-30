(* Utility functions to get data out of the specialized K3 program data
 * structure *)

open K3.AST

exception Bad_data of string

(* Data structure describing the original K3 program *)
type stmt_id_t = int
type trig_id_t = int
type trig_name_t = string
type map_id_t = int
type map_var_binding_t = id_t * int
type stmt_data_t =
    stmt_id_t * trig_id_t * map_id_t * map_var_binding_t list *
    (map_id_t * map_var_binding_t list) list
type trig_data_t =
    trig_id_t * string * (id_t * value_type_t) list * stmt_id_t list
type map_data_t = map_id_t * string * value_type_t list
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
val get_trig_list : prog_data_t -> trig_name_t list
val map_all_trigs : prog_data_t -> (trig_name_t -> 'a) -> 'a list
val get_stmt_list : prog_data_t -> stmt_id_t list
val get_map_list : prog_data_t -> map_id_t list
val find_trigger : prog_data_t -> trig_name_t -> trig_data_t
val find_map : prog_data_t -> map_id_t -> map_data_t
val find_stmt : prog_data_t -> stmt_id_t -> stmt_data_t
val trigger_id_for_name : prog_data_t -> trig_name_t -> trig_id_t
val trigger_name_for_id : prog_data_t -> trig_id_t -> trig_name_t 
val args_of_t : prog_data_t -> trig_name_t -> (id_t * value_type_t) list
val s_and_over_stmts_in_t :
  prog_data_t ->
  (prog_data_t -> stmt_id_t -> 'a list) -> trig_name_t -> (stmt_id_t * 'a) list
val stmt_has_rhs_map : prog_data_t -> stmt_id_t -> map_id_t -> bool
val stmts_without_rhs_maps_in_t : prog_data_t -> trig_name_t -> stmt_id_t list
val rhs_maps_of_stmt : prog_data_t -> stmt_id_t -> map_id_t list
val lhs_map_of_stmt : prog_data_t -> stmt_id_t -> map_id_t
val rhs_lhs_of_stmt : prog_data_t -> stmt_id_t -> (map_id_t * map_id_t) list
val find_lmap_bindings_in_stmt :
  prog_data_t -> stmt_id_t -> map_id_t -> map_var_binding_t list
val find_rmap_bindings_in_stmt :
  prog_data_t -> stmt_id_t -> map_id_t -> map_var_binding_t list
val find_map_bindings_in_stmt :
  prog_data_t -> stmt_id_t -> map_id_t -> map_var_binding_t list
val map_name_of : prog_data_t -> map_id_t -> string
val map_types_for : prog_data_t -> map_id_t -> value_type_t list

(* useful for adding vid to maps *)
val map_types_with_v_for : prog_data_t -> map_id_t -> value_type_t list
(* because we add the vid first, we need to modify the numbering of arguments in
* the key. It's easier to control this in one place *)
val adjust_key_id_for_v : int -> int
val stmts_of_t : prog_data_t -> trig_name_t -> stmt_id_t list
val trigger_of_stmt : prog_data_t -> stmt_id_t -> trig_name_t

(* returns a k3 list of maybes that has the relevant map pattern *)
val partial_key_from_bound : prog_data_t ->
  stmt_id_t -> map_id_t -> expr_t list

(* returns a k3 list of variables or CUnknown. Can't use same types as
 * partial_key *)
val slice_key_from_bound : prog_data_t ->
  stmt_id_t -> map_id_t -> expr_t list

(* return a binding pattern for a stmt of (left_index, right_index) list
 * showing how a lhs map variable corresponds to a rhs variable
 * starting at 0 index *)
val get_map_bindings_in_stmt : prog_data_t -> stmt_id_t -> map_id_t -> map_id_t
-> (int * int) list
