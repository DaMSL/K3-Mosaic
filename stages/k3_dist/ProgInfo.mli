exception Bad_data of string
type stmt_id_t = int
type trig_id_t = int
type map_id_t = int
type map_var_binding_t = K3.id_t * int
type stmt_data_t =
    stmt_id_t * trig_id_t * map_id_t * map_var_binding_t list *
    (map_id_t * map_var_binding_t list) list
type trig_data_t =
    trig_id_t * string * (K3.id_t * K3.value_type_t) list * stmt_id_t list
type map_data_t = map_id_t * string * (K3.id_t * K3.value_type_t) list
type prog_data_t = trig_data_t list * stmt_data_t list * map_data_t list
val get_trig_data : 'a * 'b * 'c -> 'a
val get_stmt_data : prog_data_t -> stmt_data_t list
val get_map_data : 'a * 'b * 'c -> 'c
val get_trig_list : ('a * 'b * 'c * 'd) list * 'e * 'f -> 'b list
val find_trigger : prog_data_t -> string -> trig_data_t
val find_map : prog_data_t -> map_id_t -> map_data_t
val find_stmt : prog_data_t -> stmt_id_t -> stmt_data_t
val trigger_id_for_name : prog_data_t -> string -> trig_id_t
val args_of_t : prog_data_t -> string -> (K3.id_t * K3.value_type_t) list
val over_stmts_in_t :
  prog_data_t ->
  (prog_data_t -> stmt_id_t -> 'a list) -> string -> (stmt_id_t * 'a) list
val read_maps_of_stmt : prog_data_t -> stmt_id_t -> map_id_t list
val stmt_has_rhs_map : prog_data_t -> stmt_id_t -> map_id_t -> bool
val stmts_without_rhs_maps_in_t : prog_data_t -> string -> stmt_id_t list
val lhs_map_of_stmt : prog_data_t -> stmt_id_t -> map_id_t
val lhs_rhs_of_stmt : prog_data_t -> stmt_id_t -> (map_id_t * map_id_t) list
val find_map_bindings_in_stmt :
  prog_data_t -> stmt_id_t -> map_id_t -> map_var_binding_t list
val map_name_of : prog_data_t -> map_id_t -> string
val stmts_of_trigger : prog_data_t -> string -> stmt_id_t list
val trigger_of_stmt : prog_data_t -> stmt_id_t -> trig_id_t
val map_types_for : prog_data_t -> map_id_t -> K3.value_type_t list
val partial_key_from_bound :
  prog_data_t ->
  stmt_id_t -> map_id_t -> ((int * K3.expr_tag_t) * int) Tree.tree_t list
