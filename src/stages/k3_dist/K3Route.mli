(* Routing functions *)
open K3.AST
open ProgInfo

type part_map_t = (id_t * (int * int) list) list

val route_for : prog_data_t -> map_id_t -> id_t
val list_of_k3_partition_map : (declaration_t * 'a) list -> part_map_t
val k3_partition_map_of_list : prog_data_t -> part_map_t -> expr_t

exception NoHashFunction of base_type_t

val functions : prog_data_t -> part_map_t ->
  (declaration_t * annotation_t) list
val global_vars : prog_data_t -> part_map_t ->
  (declaration_t * annotation_t) list

