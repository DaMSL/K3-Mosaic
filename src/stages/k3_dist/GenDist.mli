(* Distributed processing stage of K3 *)
open K3.AST
open ProgInfo
open K3Route

exception ProcessingFailed of string
val gen_dist : prog_data_t -> part_map_t -> program_t -> program_t
