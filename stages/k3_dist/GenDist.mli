(* Distributed processing stage of K3 *)
open K3.AST

exception ProcessingFailed of string
val gen_dist : ProgInfo.prog_data_t -> 'a -> declaration_t list
