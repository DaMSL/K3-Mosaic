(* Distributed processing stage of K3 *)

exception ProcessingFailed of string
val gen_dist : ProgInfo.prog_data_t -> 'a -> int K3.declaration_t list
