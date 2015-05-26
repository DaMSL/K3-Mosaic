(* Distributed processing stage of K3 *)
open Util
open K3.AST
open ProgInfo
open K3Route

val gen_dist : ?use_multiindex:bool ->
               ?enable_gc:bool ->
               ?stream_file:string ->
               ?gen_deletes:bool ->
               ?gen_correctives:bool ->
               agenda_map:K3Dist.mapping_t ->
               prog_data_t ->
               part_map_t ->
               program_t ->
               program_t

val sw_demux_nm : id_t
