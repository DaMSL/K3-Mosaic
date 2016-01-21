(* Distributed processing stage of K3 *)
open Util
open K3.AST
open ProgInfo
open K3Route
open K3Dist

val gen_dist : 
               ?gen_deletes:bool ->
               ?gen_correctives:bool ->
               ?gen_single_vid:bool ->
               ?use_opt_route:bool ->
               stream_file:string ->
               map_type:map_type ->
               agenda_map:K3Dist.mapping_t ->
               prog_data_t ->
               program_t ->
               program_t ->
               program_t * program_t

val sw_demux_nm : id_t
