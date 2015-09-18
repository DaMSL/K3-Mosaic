(* AST modification of K3 AST to K3Dist AST *)
open Util
open K3.AST
open ProgInfo
open K3Dist

exception InvalidAst of string

val unused_trig_args : program_t -> StrSet.t StrMap.t
val string_of_unused_trig_args : StrSet.t StrMap.t -> string

(* Get all loader tables and load paths in the ast *)
val loader_tables : program_t -> (id_t * string) list

(* Modify an AST for having version ids, sending the delta to a trigger etc *)
val modify_ast : config -> program_t -> stmt_id_t -> trig_name_t -> bool * expr_t

(* Modify a corrective AST for having version ids. Returns arguments to use for
 * calling the AST *)
val modify_corr_ast : config -> program_t ->
  map_id_t -> stmt_id_t -> trig_name_t -> (id_t * type_t) list * bool * expr_t

(* return global ast for map declarations, adding the vid *)
val map_inits_from_ast : config -> program_t -> expr_t IntMap.t
