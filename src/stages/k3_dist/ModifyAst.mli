(* AST modification of K3 AST to K3Dist AST *)
open Util
open K3.AST
open ProgInfo
open K3Dist

exception InvalidAst of string
exception UnhandledModification of string

(* Scan an ast for map access patterns *)
val get_map_access_patterns_ids : config -> program_t -> IndexSet.t IntMap.t 

(* Modify an AST for having version ids, sending the delta to a trigger etc *)
val modify_ast_for_s : config -> program_t -> stmt_id_t ->
  trig_name_t -> trig_name_t option -> expr_t

(* Modify a corrective AST for having version ids. Returns arguments to use for
 * calling the AST *)
val modify_corr_ast : config -> program_t ->
  map_id_t -> stmt_id_t -> trig_name_t -> trig_name_t option ->
    (id_t * value_type_t) list * expr_t

(* return global ast for map declarations, adding the vid *)
val map_inits_from_ast : config -> program_t -> expr_t IntMap.t
