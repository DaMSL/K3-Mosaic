(* AST modification of K3 AST to K3Dist AST *)
open Util
open K3.AST
open ProgInfo
open K3Dist

exception InvalidAst of string

(* Scan an ast for map access patterns *)
val get_map_access_patterns_ids : prog_data_t -> program_t -> IndexSet.t IntMap.t

(* Get all loader tables and load paths in the ast *)
val loader_tables : program_t -> (id_t * string) list

(* Modify an AST for having version ids, sending the delta to a trigger etc *)
val modify_ast_for_s : config -> program_t -> stmt_id_t ->
  trig_name_t -> (expr_t -> expr_t) -> expr_t

(* Modify a corrective AST for having version ids. Returns arguments to use for
 * calling the AST *)
val modify_corr_ast : config -> program_t ->
  map_id_t -> stmt_id_t -> trig_name_t -> (expr_t -> expr_t) ->
    (id_t * type_t) list * expr_t

(* return global ast for map declarations, adding the vid *)
val map_inits_from_ast : config -> program_t -> expr_t IntMap.t
