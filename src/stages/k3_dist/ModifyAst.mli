(* AST modification of K3 AST to K3Dist AST *)
open K3.AST
open ProgInfo

exception InvalidAst of string
exception UnhandledModification of string

(* Modify an AST for having version ids, sending the delta to a trigger etc *)
val modify_ast_for_s : prog_data_t -> program_t -> stmt_id_t ->
  trig_name_t -> trig_name_t option -> expr_t

(* Modify a corrective AST for having version ids. Returns arguments to use for
 * calling the AST *)
val modify_corr_ast : prog_data_t -> program_t ->
  map_id_t -> stmt_id_t -> trig_name_t -> trig_name_t option ->
    (id_t * value_type_t) list * expr_t

(* return global ast for map declarations, adding the vid *)
val modify_map_decl_ast : prog_data_t -> program_t -> program_t

(* gt the computation for adding the delta in a statement *)
val delta_add_of_stmt : prog_data_t -> program_t -> stmt_id_t ->
  id_t -> mod_delta_add:bool -> rename_map:((id_t * id_t) option) -> expr_t

