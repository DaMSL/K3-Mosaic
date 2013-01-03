(* AST modification of K3 AST to K3Dist AST *)
open K3.AST
open ProgInfo

exception InvalidAst of string
exception UnhandledModification of string

(* Modify an AST for having version ids, sending the delta to a trigger etc *)
val modify_ast_for_s : ProgInfo.prog_data_t -> K3.AST.program_t -> ProgInfo.stmt_id_t ->
  ProgInfo.trig_name_t -> ProgInfo.trig_name_t option -> K3.AST.expr_t

(* Modify a corrective AST for having version ids. Returns arguments to use for
 * calling the AST *)
val modify_corr_ast : ProgInfo.prog_data_t -> K3.AST.program_t ->
  ProgInfo.map_id_t -> ProgInfo.stmt_id_t -> ProgInfo.trig_name_t ->
    (K3.AST.id_t * K3.AST.value_type_t) list * K3.AST.expr_t

