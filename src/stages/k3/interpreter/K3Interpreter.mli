open K3.AST
open K3Values

val value_of_eval : eval_t -> value_t

val env_of_program : program_t -> program_env_t

val eval_expr : env_t -> expr_t -> env_t * eval_t

val eval_program : address -> id_t option -> program_t -> program_env_t

val eval_networked_program : (address * id_t option) list -> program_t -> (address * program_env_t) list

(* a comprehensive function that starts either a networked or single-site
 * interpreter based on the number of peers
 * Takes run_length -> peer_list -> node_address -> role -> program and returns
 * the complete environment *)
val interpret_k3_program : int64 -> (address * id_t option) list -> address ->
  id_t option -> program_t -> (address * program_env_t) list

