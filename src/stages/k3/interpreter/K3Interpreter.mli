open K3.AST
open K3Values

val value_of_eval : eval_t -> value_t

val env_of_program : program_t -> program_env_t

val eval_expr : env_t -> expr_t -> env_t * eval_t

val eval_program : address -> id_t option -> program_t -> program_env_t

val eval_networked_program : (address * id_t option) list -> program_t -> (address * program_env_t) list
