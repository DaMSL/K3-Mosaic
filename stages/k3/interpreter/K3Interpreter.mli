open Tree
open K3
open K3Typechecker
open K3Values

type eval_t = VDeclared of value_t ref | VTemp of value_t

val string_of_value: value_t -> string

val value_of_eval : eval_t -> value_t

val env_of_program : int tprogram_t -> program_env_t

val eval_expr : env_t -> int texpr_t -> env_t * eval_t

val eval_program : address -> int tprogram_t -> unit

val eval_networked_program : address list -> int tprogram_t -> unit