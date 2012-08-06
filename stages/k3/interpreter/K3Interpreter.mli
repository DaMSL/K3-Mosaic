open Tree
open K3
open K3Annotations
open K3Typechecker
open K3Values

type eval_t = VDeclared of value_t ref | VTemp of value_t

val value_of_eval : eval_t -> value_t

val env_of_program : annotations_t tprogram_t -> program_env_t

val eval_expr : env_t -> annotations_t texpr_t -> env_t * eval_t

val eval_program : address -> id_t option -> annotations_t tprogram_t -> unit

val eval_networked_program : (address * id_t option) list -> annotations_t tprogram_t -> unit