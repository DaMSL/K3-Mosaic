open K3.AST
open K3Values
open K3Values.Value
open K3Runtime

(* opaque type for interpreter state *)
type interpreter_t

val value_of_eval : eval_t -> value_t

val env_of_program : ?address:address -> role:id_t -> peers:K3Global.peer_t list -> scheduler_state -> program_t -> program_env_t

(* expression evaluation can be done without a scheduler for micro evaluation *)
val eval_expr : address -> scheduler_state option -> env_t -> expr_t -> env_t * eval_t

(* Takes an interpreter and runs it, either initialized or after a breakpoint *)
val interpret_k3_program : interpreter_t -> (address * program_env_t) list

(* create an interpreter state to run a k3 program *)
val init_k3_interpreter :
  ?queue_type:K3Runtime.queue_type ->
  peers:K3Global.peer_t list ->
  load_path:string ->
  ?src_interval:float ->
  program_t -> interpreter_t

