open K3.AST
open K3Values
open K3Values.Value
open K3Runtime

(* opaque type for interpreter state *)
type interpreter_t

val value_of_eval : eval_t -> value_t

val env_of_program :
  ?address:address ->
  ?json:Yojson.Safe.json ->
  role:id_t ->
  peers:K3Global.peer_t list ->
  type_aliases:(id_t, type_t) Hashtbl.t ->
  scheduler_state ->
  program_t ->
  env_t

(* expression evaluation can be done without a scheduler for micro evaluation *)
val eval_expr : address -> scheduler_state option -> env_t -> expr_t -> env_t * eval_t

(* Takes an interpreter and runs it, either initialized or after a breakpoint *)
val interpret_k3_program : interpreter_t -> (address * env_t) list

(* create an interpreter state to run a k3 program *)
val init_k3_interpreter :
  ?queue_type:K3Runtime.queue_type ->
  ?src_interval:float ->
  peers:K3Global.peer_t list ->
  load_path:string ->
  interp_file:string ->
  type_aliases:(id_t * type_t) list ->
  program_t ->
  interpreter_t

