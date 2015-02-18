open K3.AST
open K3Streams
open K3Values
open K3Values.Value

type scheduler_state

type queue_type = GlobalQ | PerNodeQ

(* send target_trig, addr, arg *)
val schedule_trigger : scheduler_state -> value_t -> value_t -> value_t -> unit

val schedule_event : scheduler_state -> resource_bindings_t -> id_t -> address -> value_t list -> unit

val sleep : scheduler_state -> address -> float -> unit

(* Runtime control helpers *)

val network_has_work : scheduler_state ->  bool

val run_scheduler : ?slice:int -> scheduler_state -> (address -> env_t) -> unit

val init_scheduler_state :
  ?queue_type:queue_type ->
  peers:K3Global.peer_t list ->
  scheduler_state


