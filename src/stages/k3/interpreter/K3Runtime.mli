open K3.AST
open K3Streams
open K3Values

type scheduler_state

val schedule_trigger : scheduler_state -> value_t -> value_t -> value_t -> unit

val schedule_event : scheduler_state -> resource_bindings_t -> id_t -> address -> value_t list -> unit

val configure_scheduler : scheduler_state -> int64 -> unit

val initialize_scheduler : scheduler_state -> address -> program_env_t -> unit

(* Runtime control helpers *)

val node_has_work : scheduler_state -> address -> bool

val network_has_work : scheduler_state ->  bool

val run_scheduler : ?slice:int -> scheduler_state -> address -> program_env_t -> bool -> unit

val init_scheduler_state : unit -> scheduler_state

