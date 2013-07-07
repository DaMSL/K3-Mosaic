open K3.AST
open K3Streams
open K3Values

type scheduler_state

(* send target_trig, addr, arg *)
val schedule_trigger : scheduler_state -> value_t -> value_t -> value_t -> unit

(* send target_trig, addr, arg, sender_address for *)
val buffer_trigger : scheduler_state -> value_t -> value_t -> value_t -> address -> unit

val schedule_event : scheduler_state -> resource_bindings_t -> id_t -> address -> value_t list -> unit

val configure_scheduler : scheduler_state -> int64 -> unit

val initialize_scheduler : scheduler_state -> address -> program_env_t -> unit

(* Runtime control helpers *)

val node_has_work : scheduler_state -> address -> bool

val network_has_work : scheduler_state ->  bool

val run_scheduler : ?slice:int -> scheduler_state -> address -> program_env_t -> unit

val init_scheduler_state : ?shuffle_tasks:bool -> unit -> scheduler_state

val use_shuffle_tasks : scheduler_state -> bool
