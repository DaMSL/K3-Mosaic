open K3.AST
open K3Streams
open K3Values

type queue_granularity_t = Global | PerTrigger

type scheduler_spec = {
    mutable mode : queue_granularity_t;
    mutable events_to_process : int64;
    mutable interleave_period : int;
  }

val scheduler_params : scheduler_spec

val schedule_trigger : value_t -> value_t -> value_t -> unit

val schedule_event : resource_bindings_t -> id_t -> address -> value_t list -> unit

val configure_scheduler : int64 -> unit

val initialize_scheduler : address -> program_env_t -> unit

(* Runtime control helpers *)

val node_has_work : address -> bool

val network_has_work : unit -> bool

val run_scheduler : ?slice:int -> address -> program_env_t -> unit