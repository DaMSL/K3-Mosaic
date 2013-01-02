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

val initialize_scheduler : address -> program_env_t -> unit

val run_scheduler : address -> program_env_t -> unit