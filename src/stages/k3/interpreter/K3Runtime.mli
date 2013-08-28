open K3.AST
open K3Streams
open K3Values

type scheduler_state

type breakpoint_t = {
  trigger : id_t;
  (* specify values for some args, others can be left out *)
  args : value_t;
  (* when the counter reaches 0, the breakpoint is activated *)
  counter : int;
  (* stop pre or post trigger *)
  post_trigger: bool;
}

type status_t = NormalExec | BreakPoint of breakpoint_t

(* send target_trig, addr, arg *)
val schedule_trigger : scheduler_state -> value_t -> value_t -> value_t -> unit

(* send target_trig, addr, arg, sender_address for *)
val buffer_trigger : scheduler_state -> value_t -> value_t -> value_t -> address -> unit

val schedule_event : scheduler_state -> resource_bindings_t -> id_t -> address -> value_t list -> unit

val initialize_scheduler : scheduler_state -> address -> program_env_t -> unit

(* Runtime control helpers *)

val node_has_work : scheduler_state -> address -> bool

val network_has_work : scheduler_state ->  bool

val run_scheduler : ?slice:int -> scheduler_state -> address -> program_env_t -> status_t

val init_scheduler_state : ?shuffle_tasks:bool -> ?breakpoints:breakpoint_t list
-> ?run_length:int64 -> unit -> scheduler_state

val use_shuffle_tasks : scheduler_state -> bool

val use_global_queueing : scheduler_state -> bool

val next_global_address : scheduler_state -> address
