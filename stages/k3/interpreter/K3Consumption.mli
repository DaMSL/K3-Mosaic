(* Dealing with Sources and Consumption. *)
open K3
open K3Values
open K3Typechecker

(* Opaque states, actions and FSMs *)
type state_id
type action_t
type stream_fsm_t

type stream_env_t = (id_t * stream_t) list
type fsm_env_t = (id_t * stream_fsm_t) list

type event_loop_t =
  stream_env_t * fsm_env_t * source_bindings_t * (instruction_t list)

val string_of_fsm : stream_fsm_t -> string

val string_of_stream_env : stream_env_t -> string
val string_of_fsm_env : fsm_env_t -> string

val compile : stream_env_t -> fsm_env_t -> stream_pattern_t -> stream_fsm_t

val initialize : stream_fsm_t -> stream_fsm_t

val run :
  fsm_env_t -> stream_fsm_t -> state_id option
  -> value_t option * state_id option

(* Returns a list of K3 roles and a default role if available *)
val roles_of_program :
  int tprogram_t -> (id_t * event_loop_t) list * event_loop_t option
