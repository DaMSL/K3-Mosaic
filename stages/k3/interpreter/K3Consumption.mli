(* K3 Stream FSM interpretation. *)
open K3
open K3Values
open K3Typechecker
open K3Streams

val initialize : stream_fsm_t -> stream_fsm_t

val run :
  fsm_env_t -> stream_fsm_t -> state_id option
  -> value_t option * state_id option
