(* K3 stream patterns. *)
open K3.AST

type state_id = int

type channel_impl_t =
  | In  of in_channel option
  | Out of out_channel option

type action_t = 
  | AEndpoint   of id_t * type_t * stream_channel_t * channel_impl_t
  | ADerived    of id_t
  | AEOF

and stream_fsm_t = (state_id * (action_t * state_id * state_id)) list

type stream_env_t = (id_t * stream_t) list
type fsm_env_t = (id_t * stream_fsm_t) list

(* stream id -> trigger id *)
type source_bindings_t = (id_t * id_t) list

type event_loop_t =
  stream_env_t * fsm_env_t * source_bindings_t * (instruction_t list)

(* Stringification *)
val string_of_fsm : stream_fsm_t -> string
val string_of_stream_env : stream_env_t -> string
val string_of_fsm_env : fsm_env_t -> string
val string_of_source_bindings : source_bindings_t -> string

val compile : stream_env_t -> fsm_env_t -> stream_pattern_t -> stream_fsm_t

(* Returns a list of K3 roles and a default role if available *)
val roles_of_program :
  program_t -> (id_t * event_loop_t) list * (id_t * event_loop_t) option
