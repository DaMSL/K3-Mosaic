(* K3 Stream FSM interpretation. *)
open K3.AST
open K3Values
open K3Streams
open K3Streams.ResourceFSM

type channel_impl_t =
  | In  of in_channel option
  | Out of out_channel option
  | InConst of expr_t list ref
  | InRand of int ref

type resource_impl_env_t = (id_t * channel_impl_t) list

val run_dispatcher :
  (resource_bindings_t -> input -> K3Values.Value.value_t list -> 'a)
  -> resource_env_t
  -> resource_impl_env_t
  -> dispatcher_t
  -> resource_impl_env_t

(* TODO:
val initialize_demultiplexer :
  resource_env_t -> resource_demultiplexer -> resource_impl_env_t

val run_demultiplexer :
  resource_demultiplexer -> state_id option -> id_t -> value_t
  -> value_t option * state_id option * id_t list
*)
