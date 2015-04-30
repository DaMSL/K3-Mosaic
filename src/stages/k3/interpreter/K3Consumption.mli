(* K3 Stream FSM interpretation. *)
open K3.AST
open K3Values
open K3Streams
open K3Streams.ResourceFSM

type dispatcher_t

type channel_impl_t =
  | In  of int ref * in_channel option (* count, channel *)
  | Out of out_channel option
  | InConst of expr_t list ref
  | InRand of int ref

type resource_impl_env_t = (id_t * channel_impl_t) list

val def_dispatcher : dispatcher_t

val is_running : dispatcher_t -> bool

val init_dispatcher : resource_env_t -> dispatcher_t -> fsm_t -> dispatcher_t

val run_step :
  (resource_bindings_t -> input -> K3Values.Value.value_t list -> 'a)
  -> dispatcher_t
  -> resource_env_t
  -> fsm_t
  -> bool

