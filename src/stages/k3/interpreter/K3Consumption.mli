(* K3 Stream FSM interpretation. *)
open K3.AST
open K3Values
open K3Streams
open K3Streams.ResourceFSM

type channel_impl_t =
  | In  of in_channel option
  | Out of out_channel option

type resource_impl_env_t = (id_t * channel_impl_t) list 

val run_dispatcher :
  address
  -> resource_env_t -> resource_impl_env_t -> dispatcher_t
  -> resource_impl_env_t  

(* TODO:
val initialize_demultiplexer :
  resource_env_t -> resource_demultiplexer -> resource_impl_env_t

val run_demultiplexer :
  resource_demultiplexer -> state_id option -> id_t -> value_t
  -> value_t option * state_id option * id_t list  
*)