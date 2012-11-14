(* K3 stream patterns. *)
open K3.AST

(* resource id -> sink id *)
type resource_bindings_t = (id_t * id_t) list

(* resource id -> source, resource *)
type resource_env_t = (id_t * (bool * flow_resource_t)) list

(* Generic FSMs *)
module type FSMLabels = sig
  type input
  type output
  val string_of_input : input -> string
  val string_of_output : output -> string
end

module type FSMType = sig
  type input
  type output
  type state_id = int
  type action_t = Output of output | Terminate
  type transition_t = action_t * state_id 

	(* Matched transitions and actions, non-match transition and action *)
	type fsm_state = (input * transition_t) * transition_t  
	
	(* state id -> state metadata *)
	type fsm_t = (state_id * fsm_state) list
	
	(* fsm id -> fsm *)
	type fsm_env_t = (id_t * fsm_t) list

  val string_of_input : input -> string
  val string_of_output : output -> string

  val string_of_fsm     : fsm_t -> string
  val string_of_fsm_env : fsm_env_t -> string
end

module FSM : functor(Labels : FSMLabels) ->
  (FSMType with type input = Labels.input and type output = Labels.output) 

(* Resource pattern FSMs *)
module ResourceActions : sig
  type input = id_t
  
  type resource_output =
      (* sink dispatch list *) 
    | Dispatch of id_t list
    | Fail

  type output =
      (* Sources have pre-entry metadata as well a resource output *)
    | Source of resource_output * id_t list
    | Sink of resource_output 

  val string_of_input : input -> string
  val string_of_output : output -> string
end

module ResourceFSM : FSMType
      with type input = ResourceActions.input
      and  type output = ResourceActions.output

open ResourceFSM
type dispatcher_t = fsm_t
type dispatcher_env_t = fsm_env_t 

type event_loop_t =
  resource_env_t * resource_bindings_t *
  dispatcher_env_t * (instruction_t list)

(* Stringification *)
val string_of_resource_bindings : resource_bindings_t -> string
val string_of_resource_env      : resource_env_t -> string

val string_of_dispatcher     : dispatcher_t -> string
val string_of_dispatcher_env : dispatcher_env_t -> string

(* Accessors *)
val id_of_state : state_id * fsm_state -> state_id
val match_action_of_state : state_id * fsm_state -> action_t
val fail_action_of_state : state_id * fsm_state -> action_t
val pre_entry_of_state : state_id * fsm_state -> id_t list
  
val handle_of_resource :
  resource_env_t -> id_t
  -> (bool * type_t * channel_type_t * channel_format_t) option

val is_net_handle  : resource_env_t -> id_t -> bool  
val is_file_handle : resource_env_t -> id_t -> bool

(* Compilation *)
val compile_dispatcher :
  resource_env_t -> resource_bindings_t
  -> dispatcher_env_t -> resource_pattern_t
  -> dispatcher_t

(* Dispatcher processing *)
val resources_of_dispatcher : dispatcher_t -> input list

val initial_resources_of_dispatcher :
  dispatcher_t -> state_id * input list

(* Event loop helpers *)
val event_loop_of_flow : flow_program_t -> event_loop_t

(* Returns a list of K3 roles and a default role if available *)
val roles_of_program :
  program_t -> (id_t * event_loop_t) list * (id_t * event_loop_t) option

(* Returns a list of K3 roles prepended with flow programs present in the
 * K3 program, and a default role if available *) 
val extended_roles_of_program :
  program_t -> (id_t * event_loop_t) list * (id_t * event_loop_t) option

